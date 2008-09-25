;;; otr-lib.el --- bindings for off-the-record messaging

;; Copyright (C) 2008  Anselm Helbig

;; Author: Anselm Helbig <anselm.helbig@googlemail.com>
;; Keywords: comm, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `otr-lib' provides bindings to libotr, the client library for
;; off-the-record messaging.
;;
;; One part of it is a small program written in C that reads symbolic
;; expressions (sexps) from stdin, translates them into libotr library
;; calls and converts the results back into sexps.
;;
;; This emacs library takes care of starting the libotr proxy, sending
;; properly formed requests to it, parsing the results and executing
;; all necessary callbacks.
;;
;; Here is an example of how you might use it in your programs
;; 
;;   (add-to-list 'load-path "your/lisp/path")
;;   (setq otr-program "path/to/emacs-otr")
;; 
;;   (require 'otr-lib)
;; 
;;   (defvar otr-private-key-filename "~/.emacs-otr/private-key"
;;     "Where to store private keys")
;;   (defvar otr-fingerprint-filename "~/.emacs-otr/fingerprint"
;;     "Where to store fingerprints ")
;; 
;;   (defmacro otr-ui-stub (name)
;;     `(defun ,name (&rest args)
;;        (message "received \"%s\" with args %S" ',name args)
;;        nil))
;;   (defun otr-policy (context)
;;     '(allow_v1 allow_v2 require_encryption send_whitespace_tag whitespace_start_ake))
;;   (defun otr-display-otr-message (accountname protocol username msg)
;;     (message "Message for account %s, protocol %s, username: %s, message: %s"
;;              accountname protocol username msg)
;;     nil)
;;   (otr-ui-stub otr-is-logged-in)
;;   (otr-ui-stub otr-update-context-list)
;; 
;;   (setq otr-ui-callbacks
;;         '((update-context-list . otr-update-context-list)
;;           (policy              . otr-policy)
;;           (display-otr-message . otr-display-otr-message)
;;           (is-logged-in        . otr-is-logged-in)))
;; 
;;   (otr-start)
;;   (otr-eval `(privkey-read              ,(expand-file-name otr-private-key-filename)))
;;   (otr-eval `(privkey-read-fingerprints ,(expand-file-name otr-fingerprint-filename))))
;;   (otr-eval `(privkey-generate          ,(expand-file-name otr-private-key-filename) "my-account" "protocol"))
;; 
;;   ;; returns the message to send instead
;;   (otr-eval `(message-sending "my-account" "protocol" "recipient-account" "Here goes the message!"))
;;   ;; returns the decoded message
;;   (otr-eval `(message-receiving "my-account" "protocol" "sender-account" "some possibly encrypted message"))

;;; Code:

(require 'cl)

(defvar otr-program "emacs-otr"
  "Path to the libotr proxy executable")

;; process handling

(defvar otr-process nil
  "Process object of the libotr proxy")
(defun otr-start ()
  ""
  (message "[OTR] starting process")
  (unless (otr-process-running-p)
    (setq otr-request-running nil
          otr-request-queue nil
          otr-process 
          (start-process "otr-process" "*otr-process*" otr-program))
    (set-process-filter otr-process 'otr-output-filter)))

(defun otr-stop ()
  "Stop the libotr proxy process"
  (process-send-eof otr-process))
  
(defun otr-process-running-p () 
  "Determine if the libotr proxy is running"
  (and otr-process 
       (eq 'run (process-status otr-process))))

(defun otr-send-form (form)
  "Send a sexp to the libotr proxy"
  (let ((string (concat (prin1-to-string form) "\n")))
    (message "[OTR] sending %S" string)
    (process-send-string otr-process string)))

;; queuing

(defvar otr-request-queue nil
  "A FIFO holding requests to the libotr proxy.

This is a list with every item being a cons, the car being the
form to be sent and the cdr the function to call with the result
when the request finishes.")

(defun otr-eval-async (form callback)
  "Asynchronously evaluate FORM. 

Let the otr proxy asynchronously evaluate FORM, call CALLBACK
with the result when finished."
  (setq otr-request-queue
        (nconc otr-request-queue (list (cons form callback))))
  (otr-start-queue))


(defvar otr-request-running nil
  "If non-nil, a request is running. 

We should not send another request until this one has finished.")

(defun otr-start-queue ()
  "Start a new request unless another one is still running."
  (when (and (not otr-request-running)
             otr-request-queue)
    (setq otr-request-running t)
    (otr-send-form (car (car otr-request-queue)))))

;; processing output

(defvar otr-output-filters 
  '(otr-copy-output-to-process-buffer otr-process-output)
  "Filters for the libotr proxy process")
(defun otr-output-filter (proc string)
  "Call all output filters on the incoming string"
  (mapc (lambda (f) (apply f (list proc string)))
        otr-output-filters))

(defun otr-copy-output-to-process-buffer (proc string)
  "Copy output to process buffer, for debugging purposes."
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
        (goto-char (process-mark proc))
        (insert (concat string "\n"))
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defvar otr-response-buffer ""
  "A string of incoming characters to be processed.

Needed if we should ever receive an incomplete response.")

(defvar otr-ui-callbacks
  '()
  "Alist holding the UI callbacks.")

(defun otr-process-output (proc string)
  "Handle incoming output from libotr proxy."
  (setq otr-response-buffer (concat otr-response-buffer string))
  (let ((raw-result (condition-case nil
                        (read-from-string otr-response-buffer)
                    (end-of-file nil))))
      (when raw-result
        (let* ((result (car raw-result))
               (chars-read (cdr raw-result))
               (type (result-type result))
               (value (result-value result)))
          (setq otr-response-buffer (substring otr-response-buffer chars-read))
          (unless otr-request-running 
            (error "Got unrequested response: %s" result))
          (case type
            ('result 
             (setq otr-request-running nil)
             (funcall (cdr (car otr-request-queue)) value)
             (setq otr-request-queue (cdr otr-request-queue))        
             (otr-start-queue))
            ('callback
             (let ((callback (cdr (assoc (car value)
                                         otr-ui-callbacks))))
               (unless callback
                 (error "callback %s not found!" (car value)))
               (otr-send-form (apply callback (cdr value)))))
            (t
             (error "Unexpected form: %S" result-form)))))))
  
(defun otr-result-value (result)
  (cdr result))
(defun otr-result-type (result)
  (car result))

;; synchronous evaluation

(defun otr-eval (request)
  "Evaluate request form synchronously."
  (lexical-let ((eval-done nil)
		(result nil))
    (otr-eval-async request (lambda (result-form)
                              (setq eval-done t
                                    result result-form)))
    (while (not eval-done) 
      (sit-for .01))
    result))

(provide 'otr-lib)
;;; otr-lib.el ends here

