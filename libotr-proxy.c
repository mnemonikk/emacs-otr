#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <libotr/proto.h> 
#include <libotr/message.h> 
#include <libotr/privkey.h> 
#include <lispreader.h>

#define Bool int
#define True 1
#define False 0

/* global variables */

lisp_stream_t stream;
OtrlUserState userstate;
Bool exit_flag = False;

/* {{{ data structure helpers */

/* make_list */
lisp_object_t *_vmake_list(lisp_object_t *first, va_list ap) {
  lisp_object_t *list, *last, *next, *current;
  list = lisp_make_cons(first, lisp_nil());
  last = list;
  while ((void*) (next = va_arg(ap, lisp_object_t*)) != (void*) -1L) {
    current = lisp_make_cons(next, lisp_nil());
    last->v.cons.cdr = current;
    last = current;
  }
  return list;
}
lisp_object_t *_make_list(lisp_object_t *first, ...) {
  va_list ap;
  lisp_object_t *result;
  va_start(ap, first);
  result = _vmake_list(first, ap);
  va_end(ap);
  return result;
}
#define make_list(...) _make_list(__VA_ARGS__, (void*) -1L)

/* make_alist */
lisp_object_t *_vmake_alist(const char *key, lisp_object_t *val, va_list ap) {
  lisp_object_t *list, *last, *current;
  const char *next_key;
  list = lisp_make_cons(lisp_make_cons(lisp_make_symbol(key), val), lisp_nil());
  last = list;
  while ((void*) (next_key = va_arg(ap, const char*)) != (void*) -1L) {
    current = lisp_make_cons(lisp_make_cons(lisp_make_symbol(next_key),
                                            va_arg(ap, lisp_object_t*)), lisp_nil());
    last->v.cons.cdr = current;
    last = current;
  }
  return list;
}
lisp_object_t *_make_alist(const char *key, lisp_object_t *val, ...) {
  va_list ap;
  lisp_object_t *result;
  va_start(ap, val);
  result = _vmake_alist(key, val, ap);
  va_end(ap);
  return result;
}
#define make_alist(...) _make_alist(__VA_ARGS__, (void*) -1L)


/* flag handling */
char *policy_flags[] = {"allow_v1", "allow_v2", "require_encryption", 
                        "send_whitespace_tag", "whitespace_start_ake", 
                        "error_start_ake", NULL};

lisp_object_t *flags_to_sexp(int flags, char **translation) {
  lisp_object_t *result = lisp_nil();
  while (*translation != NULL) { 
    printf("flag %s\n", *translation);
    if (flags & 1) {
      result = lisp_make_cons(lisp_make_symbol(*translation), result);
    }
    flags = flags >> 1;
    translation++;
  }
  return result;
}

int sexp_to_flags(lisp_object_t *sx, char **translation) {
  int flags = 0;
  while (!lisp_nil_p(sx)) {
    int i = 0;
    char **temp = translation; 
    while (*temp != NULL) {
      if (strcmp(*temp, lisp_symbol(lisp_car(sx))) == 0) {
        flags = flags | (1 << i); break;
      }
      temp++; i++;
    }
    sx = lisp_cdr(sx);
  }
  return flags;
}

lisp_object_t *lisp_make_conn_context(ConnContext *context) {
  return make_alist("username", lisp_make_string(context->username),
                    "accountname", lisp_make_string(context->accountname),
                    "protocol", lisp_make_string(context->protocol));
}

lisp_object_t *lisp_make_notify_level(OtrlNotifyLevel level) {
  switch (level) {
  case OTRL_NOTIFY_ERROR:
    return lisp_make_symbol("error");
  case OTRL_NOTIFY_WARNING:
    return lisp_make_symbol("warning");
  case OTRL_NOTIFY_INFO:
    return lisp_make_symbol("info");
  default:
    return lisp_nil();
  }
}  

/* }}} */
/* {{{ UI helper functions */

lisp_object_t *_make_question(const char *name, ...) {
  va_list ap;
  lisp_object_t *result;
  va_start(ap, name);
  result = lisp_make_cons(lisp_make_symbol("callback"), _vmake_list(lisp_make_symbol(name), ap));
  va_end(ap);
  return result;
}
#define make_question(...) _make_question(__VA_ARGS__, (void*) -1L)

lisp_object_t *ui_ask(lisp_object_t *question) {
  lisp_dump(question, stdout);
  lisp_free(question);
  return lisp_read(&stream);
}

void ui_tell(lisp_object_t *question) {
  lisp_free(ui_ask(question));
}

/* }}} */
/* {{{ UI callbacks */

/* Return the OTR policy for the given context. */
OtrlPolicy ui_policy(void *opdata, ConnContext *context) {
  OtrlPolicy result;
  lisp_object_t *answer =
    ui_ask(make_question("policy",
                         lisp_make_conn_context(context)));
  result = sexp_to_flags(answer, policy_flags);
  lisp_free(answer);
  return result;
}

/* Create a private key for the given accountname/protocol if
 * desired. */
void ui_create_privkey(void *opdata, const char *accountname,
                       const char *protocol) {
  lisp_object_t *answer =
    ui_ask(make_question("create-privkey",
                     lisp_make_string(accountname),
                     lisp_make_string(protocol)));
  lisp_free(answer);
}

/* Report whether you think the given user is online.  Return 1 if
 * you think he is, 0 if you think he isn't, -1 if you're not sure.
 *
 * If you return 1, messages such as heartbeats or other
 * notifications may be sent to the user, which could result in "not
 * logged in" errors if you're wrong. */
int ui_is_logged_in(void *opdata, const char *accountname,
                    const char *protocol, const char *recipient) {
  int result;
  lisp_object_t *answer = 
    ui_ask(make_question("is-logged-in",
                         lisp_make_string(accountname),
                         lisp_make_string(protocol),
                         lisp_make_string(recipient)));
  if (lisp_nil_p(answer)) 
    result = 0;
  else if (lisp_symbol_p(answer) && strcmp(lisp_symbol(answer), "maybe") == 0) 
    result = -1;
  else 
    result = 1;

  lisp_free(answer);

  return result;
}

/* Send the given IM to the given recipient from the given
 * accountname/protocol. */
void ui_inject_message(void *opdata, const char *accountname,
                       const char *protocol, const char *recipient, const char *message) {
  ui_tell(make_question("inject-message",
                        lisp_make_string(accountname),
                        lisp_make_string(protocol),
                        lisp_make_string(recipient),
                        lisp_make_string(message)));
}

/* Display a notification message for a particular accountname /
 * protocol / username conversation. */
void ui_notify(void *opdata, OtrlNotifyLevel level,
               const char *accountname, const char *protocol,
               const char *username, const char *title,
               const char *primary, const char *secondary) {
  ui_tell(make_question("inject-message",
                        lisp_make_notify_level(level),
                        lisp_make_string(accountname),
                        lisp_make_string(protocol),
                        lisp_make_string(username),
                        lisp_make_string(title),
                        lisp_make_string(primary),
                        lisp_make_string(secondary)));
}

/* Display an OTR control message for a particular accountname /
 * protocol / username conversation.  Return 0 if you are able to
 * successfully display it.  If you return non-0 (or if this
 * function is NULL), the control message will be displayed inline,
 * as a received message, or else by using the above notify()
 * callback. */
int ui_display_otr_message(void *opdata, const char *accountname,
                           const char *protocol, const char *username, const char *msg) {
  int result;
  lisp_object_t *answer = 
    ui_ask(make_question("display-otr-message",
                          lisp_make_string(accountname),
                          lisp_make_string(protocol),
                          lisp_make_string(username),
                          lisp_make_string(msg)));
  result = lisp_nil_p(answer) ? 0 : 1;
  lisp_free(answer);
  return result;
}

/* When the list of ConnContexts changes (including a change in
 * state), this is called so the UI can be updated. */
void ui_update_context_list(void *opdata) {
  ui_tell(make_question("update-context-list"));
}

/* Return a newly allocated string containing a human-friendly name
 * for the given protocol id */
const char *ui_protocol_name(void *opdata, const char *protocol) {
  const char *result;
  lisp_object_t *answer = ui_ask(make_question("protocol-name", lisp_make_string(protocol)));
  result = lisp_string(answer);
  lisp_free(answer);
  return result;
}

/* Deallocate a string allocated by protocol_name */
void ui_protocol_name_free(void *opdata, const char *protocol_name) {
  free(&protocol_name);
}

/* A new fingerprint for the given user has been received. */
void ui_new_fingerprint(void *opdata, OtrlUserState us,
                        const char *accountname, const char *protocol,
                        const char *username, unsigned char fingerprint[20]) {
  /* TODO: add lisp_make_userstate; find lisp representation for fingerprint */
  ui_tell(make_question("new-fingerprint",
                        lisp_make_string(accountname),
                        lisp_make_string(protocol),
                        lisp_make_string(username)));
}

/* The list of known fingerprints has changed.  Write them to disk. */
void ui_write_fingerprints(void *opdata) {
  ui_tell(make_question("write-fingerprints"));
}

/* A ConnContext has entered a secure state. */
void ui_gone_secure(void *opdata, ConnContext *context) {
  ui_tell(make_question("gone-secure",
                        lisp_make_conn_context(context)));
}

/* A ConnContext has left a secure state. */
void ui_gone_insecure(void *opdata, ConnContext *context) {
  ui_tell(make_question("gone-insecure", lisp_make_conn_context(context)));
}

/* We have completed an authentication, using the D-H keys we
 * already knew.  is_reply indicates whether we initiated the AKE. */
void ui_still_secure(void *opdata, ConnContext *context, int is_reply) {
  ui_tell(make_question("still-secure",
                        lisp_make_conn_context(context), 
                        is_reply ? lisp_make_symbol("t") : lisp_nil()));
}

/* Log a message.  The passed message will end in "\n". */
void ui_log_message(void *opdata, const char *message) {
  ui_tell(make_question("log-message", 
                        lisp_make_string(message)));
}

/* Find the maximum message size supported by this protocol. */
int ui_max_message_size(void *opdata, ConnContext *context) { 
  int result;
  lisp_object_t *answer =
    ui_ask(make_question("max-message-size",
                         lisp_make_conn_context(context)));
  result = lisp_integer(answer);
  lisp_free(answer);
  return result;
}

/* Return a newly allocated string containing a human-friendly
 * representation for the given account */
const char *ui_account_name(void *opdata, const char *account, 
                            const char *protocol) {
  const char *result;
  lisp_object_t *answer =
    ui_ask(make_question("account-name", 
                         lisp_make_string(account),
                         lisp_make_string(protocol)));
  result = lisp_string(answer);
  lisp_free(answer);
  return result;
}

/* Deallocate a string returned by account_name */
void ui_account_name_free(void *opdata, const char *account_name) {
  free(&account_name);
}

OtrlMessageAppOps ui_ops = {
  ui_policy,
  ui_create_privkey,
  ui_is_logged_in,
  ui_inject_message,
  ui_notify,
  ui_display_otr_message,
  ui_update_context_list,
  ui_protocol_name,
  ui_protocol_name_free,
  ui_new_fingerprint,
  ui_write_fingerprints,
  ui_gone_secure,
  ui_gone_insecure,
  ui_still_secure,
  ui_log_message,
  ui_max_message_size,
  ui_account_name,
  ui_account_name_free  
};
  
/* }}} */
/* {{{ primary commands */

lisp_object_t *quit(lisp_object_t *sx) {
  exit_flag = True;
  return lisp_nil();
}

/* otrl_privkey_read(userstate, privkeyfilename); */
lisp_object_t *_otrl_privkey_read(lisp_object_t *sx) {
  otrl_privkey_read(userstate, lisp_string(lisp_car(sx)));
  return lisp_nil();
};

/* otrl_privkey_read_fingerprints(userstate, fingerprintfilename,
   add_app_info, add_app_info_data); */
lisp_object_t *_otrl_privkey_read_fingerprints(lisp_object_t *sx) {
  otrl_privkey_read_fingerprints(userstate, lisp_string(lisp_car(sx)), NULL, NULL);
  return lisp_nil();
}

lisp_object_t *_otrl_message_sending(lisp_object_t *sx) {
  gcry_error_t err;
  char *newmessage = NULL;
  lisp_object_t *result = lisp_nil();

  const char *accountname = lisp_string(lisp_car(sx)); sx = lisp_cdr(sx);
  const char *protocolid  = lisp_string(lisp_car(sx)); sx = lisp_cdr(sx);
  const char *recipient   = lisp_string(lisp_car(sx)); sx = lisp_cdr(sx);
  const char *message     = lisp_string(lisp_car(sx)); 

  err = otrl_message_sending(userstate, &ui_ops, NULL, accountname,
                             protocolid, recipient, message, NULL, &newmessage,
                             NULL, NULL);
  /* if (err != 0) { ... TODO: error handling */
  if (newmessage != NULL) {
    result = lisp_make_string(newmessage);
    otrl_message_free(newmessage);
  }
  return result;
}

lisp_object_t *_otrl_message_receiving(lisp_object_t *sx) {
    int ignore_message;
    char *newmessage = NULL;
    lisp_object_t *result;

    const char *accountname = lisp_string(lisp_car(sx)); sx = lisp_cdr(sx);
    const char *protocolid  = lisp_string(lisp_car(sx)); sx = lisp_cdr(sx);
    const char *sender_name = lisp_string(lisp_car(sx)); sx = lisp_cdr(sx);
    const char *message     = lisp_string(lisp_car(sx)); 
    ignore_message = otrl_message_receiving(userstate, &ui_ops, NULL,
                                            accountname, protocolid, sender_name, message, &newmessage,
                                            NULL, NULL, NULL);
    if (ignore_message)
      result = lisp_make_symbol("ignore");
    else if (newmessage != NULL) {
      result = lisp_make_string(newmessage);
      otrl_message_free(newmessage);
    } else
      result = lisp_nil();
    return result;
}

lisp_object_t *_otrl_privkey_generate(lisp_object_t *sx) {
  gcry_error_t err;
  const char *filename    = lisp_string(lisp_car(sx)); sx = lisp_cdr(sx);
  const char *accountname = lisp_string(lisp_car(sx)); sx = lisp_cdr(sx);
  const char *protocol    = lisp_string(lisp_car(sx)); 
  err = otrl_privkey_generate(userstate, filename, accountname, protocol);
  return (err == 0 ? lisp_make_symbol("t") : lisp_nil());
}

typedef struct {
  const char *name;
  lisp_object_t *(*func)(lisp_object_t *sx);
} Command;

static Command commands[] = {
  { "quit", quit },
  { "privkey-read", _otrl_privkey_read },
  { "privkey-read-fingerprints", _otrl_privkey_read_fingerprints },
  { "privkey-generate", _otrl_privkey_generate },
  { "message-sending", _otrl_message_sending },
  { "message-receiving", _otrl_message_receiving },
  { NULL, NULL } 
};

/* }}} */
/* {{{ eval */

lisp_object_t *eval(lisp_object_t *form) {
  Command *temp;
  
  if (!lisp_cons_p(form)) return NULL;
  temp = commands;
  while ((*temp).name != NULL) {
    if (strcmp((*temp).name, lisp_symbol(lisp_car(form))) == 0) {
      return (*temp).func(lisp_cdr(form));
    }
    temp++;
  }
  return lisp_make_symbol("function-missing");
}

/* }}} */
/* {{{ main */

int main(int argc, char **argv) {
  lisp_object_t *sx, *result;

  OTRL_INIT;
  userstate = otrl_userstate_create();
  lisp_stream_init_file(&stream, stdin);

  while (exit_flag == False) {
    sx = lisp_read(&stream);
    if (lisp_type(sx) == LISP_TYPE_EOF) {
        lisp_free(sx); break;
    }
    result = lisp_make_cons(lisp_make_symbol("result"), eval(sx));
    lisp_dump(result, stdout); /* printf("\n"); */
    lisp_free(result);
  }

  otrl_userstate_free(userstate);
  
  exit(0);
}

/* }}} */
