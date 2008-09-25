OPTFLAGS   = -O3 
PROFFLAGS  = -g
DEBUGFLAGS = -g -D_DEBUG_ -O0
CFLAGS     = -Wall

INCPATH=-I.
LISPREADER_OBJS=lispreader.o allocator.o pools.o
LIBS=-lotr $(LISPREADER_OBJS)

LINK_FLAGS=$(LIBS)

SOURCES=libotr-proxy.c
EXECUTABLES=$(SOURCES:%.c=%)

all: COMPILE_FLAGS=$(CFLAGS) $(LIBPATH) $(INCPATH)
all: $(EXECUTABLES)

%.o : %.c
	$(CC) $(COMPILE_FLAGS) -c $<

libotr-proxy: libotr-proxy.c $(LISPREADER_OBJS)
	$(CC) -o $@ $(DEBUGFLAGS) $(COMPILE_FLAGS) $@.c $(LINK_FLAGS)

clean:
	rm -f libotr-proxy $(LISPREADER_OBJS)

install: 
	install libotr-proxy /usr/local/bin