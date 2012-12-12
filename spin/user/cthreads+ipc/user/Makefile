#
# HISTORY
# 19-Jun-95  Emin Gun Sirer (egs) at the University of Washington
#	Created. Makefile for test programs. Generic make is braindead.
#
CC=cc
CFLAGS=-g -Wc,-nofloat
OFILES = crt0.o cthreads.o io.o sync.o sync2.o rpc.o
LDFLAGS=-Wc,-nofloat -g -non_shared -e start
ALL=tester smallserver smallclient bigserver bigclient syncer \
	gploop \
	tester0 tester1 tester2 tester3 tester4 tester5 \
	tester6 tester7 tester8 tester9 tester10 tester11

all: ${ALL}

tester: ${OFILES} tester.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

tester0: ${OFILES} tester0.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

tester1: ${OFILES} tester1.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

tester2: ${OFILES} tester2.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

tester3: ${OFILES} tester3.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

tester4: ${OFILES} tester4.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

tester5: ${OFILES} tester5.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

tester6: ${OFILES} tester6.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

tester7: ${OFILES} tester7.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

tester8: ${OFILES} tester8.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

tester9: ${OFILES} tester9.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

tester10: ${OFILES} tester10.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

tester11: ${OFILES} tester11.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

syncer: ${OFILES} syncer.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

smallserver: ${OFILES} smallserver.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

smallclient: ${OFILES} smallclient.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

bigserver: ${OFILES} bigserver.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

bigclient: ${OFILES} bigclient.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

gploop: gploop.o crt0.o
	${CC} ${LDFLAGS} -o $@ $^ || rm $@

clean::
	rm -f *.o *.so *~ ${ALL}

clobber:	clean
	rm -f rpc.c
	rm -rf ../ALPHA_OSF

crt0.o:	crt0.s
	cc ${CFLAGS} -c crt0.s

cthreads.o: cthreads.s
	cc ${CFLAGS} -c cthreads.s

io.o:	io.s
	cc ${CFLAGS} -c io.s

sync2.o:	sync2.s
	cc ${CFLAGS} -c sync2.s

rpc.c: ../src/RPC.i3 ../src/RPC.m3
	cd .. && m3build
	./convert ../ALPHA_OSF/RPC.io ../ALPHA_OSF/RPC.mo >rpc.c
