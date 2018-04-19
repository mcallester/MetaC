mccA.c : mccA.mcc
	cp mccA.mcc mccA.c

# expandA

mccA.o : mcc.h mccA.c
	gcc -g mccA.c -c

expandA.c : expandA.mcc
	cp expandA.mcc expandA.c

expandA : mccA.o expandA.c
	gcc -g -o expandA mccA.o expandA.c -ldl -lm

# expandB

mccB.c : expandA mccB.mcc
	 ./expandA mccB.mcc mccB.c

mccB.o : mccB.c
	gcc -g mccB.c -c

expandB.c : expandA expandB.mcc
	./expandA expandB.mcc expandB.c

expandB : mccB.o expandB.c
	gcc -g -o expandB mccA.o mccB.o expandB.c -ldl -lm

#expandC

mccC.c : expandB mccC.mcc
	 ./expandB mccC.mcc mccC.c

mccC.o : mccC.c
	gcc -g mccC.c -c

expandC.c : expandB expandC.mcc
	./expandB expandC.mcc expandC.c

expandC : mccC.o expandC.c
	gcc -g -o expandC mccA.o mccC.o expandC.c -ldl -lm

#expandC2

mccC2.c : expandC mccC2.mcc
	 ./expandC mccC2.mcc mccC2.c

mccC2.o : mccC2.c
	gcc -g mccC2.c -c

expandC2.c : expandC expandC2.mcc
	./expandC expandC2.mcc expandC2.c

expandC2 : mccC2.o expandC2.c
	gcc -g -o expandC2 mccA.o mccC.o mccC2.o expandC2.c -ldl -lm

#expandD

mccD.c : mccD.mcc expandC2
	./expandC2 mccD.mcc mccD.c

mccD.o : mccD.c
	gcc -g mccD.c -c

expandD.c : expandC2 expandD.mcc
	./expandC2 expandD.mcc expandD.c

expandD : mccD.o expandD.c
	gcc -g -o expandD mccA.o mccC.o mccC2.o mccD.o expandD.c -ldl -lm

#test

test.c : expandD test.mcc
	./expandD test.mcc test.c

test.o : test.c
	gcc -g test.c -c

#expandE

mccE.c : mccE.mcc expandD
	./expandD mccE.mcc mccE.c

mccE.o : mccE.c
	gcc -g mccE.c -c

expandE.c : expandE.mcc expandD
	./expandD expandE.mcc expandE.c

expandE : mccE.o expandE.c
	gcc -g -o expandE mccA.o mccC.o mccC2.o mccD.o mccE.o expandE.c -ldl -lm

#mccrep

REPL.c : REPL.mcc expandE base_decls.h
	./expandE REPL.mcc REPL.c

REPL.o : REPL.c
	gcc -g REPL.c -c

REPL : REPL.o
	gcc -g -o REPL mccA.o mccC.o mccC2.o mccD.o mccE.o REPL.o -ldl -lm
