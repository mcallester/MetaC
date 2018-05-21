mccA.c : mccA.mcc
	cp mccA.mcc mccA.c

# expandA

mccA.o : mcc.h mccA.c
	gcc -g mccA.c -c

expandA.c : expandA.mcc mccA.c
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

#testB

testB.c : expandB testB.mcc
	./expandB testB.mcc testB.c

testB.o : testB.c
	gcc -g testB.c -c

#expandC

mccC.c : expandB mccC.mcc
	./expandB mccC.mcc mccC.c

mccC.o : mccC.c
	gcc -g mccC.c -c

expandC.c : expandB expandC.mcc
	./expandB expandC.mcc expandC.c

expandC : mccC.o expandC.c
	gcc -g -o expandC mccA.o mccB.o mccC.o expandC.c -ldl -lm

#testC

testC.c : expandB testC.mcc
	./expandB testC.mcc testC.c

testC.o : testC.c
	gcc -g testC.c -c

#expandD

mccD.c :  expandC mccD.mcc
	./expandC mccD.mcc mccD.c

mccD.o : mccD.c
	gcc -g mccD.c -c

expandD.c :  expandC expandD.mcc
	./expandC expandD.mcc expandD.c

expandD : mccD.o expandD.c
	gcc -g -o expandD mccA.o mccB.o mccC.o mccD.o expandD.c -ldl -lm

#testD

testD.c : expandB testD.mcc
	./expandB testD.mcc testD.c

testD.o : testD.c
	gcc -g testD.c -c

#expandE

mccE.c :  expandD mccE.mcc
	./expandD mccE.mcc mccE.c

mccE.o : mccE.c
	gcc -g mccE.c -c

expandE.c :  expandD expandE.mcc
	./expandD expandE.mcc expandE.c

expandE : mccE.o expandE.c
	gcc -g -o expandE mccA.o mccB.o mccC.o mccD.o mccE.o expandE.c -ldl -lm

#testE

testE.c : expandB testE.mcc
	./expandB testE.mcc testE.c

testE.o : testE.c
	gcc -g testE.c -c

#REPL

REPL.c : REPL.mcc expandD base_decls.h
	./expandD REPL.mcc REPL.c

REPL.o : REPL.c
	gcc -g REPL.c -c

REPL : REPL.o
	gcc -g -o REPL mccA.o mccB.o mccC.o mccD.o REPL.o -ldl -lm

