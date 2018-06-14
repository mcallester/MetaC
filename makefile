# expandA

mcA.o : mc.h mcA.c
	gcc -g mcA.c -c

expandA.c : expandA.mc mcA.c
	cp expandA.mc expandA.c

expandA : mcA.o expandA.c
	gcc -g -o expandA mcA.o expandA.c -ldl -lm
#testA

testA.c : expandA testA.mc
	./expandA testA.mc testA.c

testA.o : testA.c
	gcc -g testA.c -c

# expandB

mcB.c : expandA mcB.mc
	 ./expandA mcB.mc mcB.c

mcB.o : mcB.c
	gcc -g mcB.c -c

expandB.c : expandA expandB.mc
	./expandA expandB.mc expandB.c

expandB : mcB.o expandB.c
	gcc -g -o expandB mcA.o mcB.o expandB.c -ldl -lm

#testB

testB.c : expandB testB.mc
	./expandB testB.mc testB.c

testB.o : testB.c
	gcc -g testB.c -c

#expandC

mcC.c : expandB mcC.mc
	./expandB mcC.mc mcC.c

mcC.o : mcC.c
	gcc -g mcC.c -c

expandC.c : expandB expandC.mc
	./expandB expandC.mc expandC.c

expandC : mcC.o expandC.c
	gcc -g -o expandC mcA.o mcB.o mcC.o expandC.c -ldl -lm

#testC

testC.c : expandB testC.mc
	./expandB testC.mc testC.c

testC.o : testC.c
	gcc -g testC.c -c

#expandD

mcD.c :  expandC mcD.mc
	./expandC mcD.mc mcD.c

mcD.o : mcD.c
	gcc -g mcD.c -c

expandD.c :  expandC expandD.mc
	./expandC expandD.mc expandD.c

expandD : mcD.o expandD.c
	gcc -g -o expandD mcA.o mcB.o mcC.o mcD.o expandD.c -ldl -lm

#testD

testD.c : expandB testD.mc
	./expandB testD.mc testD.c

testD.o : testD.c
	gcc -g testD.c -c

#expandE

mcE.c :  expandD mcE.mc
	./expandD mcE.mc mcE.c

mcE.o : mcE.c
	gcc -g mcE.c -c

expandE.c :  expandD expandE.mc
	./expandD expandE.mc expandE.c

expandE : mcE.o expandE.c base_decls.h
	gcc -g -o expandE mcA.o mcB.o mcC.o mcD.o mcE.o expandE.c -ldl -lm

#testE

testE.c : expandE testE.mc
	./expandE testE.mc testE.c

testE.o : testE.c
	gcc -g testE.c -c

#REPL

REPL.c : REPL.mc expandE
	./expandE REPL.mc REPL.c

REPL : REPL.c
	gcc -g -o REPL mcA.o mcB.o mcC.o mcD.o mcE.o REPL.c -ldl -lm

