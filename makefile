gcc := gcc -O0

# expandA  mcA implements universal syntax and backquote.  expandA expands backquote.

mcA.o : premacros.h mc.h mcA.c premacros.h
	${gcc} -g mcA.c -c

expandA : mcA.o expandA.c
	${gcc} -g -o expandA mcA.o expandA.c -ldl -lm

#testA

testA.c : expandA testA.mc
	./expandA testA.mc testA.c

testA.o : testA.c
	${gcc} -g testA.c -c

# expandB mcB implements pattern matching ucase in terms of backquote.  expandB expands both ucase and backquote.

mcB.c : expandA mcB.mc
	 ./expandA mcB.mc mcB.c

mcB.o : mcB.c
	${gcc} -g mcB.c -c

expandB.c : expandA expandB.mc
	./expandA expandB.mc expandB.c

expandB : mcB.o expandB.c
	${gcc} -g -o expandB mcA.o mcB.o expandB.c -ldl -lm

#testB

testB.c : expandB testB.mc
	./expandB testB.mc testB.c

testB.o : testB.c
	${gcc} -g testB.c -c

#expandC  expandC expands macro definition (umacro) as well as ucase and backquote.

mcC.c : expandB mcC.mc
	./expandB mcC.mc mcC.c

mcC.o : mcC.c
	${gcc} -g mcC.c -c

expandC.c : expandB expandC.mc
	./expandB expandC.mc expandC.c

expandC : mcC.o expandC.c
	${gcc} -g -o expandC mcA.o mcB.o mcC.o expandC.c -ldl -lm

#testC

testC.c : expandC testC.mc
	./expandC testC.mc testC.c

testC.o : testC.c
	${gcc} -g testC.c -c


#expandD  expandD expands some additional generic macros --- push, dolist and sformat.

mcD.c :  expandC mcD.mc
	./expandC mcD.mc mcD.c

mcD.o : mcD.c
	${gcc} -g mcD.c -c

expandD.c :  expandC expandD.mc
	./expandC expandD.mc expandD.c

expandD : mcD.o expandD.c
	${gcc} -g -o expandD mcA.o mcB.o mcC.o mcD.o expandD.c -ldl -lm

#testD

testD.c : expandD testD.mc
	./expandD testD.mc testD.c

testD.o : testD.c
	${gcc} -g testD.c -c

#expandD2

mcD2.c :  expandD mcD2.mc
	./expandD mcD2.mc mcD2.c

mcD2.o : mcD2.c
	${gcc} -g mcD2.c -c

expandD2.c :  expandD expandD.mc
	./expandD expandD2.mc expandD2.c

expandD2 : mcD2.o expandD2.c
	${gcc} -g -o expandD2 mcA.o mcB.o mcC.o mcD.o mcD2.o expandD2.c -ldl -lm

#expandE mcE provides code for the REPL aand dynamic linking.  expandE only provides one additional macro, install_base, for installing the base symbols.

mcE.c :  expandD2 mcE.mc
	./expandD2 mcE.mc mcE.c

mcE.o : mcE.c
	${gcc} -g mcE.c -c

expandE.c :  expandD2 expandE.mc
	./expandD2 expandE.mc expandE.c

expandE : mcE.o expandE.c base_decls.h
	${gcc} -g -o expandE mcA.o mcB.o mcC.o mcD.o mcD2.o mcE.o expandE.c -ldl -lm

#REPL the REPL is simpler than the NIDE for debugging with GDB

REPL.c : REPL.mc expandE
	./expandE REPL.mc REPL.c

REPL : REPL.c
	${gcc} -g -o REPL mcA.o mcB.o mcC.o mcD.o mcD2.o mcE.o REPL.c -ldl -lm

#NIDE

NIDE.c : NIDE.mc expandE
	./expandE NIDE.mc NIDE.c

NIDE : NIDE.c
	${gcc} -g -o NIDE mcA.o mcB.o mcC.o mcD.o mcD2.o mcE.o NIDE.c -ldl -lm


