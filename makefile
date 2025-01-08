gcc := gcc -O0

# expandA  mcA implements universal syntax and backquote.  expandA expands backquote.

mcA.o : premacros.h mc.h mcA.c premacros.h
	${gcc} -fcommon -g mcA.c -c

expandA : mcA.o expandA.c
	${gcc} -fcommon -g -o expandA mcA.o expandA.c -ldl -lm

#testA

testA.c : expandA testA.mc
	./expandA testA.mc testA.c

testA.o : testA.c
	${gcc} -fcommon -g testA.c -c

# expandB mcB implements pattern matching ucase in terms of backquote.  expandB expands both ucase and backquote.

mcB.c : expandA mcB.mc
	 ./expandA mcB.mc mcB.c

mcB.o : mcB.c
	${gcc} -fcommon -g mcB.c -c

expandB.c : expandA expandB.mc
	./expandA expandB.mc expandB.c

expandB : mcB.o expandB.c
	${gcc} -fcommon -g -o expandB mcA.o mcB.o expandB.c -ldl -lm

#testB

testB.c : expandB testB.mc
	./expandB testB.mc testB.c

testB.o : testB.c
	${gcc} -fcommon -g testB.c -c

#expandC  expandC expands macro definition (umacro) as well as ucase and backquote.

mcC.c : expandB mcC.mc
	./expandB mcC.mc mcC.c

mcC.o : mcC.c
	${gcc} -fcommon -g mcC.c -c

expandC.c : expandB expandC.mc
	./expandB expandC.mc expandC.c

expandC : mcC.o expandC.c
	${gcc} -fcommon -g -o expandC mcA.o mcB.o mcC.o expandC.c -ldl -lm

#testC

testC.c : expandC testC.mc
	./expandC testC.mc testC.c

testC.o : testC.c
	${gcc} -fcommon -g testC.c -c


#expandD  expandD expands some additional generic macros --- push, dolist and sformat.

mcD.c :  expandC mcD.mc
	./expandC mcD.mc mcD.c

mcD.o : mcD.c
	${gcc} -fcommon -g mcD.c -c

expandD.c :  expandC expandD.mc
	./expandC expandD.mc expandD.c

expandD : mcD.o expandD.c
	${gcc} -fcommon -g -o expandD mcA.o mcB.o mcC.o mcD.o expandD.c -ldl -lm

#testD

testD.c : expandD testD.mc
	./expandD testD.mc testD.c

testD.o : testD.c
	${gcc} -fcommon -g testD.c -c

#mcE defines REPL and NIDE procedures but does not define macros.
#No new expansion executable is needed.

mcE.c :  expandD mcE.mc
	./expandD mcE.mc mcE.c

mcE.o : mcE.c
	${gcc} -fcommon -g mcE.c -c


#mcF defines install_base_properties macro.  Note the dependence on base_decls.h

mcF.c :  expandD mcF.mc
	./expandD mcF.mc mcF.c

mcF.o : mcF.c
	${gcc} -fcommon -g mcF.c -c

expandF.c :  expandD expandF.mc
	./expandD expandF.mc expandF.c

expandF : mcF.o expandF.c base_decls.h
	${gcc} -fcommon -g -o expandF mcA.o mcB.o mcC.o mcD.o mcF.o expandF.c -ldl -lm


#REPL the REPL is simpler than the NIDE for debugging with GDB

REPL.c : REPL.mc expandF
	./expandF REPL.mc REPL.c

REPL : REPL.c
	${gcc} -fcommon -g -o REPL mcA.o mcB.o mcC.o mcD.o mcE.o mcF.o REPL.c -ldl -lm


#NIDE

NIDE.c : NIDE.mc expandF mcE.o
	./expandF NIDE.mc NIDE.c

NIDE : NIDE.c 
	${gcc} -fcommon -g -o NIDE mcA.o mcB.o mcC.o mcD.o  mcE.o mcF.o NIDE.c -ldl -lm


