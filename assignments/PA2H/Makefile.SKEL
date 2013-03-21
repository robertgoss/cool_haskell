ASSN = 2H
CLASS= cs143
CLASSDIR= /usr/class/cs143/cool

SRC= cool.x test.cl README
CSRC= Token.hs StringTab.hs pa1-grading.pl
TSRC= mycoolc
HSRC=
CGEN= cool.hs
HGEN=
CFIL= ${CSRC} ${CGEN}
LIBS= parser semant cgen
LSRC= Makefile
OUTPUT= test.output


source : ${SRC} ${TSRC} ${LSRC} ${LIBS} lsource

lsource : ${LSRC} ${CSRC}

${OUTPUT}:	lexer test.cl
	@rm -f test.output
	-./lexer test.cl >test.output 2>&1 

lexer: cool.hs
	ghc --make cool.hs -o lexer

cool.hs: cool.x
	alex cool.x

dotest:	lexer test.cl
	./lexer test.cl

${LIBS}:
	${CLASSDIR}/etc/link-object ${ASSN} $@

# These dependencies allow you to get the starting files for
# the assignment.  They will not overwrite a file you already have.

${SRC} :								
	${CLASSDIR}/etc/copy-skel ${ASSN} ${SRC}

${LSRC} :
	${CLASSDIR}/etc/link-shared ${ASSN} ${LSRC}

${TSRC}:
	-ln -s ${CLASSDIR}/src/PA${ASSN}/$@ $@

${CSRC}:
	-ln -s ${CLASSDIR}/src/PA${ASSN}/$@ $@

clean :
	-rm -f *.s ${OUTPUT} ${CLS} *~ lexer parser cgen semant

submit-clean: ${OUTPUT}
	-rm -f *.s ${CLS} ${CGEN} *~ lexer parser cgen semant


