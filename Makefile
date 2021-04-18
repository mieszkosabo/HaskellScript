# Makefile generated by BNFC.

GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : TestHaskellScript

# Rules for building the parser.

AbsHaskellScript.hs LexHaskellScript.x ParHaskellScript.y PrintHaskellScript.hs TestHaskellScript.hs : HaskellScript.cf
	bnfc --haskell HaskellScript.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

TestHaskellScript : AbsHaskellScript.hs LexHaskellScript.hs ParHaskellScript.hs PrintHaskellScript.hs TestHaskellScript.hs
	${GHC} ${GHC_OPTS} $@

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsHaskellScript.hs AbsHaskellScript.hs.bak ComposOp.hs ComposOp.hs.bak DocHaskellScript.txt DocHaskellScript.txt.bak ErrM.hs ErrM.hs.bak LayoutHaskellScript.hs LayoutHaskellScript.hs.bak LexHaskellScript.x LexHaskellScript.x.bak ParHaskellScript.y ParHaskellScript.y.bak PrintHaskellScript.hs PrintHaskellScript.hs.bak SkelHaskellScript.hs SkelHaskellScript.hs.bak TestHaskellScript.hs TestHaskellScript.hs.bak XMLHaskellScript.hs XMLHaskellScript.hs.bak ASTHaskellScript.agda ASTHaskellScript.agda.bak ParserHaskellScript.agda ParserHaskellScript.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak HaskellScript.dtd HaskellScript.dtd.bak TestHaskellScript LexHaskellScript.hs ParHaskellScript.hs ParHaskellScript.info ParDataHaskellScript.hs Makefile


# EOF