all:
	happy -gca ParT.y
	alex -g LexT.x
	ghc --make TestT.hs -o TestT

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocT.* LexT.* ParT.* LayoutT.* SkelT.* PrintT.* TestT.* AbsT.* TestT ErrM.* SharedString.* ComposOp.* t6-4.dtd XMLT.* Makefile*
	

