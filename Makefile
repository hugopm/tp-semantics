.PHONY: all clean

all:
	@rm analyzer.bc -f
	@rm analyzer.exe -f
	@dune build analyzer.exe analyzer.bc
	@ln -s _build/default/analyzer.bc analyzer.bc
	@ln -s _build/default/analyzer.exe analyzer.exe

clean:
	@dune clean
	@rm -f analyzer.bc
	@rm -f analyzer.exe
	@rm -f *~ */*~ .*.swp */.*.swp *.tar.gz

compress: clean
	@tar -czvf ../tp-semantics.tar.gz --exclude=".git*" ../tp-semantics
	@mv ../tp-semantics.tar.gz .

