MLULEX = ml-ulex

check: parse.sml scan.ulex.sml uxml.sml
	prove --exec ./t/do-test

main: parse.sml scan.ulex.sml uxml.sml main.sml main.mlb
	mlton main.mlb

parse.sml: parse.cf
	proglr -o parse.sml parse.cf

scan.ulex.sml: scan.ulex
	$(MLULEX) --strict-sml scan.ulex

clean:
	rm -f main parse.sml scan.ulex.sml

.PHONY: check clean
