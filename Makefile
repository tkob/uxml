MLULEX = ml-ulex

check: parse.sml scan.ulex.sml uxml.sml xml-test-suite t/xmltest-valid-sa.t t/ibm-valid.t
	prove --exec ./t/do-test

xml-test-suite: xmlts20020606.tar
	tar xf xmlts20020606.tar
	touch xml-test-suite

xmlts20020606.tar:
	wget http://www.w3.org/XML/Test/xmlts20020606.tar

t/xmltest-valid-sa.t: t/xmltest-valid-sa.t.in
	autom4te -l m4sugar -o t/xmltest-valid-sa.t t/xmltest-valid-sa.t.in

t/ibm-valid.t: t/ibm-valid.t.in
	autom4te -l m4sugar -o t/ibm-valid.t t/ibm-valid.t.in

main: parse.sml scan.ulex.sml uxml.sml main.sml main.mlb
	mlton main.mlb

parse.sml: parse.cf
	proglr -o parse.sml parse.cf

scan.ulex.sml: scan.ulex
	$(MLULEX) --strict-sml scan.ulex

clean:
	rm -f main parse.sml scan.ulex.sml t/xmltest-valid-sa.t

.PHONY: check clean
