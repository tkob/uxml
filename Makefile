MLULEX = ml-ulex
TESTS = t/xmltest.t t/ibm.t t/sun.t t/oasis-pass.t t/japanese.t t/not-wf.t

check: parse.sml scan.ulex.sml uxml.sml xmlconf $(TESTS)
	prove -f --exec ./t/do-test

xmlconf: xmlts20130923.tar.gz
	tar xzf xmlts20130923.tar.gz
	touch xmlconf

xmlts20130923.tar.gz:
	wget http://www.w3.org/XML/Test/xmlts20130923.tar.gz

%.t: %.t.in
	autom4te -l m4sugar -o $@ $<

main: parse.sml scan.ulex.sml uxml.sml main.sml main.mlb
	mlton main.mlb

parse.sml: parse.cf
	proglr -o parse.sml parse.cf

scan.ulex.sml: scan.ulex
	$(MLULEX) --strict-sml scan.ulex

clean:
	rm -f main parse.sml scan.ulex.sml $(TESTS)

.PHONY: check clean
