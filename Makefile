MLULEX = ml-ulex

check: parse.sml scan.ulex.sml uxml.sml xmlconf t/xmltest-valid-sa.t t/ibm-valid.t t/sun-valid.t t/oasis-pass.t t/japanese.t
	prove -f --exec ./t/do-test

xmlconf: xmlts20130923.tar.gz
	tar xzf xmlts20130923.tar.gz
	touch xmlconf

xmlts20130923.tar.gz:
	wget http://www.w3.org/XML/Test/xmlts20130923.tar.gz

t/xmltest-valid-sa.t: t/xmltest-valid-sa.t.in
	autom4te -l m4sugar -o t/xmltest-valid-sa.t t/xmltest-valid-sa.t.in

t/ibm-valid.t: t/ibm-valid.t.in
	autom4te -l m4sugar -o t/ibm-valid.t t/ibm-valid.t.in

t/sun-valid.t: t/sun-valid.t.in
	autom4te -l m4sugar -o t/sun-valid.t t/sun-valid.t.in

t/oasis-pass.t: t/oasis-pass.t.in
	autom4te -l m4sugar -o t/oasis-pass.t t/oasis-pass.t.in

t/japanese.t: t/japanese.t.in
	autom4te -l m4sugar -o t/japanese.t t/japanese.t.in

main: parse.sml scan.ulex.sml uxml.sml main.sml main.mlb
	mlton main.mlb

parse.sml: parse.cf
	proglr -o parse.sml parse.cf

scan.ulex.sml: scan.ulex
	$(MLULEX) --strict-sml scan.ulex

clean:
	rm -f main parse.sml scan.ulex.sml t/xmltest-valid-sa.t t/ibm-valid.t t/sun-valid.t t/oasis-pass.t t/japanese.t

.PHONY: check clean
