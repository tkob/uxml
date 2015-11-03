MLULEX = ml-ulex

main: parse.sml scan.ulex.sml main.sml main.mlb
	mlton main.mlb

parse.sml: parse.cf
	proglr -o parse.sml parse.cf

scan.ulex.sml: scan.ulex
	$(MLULEX) --strict-sml scan.ulex

clean:
	rm -f main parse.sml scan.ulex.sml
