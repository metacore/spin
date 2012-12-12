all: Space.html

%.html: ../src/%.i3
	(cd ../src; m3tohtml.pl $*.i3 > ../doc/$@)
