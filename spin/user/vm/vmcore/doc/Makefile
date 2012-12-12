all: MemoryObject.html AddressSpace.html CacheObject.html PagerObject.html

%.html: ../src/%.i3
	(cd ../src; m3tohtml.pl $*.i3 > ../doc/$@)
