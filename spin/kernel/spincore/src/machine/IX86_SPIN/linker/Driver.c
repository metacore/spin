#include <stdio.h>
#include <stdlib.h>
#include "Interface.h"
#include "Module.h"
#include "Symbol.h"

typedef void (*VoidFunc)();

main(int argc, char *argv[])
{	
	int i, j;
	FILE *f;
	int length;
	char *contents;
	Module **module;
	SymbolEntry *se;
	VoidFunc addr;

	module = (Module **) malloc(argc * sizeof(Module *));

	/* load each file into memory and turn it into a module */
	for(i = 1; i < argc; i++) {
		f = fopen(argv[i], "rb");
		fseek(f, 0, SEEK_END);
		length = ftell(f);
		fseek(f, 0, SEEK_SET);
		contents = malloc(length);
		fread(contents, 1, length, f);
		fclose(f);

		module[i] = MCreate(contents);
		/*free(contents);*/
	}

	/* link each file with each of the others */
	for(i = 1; i < argc; i++) {
		for(j = 1; j < argc; j++) {
			Link(module[i], module[j]);
		}
	}

	/* check that it linked completely */
	for(i = 1; i < argc; i++) {
		if(module[i]->MS_flags != Resolved_Fully) {
			fprintf(stderr, "Unable to resolve completely.\n");
			exit(1);
		}
	}

	/* look for a symbol called _main and call it */
	for(i = 1; i < argc; i++) {
		se = Symbol_find(module[i], "_main");
		if(se) {
			addr = (VoidFunc) Symbol_get_value(se);
			(*addr)();
			exit(1);
		}
	}

	fprintf(stderr, "Couldn't find _main.\n");
}

void *spin_malloc(long size)
{
	return malloc(size);
}

void spin_free(void *ptr)
{
	free(ptr);
}

void register_clean(int a, int b)
{
}
