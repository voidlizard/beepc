#include <stdio.h>
#include <stdlib.h>

#define MAX_CODE 4096

#include "beepvm.h"


int main(int argc, char **argv)
{
	static unsigned char code[MAX_CODE];
	static VM vm;
	FILE *f = 0;

	code[MAX_CODE-1] = DOWN;

	if( argc < 2 ) {
		printf("Usage: beepvm file.bin\n");
		exit(-1);
	}
	
	f = fopen(argv[1],"r");
	if( !f ) {
		printf("Can not open file: %s\n", argv[1]);
		exit(-1);
	}

	if( !fread(code, sizeof(INSTRUCTION), MAX_CODE, f) ) {
		printf("Bad file: %s\n", argv[1]);
		exit(-1);
	}

	beepvm_run(&vm, code);
	return 0;
}

