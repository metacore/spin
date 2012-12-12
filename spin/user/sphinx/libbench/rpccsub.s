	.text
	.globl get_rpcc
	.ent get_rpcc,1

get_rpcc:
	rpcc	$0
	ret	$31, ($26), 1
	.end get_rpcc

	
	
