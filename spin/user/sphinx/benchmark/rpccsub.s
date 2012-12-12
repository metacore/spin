	.text
	.globl rpcc
	.ent rpcc,1

rpcc:
	rpcc	$0
	ret	$31, ($26), 1
	.end rpcc
	
