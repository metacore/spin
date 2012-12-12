

long   asm( const char *,...);
#pragma intrinsic( asm )
long getsp(void)
{
	asm("bis %sp,%sp,%v0");
}
