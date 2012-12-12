/* Timer using Alpha RPCC register. */
static long rpcc_start;
static long rpcc_cumulative;

void start_timer()
{
    rpcc_start = rpcc();
}
void stop_timer()
{
    long rpcc_end = rpcc();
    rpcc_start &= 0xffffffff; /* rpcc is 32bit value. */
    rpcc_end &= 0xffffffff;
    
    if (rpcc_end > rpcc_start) {
	rpcc_cumulative += (rpcc_end - rpcc_start);
    } else {
	/* counter wrapped round. */
	rpcc_cumulative += 0x100000000 - rpcc_start + rpcc_end;
    }
}

#define MHZ 175 /* CPU clock speed.  I don't know the way to get the value
		   automatically. So please
		   adjust the value by yourself!! */
long get_timer()
{
    return (unsigned long)rpcc_cumulative * 1000 / MHZ;
}
long clear_timer()
{
    rpcc_cumulative = 0;
}
