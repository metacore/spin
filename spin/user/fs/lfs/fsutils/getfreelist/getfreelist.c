

main(int argc, char **argv)
{
	if (getfreelist_extension())
		printf("getfreelist loaded\n");
	else
		printf("getfreelist link failed\n");
}
