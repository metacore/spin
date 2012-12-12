
main(int argc, char **argv)
{
	if (freelist_extension())
		printf("freelist loaded\n");
	else
		printf("freelist link failed\n");
}
