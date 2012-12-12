
main(int argc, char **argv)
{
	if (cleaner_extension())
		printf("cleaner loaded\n");
	else
		printf("cleaner link failed\n");
}
