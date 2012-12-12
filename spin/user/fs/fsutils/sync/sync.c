
main(int argc, char **argv)
{
	if (sync_extension())
		printf("sync loaded\n");
	else
		printf("sync link failed\n");
}
