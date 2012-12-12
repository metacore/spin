
main(int argc, char **argv)
{
	if (mkdir_extension())
		printf("mkdir loaded\n");
	else
		printf("mkdir link failed\n");
}
