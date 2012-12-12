
main(int argc, char **argv)
{
	if (read_extension())
		printf("read loaded\n");
	else
		printf("read link failed\n");
}
