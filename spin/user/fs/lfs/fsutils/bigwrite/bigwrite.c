
main(int argc, char **argv)
{
	if (bigwrite_extension())
		printf("bigwrite loaded\n");
	else
		printf("bigwrite link failed\n");
}
