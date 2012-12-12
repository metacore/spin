
main(int argc, char **argv)
{
	if (lfscore_extension())
		printf("lfscore loaded\n");
	else
		printf("lfscore link failed\n");
}
