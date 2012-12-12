
main(int argc, char **argv)
{
	if (cleanse_extension())
		printf("cleanse loaded\n");
	else
		printf("cleanse link failed\n");
}
