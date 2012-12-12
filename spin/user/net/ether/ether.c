
main(int argc, char **argv)
{
	if (ether_extension())
		printf("ether loaded\n");
	else
		printf("ether link failed\n");
}
