
main(int argc, char **argv)
{
	if (nanny_extension())
		printf("Nanny loaded\n");
	else
		printf("Nanny link failed\n");
}
