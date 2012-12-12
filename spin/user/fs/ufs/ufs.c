
main(int argc, char **argv)
{
	if (ufs_extension())
		printf("rofs ufs loaded\n");
	else
		printf("rofs ufs link failed\n");
}
