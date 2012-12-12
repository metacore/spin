
main(int argc, char **argv)
{
	if (rmdir_extension())
		printf("rmdir loaded\n");
	else
		printf("rmdir link failed\n");
}
