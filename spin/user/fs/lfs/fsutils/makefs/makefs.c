
main(int argc, char **argv)
{
	if (makefs_extension())
		printf("touch loaded\n");
	else
		printf("touch link failed\n");
}
