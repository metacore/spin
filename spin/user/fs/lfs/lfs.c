
main(int argc, char **argv)
{
	if (lfs_extension())
		printf("LFS loaded\n");
	else
		printf("LFS link failed\n");
}
