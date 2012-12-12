
main(int argc, char **argv)
{
	if (unlink_extension())
		printf("unlink loaded\n");
	else
		printf("unlink link failed\n");
}
