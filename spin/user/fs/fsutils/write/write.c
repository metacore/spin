
main(int argc, char **argv)
{
	if (write_extension())
		printf("write loaded\n");
	else
		printf("write link failed\n");
}
