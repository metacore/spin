
main(int argc, char **argv)
{
	if (wfs_extension())
		printf("WFS loaded\n");
	else
		printf("WFS link failed\n");
}
