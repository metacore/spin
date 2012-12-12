
main(int argc, char **argv)
{
	if (segment_extension())
		printf("segment loaded\n");
	else
		printf("segment link failed\n");
}
