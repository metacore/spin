

main(int argc, char **argv)
{
	if (getsegdesc_extension())
		printf("getsegdesc loaded\n");
	else
		printf("getsegdesc link failed\n");
}
