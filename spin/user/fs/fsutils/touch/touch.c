
main(int argc, char **argv)
{
	if (touch_extension())
		printf("touch loaded\n");
	else
		printf("touch link failed\n");
}
