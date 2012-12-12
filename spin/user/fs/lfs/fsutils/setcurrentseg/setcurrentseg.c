

main(int argc, char **argv)
{
	if (setcurrentseg_extension())
		printf("setcurrentseg loaded\n");
	else
		printf("setcurrentseg link failed\n");
}
