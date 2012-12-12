
main(int argc, char **argv)
{
	if (socketRW_extension())
		printf("SocketRW loaded\n");
	else
		printf("SocketRW link failed\n");
}
