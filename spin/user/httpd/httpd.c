
main(int argc, char **argv)
{
	if (httpd_extension())
		printf("Web server loaded\n");
	else
		printf("Web server link failed\n");
}
