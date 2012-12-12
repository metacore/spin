main(int argc, char **argv)
{
        if (extent_extension())
                printf("Extent based disk manager loaded...\n");
        else
                printf("Extent based disk manager link failed\n");
}
