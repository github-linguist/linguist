void runCode(const char *code)
{
    int c_len = strlen(code);
    int i, accumulator, bottles;
    for(i=0;i<c_len;i++)
    {
        switch(code[i])
        {
            case 'Q':
                printf("%s\n", code);
                break;

            case 'H':
                printf("Hello, world!\n");
                break;

            case '9':
                //Nice bottles song alg. from RC :)
                bottles = 99;
                do {
                    printf("%d bottles of beer on the wall\n", bottles);
                    printf("%d bottles of beer\n", bottles);
                    printf("Take one down, pass it around\n");
                    printf("%d bottles of beer on the wall\n\n", --bottles);
                } while( bottles > 0 );
                break;

            case '+':
                //Am I the only one finding this one weird? :o
                accumulator++;
                break;
        }
    }
};
