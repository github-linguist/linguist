void runCode(string code)
{
    int c_len = code.length();
    int accumulator, bottles;
    for(int i=0;i<c_len;i++)
    {
        switch(code[i])
        {
            case 'Q':
                cout << code << endl;
                break;

            case 'H':
                cout << "Hello, world!" << endl;
                break;

            case '9':
                //Nice bottles song alg. from RC :)
                bottles = 99;
                do {
                    cout << bottles << " bottles of beer on the wall" << endl;
                    cout << bottles << " bottles of beer" << endl;
                    cout << "Take one down, pass it around" << endl;
                    cout << --bottles << " bottles of beer on the wall" << endl << endl;
                } while( bottles > 0 );
                break;

            case '+':
                //Am I the only one finding this one weird? :o
                accumulator++;
                break;
        }
    }
};
