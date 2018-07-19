class MAIN
    creation make
    feature make is
        local
            a, b: REAL;
        do
            print("a = ");
            io.read_real;
            a := io.last_real;

            print("b = ");
            io.read_real;
            b := io.last_real;

            print("a + b = ");
            io.put_real(a + b);
            print("%Na - b = ");
            io.put_real(a - b);
            print("%Na * b = ");
            io.put_real(a * b);
            print("%Na / b = ");
            io.put_real(a / b);
            print("%Na %% b = ");
            io.put_real(((a / b) - (a / b).floor) * b);
            print("%Na ^ b = ");
            io.put_real(a.pow(b));
            print("%N");
        end
end
