class MAIN
    creation main
    feature main is
        local
            x, y: INTEGER;
            retried: BOOLEAN;
        do
            x := 42;
            y := 0;

            if not retried then
                io.put_real(x / y);
            else
                print("NaN%N");
            end
        rescue
            print("Caught division by zero!%N");
            retried := True;
            retry
        end
end
