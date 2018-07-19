var n, g;
n = irandom_range(1,10);
show_message("I'm thinking of a number from 1 to 10");
g = get_integer("Please enter guess", 1);
while(g != n)
    {
    g = get_integer("I'm sorry "+g+" is not my number, try again. Please enter guess", 1);
    }
show_message("Well guessed!");
