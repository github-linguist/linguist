void main() {
    immutable num = uniform(1, 10).text;

    do write("What's next guess (1 - 9)? ");
    while (readln.strip != num);

    writeln("Yep, you guessed my ", num);
}
