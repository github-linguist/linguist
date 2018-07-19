FizzBuzz

Memory:
  Zero
  Zero
  Counter 1
  Counter 2

  Zero
  ASCIIDigit 3
  ASCIIDigit 2
  ASCIIDigit 1

  Zero
  Digit 3
  Digit 2
  Digit 1

  CopyPlace
  Mod 3
  Mod 5
  PrintNumber

  TmpFlag

Counters for the loop
++++++++++[>++++++++++[>+>+<<-]<-]

Number representation in ASCII
>>>>
++++++++ ++++++++ ++++++++ ++++++++ ++++++++ ++++++++ [>+>+>+<<<-]
<<<<

>>
[
    Do hundret times:

    Decrement counter
    ->->

    Increment Number
    > >>+>
        > >>+>
        <<<<
    <<<<

    Check for Overflow
    ++++++++++
    >>> >>>>
    >++++++++++<
    [-<<< <<<<->>>> >>> >-<]
    ++++++++++
    <<< <<<<

    Restore the digit
    [->>>> >>>-<<< <<<<]
    >>>> [-]+ >>>>[<<<< - >>>>[-]]<<<< <<<<

    If there is an overflow
    >>>>[
        <<<<

        >>>----------> >>>----------<+<< <<+<<

        Check for Overflow
        ++++++++++
        >> >>>>
        >>++++++++++<<
        [-<< <<<<->>>> >> >>-<<]
        ++++++++++
        << <<<<

        Restore the digit
        [->>>> >>-<< <<<<]
        >>>> [-]+ >>>>[<<<< - >>>>[-]]<<<< <<<<

        If there (again) is an overflow
        >>>>[
            <<<<
            >>---------->> >>----------<+< <<<+<

            >>>>
            [-]
        ]<<<<

        >>>>
        [-]
    ]<<<<

    >>>> >>>>

    Set if to print the number
    >>>[-]+<<<

    Handle the Mod 3 counter
    [-]+++

    >>>>[-]+<<<<
    >+[-<->]+++<
    [->->>>[-]<<<<]
    >>>>[
        <[-]>

        [-]
        Print "Fizz"
        ++++++++ ++++++++ ++++++++ ++++++++
        ++++++++ ++++++++ ++++++++ ++++++++
        ++++++.

        ++++++++ ++++++++ ++++++++ ++++++++
        +++.

        ++++++++ ++++++++ +..

        [-]
        <<<--->>>
    ]<<<<

    Handle the Mod 5 counter
    [-]+++++

    >>>>[-]+<<<<
    >>+[-<<->>]+++++<<
    [->>->>[-]<<<<]
    >>>>[
        <[-]>

        [-]
        Print "Buzz"
        ++++++++ ++++++++ ++++++++ ++++++++
        ++++++++ ++++++++ ++++++++ ++++++++
        ++.

        ++++++++ ++++++++ ++++++++ ++++++++
        ++++++++ ++++++++ +++.

        +++++..

        [-]
        <<----->>
    ]<<<<

    Check if to print the number (Leading zeros)
    >>>[
        <<< <<<< <<<<
        >.>.>.<<<
        >>> >>>> >>>>
        [-]
    ]<<<

    <<<< <<<<

    Print New Line
    <<<<[-]++++ ++++ ++++ +.---.[-]>>
]
<<
