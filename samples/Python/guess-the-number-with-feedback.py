import random

inclusive_range = (1, 100)

print("Guess my target number that is between %i and %i (inclusive).\n"
      % inclusive_range)
target = random.randint(*inclusive_range)
answer, i = None, 0
while answer != target:
    i += 1
    txt = input("Your guess(%i): " % i)
    try:
        answer = int(txt)
    except ValueError:
        print("  I don't understand your input of '%s' ?" % txt)
        continue
    if answer < inclusive_range[0] or answer > inclusive_range[1]:
        print("  Out of range!")
        continue
    if answer == target:
        print("  Ye-Haw!!")
        break
    if answer < target: print("  Too low.")
    if answer > target: print("  Too high.")

print("\nThanks for playing.")
