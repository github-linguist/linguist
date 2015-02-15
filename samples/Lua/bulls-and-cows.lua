function ShuffleArray(array)
   for i=1,#array-1 do
      local t = math.random(i, #array)
      array[i], array[t] = array[t], array[i]
   end
end

function GenerateNumber()
   local digits = {1,2,3,4,5,6,7,8,9}

   ShuffleArray(digits)

   return digits[1] * 1000 +
          digits[2] *  100 +
          digits[3] *   10 +
          digits[4]
end

function IsMalformed(input)
   local malformed = false

   if #input == 4 then
      local already_used = {}
      for i=1,4 do
         local digit = input:byte(i) - string.byte('0')
         if digit < 1 or digit > 9 or already_used[digit] then
            malformed = true
            break
         end
         already_used[digit] = true
      end
   else
      malformed = true
   end

   return malformed
end

math.randomseed(os.time())
math.randomseed(math.random(2^31-1)) -- since os.time() only returns seconds

print("\nWelcome to Bulls and Cows!")
print("")
print("The object of this game is to guess the random 4-digit number that the")
print("computer has chosen. The number is generated using only the digits 1-9,")
print("with no repeated digits. Each time you enter a guess, you will score one")
print("\"bull\" for each digit in your guess that matches the corresponding digit")
print("in the computer-generated number, and you will score one \"cow\" for each")
print("digit in your guess that appears in the computer-generated number, but is")
print("in the wrong position. Use this information to refine your guesses. When")
print("you guess the correct number, you win.");
print("")

quit = false

repeat
   magic_number = GenerateNumber()
   magic_string = tostring(magic_number) -- Easier to do scoring with a string
   repeat
      io.write("\nEnter your guess (or 'Q' to quit): ")
      user_input = io.read()
      if user_input == 'Q' or user_input == 'q' then
         quit = true
         break
      end

      if not IsMalformed(user_input) then
         if user_input == magic_string then
            print("YOU WIN!!!")
         else
            local bulls, cows = 0, 0
            for i=1,#user_input do
               local find_result = magic_string:find(user_input:sub(i,i))

               if find_result and find_result == i then
                  bulls = bulls + 1
               elseif find_result then
                  cows = cows + 1
               end
            end
            print(string.format("You scored %d bulls, %d cows", bulls, cows))
         end
      else
         print("Malformed input. You must enter a 4-digit number with")
         print("no repeated digits, using only the digits 1-9.")
      end

   until user_input == magic_string

   if not quit then
      io.write("\nPress <Enter> to play again or 'Q' to quit: ")
      user_input = io.read()
      if user_input == 'Q' or user_input == 'q' then
         quit = true
      end
   end

   if quit then
      print("\nGoodbye!")
   end
until quit
