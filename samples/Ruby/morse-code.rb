require 'win32/sound'

class MorseCode
  MORSE = {
      "!" => "---.", "\"" => ".-..-.", "$" => "...-..-", "'" => ".----.",
      "(" => "-.--.", ")" => "-.--.-", "+" => ".-.-.", "," => "--..--",
      "-" => "-....-", "." => ".-.-.-", "/" => "-..-.", "0" => "-----",
      "1" => ".----", "2" => "..---", "3" => "...--", "4" => "....-", "5" => ".....",
      "6" => "-....", "7" => "--...", "8" => "---..", "9" => "----.", ":" => "---...",
      ";" => "-.-.-.", "=" => "-...-", "?" => "..--..", "@" => ".--.-.", "A" => ".-",
      "B" => "-...", "C" => "-.-.", "D" => "-..", "E" => ".", "F" => "..-.",
      "G" => "--.", "H" => "....", "I" => "..", "J" => ".---", "K" => "-.-",
      "L" => ".-..", "M" => "--", "N" => "-.", "O" => "---", "P" => ".--.",
      "Q" => "--.-", "R" => ".-.", "S" => "...", "T" => "-", "U" => "..-",
      "V" => "...-", "W" => ".--", "X" => "-..-", "Y" => "-.--", "Z" => "--..",
      "[" => "-.--.", "]" => "-.--.-", "_" => "..--.-",
  }

  T_UNIT = 75 # ms
  FREQ = 700
  DIT = 1 * T_UNIT
  DAH = 3 * T_UNIT
  CHARGAP = 1 * T_UNIT
  WORDGAP = 7 * T_UNIT

  def initialize(string)
    @message = string
    puts "your message is #{string.inspect}"
  end

  def send
    @message.strip.upcase.split.each do |word|
      word.each_char do |char|
        send_char char
        pause CHARGAP
        print " "
      end
      pause WORDGAP
      puts ""
    end
  end

  private
  def send_char(char)
    MORSE[char].each_char do |code|
      case code
      when '.' then beep DIT
      when '-' then beep DAH
      end
      pause CHARGAP
      print code
    end
  end

  def beep(ms)
    ::Win32::Sound.beep(FREQ, ms)
  end

  def pause(ms)
    sleep(ms.to_f/1000.0)
  end
end

MorseCode.new('sos').send
MorseCode.new('this is a test.').send
