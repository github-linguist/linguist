import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem

val morseCode = hashMapOf('a' to ".-",   'b' to "-...", 'c' to "-.-.",
                          'd' to "-..",  'e' to ".",    'f' to "..-.",
                          'g' to "--.",  'h' to "....", 'i' to "..",
                          'j' to ".---", 'k' to "-.-",  'l' to ".-..",
                          'm' to "--",   'n' to "-.",   'o' to "---",
                          'p' to ".--.", 'q' to "--.-", 'r' to ".-.",
                          's' to "...",  't' to "-",    'u' to "..-",
                          'v' to "...-", 'w' to ".--",  'x' to "-..-",
                          'y' to "-.--", 'z' to "--..",

                          '0' to ".....", '1' to "-....", '2' to "--...",
                          '3' to "---..", '4' to "----.", '5' to "-----",
                          '6' to ".----", '7' to "..---", '8' to "...--",
                          '9' to "....-",

                          ' ' to "/",       ',' to "--..--", '!' to "-.-.--",
                          '"' to ".-..-.",  '.' to ".-.-.-", '?' to "..--..",
                          '\'' to ".----.", '/' to "-..-.",  '-' to "-....-",
                          '(' to "-.--.-",  ')' to "-.--.-")

val symbolDurationInMs = hashMapOf('.' to 200, '-' to 500, '/' to 1000)

fun main(args: Array<String>) {
    args.forEach { playMorseCode(toMorseCode(it.toLowerCase())) }
}

fun toMorseCode(message: String) = message.filter { morseCode.containsKey(it) }
                                          .fold("")  { (acc, ch) -> acc + morseCode[ch]!! }

fun playMorseCode(morseCode: String) = morseCode.forEach { symbol -> beep(symbolDurationInMs[symbol]!!) }

fun beep(durationInMs: Int) {
    val soundBuffer = ByteArray(durationInMs * 8)
    for ((i, elem) in soundBuffer.withIndices()) {
        soundBuffer[i] = (Math.sin(i / 8.0 * 2.0 * Math.PI) * 80.0).toByte()
    }

    val audioFormat = AudioFormat(/*sampleRate*/ 8000F,
                                  /*sampleSizeInBits*/ 8,
                                  /*channels*/ 1,
                                  /*signed*/ true,
                                  /*bigEndian*/ false)
    with (AudioSystem.getSourceDataLine(audioFormat)!!) {
        open(audioFormat)

        start()
        write(soundBuffer, 0, soundBuffer.size)
        drain()

        close()
    }
}
