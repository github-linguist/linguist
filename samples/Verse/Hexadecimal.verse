Hexadecimal<public> := module{
    using. BitMath
    using. MathFeatures
    using. StringProcessing

    UpperCaseHexAlphabet<public> : string = "0123456789ABCDEF"
    LowerCaseHexAlphabet<public> : string = "0123456789abcdef"

    ReverseHexTable<public> : [char]int = map{
        '0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9,
        'A' => 10, 'B' => 11, 'C' => 12, 'D' => 13, 'E' => 14, 'F' => 15,
        'a' => 10, 'b' => 11, 'c' => 12, 'd' => 13, 'e' => 14, 'f' => 15
    }

    # Converts a single byte (0-255) to its two-character hexadecimal representation
    # By default uses uppercase letters, but can use lowercase if wanted (for ex in MD5 Hashes)
    # Fails if the input is outside the byte range
    EncodeByteToHex<public>(Byte:int, ?LowerCase:logic=false)<computes><decides>:string = {
        HexAlphabet := LowerCase? and LowerCaseHexAlphabet or UpperCaseHexAlphabet
        
        HighNibble := HexAlphabet[Quotient[Byte, 16]]
        LowNibble := HexAlphabet[Mod[Byte, 16]]

        "{HighNibble}{LowNibble}"
    }

    # Converts a byte array to its hexadecimal string representation
    EncodeByteArrayToHex<public>(ByteArray:[]int, ?LowerCase:logic=false)<computes><decides>:string = {
        HexParts := for (Byte : ByteArray) {
            EncodeByteToHex[Byte, ?LowerCase := LowerCase]
        }
        Concatenate(HexParts)
    }

    # WIP: Deprecate later (redundant)
    # Converts a single char to its two-character hexadecimal representation
    EncodeCharToHex<public>(Character:char, ?LowerCase:logic=false)<computes>:string = {
        ByteValue := CharToByte(Character)
        
        EncodeByteToHex[ByteValue, ?LowerCase := LowerCase] or Err("Unreachable")
    }

    # WIP: Deprecate later (redundant)
    # Converts a string to its hexadecimal string representation
    EncodeStringToHex<public>(String:string, ?LowerCase:logic=false)<computes>:string = {
        HexParts := for (Character : String) {
            EncodeCharToHex(Character, ?LowerCase := LowerCase)
        }
        Concatenate(HexParts)
    }

    # WIP: Deprecate later (redundant)
    # Converts an arbitrary-sized integer to its hexadecimal string representation
    # Fails if the input integer is negative
    EncodeIntToHex<public>(Value:int, ?LowerCase:logic=false)<computes><decides><reads>:string = {
        Value >= 0

        # Calculate how many bytes are needed to represent the unsigned value
        ByteCount := Value = 0 and 1 or Quotient[GetBitWidth(Value) + 8, 8] or Err("Unreachable")

        # 2. Iterate through the bytes from high to low
        Concatenate(for (Index := 1..ByteCount) {
            # We want to print Big-Endian (Most Significant Byte first)
            # So we invert the index
            BytePosition := ByteCount - Index
            
            # Calculate divisor: 2^(BytePosition * 8)
            Divisor := PowerOfTwo[BytePosition * 8] or Err("Unreachable")

            # Shift the value down to this byte position
            # This gives us the value of this byte AND all bytes above it
            ShiftedValue := Quotient[Value, Divisor]

            # If ShiftedValue is 0, it means this byte (and all above it) are 0.
            # We skip it (return empty string) UNLESS it's the very last byte (Divisor=1).
            # This ensures '0' prints as "00" but '0xFF' prints as "FF" (not "00FF").
            (ShiftedValue > 0 or Divisor = 1) and (
                ByteValue := Mod[ShiftedValue, 256] or Err("Unreachable")
                EncodeByteToHex[ByteValue, ?LowerCase := LowerCase] or Err("Unreachable")
            ) or ""
        })
    }

    # Converts a hexadecimal string into its respective byte array
    # Fails if the input string has an odd length or contains invalid hexadecimal characters
    DecodeHexToByteArray<public>(HexString:string)<computes><decides><reads>:[]int = {
        HexLength := HexString.Length

        Mod[HexLength, 2] = 0

        for (Index := 0..(Truncate(HexLength / 2)) - 1) {
            HighChar := HexString[Index * 2] or Err("Unreachable")
            LowChar := HexString[(Index * 2) + 1] or Err("Unreachable")

            HighValue := ReverseHexTable[HighChar]
            LowValue := ReverseHexTable[LowChar]

            (HighValue * 16) + LowValue
        }
    }

    # WIP: Deprecate later (redundant)
    # Decodes a hexadecimal string into its normal byte string representation
    # Fails if the input string has an odd length or contains invalid hexadecimal characters
    DecodeHexToString<public>(HexString:string)<decides><transacts>:string = {
        ByteArray := DecodeHexToByteArray[HexString]
        ByteArrayToString[ByteArray] or Err("Unreachable")
    }

    # WIP: Deprecate later (redundant)
    # Decodes a hexadecimal string into its integer representation
    # Fails if the input string is empty, has an odd length, or contains invalid hexadecimal characters
    DecodeHexToInt<public>(HexString:string)<decides><transacts>:int = {
        var Result : int = 0

        ByteArray := DecodeHexToByteArray[HexString]

        ByteArray.Length > 0

        for (Byte : ByteArray) {
            set Result = (Result * 256) + Byte
        }

        Result
    }
}