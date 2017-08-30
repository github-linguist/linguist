//////////////////////////////////////////////////////////////////////////////////////////////
//binary png parser
let  
    file = File.Contents("C:\path\to\file.png"), 
    fileHeader = BinaryFormat.Text(8, TextEncoding.Ascii), 
    fourCC = BinaryFormat.Text(4, TextEncoding.Ascii), 
    zeroTerminatedString =  
        let 
            listOfBytes = BinaryFormat.List(BinaryFormat.Byte, (byte) => byte <> 0), 
            transformedBytes = BinaryFormat.Transform(listOfBytes, (list) =>  
                let 
                    count = List.Count(list), 
                    listWithoutZero = List.FirstN(list, count - 1), 
                    listAsBinary = Binary.FromList(listWithoutZero), 
                    binaryAsText = Text.FromBinary(listAsBinary, TextEncoding.Ascii) 
                in 
                    binaryAsText) 
        in 
            transformedBytes, 
    headerChunk = BinaryFormat.Record([ 
        ChunkType = "IHDR", 
        Width = BinaryFormat.SignedInteger32,  
        Height = BinaryFormat.SignedInteger32,  
        BitDepth = BinaryFormat.Byte,  
        ColorType = BinaryFormat.Byte,  
        Compression = BinaryFormat.Byte,  
        Filter = BinaryFormat.Byte,  
        Interlace = BinaryFormat.Byte 
    ]), 
    dataChunk = BinaryFormat.Record([ 
        ChunkType = "IDAT", 
        Data = BinaryFormat.Binary() 
    ]), 
    endChunk = BinaryFormat.Record([ 
        ChunkType = "IEND" 
    ]), 
    physicalPixelDimensionsChunk = BinaryFormat.Record([ 
        ChunkType = "pHYs", 
        PixelsPerUnitX = BinaryFormat.SignedInteger32, 
        PixelsPerUnitY = BinaryFormat.SignedInteger32, 
        UnitSpecifier = BinaryFormat.Byte 
    ]), 
    primaryChromaticitiesAndWhitePoint = BinaryFormat.Record([ 
        ChunkType = "cHRM", 
        WhitePointX = BinaryFormat.SignedInteger32, 
        WhitePointY = BinaryFormat.SignedInteger32, 
        RedX = BinaryFormat.SignedInteger32, 
        RedY = BinaryFormat.SignedInteger32, 
        GreenX = BinaryFormat.SignedInteger32, 
        GreenY = BinaryFormat.SignedInteger32, 
        BlueX = BinaryFormat.SignedInteger32, 
        BlueY = BinaryFormat.SignedInteger32 
    ]), 
    embeddedICCProfileChunk = BinaryFormat.Record([ 
        ChunkType = "iCCP", 
        ProfileName = zeroTerminatedString, 
        CompressionMethod = BinaryFormat.Byte, 
        CompressedProfile = BinaryFormat.Binary() 
    ]), 
    knownChunks = 
    [ 
        IHDR = headerChunk, 
        IDAT = dataChunk, 
        IEND = endChunk, 
        pHYs = physicalPixelDimensionsChunk, 
        cHRM = primaryChromaticitiesAndWhitePoint, 
        iCCP = embeddedICCProfileChunk 
    ], 
    unknownChunk = (chunkType) => BinaryFormat.Record( 
    [ 
        ChunkType = chunkType, 
        Data = BinaryFormat.Binary() 
    ]), 
    chunkHeader = BinaryFormat.Record([ 
        Length = BinaryFormat.SignedInteger32, 
        ChunkType = fourCC 
    ]), 
    chunkFooter = BinaryFormat.Record([ 
        Crc = BinaryFormat.SignedInteger32 
    ]), 
    chunk = BinaryFormat.Choice(chunkHeader, (header) =>  
        let 
            chunkType = header[ChunkType], 
            chunkData = Record.FieldOrDefault(knownChunks, chunkType, unknownChunk(chunkType)), 
            chunkDataLimited = BinaryFormat.Length(chunkData, header[Length]), 
            chunkWithFooter = BinaryFormat.Record([ 
                ChunkData = chunkDataLimited,  
                ChunkFooter = chunkFooter 
            ]), 
            chunkTransformed = BinaryFormat.Transform(chunkWithFooter, each [ChunkData]) 
        in 
            chunkTransformed), 
    fileFormatBuffered = BinaryFormat.Record([ 
        Header = fileHeader, 
        Chunks = BinaryFormat.List(chunk) 
    ]), 
    fileFormatStreaming = BinaryFormat.Choice(fileHeader, (header) => BinaryFormat.List(chunk), type list) 
in 
    fileFormatBuffered(file)
