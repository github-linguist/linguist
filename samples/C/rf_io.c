/**
** Copyright (c) 2011-2012, Karapetsas Eleftherios
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
**  1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
**  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in
**     the documentation and/or other materials provided with the distribution.
**  3. Neither the name of the Original Author of Refu nor the names of its contributors may be used to endorse or promote products derived from
**
**  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
**  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
**  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
**  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
**  SERVICES;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
**  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
**  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**/

#include <rf_io.h>

#include <rf_utils.h>
#include <stdio.h>
#include "io_private.h"
#include <errno.h>
#include <String/rfc_string.h> // for rfUTF8_IsContinuationbyte
#include <stdlib.h>// for malloc
#include <string.h>// for memcpy e.t.c.


// Reads a UTF-8 file descriptor until end of line or EOF is found and returns a UTF-8 byte buffer
int32_t rfFReadLine_UTF8(FILE* f,char** utf8,uint32_t* byteLength,uint32_t* bufferSize,char* eof)
{
    int32_t bytesN;
    uint32_t bIndex=0;
#ifdef RF_NEWLINE_CRLF
    char newLineFound = false;
#endif
    // allocate the utf8 buffer
    *bufferSize = RF_OPTION_FGETS_READBYTESN+4;
    RF_MALLOC(*utf8,*bufferSize)
    *byteLength = 0;
    // read the start
    bytesN = rfFgets_UTF8(*utf8,RF_OPTION_FGETS_READBYTESN,f,eof);
    (*byteLength)+=bytesN;

    if(bytesN < 0)//error check
    {
        LOG_ERROR("Failed to read a UTF-8 file",bytesN);
        free(*utf8);
        return bytesN;
    }
    // if the last character was a newline we are done
    if(*((*utf8)+bytesN-1) == (char)RF_LF)
    {
#ifdef RF_NEWLINE_CRLF
        if(*((*utf8)+bytesN-2) == (char)RF_CR)
        {
            *((*utf8)+bytesN-2) = RF_LF;
            *((*utf8)+bytesN-1) = '\0';
            (*byteLength)-=1;
        }
#endif
        return bytesN;
    }

    if(bytesN >= RF_OPTION_FGETS_READBYTESN && (*eof)==false)// if the size does not fit in the buffer and if we did not reach the end of file
    {
        // keep reading until we have read all until newline or EOF
        while(bytesN >= RF_OPTION_FGETS_READBYTESN && (*eof)==false)
        {
            if(*byteLength+RF_OPTION_FGETS_READBYTESN+4 >= *bufferSize)
            {
                *bufferSize=(*byteLength+RF_OPTION_FGETS_READBYTESN+4)*2;
                RF_REALLOC(*utf8,char,*bufferSize);
            }
            bIndex += bytesN;
            bytesN = rfFgets_UTF8((*utf8)+bIndex,RF_OPTION_FGETS_READBYTESN,f,eof);
            (*byteLength)+=bytesN;
            if(bytesN < 0)// error check
            {
                LOG_ERROR("StringX Initialization from file failed in file reading",bytesN);
                free(*utf8);
                return bytesN;
            }
            // if the last character was a newline break
            if(*((*utf8)+bIndex+bytesN-1) == (char)RF_LF)
            {
#ifdef RF_NEWLINE_CRLF
                newLineFound = true;
#endif
                break;
            }
        }// end of reading loop
#ifdef RF_NEWLINE_CRLF
        if(newLineFound==true)
            if(*((*utf8)+bIndex+bytesN-2) == (char)RF_CR)
            {
                *((*utf8)+bIndex+bytesN-2) = RF_LF;
                *((*utf8)+bIndex+bytesN-1) = '\0';
                (*byteLength)-=1;
            }

#endif
        return bIndex;
    }// end of size not fitting the initial buffer case
    else
    {
#ifdef RF_NEWLINE_CRLF
        // if the last character was a newline
        if(*((*utf8)+bytesN-1) == (char)RF_LF)
        {
            if(*((*utf8)+bytesN-2) == (char)RF_CR)
            {
                *((*utf8)+bytesN-2) = RF_LF;
                *((*utf8)+bytesN-1) = '\0';
                (*byteLength)-=1;
            }
        }
#endif
        // case of size fully fitting the buffer
        return bytesN;
    }
}
// Reads a Little Endian UTF-16 file descriptor until end of line or EOF is found and returns a UTF-8 byte buffer
int32_t rfFReadLine_UTF16LE(FILE* f,char** utf8,uint32_t* byteLength,char* eof)
{
    char buff[RF_OPTION_FGETS_READBYTESN+5];
    int32_t bytesN;
    uint32_t *codepoints,charsN,bIndex=0,buffSize=RF_OPTION_FGETS_READBYTESN+5,accum;
    char* tempBuff = 0,buffAllocated=false;

    bytesN = rfFgets_UTF16LE(buff,RF_OPTION_FGETS_READBYTESN,f,eof);
    accum = (uint32_t)bytesN;
    tempBuff = &buff[0];// point the tempBuff to the initial buffer for now
    if(bytesN < 0)// error check
    {
        LOG_ERROR("Failed to read from a Little Endian UTF-16 file",bytesN);
        return bytesN;
    }
    else if(bytesN >= RF_OPTION_FGETS_READBYTESN && (*eof)==false)// if the size does not fit in the buffer and if we did not reach the EOF
    {
        // allocate the temporary buffer and move the previous buffer's content inside it
        buffSize=buffSize*2+5;
        RF_MALLOC(tempBuff,buffSize);
        memcpy(tempBuff,buff,bytesN);
        bIndex=bytesN;
        buffAllocated = true;
        // keep reading until we have read all until newline or EOF
        do
        {
            bytesN = rfFgets_UTF16LE(tempBuff+bIndex,RF_OPTION_FGETS_READBYTESN,f,eof);
            accum += bytesN;
            if(bytesN < 0)// error check
            {
                LOG_ERROR("Failed to read from a Little Endian UTF-16 file",bytesN);
                free(tempBuff);
                return bytesN;
            }
            // realloc to have more space in the buffer for reading if needed
            if(accum+RF_OPTION_FGETS_READBYTESN+5 >= buffSize)
            {
                buffSize=(accum+RF_OPTION_FGETS_READBYTESN+5)*2;
                RF_REALLOC(tempBuff,char,buffSize);
            }
            bIndex += bytesN;
            // if the last character was newline break off the loop
            if( *(uint16_t*)(tempBuff+bIndex-2)== (uint16_t)RF_LF)
                break;
        }while(bytesN >= RF_OPTION_FGETS_READBYTESN && (*eof)==false);//end of reading loop
    }// end of size not fitting the initial buffer case
    if(bytesN >0)//determine the amount of bytes read
        bIndex+=bytesN;
    // allocate the codepoints
    RF_MALLOC(codepoints,(bIndex+5)*2)
    // decode it into codepoints
    if(rfUTF16_Decode(tempBuff,&charsN,codepoints)==false)
    {
        free(codepoints);
        if(buffAllocated==true)
            free(tempBuff);
        LOG_ERROR("Failed to Decode UTF-16 from a File Descriptor",RE_UTF16_INVALID_SEQUENCE);
        return RE_UTF16_INVALID_SEQUENCE;
    }
    // now encode these codepoints into UTF8
    if(((*utf8)=rfUTF8_Encode(codepoints,charsN,byteLength)) == 0)
    {
        free(codepoints);
        if(buffAllocated==true)
            free(tempBuff);
        LOG_ERROR("Failed to encode the File Descriptor's UTF-16 bytestream to UTF-8",RE_UTF8_ENCODING);
        return RE_UTF8_ENCODING;// error
    }
    // success
    free(codepoints);
    if(buffAllocated==true)
        free(tempBuff);
#ifdef RF_NEWLINE_CRLF
    // if the last character was a newline
    if(*((*utf8)+(*byteLength)-1) == (char)RF_LF)
    {
        if(*((*utf8)+(*byteLength)-2) == (char)RF_CR)
        {
            *((*utf8)+(*byteLength)-2) = RF_LF;
            *((*utf8)+(*byteLength)-1) = '\0';
            (*byteLength)-=1;
        }
    }
#endif


    return bIndex;
}
// Reads a Big Endian UTF-16 file descriptor until end of line or EOF is found and returns a UTF-8 byte buffer
int32_t rfFReadLine_UTF16BE(FILE* f,char** utf8,uint32_t* byteLength,char* eof)
{
    char buff[RF_OPTION_FGETS_READBYTESN+5];
    int32_t bytesN;
    uint32_t *codepoints,charsN,bIndex=0,buffSize=RF_OPTION_FGETS_READBYTESN+5,accum;
    char* tempBuff = 0,buffAllocated=false;

    bytesN = rfFgets_UTF16BE(buff,RF_OPTION_FGETS_READBYTESN,f,eof);
    accum = (uint32_t)bytesN;
    tempBuff = &buff[0];// point the tempBuff to the initial buffer for now
    if(bytesN < 0)// error check
    {
        LOG_ERROR("Failed to read from a Big Endian UTF-16 file",bytesN);
        return bytesN;
    }
    else if(bytesN >= RF_OPTION_FGETS_READBYTESN && (*eof)==false)// if the size does not fit in the buffer and if we did not reach the EOF
    {
        // allocate the temporary buffer and move the previous buffer's content inside it
        buffSize=buffSize*2+5;
        RF_MALLOC(tempBuff,buffSize);
        memcpy(tempBuff,buff,bytesN);
        bIndex=bytesN;
        buffAllocated = true;
        // keep reading until we have read all until newline or EOF
        do
        {
            bytesN = rfFgets_UTF16BE(tempBuff+bIndex,RF_OPTION_FGETS_READBYTESN,f,eof);
            accum+=bytesN;
            if(bytesN < 0)// error check
            {
                LOG_ERROR("Failed to read from a Big Endian UTF-16 file",bytesN);
                free(tempBuff);
                return bytesN;
            }
            // realloc to have more space in the buffer for reading if needed
            if(accum+RF_OPTION_FGETS_READBYTESN+5 >= buffSize)
            {
                buffSize=(accum+RF_OPTION_FGETS_READBYTESN+5)*2;
                RF_REALLOC(tempBuff,char,buffSize);
            }
            bIndex += bytesN;
            // if the last character was newline break off the loop
            if( (*(uint16_t*)(tempBuff+bIndex-2))== (uint16_t)RF_LF)
                break;
        }while(bytesN >= RF_OPTION_FGETS_READBYTESN && (*eof)==false);// end of reading loop
    }// end of size not fitting the initial buffer case
    if(bytesN >0)// determine the amount of bytes read
        bIndex+=bytesN;
    // allocate the codepoints
    RF_MALLOC(codepoints,(bIndex+5)*2)
    // decode it into codepoints
    if(rfUTF16_Decode(tempBuff,&charsN,codepoints)==false)
    {
        free(codepoints);
        if(buffAllocated==true)
            free(tempBuff);
        LOG_ERROR("Failed to Decode UTF-16 from a File Descriptor",RE_UTF16_INVALID_SEQUENCE);
        return RE_UTF16_INVALID_SEQUENCE;
    }
    // now encode these codepoints into UTF8
    if(((*utf8)=rfUTF8_Encode(codepoints,charsN,byteLength)) == 0)
    {
        free(codepoints);
        if(buffAllocated==true)
            free(tempBuff);
        LOG_ERROR("Failed to encode the File Descriptor's UTF-16 bytestream to UTF-8",RE_UTF8_ENCODING);
        return RE_UTF8_ENCODING;//error
    }
    // success
    free(codepoints);
    if(buffAllocated==true)
        free(tempBuff);
#ifdef RF_NEWLINE_CRLF
    // if the last character was a newline
    if(*((*utf8)+(*byteLength)-1) == (char)RF_LF)
    {
        if(*((*utf8)+(*byteLength)-2) == (char)RF_CR)
        {
            *((*utf8)+(*byteLength)-2) = RF_LF;
            *((*utf8)+(*byteLength)-1) = '\0';
            (*byteLength)-=1;
        }
    }
#endif
    return bIndex;
}
// Reads a Big Endian UTF-32 file descriptor until end of line or EOF is found and returns a UTF-8 byte buffer
int32_t rfFReadLine_UTF32BE(FILE* f,char** utf8,uint32_t* byteLength,char* eof)
{
    char buff[RF_OPTION_FGETS_READBYTESN+7];
    int32_t bytesN;
    uint32_t *codepoints,bIndex=0,buffSize=RF_OPTION_FGETS_READBYTESN+7,accum;
    char* tempBuff = 0,buffAllocated=false;
    bytesN = rfFgets_UTF32BE(buff,RF_OPTION_FGETS_READBYTESN,f,eof);
    accum = (uint32_t)bytesN;
    tempBuff = &buff[0];// point the tempBuff to the initial buffer for now
    if(bytesN < 0)// error check
    {
        LOG_ERROR("Failed to read from a Big Endian UTF-32 file",bytesN);
        return bytesN;
    }
    else if(bytesN >= RF_OPTION_FGETS_READBYTESN && (*eof)==false)// if the size does not fit in the buffer and if we did not reach the EOF
    {
        // allocate the temporary buffer and move the previous buffer's content inside it
        buffSize=buffSize*2+7;
        RF_MALLOC(tempBuff,buffSize);
        memcpy(tempBuff,buff,bytesN);
        bIndex=bytesN;
        buffAllocated = true;
        // keep reading until we have read all until newline or EOF
        do
        {
            bytesN = rfFgets_UTF32BE(tempBuff+bIndex,RF_OPTION_FGETS_READBYTESN,f,eof);
            accum+=bytesN;
            if(bytesN < 0)// error check
            {
                LOG_ERROR("Failed to read from a Big Endian UTF-16 file",bytesN);
                free(tempBuff);
                return bytesN;
            }
            // realloc to have more space in the buffer for reading if needed
            if(accum+RF_OPTION_FGETS_READBYTESN+7 >= buffSize)
            {
                buffSize=(accum+RF_OPTION_FGETS_READBYTESN+7)*2;
                RF_REALLOC(tempBuff,char,buffSize);
            }
            bIndex += bytesN;
            // if the last character was newline break off the loop
            if( (*(uint32_t*)(tempBuff+bIndex-4))== (uint32_t)RF_LF)
                break;
        }while(bytesN >= RF_OPTION_FGETS_READBYTESN && (*eof)==false);// end of reading loop
    }// end of size not fitting the initial buffer case
    if(bytesN >0)//determine the amount of bytes read
        bIndex+=bytesN;
    // utf-32 is actually codepoints
    codepoints = (uint32_t*)tempBuff;
    // now encode these codepoints into UTF8
    if(((*utf8)=rfUTF8_Encode(codepoints,bIndex/4,byteLength)) == 0)
    {
        if(buffAllocated==true)
            free(tempBuff);
        LOG_ERROR("Failed to encode the File Descriptor's UTF-32 bytestream to UTF-8",RE_UTF8_ENCODING);
        return RE_UTF8_ENCODING;// error
    }
    // success
    if(buffAllocated==true)
        free(tempBuff);
#ifdef RF_NEWLINE_CRLF
    // if the last character was a newline
    if(*((*utf8)+(*byteLength)-1) == (char)RF_LF)
    {
        if(*((*utf8)+(*byteLength)-2) == (char)RF_CR)
        {
            *((*utf8)+(*byteLength)-2) = RF_LF;
            *((*utf8)+(*byteLength)-1) = '\0';
            (*byteLength)-=1;
        }
    }
#endif
    return bIndex;
}
// Reads a Little Endian UTF-32 file descriptor until end of line or EOF is found and returns a UTF-8 byte buffer
int32_t rfFReadLine_UTF32LE(FILE* f,char** utf8,uint32_t* byteLength,char* eof)
{
    char buff[RF_OPTION_FGETS_READBYTESN+7];
    int32_t bytesN;
    uint32_t *codepoints,bIndex=0,buffSize=RF_OPTION_FGETS_READBYTESN+7,accum;
    char* tempBuff = 0,buffAllocated=false;
    bytesN = rfFgets_UTF32LE(buff,RF_OPTION_FGETS_READBYTESN,f,eof);
    accum = (uint32_t) bytesN;
    tempBuff = &buff[0];// point the tempBuff to the initial buffer for now
    if(bytesN < 0)// error check
    {
        LOG_ERROR("Failed to read from a Little Endian UTF-32 file",bytesN);
        return bytesN;
    }
    else if(bytesN >= RF_OPTION_FGETS_READBYTESN && (*eof)==false)// if the size does not fit in the buffer and if we did not reach the EOF
    {
        // allocate the temporary buffer and move the previous buffer's content inside it
        buffSize=buffSize*2+7;
        RF_MALLOC(tempBuff,buffSize);
        memcpy(tempBuff,buff,bytesN);
        bIndex=bytesN;
        buffAllocated = true;
        // keep reading until we have read all until newline or EOF
        do
        {
            bytesN = rfFgets_UTF32LE(tempBuff+bIndex,RF_OPTION_FGETS_READBYTESN,f,eof);
            accum +=bytesN;
            if(bytesN < 0)// error check
            {
                LOG_ERROR("Failed to read from a Little Endian UTF-16 file",bytesN);
                free(tempBuff);
                return bytesN;
            }
            // realloc to have more space in the buffer for reading if needed
            if(accum+RF_OPTION_FGETS_READBYTESN+7 >= buffSize)
            {
                buffSize=(accum+RF_OPTION_FGETS_READBYTESN+7)*2;
                RF_REALLOC(tempBuff,char,buffSize);
            }
            bIndex += bytesN;
            // if the last character was newline break off the loop
            if( (*(uint32_t*)(tempBuff+bIndex-4))== (uint32_t)RF_LF)
                break;
        }while(bytesN >= RF_OPTION_FGETS_READBYTESN && (*eof)==false);// end of reading loop
    }// end of size not fitting the initial buffer case
    if(bytesN >0)// determine the amount of bytes read
        bIndex+=bytesN;
    // utf-32 is actually codepoints
    codepoints = (uint32_t*)tempBuff;
    // now encode these codepoints into UTF8
    if(((*utf8)=rfUTF8_Encode(codepoints,bIndex/4,byteLength)) == 0)
    {
        if(buffAllocated==true)
            free(tempBuff);
        LOG_ERROR("Failed to encode the File Descriptor's UTF-32 bytestream to UTF-8",RE_UTF8_ENCODING);
        return RE_UTF8_ENCODING;// error
    }
    // success
    if(buffAllocated==true)
        free(tempBuff);
#ifdef RF_NEWLINE_CRLF
    // if the last character was a newline
    if(*((*utf8)+(*byteLength)-1) == (char)RF_LF)
    {
        if(*((*utf8)+(*byteLength)-2) == (char)RF_CR)
        {
            *((*utf8)+(*byteLength)-2) = RF_LF;
            *((*utf8)+(*byteLength)-1) = '\0';
            (*byteLength)-=1;
        }
    }
#endif
    return bIndex;
}

// This is a function that's similar to c library fgets but it also returns the number of bytes read and works for UTF-32 encoded files
int32_t rfFgets_UTF32BE(char* buff,uint32_t num,FILE* f,char* eofReached)
{
    uint32_t size,c;
    int32_t error;
    // initialization
    *eofReached = false;
    size = 0;
    // if end of file or end of line is not found, keep reading
    do{
        if((error=rfFgetc_UTF32BE(f,(uint32_t*)(buff+size))) != RF_SUCCESS)
        {
            if(error == RE_FILE_EOF)
            {
                break;// EOF found
                *eofReached = true;
            }
            LOG_ERROR("Reading error while reading from a Big Endian UTF-32 file",error);
            return error;
        }
        size+= 4;
        // if we have read the number of characters requested by the function
        if(size >= num)
        {
            break;
        }
        // get the last character read
        c = *(uint32_t*)(buff+size-4);
    }while(c != (uint32_t)EOF && !RF_HEXEQ_UI(c,RF_LF));
    // null terminate the buffer for UTF32
    buff[size] =  buff[size+1] = buff[size+2] = buff[size+3] = '\0';
    // finally check yet again for end of file right after the new line
    if((error=rfFgetc_UTF32BE(f,&c))!=RF_SUCCESS)
    {
        if(error == RE_FILE_EOF)
        {// EOF
            *eofReached = true;
        }
        else
        {
            LOG_ERROR("Reading error while reading from a Big Endian UTF-32 file",error);
            return error;
        }
    }
    else// undo the peek ahead of the file pointer
        fseek(f,-4,SEEK_CUR);
    return size;
}
// This is a function that's similar to c library fgets but it also returns the number of bytes read and works for UTF-32 encoded files
int32_t rfFgets_UTF32LE(char* buff,uint32_t num,FILE* f,char* eofReached)
{
    uint32_t size,c;
    int32_t error;
    // initialization
    *eofReached = false;
    size = 0;
    // if end of file or end of line is not found, keep reading
    do{
        if((error=rfFgetc_UTF32LE(f,(uint32_t*)(buff+size))) != RF_SUCCESS)
        {
            if(error == RE_FILE_EOF)
            {
                break;// EOF found
                *eofReached = true;
            }
            LOG_ERROR("Reading error while reading from a Little Endian UTF-32 file",error);
            return error;
        }
        size+= 4;
        // if we have read the number of characters requested by the function
        if(size >= num)
        {
            break;
        }
        // get the last character read
        c = *(uint32_t*)(buff+size-4);
    }while(c !=(uint32_t) EOF && !RF_HEXEQ_UI(c,RF_LF));
    // null terminate the buffer for UTF32
    buff[size] =  buff[size+1] = buff[size+2] = buff[size+3] = '\0';
    // finally check yet again for end of file right after the new line
    if((error=rfFgetc_UTF32LE(f,&c))!=RF_SUCCESS)
    {
        if(error == RE_FILE_EOF)
        {// EOF
            *eofReached = true;
        }
        else
        {
            LOG_ERROR("Reading error while reading from a Little Endian UTF-32 file",error);
            return error;
        }
    }
    else// undo the peek ahead of the file pointer
        fseek(f,-4,SEEK_CUR);
    return size;
}
// Gets a number of bytes from a BIG endian UTF-16 file descriptor
int32_t rfFgets_UTF16BE(char* buff,uint32_t num,FILE* f,char* eofReached)
{
    uint32_t size,c;
    int32_t bytesN;
    // initialization
    *eofReached = false;
    size = 0;
    // if end of file or end of line is not found, keep reading
    do{
        bytesN = rfFgetc_UTF16BE(f,(uint32_t*)(buff+size),false);
        // error check
        if(bytesN < 0)
        {
            if(bytesN == RE_FILE_EOF)
            {
                break;// EOF found
                *eofReached = true;
            }
            else
                return bytesN;
        }
        size+= bytesN;
        // if we have read the number of characters requested by the function
        if(size >= num)
        {
            break;
        }
        // get the last character read
        c = *(uint32_t*)(buff+size-bytesN);
    }while(c !=(uint32_t) EOF && !RF_HEXEQ_UI(c,RF_LF));
    // null terminate the buffer for UTF16
    buff[size] =  buff[size+1] = '\0';
    // finally check yet again for end of file right after the new line
    bytesN = rfFgetc_UTF16BE(f,&c,false);
    if(bytesN < 0)
    {
        if(bytesN == RE_FILE_EOF)
        {// EOF
            *eofReached = true;
        }
        else// error
            return bytesN;
    }
    else// undo the peek ahead of the file pointer
        fseek(f,-bytesN,SEEK_CUR);
    return size;
}
// Gets a number of bytes from a Little endian UTF-16 file descriptor
int32_t rfFgets_UTF16LE(char* buff,uint32_t num,FILE* f,char* eofReached)
{
    uint32_t size,c;
    int32_t bytesN;
    // initialization
    *eofReached = false;
    size = 0;
    // if end of file or end of line is not found, keep reading
    do{
        bytesN = rfFgetc_UTF16LE(f,(uint32_t*)(buff+size),false);
        // error check
        if(bytesN < 0)
        {
            if(bytesN == RE_FILE_EOF)
            {
                break;// EOF found
                *eofReached = true;
            }
            else
                return bytesN;
        }
        size+= bytesN;
        // if we have read the number of characters requested by the function
        if(size >= num)
        {
            break;
        }
        // get the last character read
        c = *(uint32_t*)(buff+size-bytesN);
    }while(c !=(uint32_t) EOF && !RF_HEXEQ_UI(c,RF_LF));
    // null terminate the buffer for UTF16
    buff[size] =  buff[size+1] = '\0';
    // finally check yet again for end of file right after the new line
    bytesN = rfFgetc_UTF16LE(f,&c,false);
    if(bytesN < 0)
    {
        if(bytesN == RE_FILE_EOF)
        {// EOF
            *eofReached = true;
        }
        else// error
            return bytesN;
    }
    else// undo the peek ahead of the file pointer
        fseek(f,-bytesN,SEEK_CUR);

    return size;
}

// Gets a number of bytes from a UTF-8 file descriptor
int32_t rfFgets_UTF8(char* buff,uint32_t num,FILE* f,char* eofReached)
{
    uint32_t size,c;
    int32_t bytesN;
    // initialization
    *eofReached = false;
    size = 0;
    // if end of file or end of line is not found, keep reading
    do{
        bytesN = rfFgetc_UTF8(f,(uint32_t*)(buff+size),false);
        // error check
        if(bytesN < 0)
        {
            if(bytesN == RE_FILE_EOF)
            {
                break;// EOF found
                *eofReached = true;
            }
            else
                return bytesN;
        }
        size+= bytesN;
        // if we have read the number of characters requested by the function
        if(size >= num)
        {
            break;
        }
        // get the last character
        c = *(uint32_t*)(buff+size-bytesN);
    }while(c !=(uint32_t) EOF && !RF_HEXEQ_UI(c,RF_LF));
    // null terminate the buffer for UTF8
    buff[size] = '\0';
    // finally check yet again for end of file right after the new line
    if( RF_HEXEQ_C(fgetc(f),EOF))
    {// check for error
        if(ferror(f) != 0)
        {
            LOG_ERROR("During reading a UTF-8 file there was a read error",RE_FILE_READ);
            return RE_FILE_READ;
        }
        // if not it's end of file, so note it and take the pointer back by 1
        *eofReached = true;
    }// undo the peek ahead of the file pointer
    else
        fseek(f,-1,SEEK_CUR);
    return size;
}
// Gets a unicode character from a UTF-8 file descriptor
int32_t rfFgetc_UTF8(FILE* f,uint32_t *ret,char cp)
{
    char c,c2,c3,c4;
    if( (c = fgetc(f)) == EOF)
    {
        i_READ_CHECK(f,"While reading a UTF-8 character from the stream")
        else
            return RE_FILE_EOF;
    }
     // if the lead bit of the byte is 0 then range is : U+0000 to U+0007F (1 byte)
     if( ((c & 0x80)>>7) == 0 )
     {
        /// success
        if(cp == true)
            *ret = c;
        else
        {
            *ret = 0;
            char* cc = (char*) ret;
            cc[0] = c;
        }
        return 1;
     }
     else// we need more bytes
     {
        // if the leading bits are in the form of 0b110xxxxx then range is: U+0080 to U+07FF (2 bytes)
        if( RF_HEXEQ_C( ( (~(c ^  0xC0))>>5), 0x7) )
        {
            // also remember bytes 0xC0 and 0xC1 are invalid and could possibly be found in a starting byte of this type so check for them here
            if( RF_HEXEQ_C(c,0xC0) || RF_HEXEQ_C(c,0xC1))
            {
                LOG_ERROR("While decoding a UTF-8 file byte stream, an invalid byte was encountered",RE_UTF8_INVALID_SEQUENCE_INVALID_BYTE);
                return RE_UTF8_INVALID_SEQUENCE_INVALID_BYTE;
            }
            // so now read the next byte
            if( (c2 = fgetc(f)) == EOF)
            {
                i_READ_CHECK(f,"While reading a UTF-8 character from a file stream")
                else
                {
                    LOG_ERROR("While decoding a UTF-8 file byte stream, EOF was encountered abruplty in-between bytes",RE_UTF8_INVALID_SEQUENCE_END);
                    return RE_FILE_EOF;
                }
            }
            // if this second byte is NOT a continuation byte
            if( !rfUTF8_IsContinuationByte(c2))
            {
                LOG_ERROR("While decoding a UTF-8 file byte stream, and expecting a continuation byte, one was not found",RE_UTF8_INVALID_SEQUENCE_CONBYTE);
                return RE_UTF8_INVALID_SEQUENCE_CONBYTE;
            }
            /// success
            if(cp == true)// return decoded codepoint
            {
                *ret = 0;
                // from the second byte take the first 6 bits
                *ret = (c2 & 0x3F) ;
                // from the first byte take the first 5 bits and put them in the start
                *ret |= ((c & 0x1F) << 6);
            }
            else
            {
                *ret = 0;
                char* cc = (char*)ret;
                cc[0] = c; cc[1] = c2;
            }
            return 2;

        }// end of the 2 bytes case
        // if the leading bits are in the form of 0b1110xxxx then range is U+0800 to U+FFFF  (3 bytes)
        else if( RF_HEXEQ_C( ( (~(c ^ 0xE0))>>4),0xF))
        {
            // so now read the next 2 bytes
            if( (c2 = fgetc(f)) == EOF)
            {
                i_READ_CHECK(f,"While reading a UTF-8 character from a file stream")
                else
                {
                    LOG_ERROR("While decoding a UTF-8 file byte stream, EOF was encountered abruplty in-between bytes",RE_UTF8_INVALID_SEQUENCE_END);
                    return RE_FILE_EOF;
                }
            }
            if( (c3 = fgetc(f)) == EOF)
            {
                i_READ_CHECK(f,"While reading a UTF-8 character from a file stream")
                else
                {
                    LOG_ERROR("While decoding a UTF-8 file byte stream, EOF was encountered abruplty in-between bytes",RE_UTF8_INVALID_SEQUENCE_END);
                    return RE_FILE_EOF;
                }
            }
            // if the subsequent bytes are NOT  continuation bytes
            if( !rfUTF8_IsContinuationByte(c2) || !rfUTF8_IsContinuationByte(c3))
            {
                LOG_ERROR("While decoding a UTF-8 file byte stream, and expecting a continuation byte, one was not found",RE_UTF8_INVALID_SEQUENCE_CONBYTE);
                return RE_UTF8_INVALID_SEQUENCE_CONBYTE;
            }
            /// success
            if(cp == true)// if we need to decode the codepoint
            {
                *ret = 0;
                // from the third byte take the first 6 bits
                *ret = (c3 & 0x3F) ;
                // from the second byte take the first 6 bits and put them to the left of the previous 6 bits
                *ret |= ((c2 & 0x3F) << 6);
                // from the first byte take the first 4 bits and put them to the left of the previous 6 bits
                *ret |= ((c & 0xF) << 12);
            }
            else
            {
                *ret = 0;
                char* cc = (char*)ret;
                cc[0] = c; cc[1] = c2; cc[2] = c3;
            }
            return 3;
        }// end of 3 bytes case
        // if the leading bits are in the form of 0b11110xxx then range is U+010000 to U+10FFFF (4 bytes)
        else if(RF_HEXEQ_C( ( (~(c ^ 0xF0))>>3), 0x1F))
        {
            // in this type of starting byte a number of invalid bytes can be encountered. We have to check for them.
            if(RF_HEXGE_C(c,0xBF)) //invalid byte value are from 0xBF to 0xFF
            {
                LOG_ERROR("While decoding a UTF-8 file byte stream, an invalid byte was encountered",RE_UTF8_INVALID_SEQUENCE_INVALID_BYTE);
                return RE_UTF8_INVALID_SEQUENCE_INVALID_BYTE;
            }
            // so now read the next 3 bytes
            if( (c2 = fgetc(f)) == EOF)
            {
                i_READ_CHECK(f,"While reading a UTF-8 character from a file stream")
                else
                {
                    LOG_ERROR("While decoding a UTF-8 file byte stream, EOF was encountered abruplty in-between bytes",RE_UTF8_INVALID_SEQUENCE_END);
                    return RE_FILE_EOF;
                }
            }
            if( (c3 = fgetc(f)) == EOF)
            {
                i_READ_CHECK(f,"While reading a UTF-8 character from a file stream")
                else
                {
                    LOG_ERROR("While decoding a UTF-8 file byte stream, EOF was encountered abruplty in-between bytes",RE_UTF8_INVALID_SEQUENCE_END);
                    return RE_FILE_EOF;
                }
            }
            if( (c4 = fgetc(f)) == EOF)
            {
                i_READ_CHECK(f,"While reading a UTF-8 character from a file stream")
                else
                {
                    LOG_ERROR("While decoding a UTF-8 file byte stream, EOF was encountered abruplty in-between bytes",RE_UTF8_INVALID_SEQUENCE_END);
                    return RE_FILE_EOF;
                }
            }
            // if the subsequent bytes are NOT  continuation bytes
            if( !rfUTF8_IsContinuationByte(c2) || !rfUTF8_IsContinuationByte(c3) || !rfUTF8_IsContinuationByte(c4))
            {
                LOG_ERROR("While decoding a UTF-8 file byte stream, and expecting a continuation byte, one was not found",RE_UTF8_INVALID_SEQUENCE_CONBYTE);
                return RE_UTF8_INVALID_SEQUENCE_CONBYTE;
            }
            /// success
            if(cp == true) //if we need to decode the codepoint
            {
                *ret = 0;
                // from the fourth byte take the first 6 bits
                *ret = (c4 & 0x3F) ;
                // from the third byte take the first 6 bits and put them to the left of the previous 6 bits
                *ret |= ((c3 & 0x3F) << 6);
                // from the second byte take the first 6 bits and put them to the left of the previous 6 bits
                *ret |= ((c2 & 0x3F) << 12);
                // from the first byte take the first 3 bits and put them to the left of the previous 6 bits
                *ret |= ((c & 0x7) << 18);
            }
            else
            {
                *ret = 0;
                char* cc = (char*)ret;
                cc[0] = c; cc[1] = c2; cc[2] = c3; cc[3]=c4;
            }
            return 4;
        }// end of 4 bytes case
     }// end of needing more than 1 byte

    // if we get here means the 1st byte belonged to none of the 4 cases
    LOG_ERROR("While decoding a UTF-8 file byte stream, the first byte of a character was invalid UTF-8",RE_UTF8_INVALID_SEQUENCE_INVALID_BYTE);
    return RE_UTF8_INVALID_SEQUENCE_INVALID_BYTE;
}

// Gets a unicode character from a Big Endian UTF-16 file descriptor
int32_t rfFgetc_UTF16BE(FILE* f,uint32_t *c,char cp)
{
    char swapE=false;
    uint16_t v1,v2;
    // check if we need to be swapping
    if(rfUTILS_Endianess() == RF_LITTLE_ENDIAN)
            swapE = true;
    // read the first 2 bytes
    if(fread(&v1,2,1,f) != 1)
    {
        i_READ_CHECK(f,"While reading a UTF-16 from a Big Endian File stream")
        else
            return RE_FILE_EOF;
    }
    if(swapE)// swap endianess if needed
        rfUTILS_SwapEndianUS(&v1);
    /* If the value is in the surrogate area */
    if(RF_HEXGE_US(v1,0xD800) && RF_HEXLE_US(v1,0xDFFF))
    {
        if(RF_HEXL_US(v1,0xD800) || RF_HEXG_US(v1,0xDBFF))
        {
            LOG_ERROR("While reading a Big endian UTF-16 file stream the first byte encountered held an illegal value",RE_UTF16_INVALID_SEQUENCE);
            return RE_UTF16_INVALID_SEQUENCE;
        }
        // then we also need to read its surrogate pair
        if(fread(&v2,2,1,f) != 1)
        {
            i_READ_CHECK(f,"While reading a UTF-16 from a Big Endian File stream")
            else
            {
                LOG_ERROR("While decoding a UTF-16 Big Endian file byte stream, EOF was encountered abruplty when expecting a surrogate pair",RE_UTF16_NO_SURRPAIR);
                return RE_FILE_EOF;
            }
        }
        if(swapE)// swap endianess if needed
            rfUTILS_SwapEndianUS(&v2);
        if(RF_HEXL_US(v2,0xDC00) || RF_HEXG_US(v2,0xDFFF))
        {
            LOG_ERROR("While reading a Big endian UTF-16 file stream the surrogate pair encountered held an illegal value",RE_UTF16_INVALID_SEQUENCE);
            return RE_UTF16_INVALID_SEQUENCE;
        }
        if(cp == true)// if the user wants the decoded codepoint
        {
            *c = 0;
            *c = v2&0x3ff;
            *c |= (10<<v1&0x3ff);
            *c += 0x10000;
            return 4;
        }// else
        *c = 0;
        uint16_t* cc = (uint16_t*)c;
        cc[0] = v1; cc[1] = v2;
        return 4;
    }// end of surrogate pair existence case
    // else no surrogate pair exists so v1 is all we need
    *c = v1;
    return 2;
}
// Gets a unicode character from a Little Endian UTF-16 file descriptor
int32_t rfFgetc_UTF16LE(FILE* f,uint32_t *c,char cp)
{
    char swapE=false;
    uint16_t v1,v2;
    // check if we need to be swapping
    if(rfUTILS_Endianess() == RF_BIG_ENDIAN)
            swapE = true;
    // read the first 2 bytes
    if(fread(&v1,2,1,f) != 1)
    {
        i_READ_CHECK(f,"While reading a UTF-16 from a Little Endian File stream")
        else
            return RE_FILE_EOF;
    }
    if(swapE)// swap endianess if needed
        rfUTILS_SwapEndianUS(&v1);
    /* If the value is in the surrogate area */
    if(RF_HEXGE_US(v1,0xD800) && RF_HEXLE_US(v1,0xDFFF))
    {
        if(RF_HEXL_US(v1,0xD800) || RF_HEXG_US(v1,0xDBFF))
        {
            LOG_ERROR("While reading a little endian UTF-16 file stream the first byte encountered held an illegal value",RE_UTF16_INVALID_SEQUENCE);
            return RE_UTF16_INVALID_SEQUENCE;
        }
        // then we also need to read its surrogate pair
        if(fread(&v2,2,1,f) != 1)
        {
            i_READ_CHECK(f,"While reading a UTF-16 from a Little Endian File stream")
            else
            {
                LOG_ERROR("While decoding a UTF-16 Little Endian file byte stream, EOF was encountered abruplty when expecting a surrogate pair",RE_UTF16_NO_SURRPAIR);
                return RE_FILE_EOF;
            }
        }
        if(swapE)// swap endianess if needed
            rfUTILS_SwapEndianUS(&v2);
        if(RF_HEXL_US(v2,0xDC00) || RF_HEXG_US(v2,0xDFFF))
        {
            LOG_ERROR("While reading a little endian UTF-16 file stream the surrogate pair encountered held an illegal value",RE_UTF16_INVALID_SEQUENCE);
            return RE_UTF16_INVALID_SEQUENCE;
        }
        if(cp == true)// if the user wants the decoded codepoint
        {
            *c = 0;
            *c = v2&0x3ff;
            *c |= (10<<v1&0x3ff);
            *c += 0x10000;
            return 4;
        }// else
        *c = 0;
        uint16_t* cc = (uint16_t*)c;
        cc[0] = v1; cc[1] = v2;
        return 4;
    }// end of surrogate pair existence case
    // else no surrogate pair exists so v1 is all we need
    *c = v1;
    return 2;
}
// Gets a unicode character from a UTF-32 Little Endian file descriptor
int32_t rfFgetc_UTF32LE(FILE* f,uint32_t *c)
{
    // read the next 4 bytes
    if(fread(c,4,1,f) != 1)
    {
        i_READ_CHECK(f,"While reading a UTF-32 character from a Little Endian File stream")
        else
            return RE_FILE_EOF;
    }
    // check if we need to be swapping
    if(rfUTILS_Endianess() == RF_BIG_ENDIAN)
            rfUTILS_SwapEndianUI(c);
    return RF_SUCCESS;
}
// Gets a unicode character from a UTF-32 Little Endian file descriptor
int32_t rfFgetc_UTF32BE(FILE* f,uint32_t *c)
{
    // read the next 4 bytes
    if(fread(c,4,1,f) != 1)
    {
        i_READ_CHECK(f,"While reading a UTF-32 character from a Big Endian File stream")
        else
            return RE_FILE_EOF;
    }
    // check if we need to be swapping
    if(rfUTILS_Endianess() == RF_LITTLE_ENDIAN)
            rfUTILS_SwapEndianUI(c);
    return RF_SUCCESS;
}

// Moves a unicode character backwards in a big endian UTF-32 file stream
int32_t rfFback_UTF32BE(FILE* f,uint32_t *c)
{
    // go back and read the last 4 bytes
    if(fseek(f,-4,SEEK_CUR) != 0)
    {
        i_FSEEK_CHECK("Going backwards in a Big Endian UTF-32 file stream")
    }
    if(fread(c,4,1,f) != 1)
    {
        i_READ_CHECK(f,"Reading four bytes backwards in a Big Endian UTF-32 file stream")
    }
    if(fseek(f,-4,SEEK_CUR) != 0)
    {
        i_FSEEK_CHECK("Going backwards in a Big Endian UTF-32 file stream")
    }
    // check if we need to be swapping
    if(rfUTILS_Endianess() == RF_LITTLE_ENDIAN)
            rfUTILS_SwapEndianUI(c);
    return RF_SUCCESS;
}
// Moves a unicode character backwards in a little endian UTF-32 file stream
int32_t rfFback_UTF32LE(FILE* f,uint32_t *c)
{
    // go back and read the last 4 bytes
    if(fseek(f,-4,SEEK_CUR) != 0)
    {
        i_FSEEK_CHECK("Going backwards in a Big Endian UTF-32 file stream")
    }
    if(fread(c,4,1,f) != 1)
    {
        i_READ_CHECK(f,"Reading four bytes backwards in a Big Endian UTF-32 file stream")
    }
    if(fseek(f,-4,SEEK_CUR) != 0)
    {
        i_FSEEK_CHECK("Going backwards in a Big Endian UTF-32 file stream")
    }
    // check if we need to be swapping
    if(rfUTILS_Endianess() == RF_BIG_ENDIAN)
            rfUTILS_SwapEndianUI(c);
    return RF_SUCCESS;
}
// Moves a unicode character backwards in a big endian UTF-16 file stream
int32_t rfFback_UTF16BE(FILE* f,uint32_t *c)
{
    char swapE=false;
    uint16_t v1,v2;
    // check if we need to be swapping
    if(rfUTILS_Endianess() == RF_LITTLE_ENDIAN)
            swapE = true;
    // go back and read the last 2 bytes
    if(fseek(f,-2,SEEK_CUR) != 0)
    {
        i_FSEEK_CHECK("Going backwards in a Big Endian UTF-16 file stream")
    }
    if(fread(&v1,2,1,f) != 1)
    {
        i_READ_CHECK(f,"Reading two bytes backwards in a Big Endian UTF-16 file stream")
    }
    if(fseek(f,-2,SEEK_CUR) != 0)
    {
        i_FSEEK_CHECK("Going backwards in a Big Endian UTF-16 file stream")
    }
    if(swapE)// swap endianess if needed
        rfUTILS_SwapEndianUS(&v1);
    /* If the word is a surrogate pair */
    if(RF_HEXGE_US(v1,0xDC00) && RF_HEXLE_US(v1,0xDFFF))
    {
        // go back and read 2 more bytes
        if(fseek(f,-2,SEEK_CUR) != 0)
        {
            i_FSEEK_CHECK("Going backwards in a Big Endian UTF-16 file stream")
        }
        if(fread(&v2,2,1,f) != 1)
        {
            i_READ_CHECK(f,"Reading two bytes backwards in a Big Endian UTF-16 file stream")
        }
        if(fseek(f,-2,SEEK_CUR) != 0)
        {
            i_FSEEK_CHECK("Going backwards in a Big Endian UTF-16 file stream")
        }
        if(swapE)// swap endianess if needed
            rfUTILS_SwapEndianUS(&v2);
        if(RF_HEXL_US(v2,0xD800) || RF_HEXG_US(v2,0xDBFF))
        {
            LOG_ERROR("While reading bytes backwards in a Big Endian UTF-16 file stream the encountered byte was supposed to be a surrogate pair but its pair is of illegal value",RE_UTF16_INVALID_SEQUENCE);
            return RE_UTF16_INVALID_SEQUENCE;
        }
        // get the codepoint
        *c = 0;
        *c = v1&0x3ff;
        *c |= (10<<v2&0x3ff);
        *c += 0x10000;
        return 4;
    }// end of surrogate pair case
    // getting here means this word is what we seek. Let's confirm
    if(RF_HEXL_US(v1,0xD800) || RF_HEXG_US(v1,0xDFFF))
    {
        // get the codepoint
         *c = 0;
        uint16_t* cc = (uint16_t*) c;
        cc[0] = v1;
        return 2;
    }
    // else invald sequence
    LOG_ERROR("While reading bytes backwards in a Big Endian UTF-16 file stream the encountered byte had an illegal value",RE_UTF16_INVALID_SEQUENCE);
    return RE_UTF16_INVALID_SEQUENCE;
}

// Moves a unicode character backwards in a little endian UTF-16 file stream
int32_t rfFback_UTF16LE(FILE* f,uint32_t *c)
{
    char swapE=false;
    uint16_t v1,v2;
    // check if we need to be swapping
    if(rfUTILS_Endianess() == RF_BIG_ENDIAN)
            swapE = true;
    // go back and read the last 2 bytes
    if(fseek(f,-2,SEEK_CUR) != 0)
    {
        i_FSEEK_CHECK("Going backwards in a Little Endian UTF-16 file stream")
    }
    if(fread(&v1,2,1,f) != 1)
    {
        i_READ_CHECK(f,"Reading two bytes backwards in a Little Endian UTF-16 file stream")
    }
    if(fseek(f,-2,SEEK_CUR) != 0)
    {
        i_FSEEK_CHECK("Going backwards in a Big Endian UTF-16 file stream")
    }
    if(swapE)// swap endianess if needed
        rfUTILS_SwapEndianUS(&v1);
    /* If the word is a surrogate pair */
    if(RF_HEXGE_US(v1,0xDC00) && RF_HEXLE_US(v1,0xDFFF))
    {
        // go back and read 2 more bytes
        if(fseek(f,-2,SEEK_CUR) != 0)
        {
            i_FSEEK_CHECK("Going backwards in a Little Endian UTF-16 file stream")
        }
        if(fread(&v2,2,1,f) != 1)
        {
            i_READ_CHECK(f,"Reading two bytes backwards in a Little Endian UTF-16 file stream")
        }
        if(fseek(f,-2,SEEK_CUR) != 0)
        {
            i_FSEEK_CHECK("Going backwards in a Big Endian UTF-16 file stream")
        }
        if(swapE)// swap endianess if needed
            rfUTILS_SwapEndianUS(&v2);
        if(RF_HEXL_US(v2,0xD800) || RF_HEXG_US(v2,0xDBFF))
        {
            LOG_ERROR("While reading bytes backwards in a Little Endian UTF-16 file stream the encountered byte was supposed to be a surrogate pair but its pair is of illegal value",RE_UTF16_INVALID_SEQUENCE);
            return RE_UTF16_INVALID_SEQUENCE;
        }
        // get the codepoint
        *c = 0;
        *c = v1&0x3ff;
        *c |= (10<<v2&0x3ff);
        *c += 0x10000;
        return 4;
    }// end of surrogate pair case
    // getting here means this word is what we seek. Let's confirm
    if(RF_HEXL_US(v1,0xD800) || RF_HEXG_US(v1,0xDFFF))
    {
        // get the codepoint
         *c = 0;
        uint16_t* cc = (uint16_t*) c;
        cc[0] = v1;
        return 2;
    }
    // else invald sequence
    LOG_ERROR("While reading bytes backwards in a Little Endian UTF-16 file stream the encountered byte had an illegal value",RE_UTF16_INVALID_SEQUENCE);
    return RE_UTF16_INVALID_SEQUENCE;
}

// Moves a unicode character backwards in a UTF-8 file stream
int32_t rfFback_UTF8(FILE* f,uint32_t *c)
{
    // read one byte before the current
    int i = 0;
    char bytes[4];
    do
    {
        if(fseek(f,-1,SEEK_CUR) != 0)
        {
            i_FSEEK_CHECK("Going backwards in a UTF-8 file")
        }
        if((bytes[i] = fgetc(f)) == EOF)
        {
            i_READ_CHECK(f,"Reading a byte backwards in a UTF-8 file")
        }
        if(fseek(f,-1,SEEK_CUR) != 0)
        {
            i_FSEEK_CHECK("Going backwards in a UTF-8 file")
        }
        i++;
    }while(rfUTF8_IsContinuationByte(bytes[i-1]));
    switch(i)// depending on the number of bytes read backwards
    {
        case 4:
            *c = 0;
            // from the fourth byte take the first 6 bits
            *c = (bytes[0] & 0x3F) ;
            // from the third byte take the first 6 bits and put them to the left of the previous 6 bits
            *c |= ((bytes[1] & 0x3F) << 6);
            // from the second byte take the first 6 bits and put them to the left of the previous 6 bits
            *c |= ((bytes[2] & 0x3F) << 12);
            // from the first byte take the first 3 bits and put them to the left of the previous 6 bits
            *c |= ((bytes[3] & 0x7) << 18);
        break;
        case 3:
            *c = 0;
            // from the third byte take the first 6 bits
            *c = (bytes[0] & 0x3F) ;
            // from the second byte take the first 6 bits and put them to the left of the previous 6 bits
            *c |= ((bytes[1] & 0x3F) << 6);
            // from the first byte take the first 4 bits and put them to the left of the previous 6 bits
            *c |= ((bytes[2] & 0xF) << 12);
        break;
        case 2:
            *c = 0;
            // from the second byte take the first 6 bits
            *c = (bytes[0] & 0x3F) ;
            // from the first byte take the first 5 bits and put them in the start
            *c |= ((bytes[1] & 0x1F) << 6);
        break;
        case 1:
            *c = bytes[0];
        break;
        default:
            LOG_ERROR("During moving one unicode character back in a UTF-8 filestream moved an abnormal number of bytes",RE_UTF8_INVALID_SEQUENCE);
            return RE_UTF8_INVALID_SEQUENCE;
        break;
    }
    return i;
}

