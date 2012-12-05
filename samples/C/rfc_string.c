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
#include <errno.h>

#include <String/rfc_string.h>
// include bitwise operations
#include <rf_utils.h>
// include the private functions and macros
#include "string_private.h"
// include io_private only for the write check
#include "../IO/io_private.h"
// include the extended strin
#include <String/rfc_stringx.h>
// for HUGE_VAL definition
#include <math.h>

#include <rf_localmem.h> // for the local stack memory

/*********************************************************************** Start of the RF_String functions *****************************************************************************************/

/*-------------------------------------------------------------------------Methods to create an RF_String-------------------------------------------------------------------------------*/

// Allocates and returns a string with the given characters a refu string with the given characters. Given characters have to be in UTF-8. A check for valide sequence of bytes is performed.
#ifndef RF_OPTION_DEFAULT_ARGUMENTS
RF_String* rfString_Create(const char* s,...)
#else
RF_String* i_rfString_Create(const char* s,...)
#endif
{
    READ_VSNPRINTF_ARGS(s,s,0)

    // check for validity of the given sequence and get the character length
    uint32_t byteLength;
    if( rfUTF8_VerifySequence(buff,&byteLength) == RF_FAILURE)
    {
        LOG_ERROR("Error at String Allocation due to invalid UTF-8 byte sequence",RE_STRING_INIT_FAILURE);
        if(buffAllocated == true)
            free(buff);
        return 0;
    }

    RF_String* ret;
    RF_MALLOC(ret,sizeof(RF_String));
    // get length
    ret->byteLength = byteLength;

    // now that we know the length we can allocate the buffer and copy the bytes
    RF_MALLOC(ret->bytes,ret->byteLength+1);
    memcpy(ret->bytes,buff,ret->byteLength+1);
    if(buffAllocated==true)
        free(buff);
    return ret;
}
#ifdef RF_OPTION_DEFAULT_ARGUMENTS
RF_String* i_NVrfString_Create(const char* s)
{
    // check for validity of the given sequence and get the character length
    uint32_t byteLength;
    if( rfUTF8_VerifySequence(s,&byteLength) == RF_FAILURE)
    {
        LOG_ERROR("Error at String Allocation due to invalid UTF-8 byte sequence",RE_STRING_INIT_FAILURE);
        return 0;
    }

    RF_String* ret;
    RF_MALLOC(ret,sizeof(RF_String));
    // get length
    ret->byteLength = byteLength;

    // now that we know the length we can allocate the buffer and copy the bytes
    RF_MALLOC(ret->bytes,ret->byteLength+1);
    memcpy(ret->bytes,s,ret->byteLength+1);

    return ret;
}
#endif


// Allocates and returns a string with the given characters a refu string with the given characters. Given characters have to be in UTF-8. A check for valid sequence of bytes is performed.
RF_String* i_rfString_CreateLocal1(const char* s,...)
{
#if RF_OPTION_SOURCE_ENCODING != RF_UTF8
    uint32_t characterLength,*codepoints,i=0,j;
#endif
    // remember the stack pointer before this macro evaluation
    rfLMS_MacroEvalPtr(RF_LMS);
    // read the var args
    READ_VSNPRINTF_ARGS(s,s,0)
// /===Start of Non-UTF-8 code===// /
#if (RF_OPTION_SOURCE_ENCODING  ==  RF_UTF16_LE) || (RF_OPTION_SOURCE_ENCODING  ==  RF_UTF16_BE)
    // find the bytelength of the UTF-16 buffer
    while(buff[i] != '\0' && buff[i+1]!= '\0')
        i++;
    i+=2;
    // allocate the codepoint buffer
    RF_MALLOC(codepoints,i/2)
#elif (RF_OPTION_SOURCE_ENCODING  ==  RF_UTF32_LE) || (RF_OPTION_SOURCE_ENCODING  ==  RF_UTF32_BE)
    // find the bytelength of the UTF-32 buffer
    while(buff[i] != '\0' && buff[i+1]!= '\0' && buff[i+2]!= '\0' && buff[i+3]!= '\0')
        i++;
    i+=4;
    // allocate the codepoint buffer
    RF_MALLOC(codepoints,i)
#endif
#if (RF_OPTION_SOURCE_ENCODING  ==  RF_UTF16_LE)// decode the UTF16
    if(rfUTILS_Endianess() == RF_LITTLE_ENDIAN)
        if(rfUTF16_Decode(buff,&characterLength,codepoints) == false)
            goto cleanup;
    else
        if(rfUTF16_Decode_swap(buff,&characterLength,codepoints)==false)
            goto cleanup;

#elif RF_OPTION_SOURCE_ENCODING == RF_UTF16_BE// decode the UTF16
    if(rfUTILS_Endianess() == RF_LITTLE_ENDIAN)
        if(rfUTF16_Decode_swap(buff,&characterLength,codepoints) == false)
            goto cleanup;
    else
        if(rfUTF16_Decode(buff,&characterLength,codepoints)==false)
            goto cleanup;
#elif RF_OPTION_SOURCE_ENCODING == RF_UTF32_LE// copy the UTF32 into the codepoint
    memcpy(codepoints,buff,i);
    if(rfUTILS_Endianess != RF_LITTLE_ENDIAN)
    {
        for(j=0;j<i;j+=4)
        {
            rfUTILS_SwapEndianUI((uint32_t*)(codepoints+j))
        }
    }
#elif RF_OPTION_SOURCE_ENCODING == RF_UTF32_BE// copy the UTF32 into the codepoint
    memcpy(codepoints,buff,i);
    if(rfUTILS_Endianess !RF_BIG_ENDIAN RF_LITTLE_ENDIAN)
    {
        for(j=0;j<i;j+=4)
        {
            rfUTILS_SwapEndianUI((uint32_t*)(codepoints+j))
        }
    }
#endif
#if RF_OPTION_SOURCE_ENCODING != RF_UTF8 // in any case other than UTF-8 encode the codepoints into UTF-8 , and free them
    if(buffAllocated == true)
        free(buff);
    buffAllocated = true;
    if((buff =  rfUTF8_Encode(codepoints,characterLength,&byteLength)) == 0)
    {
        LOG_ERROR("While attempting to create a temporary RF_String the given byte sequence could not be properly encoded into UTF-8",RE_UTF8_ENCODING);
        free(codepoints);
        return 0;
    }
    free(codepoints);
#endif
// /===End of Non-UTF-8 code===// /
    // /progress normally since here we have a UTF-8 buffer
    // check for validity of the given sequence and get the character length
    uint32_t byteLength;
    if( rfUTF8_VerifySequence(buff,&byteLength) == RF_FAILURE)
    {
        LOG_ERROR("Error at String Allocation due to invalid UTF-8 byte sequence",RE_STRING_INIT_FAILURE);
        if(buffAllocated == true)
            free(buff);
        return 0;
    }

    RF_String* ret;
    ret = rfLMS_Push(RF_LMS,sizeof(RF_String));
    if(ret == 0)
    {
        LOG_ERROR("Memory allocation from the Local Memory Stack failed. Insufficient local memory stack space. Consider compiling the library with bigger stack space. Quitting proccess...",
                  RE_LOCALMEMSTACK_INSUFFICIENT);
        exit(RE_LOCALMEMSTACK_INSUFFICIENT);
    }
    // get length
    ret->byteLength = byteLength;

    // now that we know the length we can allocate the buffer and copy the bytes
    ret->bytes = rfLMS_Push(RF_LMS,ret->byteLength+1);
    if(ret->bytes == 0)
    {
        LOG_ERROR("Memory allocation from the Local Memory Stack failed. Insufficient local memory stack space. Consider compiling the library with bigger stack space. Quitting proccess...",
                  RE_LOCALMEMSTACK_INSUFFICIENT);
        exit(RE_LOCALMEMSTACK_INSUFFICIENT);
    }
    memcpy(ret->bytes,buff,ret->byteLength+1);
    // finally free stuff if needed
    if(buffAllocated == true)
        free(buff);
    return ret;

// /cleanup code for non-UTF-8 cases
#if (RF_OPTION_SOURCE_ENCODING == RF_UTF16_LE) || (RF_OPTION_SOURCE_ENCODING == RF_UTF16_BE)
cleanup:
#if RF_OPTION_SOURCE_ENCODING == RF_UTF16_LE
    LOG_ERROR("Temporary RF_String creation from a UTF-16 Little Endian buffer failed due to UTF-16 decoding failure",RE_UTF16_INVALID_SEQUENCE);
#elif RF_OPTION_SOURCE_ENCODING == RF_UTF16_BE
    LOG_ERROR("Temporary RF_String creation from a UTF-16 Big Endian buffer failed due to UTF-16 decoding failure",RE_UTF16_INVALID_SEQUENCE);
#endif
    free(codepoints);
    if(buffAllocated == true)
        free(buff);
    return 0;
#endif
}
RF_String* i_NVrfString_CreateLocal(const char* s)
{
#if RF_OPTION_SOURCE_ENCODING != RF_UTF8
    uint32_t characterLength,*codepoints,i=0,j;
    char* buff;
#endif
    // remember the stack pointer before this macro evaluation
    rfLMS_MacroEvalPtr(RF_LMS);
// /===Start of Non-UTF-8 code===// /
#if (RF_OPTION_SOURCE_ENCODING  ==  RF_UTF16_LE) || (RF_OPTION_SOURCE_ENCODING  ==  RF_UTF16_BE)
    // find the bytelength of the UTF-16 buffer
    while(s[i] != '\0' &&s[i+1]!= '\0')
        i++;
    i+=2;
    // allocate the codepoint buffer
    RF_MALLOC(codepoints,i/2)
#elif (RF_OPTION_SOURCE_ENCODING  ==  RF_UTF32_LE) || (RF_OPTION_SOURCE_ENCODING  ==  RF_UTF32_BE)
    // find the bytelength of the UTF-32 buffer
    while(s[i] != '\0' && s[i+1]!= '\0' && s[i+2]!= '\0' && s[i+3]!= '\0')
        i++;
    i+=4;
    // allocate the codepoint buffer
    RF_MALLOC(codepoints,i)
#endif
#if (RF_OPTION_SOURCE_ENCODING  ==  RF_UTF16_LE)// decode the UTF16
    if(rfUTILS_Endianess() == RF_LITTLE_ENDIAN)
        if(rfUTF16_Decode(s,&characterLength,codepoints) == false)
            goto cleanup;
    else
        if(rfUTF16_Decode_swap(s,&characterLength,codepoints)==false)
            goto cleanup;

#elif RF_OPTION_SOURCE_ENCODING == RF_UTF16_BE// decode the UTF16
    if(rfUTILS_Endianess() == RF_LITTLE_ENDIAN)
        if(rfUTF16_Decode_swap(s,&characterLength,codepoints) == false)
            goto cleanup;
    else
        if(rfUTF16_Decode(s,&characterLength,codepoints)==false)
            goto cleanup;
#elif RF_OPTION_SOURCE_ENCODING == RF_UTF32_LE// copy the UTF32 into the codepoint
    memcpy(codepoints,s,i);
    if(rfUTILS_Endianess != RF_LITTLE_ENDIAN)
    {
        for(j=0;j<i;j+=4)
        {
            rfUTILS_SwapEndianUI((uint32_t*)(codepoints+j))
        }
    }
#elif RF_OPTION_SOURCE_ENCODING == RF_UTF32_BE// copy the UTF32 into the codepoint
    memcpy(codepoints,s,i);
    if(rfUTILS_Endianess !RF_BIG_ENDIAN RF_LITTLE_ENDIAN)
    {
        for(j=0;j<i;j+=4)
        {
            rfUTILS_SwapEndianUI((uint32_t*)(codepoints+j))
        }
    }
#endif
#if RF_OPTION_SOURCE_ENCODING != RF_UTF8 // in any case other than UTF-8 encode the codepoints into UTF-8 , and free them
    if((buff =  rfUTF8_Encode(codepoints,characterLength,&byteLength)) == 0)
    {
        LOG_ERROR("While attempting to create a temporary RF_String the given byte sequence could not be properly encoded into UTF-8",RE_UTF8_ENCODING);
        free(codepoints);
        return 0;
    }
    free(codepoints);
#endif
// /===End of Non-UTF-8 code===// /
    // check for validity of the given sequence and get the character length
    uint32_t byteLength;
#if RF_OPTION_SOURCE_ENCODING == RF_UTF8
    if( rfUTF8_VerifySequence(s,&byteLength) == RF_FAILURE)
#else
    if( rfUTF8_VerifySequence(buff,&byteLength) == RF_FAILURE)
#endif
    {
        LOG_ERROR("Error at String Allocation due to invalid UTF-8 byte sequence",RE_STRING_INIT_FAILURE);
        return 0;
    }

    RF_String* ret;
    ret = rfLMS_Push(RF_LMS,sizeof(RF_String));
    if(ret == 0)
    {
        LOG_ERROR("Memory allocation from the Local Memory Stack failed during string allocation. Insufficient local memory stack space. Consider compiling the library with bigger stack space. Quitting proccess...",
                  RE_LOCALMEMSTACK_INSUFFICIENT);
        exit(RE_LOCALMEMSTACK_INSUFFICIENT);
    }
    // get length
    ret->byteLength = byteLength;

    ret->bytes = rfLMS_Push(RF_LMS,ret->byteLength+1);
    if(ret->bytes == 0)
    {
        LOG_ERROR("Memory allocation from the Local Memory Stack failed during string allocation. Insufficient local memory stack space. Consider compiling the library with bigger stack space. Quitting proccess...",
                  RE_LOCALMEMSTACK_INSUFFICIENT);
        exit(RE_LOCALMEMSTACK_INSUFFICIENT);
    }
#if RF_OPTION_SOURCE_ENCODING == RF_UTF8
    memcpy(ret->bytes,s,ret->byteLength+1);
#else
    memcpy(ret->bytes,buff,ret->byteLength+1);
#endif
    return ret;

// /cleanup code for non-UTF-8 cases
#if (RF_OPTION_SOURCE_ENCODING == RF_UTF16_LE) || (RF_OPTION_SOURCE_ENCODING == RF_UTF16_BE)
cleanup:
#if RF_OPTION_SOURCE_ENCODING == RF_UTF16_LE
    LOG_ERROR("Temporary RF_String creation from a UTF-16 Little Endian buffer failed due to UTF-16 decoding failure",RE_UTF16_INVALID_SEQUENCE);
#elif RF_OPTION_SOURCE_ENCODING == RF_UTF16_BE
    LOG_ERROR("Temporary RF_String creation from a UTF-16 Big Endian buffer failed due to UTF-16 decoding failure",RE_UTF16_INVALID_SEQUENCE);
#endif
    free(codepoints);
    return 0;
#endif
}



// Initializes a string with the given characters. Given characters have to be in UTF-8. A check for valide sequence of bytes is performed.<b>Can't be used with RF_StringX</b>
#ifndef RF_OPTION_DEFAULT_ARGUMENTS
char rfString_Init(RF_String* str,const char* s,...)
#else
char i_rfString_Init(RF_String* str,const char* s,...)
#endif
{
    READ_VSNPRINTF_ARGS(s,s,false)
    // check for validity of the given sequence and get the character length
    uint32_t byteLength;
    if( rfUTF8_VerifySequence(buff,&byteLength) == RF_FAILURE)
    {
        LOG_ERROR("Error at String Initialization due to invalid UTF-8 byte sequence",RE_STRING_INIT_FAILURE);
        if(buffAllocated==true)
            free(buff);
        return false;
    }

    // get length
    str->byteLength = byteLength;

    // now that we know the length we can allocate the buffer and copy the bytes
    RF_MALLOC(str->bytes,str->byteLength+1);
    memcpy(str->bytes,buff,str->byteLength+1);
    if(buffAllocated == true)
        free(buff);
    return true;
}
#ifdef RF_OPTION_DEFAULT_ARGUMENTS
char i_NVrfString_Init(RF_String* str,const char* s)
{
    // check for validity of the given sequence and get the character length
    uint32_t byteLength;
    if( rfUTF8_VerifySequence(s,&byteLength) == RF_FAILURE)
    {
        LOG_ERROR("Error at String Initialization due to invalid UTF-8 byte sequence",RE_STRING_INIT_FAILURE);
        return false;
    }

    // get length
    str->byteLength = byteLength;

    // now that we know the length we can allocate the buffer and copy the bytes
    RF_MALLOC(str->bytes,str->byteLength+1);
    memcpy(str->bytes,s,str->byteLength+1);

    return true;
}
#endif

// Allocates a String by turning a unicode code point in a String (encoded in UTF-8).
RF_String* rfString_Create_cp(uint32_t codepoint)
{
    RF_String* ret;
    RF_MALLOC(ret,sizeof(RF_String));
    if(rfString_Init_cp(ret,codepoint) == true)
    {
        return ret;
    }
    // failure
    free(ret);
    return 0;
}

// Initializes a string by turning a unicode code point in a String (encoded in UTF-8).
char rfString_Init_cp(RF_String* str, uint32_t codepoint)
{
    // alloc enough for a character
    RF_MALLOC(str->bytes,5)

    // if we only need a byte to encode it
    if(RF_HEXLE_UI(codepoint,0x007f))
    {
        str->bytes[0] = codepoint;
        str->bytes[1] = '\0';
        str->byteLength = 1;
    }
    // if we need 2 bytes to encode it
    else if( RF_HEXGE_UI(codepoint,0x0080) && RF_HEXLE_UI(codepoint,0x07ff))
    {
        // get the first bits of the first byte and encode them to the first byte
        str->bytes[1] = (codepoint & 0x3F)|(0x02<<6);
        // get the 5 following bits and encode them in the second byte
        str->bytes[0] = ((codepoint & 0x7C0) >> 6)  | (0x6<<5);
        str->bytes[2] = '\0';
        str->byteLength = 2;
    }
    // if we need 3 bytes to encode it
    else if( RF_HEXGE_UI(codepoint,0x0800) && RF_HEXLE_UI(codepoint,0x0ffff))
    {
        // get the first bits of the first byte and encode them to the first byte
        str->bytes[2] = (codepoint & 0x3F)|(0x02<<6);
        // get the 6 following bits and encode them in the second byte
        str->bytes[1] = ((codepoint & 0xFC0) >> 6)  | (0x02<<6);
        // get the 4 following bits and encode them in the third byte
        str->bytes[0] = (((codepoint & 0xF000))>>12) | (0xE<<4);
        str->bytes[3] = '\0';
        str->byteLength = 3;
    }
    // if we need 4 bytes to encode it
    else if( RF_HEXGE_UI(codepoint,0x10000) && RF_HEXLE_UI(codepoint,0x10ffff))
    {
        // get the first bits of the first byte and encode them to the first byte
        str->bytes[3] = (codepoint & 0x3F)|(0x02<<6);
        // get the 6 following bits and encode them in the second byte
        str->bytes[2] = ((codepoint & 0xFC0) >> 6)  | (0x02<<6);
        // get the 6 following bits and encode them in the third byte
        str->bytes[1] = (((codepoint & 0x3F000))>>12) | (0x02<<6);
        // get the 3 following bits and encode them in the fourth byte
        str->bytes[0] = (((codepoint & 0x1C0000))>>18) | (0x1E<<3);
        str->bytes[4] = '\0';
        str->byteLength = 4;
    }
    else
    {
        LOG_ERROR("Attempted to encode an invalid unicode code point into a string",RE_UTF8_INVALID_CODE_POINT);
        free(str->bytes);
        return false;
    }

    return true;
}


// Allocates and returns a string with the given integer
RF_String* rfString_Create_i(int32_t i)
{
    // the size of the int32_t buffer
    int32_t numLen;
    // put the int32_t into a buffer and turn it in a char*
    char buff[12];// max uint32_t is 4,294,967,295 in most environment so 12 chars will certainly fit it
    sprintf(buff,"%d",i);
    numLen = strlen(buff);

    // initialize the string and return it
    RF_String* ret;
    RF_MALLOC(ret,sizeof(RF_String));
    ret->byteLength = numLen;
    RF_MALLOC(ret->bytes,numLen+1);
    strcpy(ret->bytes,buff);
    return ret;
}
// Initializes a string with the given integer.
char rfString_Init_i(RF_String* str, int32_t i)
{
    // the size of the int32_t buffer
    int32_t numLen;
    // put the int32_t into a buffer and turn it in a char*
    char buff[12];// max uint32_t is 4,294,967,295 in most environment so 12 chars will certainly fit it
    sprintf(buff,"%d",i);
    numLen = strlen(buff);


    str->byteLength = numLen;
    RF_MALLOC(str->bytes,numLen+1);
    strcpy(str->bytes,buff);

    return true;
}

// Allocates and returns a string with the given float
RF_String* rfString_Create_f(float f)
{
    // allocate a buffer for the float in characters
    char* buff;
    RF_MALLOC(buff,128);
    sprintf(buff,"%f",f);
    uint32_t len = strlen(buff);

    // initialize and return the string
    RF_String* ret;
    RF_MALLOC(ret,sizeof(RF_String));
    ret->byteLength = len;
    RF_MALLOC(ret->bytes,len+1);
    strcpy(ret->bytes,buff);

    free(buff);

    return ret;
}
// Initializes a string with the given float
char rfString_Init_f(RF_String* str,float f)
{
    // allocate a buffer for the float in characters
    char* buff;
    RF_MALLOC(buff,128);
    sprintf(buff,"%f",f);
    uint32_t len = strlen(buff);


    str->byteLength = len;
    RF_MALLOC(str->bytes,len+1);
    strcpy(str->bytes,buff);
    free(buff);

    // success
    return true;
}

// Allocates and returns a string with the given UTF-16 byte sequence. Given characters have to be in UTF-16. A check for valid sequence of bytes is performed.<b>Can't be used with RF_StringX</b>
RF_String* rfString_Create_UTF16(const char* s,char endianess)
{
    RF_String* ret;
    RF_MALLOC(ret,sizeof(RF_String));
    if(rfString_Init_UTF16(ret,s,endianess)==false)
    {
        free(ret);
        return 0;
    }
    return ret;
}
// Initializes a string with the given UTF-16 byte sequence. Given characters have to be in UTF-16. A check for valid sequence of bytes is performed.<b>Can't be used with RF_StringX</b>
char rfString_Init_UTF16(RF_String* str,const char* s,char endianess)
{
    // decode the utf-16 and get the code points
    uint32_t* codepoints;
    uint32_t byteLength,characterLength,utf8ByteLength;
    char* utf8;
    byteLength = 0;
    while(s[byteLength]!= 0 || s[byteLength+1]!=0)
    {
        byteLength++;
    }
    byteLength+=3;// for the last utf-16 null termination character
    RF_MALLOC(codepoints,byteLength*2) // allocate the codepoints
    // parse the given byte stream depending on the endianess parameter
    switch(endianess)
    {
        case RF_LITTLE_ENDIAN:
        case RF_BIG_ENDIAN:
            if(rfUTILS_Endianess() == endianess)// same endianess as the local
            {
                if(rfUTF16_Decode(s,&characterLength,codepoints) == false)
                {
                    free(codepoints);
                    LOG_ERROR("String initialization failed due to invalide UTF-16 sequence",RE_STRING_INIT_FAILURE);
                    return false;
                }
            }
            else// different
            {
                if(rfUTF16_Decode_swap(s,&characterLength,codepoints) == false)
                {
                    free(codepoints);
                    LOG_ERROR("String initialization failed due to invalide UTF-16 sequence",RE_STRING_INIT_FAILURE);
                    return false;
                }
            }
        break;
        default:
            LOG_ERROR("Illegal endianess value provided",RE_INPUT);
            free(codepoints);
            return false;
        break;
    }// switch ends
    // now encode these codepoints into UTF8
    if( (utf8 = rfUTF8_Encode(codepoints,characterLength,&utf8ByteLength))==0)
    {
        free(codepoints);
        return false;
    }
    // success
    free(codepoints);
    str->bytes = utf8;
    str->byteLength = utf8ByteLength;
    return true;

}

// Allocates and returns a string with the given UTF-32 byte sequence. Given characters have to be in UTF-32.
RF_String* rfString_Create_UTF32(const char* s)
{
    RF_String* ret;
    RF_MALLOC(ret,sizeof(RF_String));
    if(rfString_Init_UTF32(ret,s)==false)
    {
        free(ret);
        return 0;
    }
    return ret;
}
// Initializes a string with the given UTF-32 byte sequence. Given characters have to be in UTF-32.
char rfString_Init_UTF32(RF_String* str,const char* s)
{
    char swapE = false;
    uint32_t off = 0;
    int32_t i = 0;

    // get the buffer and if swapping is needed do it for all character
    uint32_t* codeBuffer = (uint32_t*)(s+off);

    // first of all check for existence of BOM in the beginning of the sequence
    if(RF_HEXEQ_UI(codeBuffer[0],0xFEFF))// big endian
    {
        if(rfUTILS_Endianess()==RF_LITTLE_ENDIAN)
            swapE = true;
    }
    if(RF_HEXEQ_UI(codeBuffer[0],0xFFFE0000))// little
    {
        if(rfUTILS_Endianess()==RF_BIG_ENDIAN)
            swapE = true;
    }
    else// according to the standard no BOM means big endian
    {
        if(rfUTILS_Endianess() == RF_LITTLE_ENDIAN)
            swapE = true;
    }

    // if we need to have endianess swapped do it
    if(swapE==true)
    {
        while(codeBuffer[i] != 0)
        {
            rfUTILS_SwapEndianUI(codeBuffer+i);
            i++;
        }
    }
    // find the length of the utf32 buffer in characters
    uint32_t length;
    rfUTF32_Length(codeBuffer,length);

    // turn the codepoints into a utf-8 encoded buffer
    char* utf8;uint32_t utf8ByteLength;
    if((utf8=rfUTF8_Encode(codeBuffer,length,&utf8ByteLength)) == 0)
    {
        return false;// error
    }
    // if the encoding happened correctly
    if(codeBuffer != 0)
    {
        str->bytes = (char*)codeBuffer;
        str->byteLength = utf8ByteLength;
        return true;
    }
    // else return failure
    return false;
}

// Assigns the value of the source string to the destination.Both strings should already be initialized and hold a value. It is an error to give null parameters.
void i_rfString_Assign(RF_String* dest,void* sourceP)
{
    RF_String* source = (RF_String*)sourceP;
    // only if the new string value won't fit in the buffer reallocate the buffer (let's avoid unecessary reallocs)
    if(source->byteLength > dest->byteLength)
    {
        RF_REALLOC(dest->bytes,char,source->byteLength+1);
    }
    // now copy the value
    memcpy(dest->bytes,source->bytes,source->byteLength+1);
    // and fix the lengths
    dest->byteLength = source->byteLength;
}

// Assigns the value of a unicode character to the string
char rfString_Assign_char(RF_String* str,uint32_t codepoint)
{
    // realloc if needed
    if(str->byteLength <5)
    {
        RF_REALLOC(str->bytes,char,5);
    }
    // if we only need a byte to encode it
    if(RF_HEXLE_UI(codepoint,0x007f))
    {
        str->bytes[0] = codepoint;
        str->bytes[1] = '\0';
        str->byteLength = 1;
    }
    // if we need 2 bytes to encode it
    else if( RF_HEXGE_UI(codepoint,0x0080) && RF_HEXLE_UI(codepoint,0x07ff))
    {
        // get the first bits of the first byte and encode them to the first byte
        str->bytes[1] = (codepoint & 0x3F)|(0x02<<6);
        // get the 5 following bits and encode them in the second byte
        str->bytes[0] = ((codepoint & 0x7C0) >> 6)  | (0x6<<5);
        str->bytes[2] = '\0';
        str->byteLength = 2;
    }
    // if we need 3 bytes to encode it
    else if( RF_HEXGE_UI(codepoint,0x0800) && RF_HEXLE_UI(codepoint,0x0ffff))
    {
        // get the first bits of the first byte and encode them to the first byte
        str->bytes[2] = (codepoint & 0x3F)|(0x02<<6);
        // get the 6 following bits and encode them in the second byte
        str->bytes[1] = ((codepoint & 0xFC0) >> 6)  | (0x02<<6);
        // get the 4 following bits and encode them in the third byte
        str->bytes[0] = (((codepoint & 0xF000))>>12) | (0xE<<4);
        str->bytes[3] = '\0';
        str->byteLength = 3;
    }
    // if we need 4 bytes to encode it
    else if( RF_HEXGE_UI(codepoint,0x10000) && RF_HEXLE_UI(codepoint,0x10ffff))
    {
        // get the first bits of the first byte and encode them to the first byte
        str->bytes[3] = (codepoint & 0x3F)|(0x02<<6);
        // get the 6 following bits and encode them in the second byte
        str->bytes[2] = ((codepoint & 0xFC0) >> 6)  | (0x02<<6);
        // get the 6 following bits and encode them in the third byte
        str->bytes[1] = (((codepoint & 0x3F000))>>12) | (0x02<<6);
        // get the 3 following bits and encode them in the fourth byte
        str->bytes[0] = (((codepoint & 0x1C0000))>>18) | (0x1E<<3);
        str->bytes[4] = '\0';
        str->byteLength = 4;
    }
    else
    {
        LOG_ERROR("Attempted to encode an invalid unicode code point into a string",RE_UTF8_INVALID_CODE_POINT);
        return false;
    }

    return true;
}

// Allocates and returns a string with the given characters. NO VALID-UTF8 check is performed
#ifndef RF_OPTION_DEFAULT_ARGUMENTS
RF_String* rfString_Create_nc(const char* s,...)
#else
RF_String* i_rfString_Create_nc(const char* s,...)
#endif
{
    RF_String* ret;
    RF_MALLOC(ret,sizeof(RF_String));
    // get  the formatted string
    READ_VSNPRINTF_ARGS(s,s,0);
    // get the lengt of the byte buffer
    ret->byteLength = bytesWritten;

    // now that we know the length we can allocate the buffer and copy the bytes
    RF_MALLOC(ret->bytes,ret->byteLength+1);
    memcpy(ret->bytes,buff,ret->byteLength+1);
    if(buffAllocated)
        free(buff);
    return ret;
}
#ifdef RF_OPTION_DEFAULT_ARGUMENTS
RF_String* i_NVrfString_Create_nc(const char* s)
{
    RF_String* ret;
    RF_MALLOC(ret,sizeof(RF_String));
    // get length
    ret->byteLength = strlen(s);

    // now that we know the length we can allocate the buffer and copy the bytes
    RF_MALLOC(ret->bytes,ret->byteLength+1);
    memcpy(ret->bytes,s,ret->byteLength+1);
    return ret;
}
#endif

// Initializes a string with the given characters. NO VALID-UTF8 check is performed
#ifndef RF_OPTION_DEFAULT_ARGUMENTS
char rfString_Init_nc(struct RF_String* str,const char* s,...)
#else
char i_rfString_Init_nc(struct RF_String* str,const char* s,...)
#endif
{
    // get the formatted string
    READ_VSNPRINTF_ARGS(s,s,false)
    // get its length
    str->byteLength = bytesWritten;

    // now that we know the length we can allocate the buffer and copy the bytes
    RF_MALLOC(str->bytes,str->byteLength+1);
    memcpy(str->bytes,buff,str->byteLength+1);
    if(buffAllocated == true)
        free(buff);
    return true;
}
#ifdef RF_OPTION_DEFAULT_ARGUMENTS
char i_NVrfString_Init_nc(struct RF_String* str,const char* s)
{
    // get its length
    str->byteLength = strlen(s);

    // now that we know the length we can allocate the buffer and copy the bytes
    RF_MALLOC(str->bytes,str->byteLength+1);
    memcpy(str->bytes,s,str->byteLength+1);
    return true;
}
#endif

/*-------------------------------------------------------------------------Methods to get rid of an RF_String-------------------------------------------------------------------------------*/

// Deletes a string object and also frees its pointer.It is an error to give a NULL(0x0) string for deleting. Will most probably lead to a segmentation fault
void rfString_Destroy(RF_String* s)
{
    free(s->bytes);
    free(s);
}
// Deletes a string object only, not its memory.It is an error to give a NULL(0x0) string for deleting. Will most probably lead to a segmentation fault
void rfString_Deinit(RF_String* s)
{
    free(s->bytes);
}
/*------------------------------------------------------------------------ RF_String unicode conversio functions-------------------------------------------------------------------------------*/

// Returns the strings contents as a UTF-16 buffer
uint16_t* rfString_ToUTF16(RF_String* s,uint32_t* length)
{
    uint32_t* codepoints,charsN;
    // get the unicode codepoints, no check here since RF_String is always guaranteed to have valid UTF=8 and as such valid codepoints
    codepoints = rfUTF8_Decode(s->bytes,s->byteLength,&charsN);
    // encode them in UTF-16, no check here since it comes from an RF_String which is always guaranteed to have valid UTF-8 and as such valid codepoints
    return rfUTF16_Encode(codepoints,charsN,length);
}

// Returns the strings contents as a UTF-32 buffer
uint32_t* rfString_ToUTF32(RF_String* s,uint32_t* length)
{
    // get the unicode codepoints, no check here since RF_String is always guaranteed to have valid UTF=8 and as such valid codepoints
    return rfUTF8_Decode(s->bytes,s->byteLength,length);
}

/*------------------------------------------------------------------------ RF_String retrieval functions-------------------------------------------------------------------------------*/
// Finds the length of the string in characters
uint32_t rfString_Length(void* str)
{
    RF_String* s = (RF_String*)str;
    uint32_t length,i;
    RF_STRING_ITERATE_START(s,length,i)
    RF_STRING_ITERATE_END(length,i);
    return length;
}

// Retrieves the unicode code point of the parameter character.
uint32_t rfString_GetChar(void* str,uint32_t c)
{
    RF_String* thisstr = (RF_String*)str;
    uint32_t length,i;
    uint32_t codePoint = RF_STRING_INDEX_OUT_OF_BOUNDS;
    RF_STRING_ITERATE_START(thisstr,length,i)
        // if we found the character,inspect the 4 different cases
        if(length == c)
        {
            // take the codepoint from the byte position and break from the loop
            codePoint = rfString_BytePosToCodePoint(thisstr,i);
            break;
        }
    RF_STRING_ITERATE_END(length,i)

    // and return the code point. Notice that if the character was not found this will return RF_STRING_INDEX_OUT_OF_BOUNDS
    return codePoint;
}

// Retrieves the unicode code point of the parameter bytepos of the string. If the byte position is not the start of a character 0 is returned. This is an internal function, there is no need to use it. <i>Can be used with StringX</i>
uint32_t rfString_BytePosToCodePoint(void* str,uint32_t i)
{
    uint32_t codePoint=0;
    RF_String* thisstr = (RF_String*)str;
    // /Here I am not checking if byte position 'i' is withing bounds and especially if it is a start of a character
    // / This is assumed to have been checked or to be known beforehand by the programmer. That's one of the reasons
    // / why this is an internal function and should not be used unless you know what you are doing
    // if the lead bit of the byte is 0 then range is : U+0000 to U+0007F (1 byte)
    if( ((thisstr->bytes[i] & 0x80)>>7) == 0 )
    {
        // and the code point is this whole byte only
        codePoint = thisstr->bytes[i];
    }
    // if the leading bits are in the form of 0b110xxxxx then range is: U+0080 to U+07FF (2 bytes)
    else if ( RF_HEXEQ_C( ( (~(thisstr->bytes[i] ^  0xC0))>>5),0x7) )
    {
        codePoint =0;
        // from the second byte take the first 6 bits
        codePoint = (thisstr->bytes[i+1] & 0x3F) ;
        // from the first byte take the first 5 bits and put them in the start
        codePoint |= ((thisstr->bytes[i] & 0x1F) << 6);
    }
    // if the leading bits are in the form of 0b1110xxxx then range is U+0800 to U+FFFF  (3 bytes)
    else if( RF_HEXEQ_C( ( (~(thisstr->bytes[i] ^ 0xE0))>>4),0xF) )
    {
        codePoint = 0;
        // from the third byte take the first 6 bits
        codePoint = (thisstr->bytes[i+2] & 0x3F) ;
        // from the second byte take the first 6 bits and put them to the left of the previous 6 bits
        codePoint |= ((thisstr->bytes[i+1] & 0x3F) << 6);
        // from the first byte take the first 4 bits and put them to the left of the previous 6 bits
        codePoint |= ((thisstr->bytes[i] & 0xF) << 12);
    }
    // if the leading bits are in the form of 0b11110xxx then range is U+010000 to U+10FFFF (4 bytes)
    else if( RF_HEXEQ_C( ( (~(thisstr->bytes[i] ^ 0xF0))>>3), 0x1F))
    {
        codePoint = 0;
        // from the fourth byte take the first 6 bits
        codePoint = (thisstr->bytes[i+3] & 0x3F) ;
        // from the third byte take the first 6 bits and put them to the left of the previous 6 bits
        codePoint |= ((thisstr->bytes[i+2] & 0x3F) << 6);
        // from the second byte take the first 6 bits and put them to the left of the previous 6 bits
        codePoint |= ((thisstr->bytes[i+1] & 0x3F) << 12);
        // from the first byte take the first 3 bits and put them to the left of the previous 6 bits
        codePoint |= ((thisstr->bytes[i] & 0x7) << 18);
    }

    return codePoint;
}


// Retrieves character position of a byte position
uint32_t rfString_BytePosToCharPos(void* thisstrP,uint32_t bytepos,char before)
{
    // /here there is no check if this is actually a byte pos inside the string's
    // /byte buffer. The programmer should have made sure it is before hand. This is why it is
    // / an internal function and should only be used if you know what you are doing
    RF_String* thisstr = (RF_String*)thisstrP;
    uint32_t charPos = 0;
    uint32_t byteI = 0;
    // iterate the string's bytes until you get to the required byte
    // if it is not a continuation byte, return the position
    if(rfUTF8_IsContinuationByte(thisstr->bytes[bytepos])==false)
    {
        RF_STRING_ITERATE_START(thisstr,charPos,byteI)
            if(byteI == bytepos)
                return charPos;
        RF_STRING_ITERATE_END(charPos,byteI)
    }
    // else  iterate the string's bytes until you get anything bigger than the required byte
    RF_STRING_ITERATE_START(thisstr,charPos,byteI)
        if(byteI > bytepos)
            break;
    RF_STRING_ITERATE_END(charPos,byteI)
    // if we need the previous one return it
    if(before == true)
        return charPos-1;
    // else return this
    return charPos;
}

// Compares two Strings and returns true if they are equal and false otherwise
char i_rfString_Equal(void* s1P,void* s2P)
{
    RF_String* s1 = (RF_String*)s1P;
    RF_String* s2 = (RF_String*)s2P;
    if( strcmp(s1->bytes,s2->bytes)==0)
    {
        return true;
    }
    return false;
}

// Finds the existence of String sstr inside this string, either matching case or not
int32_t i_rfString_Find(const void* str,const void* sstrP,const char* optionsP)
{
    // / @note TO SELF: If I make any changes to this function do not forget to change the private version that returns byte position too
    // / located at string_private.c and called rfString_FindByte and rfString_FindByte_s
    RF_String* thisstr = (RF_String*)str;
    RF_String* sstr = (RF_String*)sstrP;
    char options = *optionsP;

    char* found = 0;
    // if we want to match the case of the string then it's a simple search of matching characters
    if( (RF_BITFLAG_ON( options,RF_CASE_IGNORE)) == false)
    {
        // if it is not found
        if( (found = strstr(thisstr->bytes,sstr->bytes)) == 0)
        {
            return RF_FAILURE;
        }
        // get the byte position
        uint32_t bytepos = found-thisstr->bytes;
        // if we need the exact string as it is given
        if(RF_BITFLAG_ON( options,RF_MATCH_WORD))
        {
            // check before the found string
            if(bytepos != 0)
            {
                // if is is not a character
                switch(thisstr->bytes[bytepos-1])
                {
                    case ' ':case '\t':case '\n':
                    break;
                    default:
                        return RF_FAILURE;
                    break;
                }
            }
            // check after the found string
            if(bytepos+sstr->byteLength != thisstr->byteLength)
            {
                // if is is not a character
                switch(thisstr->bytes[bytepos+sstr->byteLength])
                {
                    case ' ':case '\t':case '\n':
                    break;
                    default:
                        return RF_FAILURE;
                    break;
                }
            }
        }// end of the exact string option
        // success
        return rfString_BytePosToCharPos(thisstr,bytepos,false);
    }

    // else ignore case matching
    uint32_t i,j;
    // if(cstr[0] >= 0x41 && cstr[0] <= 0x5a)
    for(i=0;i<thisstr->byteLength; i ++)
    {
        // if i matches the start of the substring
        for(j = 0; j < sstr->byteLength; j++)
        {
            // if the jth char is a big letter
            if(sstr->bytes[j] >= 0x41 && sstr->bytes[j] <= 0x5a)
            {
                // no match
                if(sstr->bytes[j] != thisstr->bytes[i+j] && sstr->bytes[j]+32 != thisstr->bytes[i+j])
                    break;
                // there is a match in the jth character so let's perform additional checks if needed
                if(RF_BITFLAG_ON( options,RF_MATCH_WORD))
                {
                    // if it's the first substring character and if the string we search is not in it's beginning, check for EXACT string before
                    if(j == 0 && i != 0)
                    {
                        switch(thisstr->bytes[i-1])
                        {
                            case ' ':case '\t':case '\n':
                            break;
                            default:
                                return RF_FAILURE;
                            break;
                        }
                    }
                }// exact string check if ends
            }
            // small letter
            else if(sstr->bytes[j] >= 0x61 && sstr->bytes[j] <= 0x7a)
            {
                // no match
                if(sstr->bytes[j] != thisstr->bytes[i+j] && sstr->bytes[j]-32 != thisstr->bytes[i+j])
                    break;
                // there is a match in the jth character so let's perform additional checks if needed
                if(RF_BITFLAG_ON(options,RF_MATCH_WORD))
                {
                    // if it's the first substring character and if the string we search is not in it's beginning, check for EXACT string before
                    if(j == 0 && i != 0)
                    {
                        switch(thisstr->bytes[i-1])
                        {
                            case ' ':case '\t':case '\n':
                            break;
                            default:
                                return RF_FAILURE;
                            break;
                        }
                    }
                }// exact string check if ends
            }
            // not a letter and no match
            else if(sstr->bytes[j] != thisstr->bytes[i+j])
                break;// break off the substring search loop

            // if we get here and it's the last char of the substring we either found it or need to perform one last check for exact string
            if(j == sstr->byteLength-1)
            {
                // only if the end of the string is not right after the substring
                if( RF_BITFLAG_ON(options,RF_MATCH_WORD) && i+sstr->byteLength < thisstr->byteLength)
                {
                    switch(thisstr->bytes[i+sstr->byteLength])
                    {
                        case ' ':case '\t':case '\n':
                        break;
                        default:
                            return RF_FAILURE;
                        break;
                    }
                }// end of the exact string check
                // succes
                return rfString_BytePosToCharPos(thisstr,i,false);
            }// end of it's the last char of the substring check
        }// substring iteration ends
    }// this string iteration ends
    return RF_FAILURE;
}

// Returns the integer value of the string if and only if it contains only numbers. If it contains anything else the function fails.
char rfString_ToInt(void* str,int32_t* v)
{
    RF_String* thisstr = (RF_String*)str;
    char* end;
    // get the integer
    *v = strtol ( thisstr->bytes, &end,10);

// /This is the non-strict case. Takes the number out of the string no matter what else it has inside
/*    // if we did get something
    if(strlen(end) < this->length())
        return true;
*/
// /This is the strict case, and the one we will go with. The non-strict case might be moved to its own function, if ever in the future
    if(end[0] == '\0')
        return true;

    // else false
    return false;
}

// Returns the float value of a String
int rfString_ToDouble(void* thisstrP,double* f)
{
    RF_String* str = (RF_String*)thisstrP;
    *f = strtod(str->bytes,NULL);
    // check the result
    if(*f == 0.0)
    {
        // if it's zero and the string equals to zero then we are okay
        if(rfString_Equal(str,RFS_("0")) || rfString_Equal(str,RFS_("0.0")))
            return RF_SUCCESS;
        // underflow error
        if(errno == ERANGE)
            return RE_STRING_TOFLOAT_UNDERFLOW;
        // in any other case it's a conversion error
        return RE_STRING_TOFLOAT;
    }
    // if the result is a HUGE_VAL and errno is set,the number is not representable by a double
    if(*f == HUGE_VAL && errno == ERANGE)
        return RE_STRING_TOFLOAT_RANGE;

    // any other case success
    return RF_SUCCESS;
}

// Returns a cstring version of the string.
const char* rfString_ToCstr(const void* str)
{
    RF_String* thisstr = (RF_String*)str;
    return thisstr->bytes;
}

// Creates and returns an allocated copy of the given string
RF_String* rfString_Copy_OUT(void* srcP)
{
    RF_String* src = (RF_String*)srcP;
    // create the new string
    RF_String* ret;
    RF_MALLOC(ret,sizeof(RF_String));
    // get the length
    ret->byteLength = src->byteLength;
    // copy the bytes
    RF_MALLOC(ret->bytes,ret->byteLength+1);
    memcpy(ret->bytes,src->bytes,ret->byteLength+1);
    return ret;

}
// Copies all the contents of a string to another
void rfString_Copy_IN(RF_String* dst,void* srcP)
{
    RF_String* src = (RF_String*)srcP;
    // get the length
    dst->byteLength = src->byteLength;
    // copy the bytes
    RF_MALLOC(dst->bytes,src->byteLength+1);
    memcpy(dst->bytes,src->bytes,dst->byteLength+1);
    return;

}
// Copies a certain number of characters from a string
void rfString_Copy_chars(RF_String* dst,void* srcP,uint32_t charsN)
{
    uint32_t i = 0,bytePos;
    RF_String* src = (RF_String*)srcP;

    // find the byte position until which we need to copy
    RF_STRING_ITERATE_START(src,i,bytePos)
        if(i == charsN)
            break;
    RF_STRING_ITERATE_END(i,bytePos)
    dst->byteLength = bytePos;
    RF_MALLOC(dst->bytes,dst->byteLength+1);
    memcpy(dst->bytes,src->bytes,dst->byteLength+1);
    dst->bytes[dst->byteLength] = '\0';// null terminate it
}


// Applies a limited version of sscanf after the specified substring
char i_rfString_ScanfAfter(void* str,void* afterstrP,const char* format,void* var)
{
    RF_String* thisstr = (RF_String*)str;
    RF_String* afterstr = (RF_String*)afterstrP;
    // return false if the substring is not found
    char* found,*s;
    if( (found = strstr(thisstr->bytes,afterstr->bytes)) ==0 )
    {
        return false;
    }
    // get a pointer to the start of the position where sscanf will be used
    s = thisstr->bytes + (found-thisstr->bytes+afterstr->byteLength);

    // use sscanf
    if(sscanf(s,format,var) <=0)
    {
        return false;
    }
    return true;
}

// Counts how many times a substring s occurs inside the string
int32_t i_rfString_Count(void* str,void* sstr2,const char* optionsP)
{
    RF_String* thisstr = (RF_String*)str;
    RF_String* sstr = (RF_String*)sstr2;
    char options = *optionsP;
    int32_t index = 0;
    int32_t move;
    int32_t n = 0;

    // as long as the substring is found in the string
    while ((move = rfString_FindBytePos(thisstr,sstr,options)) != RF_FAILURE)
    {
        move+= sstr->byteLength;
        // proceed searching inside the string and also increase the counter
        n++;
        thisstr->bytes+=move;
        index +=move;
        thisstr->byteLength -=move;
    }

    // return string to its original state and return the number of occurences, also returns 0 if not found
    thisstr->bytes-=index;
    thisstr->byteLength += index;
    // success
    return n;
}

// Tokenizes the given string. Separates it into @c tokensN depending on how many substrings can be created from the @c sep separatior and stores them
// into the Array of RF_String* that should be passed to the function
i_DECLIMEX_ char rfString_Tokenize(void* str,char* sep,uint32_t* tokensN,RF_String** tokens)
{
    RF_String* thisstr = (RF_String*)str;
    uint32_t i;
    // first find the occurences of the separator, and then the number of tokens
    *tokensN = rfString_Count(thisstr,RFS_(sep),0)+1;
    // error checking
    if(*tokensN == 0)
        return false;

    // allocate the tokens
    RF_MALLOC(*tokens,sizeof(RF_String) *(*tokensN));
    // find the length of the separator
    uint32_t sepLen = strlen(sep);
    char* s,*e;
    s = thisstr->bytes;
    for(i = 0; i < (*tokensN)-1; i ++)
    {
        // find each substring
        e = strstr(s,sep);
        (*tokens)[i].byteLength = e-s;
        RF_MALLOC((*tokens)[i].bytes,(*tokens)[i].byteLength+1);
        // put in the data
        strncpy((*tokens)[i].bytes,s,(*tokens)[i].byteLength);
        // null terminate
        (*tokens)[i].bytes[(*tokens)[i].byteLength] = '\0';

        // prepare for next sub-string
        s = e+sepLen;

    }
    // /make sure that if it's the last substring we change strategy
    (*tokens)[i].byteLength = strlen(s);
    RF_MALLOC((*tokens)[i].bytes,(*tokens)[i].byteLength+1);
    // put in the data
    strncpy((*tokens)[i].bytes,s,(*tokens)[i].byteLength);
    // null terminate
    (*tokens)[i].bytes[(*tokens)[i].byteLength] = '\0';

    // success
    return true;
}
// Initializes the given string as the first substring existing between the left and right parameter substrings.
char i_rfString_Between(void* thisstrP,void* lstrP,void* rstrP,RF_String* result,const char* optionsP)
{
    int32_t start,end;
    RF_String* thisstr = (RF_String*)thisstrP;
    RF_String* lstr = (RF_String*)lstrP;
    RF_String* rstr = (RF_String*)rstrP;
    char options = *optionsP;
    RF_String temp;
    // find the left substring
    if( (start = rfString_FindBytePos(thisstr,lstr,options))== RF_FAILURE)
    {
        return false;
    }
    // get what is after it
    rfString_After(thisstr,lstr,&temp,options);
    // find the right substring in the remaining part
    if( (end = rfString_FindBytePos(&temp,rstr,options))== RF_FAILURE)
    {
        return false;
    }
    // free temp string
    rfString_Deinit(&temp);
    // initialize the string to return
    result->byteLength = end;
    RF_MALLOC(result->bytes,result->byteLength+1);
    memcpy(result->bytes,thisstr->bytes+start+lstr->byteLength,result->byteLength+1);
    result->bytes[end]= '\0';
    // success
    return true;
}

// Initializes the given string as the substring from the start until any of the given Strings are found.
#ifndef RF_OPTION_DEFAULT_ARGUMENTS
char rfString_Beforev(void* thisstrP,RF_String* result,const char* optionsP,const unsigned char* parNP, ...)
#else
char i_rfString_Beforev(void* thisstrP,RF_String* result,const char* optionsP,const unsigned char* parNP, ...)
#endif
{
    RF_String* thisstr = (RF_String*)thisstrP;
    RF_String* s;
    char options = *optionsP;
    unsigned char parN = *parNP;
    int32_t i,minPos,thisPos;
    // will keep the argument list
    va_list argList;
    // get the parameter characters
    va_start(argList,parNP);

    minPos = 9999999;
    for(i = 0; i < parN; i++)
    {
        s = (RF_String*) va_arg(argList,RF_String*);
        if( (thisPos= rfString_FindBytePos(thisstr,s,options))!= RF_FAILURE)
        {
            if(thisPos < minPos)
                minPos = thisPos;
        }
    }
    va_end(argList);

    // if it is not found
    if(minPos == 9999999)
    {
        return false;
    }
    // if it is found initialize the substring
    result->byteLength = minPos;
    RF_MALLOC(result->bytes,minPos+1);
    memcpy(result->bytes,thisstr->bytes,minPos);
    result->bytes[minPos] = '\0';
    // success
    return true;
}

// Initializes the given string as the substring from the start until the given string is found
char i_rfString_Before(void* thisstrP,void* sstrP,RF_String* result,const char* optionsP)
{
    RF_String* thisstr = (RF_String*)thisstrP;
    RF_String* sstr = (RF_String*) sstrP;
    char options = *optionsP;
    int32_t ret;
    // find the substring
    if( (ret = rfString_FindBytePos(thisstr,sstr,options)) == RF_FAILURE)
    {
        return false;
    }
    // if it is found get the result initialize the substring
    result->byteLength = ret;
    RF_MALLOC(result->bytes,result->byteLength+1);
    memcpy(result->bytes,thisstr->bytes,result->byteLength);
    result->bytes[result->byteLength] = '\0';
    // success
    return true;
}


// Initializes the given String with the substring located after (and not including) the after substring inside the parameter string. If the substring is not located the function returns false.
char i_rfString_After(void* thisstrP,void* afterP,RF_String* out,const char* optionsP)
{
    RF_String* thisstr = (RF_String*)thisstrP;
    RF_String* after = (RF_String*)afterP;
    char options = *optionsP;
    int32_t bytePos;
    // check for substring existence
    if( (bytePos = rfString_FindBytePos(thisstr,after,options)) == RF_FAILURE)
    {
        return false;
    }
    // done so let's get it. Notice the use of the non-checking initialization
    rfString_Init_nc(out,thisstr->bytes+bytePos+after->byteLength);
    // success
    return true;
}


// Initialize a string after the first of the given substrings found
#ifndef RF_OPTION_DEFAULT_ARGUMENTS
char rfString_Afterv(void* thisstrP,RF_String* result,const char* optionsP,const unsigned char* parNP,...)
#else
char i_rfString_Afterv(void* thisstrP,RF_String* result,const char* optionsP,const unsigned char* parNP,...)
#endif
{
    RF_String* thisstr = (RF_String*)thisstrP;
    RF_String* s;
    char options = *optionsP;
    unsigned char parN = *parNP;
    int32_t i,minPos,thisPos;
    uint32_t minPosLength;
    // will keep the argument list
    va_list argList;
    // get the parameter characters
    va_start(argList,parNP);

    minPos = 9999999;
    for(i = 0; i < parN; i++)
    {
        s = (RF_String*) va_arg(argList,RF_String*);
        if( (thisPos= rfString_FindBytePos(thisstr,s,options))!= RF_FAILURE)
        {
            if(thisPos < minPos)
            {
                minPos = thisPos;
                minPosLength = s->byteLength;
            }
        }
    }
    va_end(argList);
    // if it is not found
    if(minPos == 9999999)
    {
        return false;
    }
    // if it is found initialize the substring
    minPos += minPosLength;// go after the found substring
    result->byteLength = thisstr->byteLength-minPos;
    RF_MALLOC(result->bytes,result->byteLength);
    memcpy(result->bytes,thisstr->bytes+minPos,result->byteLength);
    result->bytes[result->byteLength] = '\0';
    // success
    return true;
}

/*------------------------------------------------------------------------ RF_String manipulation functions-------------------------------------------------------------------------------*/


// Appends the parameter String to this one
void i_rfString_Append(RF_String* thisstr,void* otherP)
{
    RF_String* other = (RF_String*)otherP;
    // /@note Here if a null addition is given lots of actions are done but the result is safe and the same string as the one entered.
    // /A check here would result in an additional check for every appending so I decided against it
    // calculate the new length
    thisstr->byteLength +=other->byteLength;
    // reallocate this string to fit the new addition
    RF_REALLOC(thisstr->bytes,char,thisstr->byteLength+1);
    // add the string to this one
    strncat(thisstr->bytes,other->bytes,other->byteLength);
}

// Appends an integer to the string
void rfString_Append_i(RF_String* thisstr,const int32_t i)
{
    // create a new buffer for the string big enough to fit any number plus the original string
    char* buff;
    RF_MALLOC(buff,thisstr->byteLength+15);// max uint32_t is 4,294,967,295 in most environment so 12 chars will certainly fit it
    // put the int32_t inside the string
    sprintf(buff,"%s%i",thisstr->bytes,i);
    // free the previous c string
    free(thisstr->bytes);
    // point the string pointer to the new string
    thisstr->bytes = buff;
    thisstr->byteLength = strlen(thisstr->bytes);
}
// Appends a float to the string. <b>Can't be used with RF_StringX</b>
void rfString_Append_f(RF_String* thisstr,const float f)
{
    // a temporary buffer to hold the float and the string
    char* buff;
    RF_MALLOC(buff,thisstr->byteLength+64);
    // put the float inside the string
    sprintf(buff,"%s%f",thisstr->bytes,f);
    // free the previous c string
    free(thisstr->bytes);
    // point the string pointer to the new string
    thisstr->bytes = buff;
    thisstr->byteLength = strlen(thisstr->bytes);
}

// Prepends the parameter String to this string
void i_rfString_Prepend(RF_String* thisstr,void* otherP)
{
    RF_String* other = (RF_String*)otherP;
    uint32_t size;
    int32_t i;// is not unsigned since it goes to -1 in the loop
    // keeep the original byte size of the string
    size = thisstr->byteLength;
    // calculate the new lengths
    thisstr->byteLength += other->byteLength;
    // reallocate this string to fit the new addition
    RF_REALLOC(thisstr->bytes,char,thisstr->byteLength+1);
    // move the pre-existing string to the end of the buffer, by dislocating each byte by cstrlen
    for(i =size; i >=0 ; i--)
        thisstr->bytes[i+other->byteLength] = thisstr->bytes[i];
    // and now add the new string to the start
    memcpy(thisstr->bytes,other->bytes,other->byteLength);
}

// Removes all of the specifed string occurences from this String matching case or not, DOES NOT reallocate buffer size.
char i_rfString_Remove(void* thisstrP,void* rstrP,uint32_t* numberP,const char* optionsP)
{
    RF_String* thisstr = (RF_String*)thisstrP;
    RF_String* rstr = (RF_String*)rstrP;
    char options = *optionsP;
    uint32_t number = *numberP;
    uint32_t i,count,occurences=0;
    int32_t bytePos;
    char found = false;
    // as long as we keep finding rstr in the string keep removing it
    do
    {   // if the substring is not found
        if( (bytePos = rfString_FindBytePos(thisstr,rstr,options)) == RF_FAILURE)
        {
            // if we have not even found it once , we fail
            if(found == false)
            {
                return false;
            }
            else // else we are done
                break;
        }

        // substring found
        found = true;
        // move all of the string a position back
        count = 0;
        for(i = bytePos; i <=thisstr->byteLength; i ++)
        {
            thisstr->bytes[i] = thisstr->bytes[i+rstr->byteLength];
            count++;
        }
        // now change the byte length
        thisstr->byteLength -= rstr->byteLength;
        // count the number of occurences and if we reached the required amount, stop
        occurences++;
        if(occurences == number)
            break;
    }while(bytePos != RF_FAILURE);
    // succcess
    return true;
}

// Removes all of the characters of the string except those specified
void i_rfString_KeepOnly(void* thisstrP,void* keepstrP)
{
    uint32_t keepLength,i,j,charValue,temp;
    uint32_t *keepChars;
    RF_String* thisstr = (RF_String*)thisstrP;
    RF_String* keepstr = (RF_String*)keepstrP;
    char exists,charBLength;
    // first let's get all of the characters of the keep string in an array
    i=0;
    keepLength = rfString_Length(keepstr);
    RF_MALLOC(keepChars,4*keepLength);
    rfString_Iterate_Start(keepstr,i,charValue)
        keepChars[i] = charValue;
    rfString_Iterate_End(i)
    // now iterate every character of this string
    i=0;
    rfString_Iterate_Start(thisstr,i,charValue)
        // for every character check if it exists in the keep str
        exists = false;
        for(j=0;j<keepLength; j++)
        {
            if(keepChars[j] == charValue)
                exists = true;
        }
        // if it does not exist, move the string back to cover it so that it effectively gets deleted
        if(exists == false)
        {
            charBLength = rfUTF8_FromCodepoint(charValue,&temp);
            // this is kind of a non-clean way to do it. the rfString_Iterate_Start macro internally uses a byteIndex_ variable
            // we use that here to determine the current byteIndex_ of the string in the iteration and move the string backs
            memmove(thisstr->bytes+byteIndex_,thisstr->bytes+byteIndex_+charBLength,thisstr->byteLength-byteIndex_+charBLength);
            thisstr->byteLength-=charBLength;
            continue;// by contiuing here we make sure that the current string position won't be moved to assure that we also check the newly move characters
        }
    rfString_Iterate_End(i)
    // before returning free the keep string's character array
    free(keepChars);
}

// Removes the first n characters from the start of the string
char rfString_PruneStart(void* thisstrP,uint32_t n)
{
    RF_String* thisstr = (RF_String*)thisstrP;
    // iterate the characters of the string
    uint32_t i;
    uint32_t length = 0;
    unsigned nBytePos = 0;
    char found = false;
    RF_STRING_ITERATE_START(thisstr,length,i);
        // if we reach the number of characters passed as a parameter, note it
        if(length == n)
        {
            // remember that now i is the byte position we need
            nBytePos = i;
            found = true;
            break;
        }
    RF_STRING_ITERATE_END(length,i)

    // if the string does not have n chars to remove it becomes an empty string and we return failure
    if(found == false)
    {
        thisstr->bytes[0] = '\0';
        thisstr->byteLength = 0;
        return false;
    }

    // move the string back to cover the empty places.reallocation here would be an overkill, everything will be freed together when the string gets freed
    for(i =0; i < thisstr->byteLength-nBytePos+1;i++ )
        thisstr->bytes[i] = thisstr->bytes[i+nBytePos];

    // get the new bytelength
    thisstr->byteLength -= nBytePos;

    return true;
}

// Removes the last n characters from the end of the string
char rfString_PruneEnd(void* thisstrP,uint32_t n)
{
    RF_String* thisstr = (RF_String*)thisstrP;
    // start the iteration of the characters from the end of the string
    int32_t nBytePos = -1;
    uint32_t length,i;
    RF_STRING_ITERATEB_START(thisstr,length,i)
        // if we found the requested number of characters from the end of the string
        if(length == n)
        {
            // remember that now i is the byte position we need
            nBytePos = i;
            break;
        }
    RF_STRING_ITERATEB_END(length,i)

    // if the string does not have n chars to remove it becomes an empty string and we return failure
    if(nBytePos == -1)
    {
        thisstr->bytes[0] = '\0';
        return false;
    }

    // just set the end of string character characters back, reallocation here would be an overkill, everything will be freed together when the string gets freed
    thisstr->bytes[nBytePos] = '\0';
    // and also set the new byte length
    thisstr->byteLength -= (thisstr->byteLength - nBytePos);
    // success
    return true;
}

// Removes n characters from the position p of the string counting backwards. If there is no space to do so, nothing is done and returns false.
char rfString_PruneMiddleB(void* thisstrP,uint32_t p,uint32_t n)
{
    RF_String* thisstr = (RF_String*)thisstrP;
    // if we ask to remove more characters from the position that it would be possible do nothign and return false
    if(n>p+1)
        return false;

    // iterate the characters of the string
    uint32_t j,i,length;
    int32_t pBytePos,nBytePos;
    pBytePos = nBytePos = -1;
    RF_STRING_ITERATE_START(thisstr,length,i)
        // if we reach the number of characters passed as a parameter, note it
        if(length == p+1)
        {
            // we search for p+1  because we want to include all of the p character
            pBytePos = i;
            // also break since we don't care after position p
            break;
        }
        if(length == p-n+1)// +1 is to make sure that indexing works from 0
            nBytePos = i;

    RF_STRING_ITERATE_END(length,i)

    // if the position was not found in the string do nothing
    if(pBytePos == -1 || nBytePos == -1)
        return false;

    // move the bytes in the buffer to remove the requested characters
    for(i=nBytePos,j=0;j<= thisstr->byteLength-pBytePos+1; i ++,j++) // here +2 is for (+1 for pbytePos to go to the start of pth character) (+1 for the byteLength to include the null termination character)
    {
        thisstr->bytes[i] = thisstr->bytes[pBytePos+j];
    }

    // find the new byte length
    thisstr->byteLength -= (nBytePos - pBytePos);

    return true;
}

// Removes n characters from the position p of the string counting forwards. If there is no space, nothing is done and returns false.
char rfString_PruneMiddleF(void* thisstrP,uint32_t p,uint32_t n)
{
    RF_String* thisstr = (RF_String*)thisstrP;
    // iterate the characters of the string
    uint32_t j,i,length;
    int32_t pBytePos,nBytePos;
    pBytePos = nBytePos = -1;
    RF_STRING_ITERATE_START(thisstr,length,i)
        // if we reach the number of characters passed as a parameter, note it
        if(length == p)
            pBytePos = i;

        if(length == p+n)
        {
            nBytePos = i;
            break;// since we got all the data we needed
        }

    RF_STRING_ITERATE_END(length,i)

    // if the position was not found in the string do nothing
    if(pBytePos == -1 )
        return false;

    // if we did not find the byte position of p+n then we remove everything from pBytePos until the end of the string
    if(nBytePos == -1)
    {
        thisstr->bytes[pBytePos] = '\0';
        thisstr->byteLength -= (thisstr->byteLength-pBytePos);
        return true;
    }

    // move the bytes in the buffer to remove the requested characters
    for(i=pBytePos,j=0;j<= thisstr->byteLength-nBytePos+1; i ++,j++) // here +2 is for (+1 for pbytePos to go to the start of pth character) (+1 for the byteLength to include the null termination character)
    {
        thisstr->bytes[i] = thisstr->bytes[nBytePos+j];
    }

    // find the new byte length
    thisstr->byteLength -= (nBytePos - pBytePos);
    return true;
}

// Replaces all of the specified sstr substring from the String with rstr and reallocates size, unless the new size is smaller
char i_rfString_Replace(RF_String* thisstr,void* sstrP,void* rstrP,const uint32_t* numP,const char* optionsP)
{
    RF_String* sstr = (RF_String*)sstrP;
    RF_String* rstr = (RF_String*)rstrP;
    char options = *optionsP;
    uint32_t num = *numP;
    RF_StringX temp;// just a temporary string for finding the occurences
    // will keep the number of found instances of the substring
    uint32_t foundN = 0;
    // will keep the number of given instances to find
    uint32_t number = num;
    uint32_t diff,i,j;
    // if the substring string is not even found return false
    if(rfString_FindBytePos(thisstr,sstr,options) == RF_FAILURE)
    {
        return false;
    }
    // create a buffer that will keep the byte positions
    uint32_t bSize = 50;
    int32_t * bytePositions;
    RF_MALLOC(bytePositions,bSize*sizeof(int32_t));
    // if the given num is 0 just make sure we replace all
    if(number == 0)
        number = 999999;// max number of occurences

    // find how many occurences exist
    rfStringX_FromString_IN(&temp,thisstr);
    while( (bytePositions[foundN] = rfString_FindBytePos(&temp,sstr,options))  != RF_FAILURE)
    {
        int32_t move = bytePositions[foundN] + sstr->byteLength;
        bytePositions[foundN] = bytePositions[foundN]+temp.bIndex;
        temp.bIndex += move;
        temp.bytes += move;
        temp.byteLength -= move;
        foundN++;
        // if buffer is in danger of overflow realloc it
        if(foundN > bSize)
        {
            bSize *=2;
            RF_REALLOC(bytePositions,int32_t,bSize);
        }
        // if we found the required number of occurences break;
        if(foundN >= number)
            break;
    }
    rfStringX_Deinit(&temp);
    // make sure that the number of occurence to replace do not exceed the actual number of occurences
    if(number > foundN)
        number = foundN;
    // act depending on the size difference of rstr and sstr
    if(rstr->byteLength > sstr->byteLength) // replace string is bigger than the removed one
    {
        int32_t orSize,nSize;

        diff = rstr->byteLength - sstr->byteLength;
        // will keep the original size in bytes
        orSize = thisstr->byteLength +1;
        // reallocate the string to fit the new bigger size
        nSize= orSize + number*diff;
        RF_REALLOC(thisstr->bytes,char,nSize)
        // now replace all the substrings one by one
        for(i = 0; i < number; i ++)
        {
            // move all of the contents of the string to fit the replacement
            for(j =orSize+diff-1; j > bytePositions[i]+sstr->byteLength; j -- )
                thisstr->bytes[j] = thisstr->bytes[j-diff];
            // copy in the replacement
            strncpy(thisstr->bytes+bytePositions[i],rstr->bytes,rstr->byteLength);
            // also increase the original size (since now we moved the whole string by one replacement)
            orSize += diff;
            // also increase all the subsequent found byte positions since there is a change of string size
            for(j = i+1; j < number; j ++)
                bytePositions[j] = bytePositions[j]+diff;

        }
        // finally let's keep the new byte length
        thisstr->byteLength = nSize-1;
    }
    else if( rstr->byteLength < sstr->byteLength) // replace string is smaller than the removed one
    {
        // get the differenc in byte length of removed substring and replace string
        diff = sstr->byteLength-rstr->byteLength;

        // now replace all the substrings one by one
        for(i =0; i < number; i ++)
        {
            // copy in the replacement
            strncpy(thisstr->bytes+bytePositions[i],rstr->bytes,rstr->byteLength);
            // move all of the contents of the string to fit the replacement
            for(j =bytePositions[i]+rstr->byteLength; j < thisstr->byteLength; j ++ )
                thisstr->bytes[j] = thisstr->bytes[j+diff];
            // also decrease all the subsequent found byte positions since there is a change of string size
            for(j = i+1; j < number; j ++)
                bytePositions[j] = bytePositions[j]-diff;
        }
        // finally let's keep the new byte length
        thisstr->byteLength -= diff*number;
        // just note that reallocating downwards is not necessary
    }
    else // replace and remove strings are equal
    {
        for(i = 0; i < number; i ++)
            strncpy(thisstr->bytes+bytePositions[i],rstr->bytes,rstr->byteLength);
    }
    free(bytePositions);
    // success
    return true;
}

// Removes all characters of a substring only from the start of the String
char i_rfString_StripStart(void* thisstrP,void* subP)
{
    RF_String* thisstr = (RF_String*) thisstrP;
    RF_String*sub = (RF_String*) subP;
    char ret = false,noMatch;
    uint32_t charValue,i = 0,*subValues,j,subLength,bytePos;

    // firstly get all of the characters of the substring in an array
    subLength = rfString_Length(sub);
    RF_MALLOC(subValues,4*subLength)
    rfString_Iterate_Start(sub,i,charValue)
    subValues[i] = charValue;
    rfString_Iterate_End(i)

    // iterate thisstring from the beginning
    i = 0;
    RF_STRING_ITERATE_START(thisstr,i,bytePos)
        noMatch = true;
        // for every substring character
        for(j = 0;j < subLength; j++)
        {
            // if we got a match
            if(rfString_BytePosToCodePoint(thisstr,bytePos) == subValues[j])
            {
                ret = true;
                noMatch = false;
                break;
            }
        }
        // if we get out of iterating the substring without having found a match, we get out of the iteration in general
        if(noMatch)
            break;
    RF_STRING_ITERATE_END(i,bytePos)

    // if we had any match
    if(ret == true)
    {
        // remove the characters
        for(i =0; i < thisstr->byteLength-bytePos+1;i++ )
            thisstr->bytes[i] = thisstr->bytes[i+bytePos];
        // also change bytelength
        thisstr->byteLength -= bytePos;
    }
    // free stuff and return
    free(subValues);
    return ret;
}

// Removes all characters of a substring starting from the end of the String
char i_rfString_StripEnd(void* thisstrP,void* subP)
{
    RF_String* thisstr = (RF_String*) thisstrP;
    RF_String*sub = (RF_String*) subP;
    char ret = false,noMatch;
    uint32_t charValue,i = 0,*subValues,j,subLength,bytePos,lastBytePos,testity;

    // firstly get all of the characters of the substring in an array
    subLength = rfString_Length(sub);
    RF_MALLOC(subValues,4*subLength)
    rfString_Iterate_Start(sub,i,charValue)
    subValues[i] = charValue;
    rfString_Iterate_End(i)

    // iterate thisstring from the end
    i = 0;
    RF_STRING_ITERATEB_START(thisstr,i,bytePos)
        noMatch = true;
        // for every substring character
        for(j = 0;j < subLength; j++)
        {
            // if we got a match
            if((testity=rfString_BytePosToCodePoint(thisstr,bytePos)) == subValues[j])
            {
                ret = true;
                noMatch = false;
                lastBytePos = bytePos;
                break;
            }
        }
        // if we get out of iterating the substring without having found a match, we get out of the iteration in general
        if(noMatch)
            break;
    RF_STRING_ITERATEB_END(i,bytePos)

    // if we had any match
    if(ret == true)
    {
        // just set the end of string there
        thisstr->bytes[lastBytePos] = '\0';
        // and also set the new byte length
        thisstr->byteLength -= (thisstr->byteLength - lastBytePos);
    }

    // free stuff and return
    free(subValues);
    return ret;
}

// Removes all characters of a substring from both ends of the given String
char i_rfString_Strip(void* thisstrP,void* subP)
{
    char res1 = rfString_StripStart(thisstrP,subP);
    char res2 = rfString_StripEnd(thisstrP,subP);
    return res1|res2;
}


/*------------------------------------------------------------------------ RF_String File I/O functions-------------------------------------------------------------------------------*/

// Allocates and returns a string from file parsing. The file's encoding must be UTF-8.If for some reason (like EOF reached) no string can be read then null is returned
RF_String* rfString_Create_fUTF8(FILE* f, char* eof)
{
    RF_String* ret;
    RF_MALLOC(ret,sizeof(RF_String));
    if(rfString_Init_fUTF8(ret,f,eof) < 0)
    {
        free(ret);
        return 0;
    }
    return ret;
}
// Initializes a string from file parsing. The file's encoding must be UTF-8.If for some reason (like EOF reached) no string can be read then null is returned
int32_t rfString_Init_fUTF8(RF_String* str,FILE* f,char* eof)
{
    int32_t bytesN;
    uint32_t bufferSize;// unused
    if((bytesN=rfFReadLine_UTF8(f,&str->bytes,&str->byteLength,&bufferSize,eof)) < 0)
    {
        LOG_ERROR("Failed to initialize String from a UTF-8 file",bytesN);
        return bytesN;
    }
    // success
    return bytesN;
}
// Assigns to a String from UTF-8 file parsing
int32_t rfString_Assign_fUTF8(RF_String* str,FILE*f,char* eof)
{
    int32_t bytesN;
    uint32_t utf8ByteLength,utf8BufferSize;// bufferSize unused in this function
    char* utf8 = 0;
    if((bytesN=rfFReadLine_UTF8(f,&utf8,&utf8ByteLength,&utf8BufferSize,eof)) < 0)
    {
        LOG_ERROR("Failed to assign the contents of a UTF-8 file to a String",bytesN);
        return bytesN;
    }
    // success
    // assign it to the string
    if(str->byteLength <= utf8ByteLength)
    {
        RF_REALLOC(str->bytes,char,utf8ByteLength+1);
    }
    memcpy(str->bytes,utf8,utf8ByteLength+1);
    str->byteLength = utf8ByteLength;
    // free the file's utf8 buffer
    free(utf8);
    return bytesN;
}
// Appends to a String from UTF-8 file parsing
int32_t rfString_Append_fUTF8(RF_String* str,FILE*f,char* eof)
{
    int32_t bytesN;
    uint32_t utf8ByteLength,utf8BufferSize;// bufferSize unused in this function
    char* utf8 = 0;
    if((bytesN=rfFReadLine_UTF8(f,&utf8,&utf8ByteLength,&utf8BufferSize,eof)) < 0)
    {
        LOG_ERROR("Failed to assign the contents of a UTF-8 file to a String",bytesN);
        return bytesN;
    }
    // append the utf8 to the given string
    rfString_Append(str,RFS_(utf8));
    // free the file's utf8 buffer
    free(utf8);
    return bytesN;
}

// Allocates and returns a string from file parsing. The file's encoding must be UTF-16.If for some reason (like EOF reached) no string can be read then null is returned. A check for a valid sequence of bytes is performed.
RF_String* rfString_Create_fUTF16(FILE* f,char endianess,char* eof)
{
    RF_String* ret;
    RF_MALLOC(ret,sizeof(RF_String));
    if(rfString_Init_fUTF16(ret,f,endianess,eof) < 0)
        return 0;
    return ret;
}
// Initializes a string from file parsing. The file's encoding must be UTF-16.If for some reason (like EOF reached) no string can be read then null is returned. A check for a valid sequence of bytes is performed.
int32_t rfString_Init_fUTF16(RF_String* str,FILE* f, char endianess,char* eof)
{
    int32_t bytesN;
    // depending on the file's endianess
    if(endianess == RF_LITTLE_ENDIAN)
    {
        if((bytesN=rfFReadLine_UTF16LE(f,&str->bytes,&str->byteLength,eof)) < 0)
        {
            LOG_ERROR("Failure to initialize a String from reading a UTF-16 file",bytesN);
            return bytesN;
        }
    }// end of little endian
    else// big endian
    {
        if((bytesN=rfFReadLine_UTF16BE(f,&str->bytes,&str->byteLength,eof)) < 0)
        {
            LOG_ERROR("Failure to initialize a String from reading a UTF-16 file",bytesN);
            return bytesN;
        }
    }// end of big endian case
    // success
    return bytesN;
}

// Assigns to an already initialized String from File parsing
int32_t rfString_Assign_fUTF16(RF_String* str,FILE* f, char endianess,char* eof)
{

    uint32_t utf8ByteLength;
    int32_t bytesN;
    char* utf8 = 0;
    // depending on the file's endianess
    if(endianess == RF_LITTLE_ENDIAN)
    {
        if((bytesN=rfFReadLine_UTF16LE(f,&utf8,&utf8ByteLength,eof)) < 0)
        {
            LOG_ERROR("Failure to assign the contents of a Little Endian UTF-16 file to a String",bytesN);
            return bytesN;
        }
    }// end of little endian
    else// big endian
    {
        if((bytesN=rfFReadLine_UTF16BE(f,&utf8,&utf8ByteLength,eof)) < 0)
        {
            LOG_ERROR("Failure to assign the contents of a Big Endian UTF-16 file to a String",bytesN);
            return bytesN;
        }
    }// end of big endian case
    // success
    // assign it to the string
    if(str->byteLength <= utf8ByteLength)
    {
        RF_REALLOC(str->bytes,char,utf8ByteLength+1);
    }
    memcpy(str->bytes,utf8,utf8ByteLength+1);
    str->byteLength = utf8ByteLength;
    // free the file's utf8 buffer
    free(utf8);
    return bytesN;
}

// Appends to an already initialized String from File parsing
int32_t rfString_Append_fUTF16(RF_String* str,FILE* f, char endianess,char* eof)
{
    char*utf8;
    uint32_t utf8ByteLength;
    int32_t bytesN;
    // depending on the file's endianess
    if(endianess == RF_LITTLE_ENDIAN)
    {
        if((bytesN=rfFReadLine_UTF16LE(f,&utf8,&utf8ByteLength,eof)) < 0)
        {
            LOG_ERROR("Failure to append the contents of a Little Endian UTF-16 file to a String",bytesN);
            return bytesN;
        }
    }// end of little endian
    else// big endian
    {
        if((bytesN=rfFReadLine_UTF16BE(f,&utf8,&utf8ByteLength,eof)) < 0)
        {
            LOG_ERROR("Failure to append the contents of a Big Endian UTF-16 file to a String",bytesN);
            return bytesN;
        }
    }// end of big endian case
    // success
    rfString_Append(str,RFS_(utf8));
    free(utf8);
    return bytesN;
}

// Allocates and returns a string from file parsing. The file's encoding must be UTF-32.If for some reason (like EOF reached) no string can be read then null is returned. A check for a valid sequence of bytes is performed.
RF_String* rfString_Create_fUTF32(FILE* f,char endianess,char* eof)
{
    RF_String* ret;
    RF_MALLOC(ret,sizeof(RF_String));
    if(rfString_Init_fUTF32(ret,f,endianess,eof) < 0)
    {
        free(ret);
        return 0;
    }
    return ret;
}
// Initializes a string from file parsing. The file's encoding must be UTF-32.If for some reason (like EOF reached) no string can be read then null is returned. A check for a valid sequence of bytes is performed.
int32_t rfString_Init_fUTF32(RF_String* str,FILE* f,char endianess,char* eof)
{
    int32_t bytesN;
    // depending on the file's endianess
    if(endianess == RF_LITTLE_ENDIAN)
    {
        if((bytesN=rfFReadLine_UTF32LE(f,&str->bytes,&str->byteLength,eof)) <0)
        {
            LOG_ERROR("Failure to initialize a String from reading a Little Endian UTF-32 file",bytesN);
            return bytesN;
        }
    }// end of little endian
    else// big endian
    {
        if((bytesN=rfFReadLine_UTF16BE(f,&str->bytes,&str->byteLength,eof)) < 0)
        {
            LOG_ERROR("Failure to initialize a String from reading a Big Endian UTF-32 file",bytesN);
            return bytesN;
        }
    }// end of big endian case
    // success
    return bytesN;
}
// Assigns the contents of a UTF-32 file to a string
int32_t rfString_Assign_fUTF32(RF_String* str,FILE* f,char endianess, char* eof)
{
    int32_t bytesN;
    char*utf8;
    uint32_t utf8ByteLength;
    // depending on the file's endianess
    if(endianess == RF_LITTLE_ENDIAN)
    {
        if((bytesN=rfFReadLine_UTF32LE(f,&utf8,&utf8ByteLength,eof)) < 0)
        {
            LOG_ERROR("Failure to assign to a String from reading a Little Endian UTF-32 file",bytesN);
            return bytesN;
        }
    }// end of little endian
    else// big endian
    {
        if((bytesN=rfFReadLine_UTF16BE(f,&utf8,&utf8ByteLength,eof)) < 0)
        {
            LOG_ERROR("Failure to assign to a String from reading a Big Endian UTF-32 file",bytesN);
            return bytesN;
        }
    }// end of big endian case
    // success
    // assign it to the string
    if(str->byteLength <= utf8ByteLength)
    {
        RF_REALLOC(str->bytes,char,utf8ByteLength+1);
    }
    memcpy(str->bytes,utf8,utf8ByteLength+1);
    str->byteLength = utf8ByteLength;
    // free the file's utf8 buffer
    free(utf8);
    return bytesN;
}
// Appends the contents of a UTF-32 file to a string
int32_t rfString_Append_fUTF32(RF_String* str,FILE* f,char endianess, char* eof)
{
    int32_t bytesN;
    char*utf8;
    uint32_t utf8ByteLength;
    // depending on the file's endianess
    if(endianess == RF_LITTLE_ENDIAN)
    {
        if((bytesN=rfFReadLine_UTF32LE(f,&utf8,&utf8ByteLength,eof)) < 0)
        {
            LOG_ERROR("Failure to append to a String from reading a Little Endian UTF-32 file",bytesN);
            return bytesN;
        }
    }// end of little endian
    else// big endian
    {
        if((bytesN=rfFReadLine_UTF16BE(f,&utf8,&utf8ByteLength,eof)) < 0)
        {
            LOG_ERROR("Failure to append to a String from reading a Big Endian UTF-32 file",bytesN);
            return bytesN;
        }
    }// end of big endian case
    // success
    // append it
    rfString_Append(str,RFS_(utf8));
    // free the file'sutf8 buffer
    free(utf8);
    return bytesN;
}

// Writes a string to a file in UTF-8 encoding.
int32_t i_rfString_Fwrite(void* sP,FILE* f,char* encodingP)
{
    uint32_t *utf32,length,i;
    uint16_t* utf16;
    RF_String* s = (RF_String*)sP;
    char encoding = *encodingP;
    // depending on the encoding
    switch(encoding)
    {
        case RF_UTF8:
            if(fwrite(s->bytes,1,s->byteLength,f) != s->byteLength)
                break;// and go to error logging
            return RF_SUCCESS;
        break;
        case RF_UTF16_LE:
            utf16 = rfString_ToUTF16(s,&length);
            if(rfUTILS_Endianess() != RF_LITTLE_ENDIAN)
            {
                for(i=0;i<length;i++)
                {
                    rfUTILS_SwapEndianUS(&utf16[i]);
                }
            }
            if(fwrite(utf16,2,length,f) != length)
            {
                free(utf16);
                break;// and go to error logging
            }
            free(utf16);
            return RF_SUCCESS;
        break;
        case RF_UTF16_BE:
            utf16 = rfString_ToUTF16(s,&length);
            if(rfUTILS_Endianess() != RF_BIG_ENDIAN)
            {
                for(i=0;i<length;i++)
                {
                    rfUTILS_SwapEndianUS(&utf16[i]);
                }
            }
            if(fwrite(utf16,2,length,f) != length)
            {
                free(utf16);
                break;// and go to error logging
            }
            free(utf16);
            return RF_SUCCESS;
        break;
        case RF_UTF32_LE:
            utf32 = rfString_ToUTF32(s,&length);
            if(rfUTILS_Endianess() != RF_LITTLE_ENDIAN)
            {
                for(i=0;i<length;i++)
                {
                    rfUTILS_SwapEndianUI(&utf32[i]);
                }
            }
            if(fwrite(utf32,4,length,f) != length)
            {
                free(utf32);
                break;// and go to error logging
            }
            free(utf32);
            return RF_SUCCESS;
        break;
        case RF_UTF32_BE:
            utf32 = rfString_ToUTF32(s,&length);
            if(rfUTILS_Endianess() != RF_BIG_ENDIAN)
            {
                for(i=0;i<length;i++)
                {
                    rfUTILS_SwapEndianUI(&utf32[i]);
                }
            }
            if(fwrite(utf32,4,length,f) != length)
            {
                free(utf32);
                break;// and go to error logging
            }
            free(utf32);
            return RF_SUCCESS;
        break;
    }
    // if we get here it means an error, and we log it with the macro
    i_WRITE_CHECK(f,"Writting a string to a file")
    return RE_FILE_WRITE;
}


