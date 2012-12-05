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

#ifndef REFU_USTRING_H
#define REFU_USTRING_H

#include <rf_options.h>

#ifdef RF_MODULE_STRINGS// check if the strings are included as a module

#include <stdio.h>
#include <rf_setup.h>

#include <Preprocessor/rf_xmacro_argcount.h> // for the argument count
#include <rf_localmem.h> // for the local memory function wrapping functionality
#include <IO/rf_unicode.h>// for unicode


#ifdef __cplusplus
extern "C"
{// opening bracket for calling from C++
#endif

// An option for some string functions. Means that the case should not be exactly matched in the string replacing,finding e.t.c.
#define RF_CASE_IGNORE  0x1
// An options for some string functions. Means that the exact string should be found/replaced e.t.c.
#define RF_MATCH_WORD 0x2


// Denotes that a requested character/byte index in an RF_String is out of bounds
#define RF_STRING_INDEX_OUT_OF_BOUNDS   ((uint32_t)0xFF0FFFF)

/* These are here so that the iteration macros can work*/

// Checks if a given byte is a continuation byte
#define rfUTF8_IsContinuationByte2(b__)  ( b__ >= 0x80 && b__<= 0xBF )

#pragma pack(push,1)
/**
** @internal
** @author Lefteris
** @date 09/12/2010
** @endinternal
** @brief A unicode String with UTF-8 internal representation
**
** The Refu String is a Unicode String that has two versions. One is this and for the other check @ref RF_StringX to see what operations can be performed on extended Strings.
** Functions to convert to and from all UTF encoding exists but the internal representation is always at UTF-8. Once a
** a String has been created it is always assumed that the stream of bytes inside it is valid UTF-8 since every function
** performs a UTF-8 check unless otherwise specified.
**
** All the functions which have @isinherited{StringX} on their description can be used with extended strings safely, since no specific
** version of the function exists, or needs to exist to manipulate Extended Strings. To make the documentation even clearer the functions that should not
** be used with the extended string are marked with @notinherited{StringX}
** @internal
** @cppcode
** //default constructor
** String(){this->i_StringCHandle = rfString_Create("");}
** @endcpp
** @endinternal
*/
typedef struct RF_String
{
    // The string's data
    char* bytes;
    // The string's length in bytes (not including the null termination). The string keeps its length in bytes
    // to avoid multiple calls to strlen()
    uint32_t byteLength;
}RF_String;
#pragma pack(pop)


// @memberof RF_String
// @brief Create a termporary String from a String literal
//
// A macro to be used only inside a function call that accepts an @ref RF_String to create a Temporary RF_String*
// that will be used by the function. This macro accepts from 1 to N arguments.
//
// The first argument shall either be a String literal or a printf styled string literal
// given in the source file's encoding(default is UTF-8). For other encodings look at the compile time
// option @c RF_OPTION_SOURCE_ENCODING that can be provided during building the library, but it is
// @b strongly recommended to use UTF-8 encoded source files.
//
// Optionally the first argument can be followed by a sequence of additional arguments,
// each containing one value to be inserted instead of each %-tag specified in the string literal
// parameter, if any. There should be
// the same number of these arguments as the number of %-tags that expect a value.
// Basically the usage is the same as @ref rfString_Create
//
// @param s The formatted string that will constitute the RF_String. Must be in the same encoding as that of the source file.
// Default is UTF-8.
// @param ... \rfoptional{nothing}  Depending on the string literal, the function may expect a sequence of additional arguments,
// each containing one value to be inserted instead of each %-tag specified in the @c slit parameter, if any. There should be
// the same number of these arguments as the number of %-tags that expect a value.
// @return Returns true in case of correct initialization and false , due to invalid byte sequence for the given encoding
// @isinherited{StringX}
#ifdef RF_IAMHERE_FOR_DOXYGEN
RF_String* RFS_(const char* s,...);
#else
#define RFS_(...) i_rfString_CreateLocal(__VA_ARGS__)
#endif



/*-------------------------------------------------------------------------Methods to create an RF_String-------------------------------------------------------------------------------*/
// @name Creating an RF_String
// @{


// @memberof RF_String
// @opassign
// @brief Allocates and returns a string with the given characters
//
// Given characters have to be in UTF-8. A check for valid sequence of bytes is performed. @notinherited{StringX}
// @param s The sequence of bytes for the characters in UTF-8 (the default). Can also follow a printf-like format which will be formatted with
// the variables that follow it. A check to see if it is a valid UTF-8 sequence is performed
// @param ... \rfoptional{nothing}  Depending on the string literal, the function may expect a sequence of additional arguments,
// each containing one value to be inserted instead of each %-tag specified in the @c slit parameter, if any. There should be
// the same number of these arguments as the number of %-tags that expect a value.
// @return Returns the initialized RF_string or null in case of failure to initialize, due to invalid utf-8 sequence
#ifndef RF_OPTION_DEFAULT_ARGUMENTS
i_DECLIMEX_ RF_String* rfString_Create(const char* s,...);
#else
i_DECLIMEX_ RF_String* i_rfString_Create(const char* s,...);
i_DECLIMEX_ RF_String* i_NVrfString_Create(const char* s);
#define rfString_Create(...)  RP_SELECT_FUNC_IF_NARGIS(i_SELECT_RF_STRING_CREATE,1,__VA_ARGS__)
#define i_SELECT_RF_STRING_CREATE1(...) i_NVrfString_Create(__VA_ARGS__)
#define i_SELECT_RF_STRING_CREATE0(...) i_rfString_Create(__VA_ARGS__)
#endif

///Internal function that creates a temporary RF_String*
i_DECLIMEX_ RF_String* i_rfString_CreateLocal1(const char* s,...);
i_DECLIMEX_ RF_String* i_NVrfString_CreateLocal(const char* s);
#define i_rfString_CreateLocal(...)  RP_SELECT_FUNC_IF_NARGIS(i_SELECT_RF_STRING_CREATELOCAL,1,__VA_ARGS__)
#define i_SELECT_RF_STRING_CREATELOCAL1(...) i_NVrfString_CreateLocal(__VA_ARGS__)
#define i_SELECT_RF_STRING_CREATELOCAL0(...) i_rfString_CreateLocal1(__VA_ARGS__)


// @memberof RF_String
// @brief Initializes a string with the given characters.
//
// @notinherited{StringX}
// Given characters have to be in UTF-8. A check for valide sequence of bytes is performed.
// @param str The string to initialize
// @param s The sequence of bytes for the characters in UTF-8 (the default).Can also follow a printf-like format which will be formatted with
// the variables that follow it. A check to see if it is a valid UTF-8 sequence is performed
// @param ... \rfoptional{nothing}  Depending on the string literal, the function may expect a sequence of additional arguments,
// each containing one value to be inserted instead of each %-tag specified in the @c slit parameter, if any. There should be
// the same number of these arguments as the number of %-tags that expect a value.
// @return Returns true in case of correct initialization and false , due to invalid utf-8 sequence
#ifndef RF_OPTION_DEFAULT_ARGUMENTS
i_DECLIMEX_ char rfString_Init(RF_String* str,const char* s,...);
#else
i_DECLIMEX_ char i_rfString_Init(RF_String* str,const char* s,...);
i_DECLIMEX_ char i_NVrfString_Init(RF_String* str,const char* s);
#define rfString_Init(...)  RP_SELECT_FUNC_IF_NARGIS(i_SELECT_RF_STRING_INIT,2,__VA_ARGS__)
#define i_SELECT_RF_STRING_INIT1(...) i_NVrfString_Init(__VA_ARGS__)
#define i_SELECT_RF_STRING_INIT0(...) i_rfString_Init(__VA_ARGS__)
#endif

// @memberof RF_String
// @cppnotctor
// @brief Allocates a String by turning a unicode code point in a String (encoded in UTF-8).
//
// @notinherited{StringX}
// @param code The unicode code point to encode
// @return A String with the code point encoded in it or a null pointer in case of an illegal code point value
i_DECLIMEX_ RF_String* rfString_Create_cp(uint32_t code);
// @memberof RF_String
// @brief Initializes a string by turning a unicode code point in a String (encoded in UTF-8).
//
// @notinherited{StringX}
// @param str The string to initialize
// @param code The unicode code point to encode
// @return Returns true in case of correct initialization and false , due to illegal code point value
i_DECLIMEX_ char rfString_Init_cp(RF_String* str,uint32_t code);


#ifndef RF_OPTION_DEFAULT_ARGUMENTS
// @memberof RF_String
// @cppnotctor
// @brief Allocates and returns a string with the given characters with no checking.
//
// @notinherited{StringX}
// @warning NO VALID-UTF8 check is performed.
// @param s The sequence of bytes for the characters in UTF-8 (the default).Can also follow a printf-like format which will be formatted with
// the variables that follow it. No check for valid bytestream is performed
// @param ... \rfoptional{nothing}  Depending on the string literal, the function may expect a sequence of additional arguments,
// each containing one value to be inserted instead of each %-tag specified in the @c slit parameter, if any. There should be
// the same number of these arguments as the number of %-tags that expect a value.
// @return Returns the initialized RF_string or null in case of failure to initialize
i_DECLIMEX_ RF_String* rfString_Create_nc(const char* s,...);
#else
i_DECLIMEX_ RF_String* i_rfString_Create_nc(const char* s,...);
i_DECLIMEX_ RF_String* i_NVrfString_Create_nc(const char* s);
#define rfString_Create_nc(...)  RP_SELECT_FUNC_IF_NARGIS(i_SELECT_RF_STRING_CREATE_NC,1,__VA_ARGS__)
#define i_SELECT_RF_STRING_CREATE_NC1(...) i_NVrfString_Create_nc(__VA_ARGS__)
#define i_SELECT_RF_STRING_CREATE_NC0(...) i_rfString_Create_nc(__VA_ARGS__)
#endif

#ifndef RF_OPTION_DEFAULT_ARGUMENTS
// @memberof RF_String
// @brief Initializes a string with the given characters with no checking
//
// @notinherited{StringX}
// @warning NO VALID-UTF8 check is performed.
// @param str The string to initialize
// @param s The sequence of bytes for the characters in UTF-8 (the default).Can also follow a printf-like format which will be formatted with
// the variables that follow it. No check for valid bytestream is performed
// @param ... \rfoptional{nothing}  Depending on the string literal, the function may expect a sequence of additional arguments,
// each containing one value to be inserted instead of each %-tag specified in the @c slit parameter, if any. There should be
// the same number of these arguments as the number of %-tags that expect a value.
// @return Returns true in case of correct initialization and false otherwise
i_DECLIMEX_ char rfString_Init_nc(RF_String* str,const char* s,...);
#else
i_DECLIMEX_ char i_rfString_Init_nc(RF_String* str,const char* s,...);
i_DECLIMEX_ char i_NVrfString_Init_nc(RF_String* str,const char* s);
#define rfString_Init_nc(...)  RP_SELECT_FUNC_IF_NARGIS(i_SELECT_RF_STRING_INIT_NC,2,__VA_ARGS__)
#define i_SELECT_RF_STRING_INIT_NC1(...) i_NVrfString_Init_nc(__VA_ARGS__)
#define i_SELECT_RF_STRING_INIT_NC0(...) i_rfString_Init_nc(__VA_ARGS__)
#endif

// @memberof RF_String
// @opassign
// @brief Allocates and returns a string with the given integer.
//
// @notinherited{StringX}
// @param i The integer to turn into a string
// @return Returns the initialized RF_string
i_DECLIMEX_ RF_String* rfString_Create_i(int32_t i);
// @memberof RF_String
// @brief Initializes a string with the given integer.
//
// @notinherited{StringX}
// @param str The string to initialize
// @param i The integer to turn into a string
// @return Returns true in case of correct initialization and false otherwise
i_DECLIMEX_ char rfString_Init_i(RF_String* str,int32_t i);
// @memberof RF_String
// @opassign
// @brief Allocates and returns a string with the given float.
//
// @notinherited{StringX}
// @param f The float to turn into a string
// @return Returns the initialized RF_string
i_DECLIMEX_ RF_String* rfString_Create_f(float f);
// @memberof RF_String
// @brief Initializes a string with the given float.
//
// @notinherited{StringX}
// @param str The string to initialize
// @param f The float to turn into a string
// @return Returns true in case of correct initialization and false otherwise
i_DECLIMEX_ char rfString_Init_f(RF_String* str,float f);

// @memberof RF_String
// @brief Allocates and returns a string with the given UTF-16 byte sequence.
//
// @notinherited{StringX}
// Given characters have to be in UTF-16
// @param s The sequence of bytes for the characters in UTF-16.
// @param endianess A flag that determined in what endianess the sequence of UTF-16 bytes is in. Possible values here is
// @c RF_LITTLE_ENDIAN and @c RF_BIG_ENDIAN.
// @return Returns the initialized RF_string or null in case of failure to initialize, due to invalid utf-16 sequence or illegal endianess value
i_DECLIMEX_ RF_String* rfString_Create_UTF16(const char* s,char endianess);
// @memberof RF_String
// @brief Initializes a string with the given UTF-16 byte sequence.
//
// @notinherited{StringX}
// Given characters have to be in UTF-16
// @param str The string to initialize
// @param s The sequence of bytes for the characters in UTF-16.
// @param endianess A flag that determined in what endianess the sequence of UTF-16 bytes is in. Possible values here is
// @c RF_LITTLE_ENDIAN and @c RF_BIG_ENDIAN.
// @return Returns true for succesfull initialization and false otherwise due to invalid utf-16 sequence or illegal endianess value
i_DECLIMEX_ char rfString_Init_UTF16(RF_String* str,const char* s,char endianess);

// @memberof RF_String
// @cppnotctor
// @brief Allocates and returns a string with the given UTF-32 byte sequence.
//
// @notinherited{StringX}
// Given characters have to be in UTF-32
// @param s The sequence of bytes for the characters in UTF-32. Needs to be null terminated.
// @return Returns the initialized RF_string or null in case of failure to initialize
i_DECLIMEX_ RF_String* rfString_Create_UTF32(const char* s);
// @memberof RF_String
// @brief Initializes a string with the given UTF-32 byte sequence.
//
// @notinherited{StringX}
// Given characters have to be in UTF-32
// @param str The string to initialize
// @param s The sequence of bytes for the characters in UTF-32. Needs to be null terminated.
// @return Returns true for successful initialization and false otherwise
i_DECLIMEX_ char rfString_Init_UTF32(RF_String* str,const char* s);
//@}

/*-------------------------------------------------------------------------Methods to copy/assign an RF_String-------------------------------------------------------------------------------*/
// @name Copying - Assigning a String
// @{

// @memberof RF_String
// @brief Assigns the value of the source string to the destination.
//
// @notinherited{StringX}
// @lmsFunction
// Both strings should already be initialized and hold a value. It is an error to give null parameters.
// @param dest The destination string, which should get assigned
// @param source The source string, whose values to copy. @inhtype{String,StringX} @tmpSTR
#if defined(RF_IAMHERE_FOR_DOXYGEN)
void rfString_Assign(RF_String* dest,void* source);
#else
i_DECLIMEX_ void i_rfString_Assign(RF_String* dest,void* source);
#define rfString_Assign(i_DESTINATION_,i_SOURCE_) i_rfLMS_WRAP2(void,i_rfString_Assign,i_DESTINATION_,i_SOURCE_)
#endif

// @memberof RF_String
// @brief Assigns the value of a unicode character to the string
//
// @notinherited{StringX}
// @param thisstr The string to assign to
// @param character The unicode character codepoint to assign to the String
// @return Returns @c true for succesfull assignment and @c false if the given @c character was not a valid unicode codepoint
i_DECLIMEX_ char rfString_Assign_char(RF_String* thisstr,uint32_t character);

// @}
/*-------------------------------------------------------------------------Methods to get rid of an RF_String-------------------------------------------------------------------------------*/
// @name Getting rid of an RF_String
// @{

// @memberof RF_String
// @cppignore
// @brief Deletes a string object and also frees its pointer.
//
// @notinherited{StringX}
// It is an error to give a NULL(0x0) string for deleting. Will most probably lead to a segmentation fault
// Use it for strings made with _Create
// @param s The string for deletion
i_DECLIMEX_ void rfString_Destroy(RF_String* s);
// @memberof RF_String
// @cppignore
// @brief Deletes a string object only, not its memory.
//
// @notinherited{StringX}
// It is an error to give a NULL(0x0) string for deleting. Will most probably lead to a segmentation fault
// Use it for strings made with _Init
// @param s The string for deletion
i_DECLIMEX_ void rfString_Deinit(RF_String* s);


// @}
/*------------------------------------------------------------------------ RF_String unicode conversion-------------------------------------------------------------------------------*/
// @name Unicode Conversion Functions
// @{

// @memberof RF_String
// @brief Returns the strings contents as a UTF-8 buffer
//
// @isinherited{StringX}
// This is just a macro wrapper of @ref rfString_ToStr() and exists here
// just so that users can guess function names for all unicode encodings.
//
//  Note that just like in @ref rfString_ToStr() this is just a pointer to
// the String's internal UTF8 buffer and as such should be read only. If there
// is a need to do anything other than that copy the buffer.
// @param s The string in question
// @return Returns a pointer to the String's internal UTF-8 uffer
#if defined(RF_IAMHERE_FOR_DOXYGEN)
i_DECLIMEX_ const char* rfString_ToUTF8(RF_String* s);
#else
#define rfString_ToUTF8(i_STRING_)  rfString_ToCstr(i_STRING_)
#endif

// @memberof RF_String
// @brief Returns the strings contents as a UTF-16 buffer
//
// @isinherited{StringX}
// This function allocates a UTF-16 buffer in which the string's
// UTF-8 contents are encoded as UTF-16. The endianess of the buffer
// is that of the system. The returned buffer needs to be freed by the user
// later.
// @param[in] s The string in question
// @param[out] length Give a reference to a uint32_t in this argument to receive the length of
// the returned UTF-16 buffer in 16-bit words
// @return Returns an allocated UTF-16 buffer. Needs to be freed by the user later.
i_DECLIMEX_ uint16_t* rfString_ToUTF16(RF_String* s,uint32_t* length);

// @memberof RF_String
// @brief Returns the strings contents as a UTF-32 buffer
//
// @isinherited{StringX}
// This function allocates a UTF-32 buffer in which the string's
// UTF-8 contents are encoded as UTF-32. The endianess of the buffer
// is that of the system. The returned buffer needs to be freed by the user
// later.
// @param[in] s The string in question
// @param[out] length Give a reference to a uint32_t in this argument to receive the length
// of the returned UTF-32 buffer in codepoints. (32-bit) words
// @return Returns an allocated UTF-16 buffer. Needs to be freed by the user later.
i_DECLIMEX_ uint32_t* rfString_ToUTF32(RF_String* s,uint32_t*length);

// @}
/*------------------------------------------------------------------------ RF_String retrieval functions-------------------------------------------------------------------------------*/
// @name String Retrieval
// @{



    //-- String iteration --/ /

// Two macros to accomplish iteration of an RF_String from any given character going forwards. This macro should be used with its end pair.
// We take advantage of the fact that an RF_String is always guaranteed to contain a valid UTF-8 sequence and thus no checks are performed.
/**
** @memberof RF_String
** @cppignore
** @brief Starts an RF_String forward iteration scope.
**
** @isinherited{StringX}
** Use this macro to iterate every character inside an RF_String or RF_StringX\n
** Must be used with its pair macro #rfString_Iterate_End.\n
** As an example consider this code that iterates every character of a string from the start to finish
** @code
** uint32_t i = 0;
** uint32_t charValue;
** RF_String foo;rfString_Init(&foo,"I am a String");
** rfString_Iterate_Start(&foo,i,charValue)
**      //for every character in the string,let's print it
**      printf("Character at index %d is %c\n",i,charValue);
** rfString_Iterate_End(i)
** @endcode
** @param[in] string_ The string to iterate. Must be a pointer to string
** @param[in,out] startCharacterPos_ Here give an uint32_t which will be the character position from which to start the iteration. In each iteration this will hold the character index. If the given position is out of bounds then the iteration does not happen
** @param[in,out] characterUnicodeValue_ Here pass an uint32_t which in each iteration will hold the unicode code point of the character at position startCharacterPos_
**/
#define rfString_Iterate_Start(string_,startCharacterPos_,characterUnicodeValue_)     {\
            /* b index sec is the byte index and j the character index*/\
            uint32_t byteIndex_ = 0;uint32_t j_=0;\
            /*iterate until we find the character position requested and its equivalent byte position*/\
            while(j_!=startCharacterPos_)\
            {\
                if( rfUTF8_IsContinuationByte( (string_)->bytes[byteIndex_]) ==false)\
                {\
                    j_++;\
                }\
                byteIndex_++;\
            }\
            /*now start the requested iteration*/\
            while( (string_)->bytes[byteIndex_]!='\0')\
            {\
                /*if it's a character*/\
                if( rfUTF8_IsContinuationByte( (string_)->bytes[byteIndex_]) ==false)\
                {/*Give the character value to the user*/\
                    characterUnicodeValue_ = rfString_BytePosToCodePoint( (string_),byteIndex_);

// @memberof RF_String
// @cppignore
// @brief Ends an RF_String/RF_StringX forward iteration scope.
//
// @isinherited{StringX}
// Look at #rfString_Iterate_Start for an example usage
// @param[in,out] startCharacterPos_ Here give the uint32_t given to #rfString_Iterate_Start
#define rfString_Iterate_End(startCharacterPos_)  startCharacterPos_++;}byteIndex_++;}}

//Two macros to accomplish iteration of an RF_String from any given character going backwards. This macro should be used with its end pair.
// We take advantage of the fact that an RF_String is always guaranteed to contain a valid UTF-8 sequence and thus no checks are performed.

/**
** @memberof RF_String
** @cppignore
** @brief Starts an RF_String backward iteration scope.
**
** @isinherited{StringX}
** Use this macro to iterate every character inside an RF_String or RF_StringX going backwards\n
** Must be used with its pair macro #rfString_IterateB_End.\n
**
** As an example consider this code that iterates every character of a string from the start to finish
** @code
** uint32_t charValue;
** RF_String foo;rfString_Init(&foo,"I am a String");
** uint32_t i = rfString_Length(&foo);
** rfString_IterateB_Start(&foo,i,charValue)
**      //for every character in the string,let's print it
**      printf("Character at index %d is %c\n",i,charValue);
** rfString_IterateB_End(i)
** @endcode
** @param[in] string_ The string to iterate. Must be a pointer to string
** @param[in,out] characterPos_ Here give an uint32_t which will be the character position from which to start the iteration. In each iteration this will hold the character index. If the given position is out of bounds then the iteration does not happen
** @param[in,out] characterUnicodeValue_ Here pass an uint32_t which in each iteration will hold the unicode code point of the character at position characterPos_
**/
#define rfString_IterateB_Start(string_,characterPos_,characterUnicodeValue_)     {\
            /* b index is the byte index and j the character index*/\
            uint32_t b_index_ = 0;uint32_t j_=0;\
            /* c index sec is another signed copy of the character index (and is int64_t so that it can cater for any situation). Reason is cause going backwards we gotta have -1 too */\
            int64_t c_index_ = characterPos_;\
            /*iterate until we find the character position requested and its equivalent byte position*/\
            while(j_!=characterPos_)\
            {\
                if( rfUTF8_IsContinuationByte( (string_)->bytes[b_index_]) ==false)\
                {\
                    j_++;\
                }\
                b_index_++;\
            }\
            /*now start the requested iteration - notice that the end condition is to reach the first character position*/\
            while(c_index_!=-1)\
            {\
                /*if it's a character*/\
                if( rfUTF8_IsContinuationByte( (string_)->bytes[b_index_]) ==false)\
                {/*Give the character value to the user*/\
                    characterUnicodeValue_ = rfString_BytePosToCodePoint( (string_),b_index_);

// @memberof RF_String
// @cppignore
// @brief Ends an RF_String/RF_StringX backward iteration scope.
//
// @isinherited{StringX}
// Look at #rfString_IterateB_Start for an example usage
// @param[in,out] characterPos_ Here give the uint32_t given to #rfString_IterateB_Start
#define rfString_IterateB_End(characterPos_)  c_index_-- ;characterPos_--;}b_index_--;}}

// @memberof RF_String
// @brief Finds the length of the string in characters.
//
// @isinherited{StringX}
// @param s The string whose number of characters to find. @inhtype{String,StringX}
// @return Returns the length of the sting in characters, not including the null termintion character
i_DECLIMEX_ uint32_t rfString_Length(void * s);

// @memberof RF_String
// @brief Retrieves the unicode code point of the parameter character.
//
// @isinherited{StringX}
// If the character position is out of bounds RF_STRING_INDEX_OUT_OF_BOUNDS is returned.
// @param thisstr The string whose character code point we need. @inhtype{String,StringX}
// @param c The character index whose unicode code point to return. Must be a positive (including zero) integer.
// @return Returns the code point as an uint32_t or the value RF_STRING_INDEX_OUT_OF_BOUNDS if the requested character index is out of bounds
i_DECLIMEX_ uint32_t rfString_GetChar(void* thisstr,uint32_t c);

// @internal
// @memberof RF_String
// @cppignore
// @brief Retrieves the unicode code point of the parameter bytepos of the string.
//
// @isinherited{StringX}
// This is an internal function, there is no need to use it. The reason it is exposed here is that it is utilized in the iteration macros.
// @warning DO NOT use this function unless you know what you are doing
// @param thisstr The string whose byte position code point we need. @inhtype{String,StringX}
// @param bytepos The byte position of the string from where to get the code point.
// @warning If this is out of bounds then nothing can detect it and at best it will cause a SEG FAULT.
//                 Moreover no check to see if this is not a continutation byte is made. All the checks must have been made before calling the function.
// @return Returns the code point of the byte position as an uint32_t
// @endinternal
i_DECLIMEX_ uint32_t rfString_BytePosToCodePoint(void* thisstr,uint32_t bytepos);

// @internal
// @memberof RF_String
// @cppignore
// @brief Retrieves character position of a byte position
//
// @isinherited{StringX}
// This is an internal function, there is no need to use it. It attempts to retrieve character position from a byte position. If the byte
// position is a continutation byte and does not constitute the start of a character then depending on the option the function will find
// either the next character or the previous character position from this byte position
//
// @warning DO NOT use this function unless you know what you are doing
// @param thisstr The string whose byte position code point we need. @inhtype{String,StringX}
// @param bytepos The byte position of the string from where to get the character position
// @param before A boolean flag denoting the behaviour in case this byte position is a continutation byte. If @c before is true then
// the function will retrieve the first character position before the byte. If it is false, it will retrieve the first character position
// after the continuation byte.
// @endinternal
i_DECLIMEX_ uint32_t rfString_BytePosToCharPos(void* thisstr,uint32_t bytepos,char before);

// @memberof RF_String
// @opcmpeq
// @brief Compares two Strings and returns true if they are equal and false otherwise
//
// @isinherited{StringX}
// A macro comparing two String and returning true if they are equal and false otherwise. Use it to compare ONLY Strings here not string literals (c strings)
// If you need to compare a String with a string literal (c string) use #rfString_Equal_s
// @lmsFunction
// @param s1 The first string to compare @inhtype{String,StringX} @tmpSTR
// @param s2 The second string to compare @inhtype{String,StringX} @tmpSTR
// @return True in case the strings are equal and false otherwise
#if defined(RF_IAMHERE_FOR_DOXYGEN)
i_DECLIMEX_ char rfString_Equal(void* s1,void* s2);
#else
i_DECLIMEX_ char i_rfString_Equal(void* s1,void* s2);
#define rfString_Equal(i_STRING1_,i_STRING2_)   i_rfLMSX_WRAP2(char,i_rfString_Equal,i_STRING1_,i_STRING2_)
#endif


// @memberof RF_String
// @brief Finds if a substring exists inside another string.
//
// @isinherited{StringX}
// Finds the existence of String sstr inside this string with the given options. You have the
// option to either match case or perform a case-insensitive search. In addition you can search
// for the exact string and not it just being a part of another string.
// @lmsFunction
// @param thisstr This string we want to search in @inhtype{String,StringX}
// @param sstr The substring string we want to search for @inhtype{String,StringX} @tmpSTR
// @param options \rfoptional{0}. Bitflag options denoting some options for the search.Can have values:
// + @c RF_CASE_IGNORE: If you want the found substring to ignore the case and returns success for any occurence of the string in any case.
//     Default search option is to @b match the case. For now this works only for characters of the english language.
// + @c RF_MATCH_WORD: If you want the found substring to be exact. For example an exact search for @e "HELLO" in the string
//     @e "HELLOWORLD" would return a failure. Default search is to return any found substring.
// @return Returns the character position of the found substring or RF_FAILURE for not found
#if defined(RF_IAMHERE_FOR_DOXYGEN)
i_DECLIMEX_ int32_t rfString_Find(const void* thisstr,const void* sstr,const char options);
#else
i_DECLIMEX_ int32_t i_rfString_Find(const void* thisstr,const void* sstr,const char* options);
    #ifndef RF_OPTION_DEFAULT_ARGUMENTS
        #define rfString_Find(i_THISSTR_,i_SEARCHSTR_,i_OPTIONS_)  i_rfLMS_WRAP3(int32_t,i_rfString_Find,i_THISSTR_,i_SEARCHSTR_,i_RFI8_(i_OPTIONS_))
    #else
        #define rfString_Find(...) RF_SELECT_FUNC_IF_NARGGT(i_NPSELECT_RF_STRING_FIND,3,__VA_ARGS__)
        #define i_NPSELECT_RF_STRING_FIND1(...) RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_Find() accepts from 2 to 3 arguments\"")
        #define i_NPSELECT_RF_STRING_FIND0(...) RF_SELECT_FUNC(i_SELECT_RF_STRING_FIND,__VA_ARGS__)
        #define i_SELECT_RF_STRING_FIND2(i_THISSTR_,i_SEARCHSTR_) i_rfLMS_WRAP3(int32_t,i_rfString_Find,i_THISSTR_,i_SEARCHSTR_,i_RFI8_(0))
        #define i_SELECT_RF_STRING_FIND3(i_THISSTR_,i_SEARCHSTR_,i_OPTIONS_) i_rfLMS_WRAP3(int32_t,i_rfString_Find,i_THISSTR_,i_SEARCHSTR_,i_RFI8_(i_OPTIONS_))
        #define i_SELECT_RF_STRING_FIND1(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Find() accepts from 2 to 3 arguments\"")
        #define i_SELECT_RF_STRING_FIND0(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Find() accepts from 2 to 3 arguments\"")
    #endif
#endif


// @memberof RF_String
// @brief Returns the integer value of a String
//
// @isinherited{StringX}
// The parameter string must contains only numbers. If it contains anything else the function fails.
// @param thisstr The string whose integer value to return. @inhtype{String,StringX}
// @param[out] v A refence to an integer that will return the float value
// @return Returns true in case of succesfull conversion or false if no integer was represented by the string
i_DECLIMEX_ char rfString_ToInt(void* thisstr,int32_t* v);

// @memberof RF_String
// @brief Returns the double value of a String
//
// @isinherited{StringX}
// The parameter string must contain only a number. If it contains anything else the function fails.
// @param thisstr The string whose floating point value to return. @inhtype{String,StringX}
// @param[out] f A refence to a double that will return the floating point number value
// @return Returns RF_SUCCESS in case of succesfull conversion or error if there was failure. Possible errors are:
// + @c RE_STRING_TOFLOAT: There was a conversion error. The string probably does not represent a float
// + @c RE_STRING_TOFLOAT_RANGE: The represented floating point number is of a range bigger than what can be
// represented by the system
// + @c RE_STRING_TOFLOAT_UNDERFLOW: Representing the string's floating point number in a double would cause underflow
i_DECLIMEX_ int rfString_ToDouble(void* thisstr,double* f);

// @memberof RF_String
// @brief Returns a cstring version of the string
//
// @isinherited{StringX}
// Remember that this is just a pointer to the string data. It can't be modified. memcpy it if you need a copy of it.
// @param str The string whose cstring to return. @inhtype{String,StringX}
// @return Returns a c string version of the string
i_DECLIMEX_ const char* rfString_ToCstr(const void* str);


// @memberof RF_String
// @cppignore
// @brief Creates and returns an allocated copy of the given string
//
// @isinherited{StringX}
// @note The Returned Substring needs to be freed by the user. BEWARE when assigning to a string using this function since if any previous string exists there IS NOT getting freed. You have to free it explicitly
// @param src The string to copy from. @inhtype{String,StringX}
// @return Returns a string copied from the previous one or null if the original string was null
i_DECLIMEX_ RF_String* rfString_Copy_OUT(void* src);
// @memberof RF_String
// @cppignore
// @brief Copies all the contents of a string to another
//
// @isinherited{StringX}
// @param dst The string to copy in.
// @param src The string to copy from. @inhtype{String,StringX}
// If the value is bigger than the maximum number of characters then still all characters are copied.
i_DECLIMEX_ void rfString_Copy_IN(RF_String* dst,void* src);
// @memberof RF_String
// @brief Copies a certain number of characters from a string
//
// @isinherited{StringX}
// Copies @c n characters from @c src String into the destination @c dst string.
// @param dst The string to copy in
// @param src The string to copy from. @inhtype{String,StringX}
// @param n The number of characters to copy from the @c src string
// If the value is bigger than the maximum number of characters then still all characters are copied.
i_DECLIMEX_ void rfString_Copy_chars(RF_String* dst,void* src,uint32_t n);


// @memberof RF_String
// @brief Applies a limited version of sscanf after the specified substring
//
// @isinherited{StringX}
// @lmsFunction
// @param thisstr The current string. @inhtype{String,StringX}
// @param afterstr The substring after which to apply sscanf. @inhtype{String,StringX} @tmpSTR
// @param format The tokens parameter which give the format of scanf
// @param var A void* to pass in any variable we need to get a value
// @return Returns true if a value was read and false otherwise, substring not being found in the string or sscanf unable to read into the variable
#if defined(RF_IAMHERE_FOR_DOXYGEN)
    i_DECLIMEX_ char rfString_ScanfAfter(void* thisstr,void* afterstr,const char* format,void* var);
#else
    i_DECLIMEX_ char i_rfString_ScanfAfter(void* thisstr,void* afterstr,const char* format,void* var);
    #define rfString_ScanfAfter(i_THISSTR_,i_AFTERSTR_,i_FORMAT_,i_VAR_) i_rfLMSX_WRAP4(char,i_rfString_ScanfAfter,i_THISSTR_,i_AFTERSTR_,i_FORMAT_,i_VAR_)
#endif

// @memberof RF_String
// @brief Counts how many times a substring occurs inside the string.
//
// @isinherited{StringX}
// @lmsFunction
// @param thisstr The string inside which to count. @inhtype{String,StringX}
// @param sstr The substring for which to search. @inhtype{String,StringX} @tmpSTR
// @param options \rfoptional{0}. Bitflag options denoting some options for the search. Give 0 for the defaults.Can have values:
// + @c RF_CASE_IGNORE: If you want the found substring to ignore the case and returns success for any occurence of the string in any case.
//     Default search option is to @b match the case. For now this works only for characters of the english language.
// + @c RF_MATCH_WORD: If you want the found substring to be exact. For example an exact search for @e "HELLO" in the string
//     @e "HELLOWORLD" would return a failure. Default search is to return any found substring.
// @return Returns the number of times cstr exists inside the string (0 is returned in case it's not found at all
#if defined(RF_IAMHERE_FOR_DOXYGEN)
i_DECLIMEX_ int32_t rfString_Count(void* thisstr,void* sstr,const char options);
#else
i_DECLIMEX_ int32_t i_rfString_Count(void* thisstr,void* sstr,const char* options);
    #ifdef RF_OPTION_DEFAULT_ARGUMENTS
        #define rfString_Count(...) RF_SELECT_FUNC_IF_NARGGT(i_NPSELECT_RF_STRING_COUNT,3,__VA_ARGS__)
        #define i_NPSELECT_RF_STRING_COUNT1(...) RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_Count() accepts from 2 to 3 arguments\"")
        #define i_NPSELECT_RF_STRING_COUNT0(...) RF_SELECT_FUNC(i_SELECT_RF_STRING_COUNT,__VA_ARGS__)
        #define i_SELECT_RF_STRING_COUNT2(i_THISSTR_,i_SEARCHSTR_)  i_rfLMSX_WRAP3(int32_t,i_rfString_Count,i_THISSTR_,i_SEARCHSTR_,i_RFI8_(0))
        #define i_SELECT_RF_STRING_COUNT3(i_THISSTR_,i_SEARCHSTR_,i_OPTIONS_) i_rfLMS_WRAP3(int32_t,i_rfString_Count,i_THISSTR_,i_SEARCHSTR_,i_RFI8_(i_OPTIONS_))
        #define i_SELECT_RF_STRING_COUNT1(...)  RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_Count() accepts from 2 to 3 arguments\"")
        #define i_SELECT_RF_STRING_COUNT0(...)  RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_Count() accepts from 2 to 3 arguments\"")
    #else
        #define rfString_Count(i_THISSTR_,i_SEARCHSTR_,i_OPTIONS_) i_rfLMSX_WRAP3(int32_t,i_rfString_Count,i_THISSTR_,i_SEARCHSTR_,i_RFI8_(i_OPTIONS_))
    #endif
#endif


// @memberof RF_String
// @brief Tokenizes the given string
//
// @isinherited{StringX}
// Separates it into @c tokensN depending on how many substrings can be created from the @c sep separatior and stores them
// into the Array of RF_String* that should be passed to the function. The array gets initialized inside the function and
// <b>has to be freed explicitly</b> later by thg user. Also each String inside the array has to be Deinitialized too.
// Here is an example usage:
// @snippet Strings/tokenize.cpp Tokenize_C
// @cppsnippet Tokenize_CPP
// @param[in] thisstr The string to tokenize. @inhtype{String,StringX}
// @param[in] sep A string literal that will be used as a separator to tokenize the given string
// @param[out] tokensN The number of tokens that got created
// @param[out] tokens Pass a pointer to an array of RF_Strings. @keepptr
// @return Returns true in success and false in case the the separating character has not been found
// @internal @cppcode
// char String::Tokenize(char* sep,uint32_t* tokensN, String*** tokens)
// {
//     RF_String* t;
//     uint32_t i;
//     if(rfString_Tokenize(this->i_StringCHandle,sep,tokensN,&t)==false)
//         return false;
//
//     *tokens = (String**) malloc(sizeof(String*)* (*tokensN));
//     for(i=0;i<(*tokensN);i++)
//     {
//        (*tokens)[i] = new String((RF_String*)&t[i]);
//     }
//     return true;
// }
// @endcpp @endinternal
i_DECLIMEX_ char rfString_Tokenize(void* thisstr,char* sep,uint32_t* tokensN,RF_String** tokens);


// @memberof RF_String
// @brief Initializes the first substring, between two given strings
//
// @isinherited{StringX}
// Initializes the given string as the first substring existing between the left and right parameter substrings
// @lmsFunction
// @note The Returned Substring needs to be deinitialized by the user.
// @param thisstr This current string. @inhtype{String,StringX}
// @param[in] lstr The left substring that will define the new substring. @inhtype{String,StringX} @tmpSTR
// @param[in] rstr The right substring that will define the new substring. @inhtype{String,StringX} @tmpSTR
// @param[out] result The resulting substring.
// @param options \rfoptional{0} Bitflag options denoting the method with which to search for the substring literals inside the string. Give 0 for the defaults.
// Can have values:
// + @c RF_CASE_IGNORE: If you want to search for any occurence of the substring disregarding CAPS or not.
//     Default search option is to @b match the case. For now this works only for characters of the english language.
// + @c RF_MATCH_WORD: If you to find only exact matches of the substring. For example an exact search for @e "HELLO" in the string
//     @e "HELLOWORLD" would find nothing. Default is with this flag off.
// @return Returns true if the substring is found and initialized and false otherwise
#if defined(RF_IAMHERE_FOR_DOXYGEN)
i_DECLIMEX_ char rfString_Between(void* thisstr,void* lstr,void* rstr,RF_String* result,const char options);
#else
i_DECLIMEX_ char i_rfString_Between(void* thisstr,void* lstr,void* rstr,RF_String* result,const char* options);
    #ifdef RF_OPTION_DEFAULT_ARGUMENTS
        #define rfString_Between(...) RF_SELECT_FUNC_IF_NARGGT(i_NPSELECT_RF_STRING_BETWEEN,5,__VA_ARGS__)
        #define i_NPSELECT_RF_STRING_BETWEEN1(...)  RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_Between() accepts from 4 to 5 arguments\"")
        #define i_NPSELECT_RF_STRING_BETWEEN0(...)  RF_SELECT_FUNC(i_SELECT_RF_STRING_BETWEEN,__VA_ARGS__)
        #define i_SELECT_RF_STRING_BETWEEN4(i_THISSTR_,i_LEFTSTR_,i_RIGHTSTR_,i_RESULT_) \
            i_rfLMSX_WRAP5(char,i_rfString_Between,i_THISSTR_,i_LEFTSTR_,i_RIGHTSTR_,i_RESULT_,i_RFI8_(0))
        #define i_SELECT_RF_STRING_BETWEEN5(i_THISSTR_,i_LEFTSTR_,i_RIGHTSTR_,i_RESULT_,i_OPTIONS_) \
            i_rfLMSX_WRAP5(char,i_rfString_Between,i_THISSTR_,i_LEFTSTR_,i_RIGHTSTR_,i_RESULT_,i_RFI8_(i_OPTIONS_))
        #define i_SELECT_RF_STRING_BETWEEN3(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Between() accepts from 4 to 5 arguments\"")
        #define i_SELECT_RF_STRING_BETWEEN2(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Between() accepts from 4 to 5 arguments\"")
        #define i_SELECT_RF_STRING_BETWEEN1(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Between() accepts from 4 to 5 arguments\"")
        #define i_SELECT_RF_STRING_BETWEEN0(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Between() accepts from 4 to 5 arguments\"")
    #else
        #define rfString_Between(i_THISSTR_,i_LEFTSTR_,i_RIGHTSTR_,i_RESULT_,i_OPTIONS_) \
            i_rfLMSX_WRAP5(char,i_rfString_Between,i_THISSTR_,i_LEFTSTR_,i_RIGHTSTR_,i_RESULT_,i_RFI8_(i_OPTIONS_))
    #endif
#endif


// @memberof RF_String
// @brief Initializes the given string as the substring from the start until any of the given Strings are found
//
// @isinherited{StringX}
// The parameters that have to be given as variable argument <b>must</b> be of type RF_String* or RF_StringX* or even
// string initialized with the temporary string macro
// @rfNoDefArgsWarn1
// @warning if the library has been compiled with @c DEFAULT_ARGUMENTS off then arguments @c options and @c parN are actually pointers
// to @c char and @c unsigned char respectively
// @lmsFunction
// @param thisstr The string to operate in. @inhtype{String,StringX}
// @param result The resulting substring.
// @param options Bitflag options denoting the method with which to search for the substring literals inside the string. Give 0 for the defaults.
//  Can have values:
// + @c RF_CASE_IGNORE: If you want to search for any occurence of the substring disregarding CAPS or not.
//     Default search option is to @b match the case. For now this works only for characters of the english language.
// + @c RF_MATCH_WORD: If you to find only exact matches of the substring. For example an exact search for @e "HELLO" in the string
//     @e "HELLOWORLD" would find nothing. Default is with this flag off.
// @param parN The number of strings to search for
// @param ... The strings to search for. @inhtype{String,StringX} @tmpSTR
// @extraVarArgLim
// @return Returns true if the substring was initialized and false if none of the parameters were found or an invalid UTF-8 sequence was given. In the latter case an error is also logged.
#ifdef RF_IAMHERE_FOR_DOXYGEN
i_DECLIMEX_ char rfString_Beforev(void* thisstr,RF_String* result,const char options,const unsigned char parN, ...);
#endif
#ifdef RF_OPTION_DEFAULT_ARGUMENTS
    i_DECLIMEX_ char i_rfString_Beforev(void* thisstr,RF_String* result,const char* options,const unsigned char* parN, ...);
    #define rfString_Beforev(...)  RF_SELECT_FUNC_IF_NARGGT(i_NPSELECT_RF_STRING_BEFOREV,4,__VA_ARGS__)
    #define i_NPSELECT_RF_STRING_BEFOREV1(...)  RF_SELECT_FUNC_IF_NARGGT2(i_LIMSELECT_RF_STRING_BEFOREV,18,__VA_ARGS__)
    #define i_NPSELECT_RF_STRING_BEFOREV0(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Beforev() needs to receive more than 4 arguments\"")
    #define i_LIMSELECT_RF_STRING_BEFOREV1(...) RF_COMPILE_ERROR("message \"Extra Arguments Limit Reached: Function rfString_Beforev() received more extra arguments than the limit permits\"")
    #define i_LIMSELECT_RF_STRING_BEFOREV0(...) RF_SELECT_FUNC(i_SELECT_RF_STRING_BEFOREV,__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV5(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...)    \
        i_rfLMSX_WRAP5(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV6(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP6(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV7(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP7(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV8(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP8(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV9(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP9(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV10(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP10(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV11(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP11(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV12(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP12(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV13(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP13(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV14(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP14(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV15(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP15(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV16(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP16(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV17(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP17(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_BEFOREV18(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP18(char,i_rfString_Beforev,i_ARG1_,i_ARG2_,i_RFI8_(i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
#else
   i_DECLIMEX_ char rfString_Beforev(void* thisstr,RF_String* result,const char* options,const unsigned char* parN, ...);
#endif

// @memberof RF_String
// @brief Initializes the given string as the substring from the start until the given string is found
//
// @isinherited{StringX}
// @lmsFunction
// @param thisstr The string to operate in. @inhtype{String,StringX}
// @param sstr The substring that we want to find inside the string @inhtype{String,StringX} @tmpSTR
// @param result The resulting substring.
// @param options \rfoptional{0} Bitflag options denoting the method with which to search for the substring literals inside the string. Give 0 for the defaults.
// Can have values:
// + @c RF_CASE_IGNORE: If you want to search for any occurence of the substring disregarding CAPS or not.
//     Default search option is to @b match the case. For now this works only for characters of the english language.
// + @c RF_MATCH_WORD: If you to find only exact matches of the substring. For example an exact search for @e "HELLO" in the string
//     @e "HELLOWORLD" would find nothing. Default is with this flag off.
// @return Returns true if the substring was initialized and false if none of the parameters were found or an invalid UTF-8 sequence was given. In the latter case an error is also logged.
#if defined(RF_IAMHERE_FOR_DOXYGEN)
i_DECLIMEX_ char rfString_Before(void* thisstr,void* sstr,RF_String* result,const char options);
#else
i_DECLIMEX_ char i_rfString_Before(void* thisstr,void* sstr,RF_String* result,const char* options);
    #ifdef RF_OPTION_DEFAULT_ARGUMENTS
        #define rfString_Before(...) RF_SELECT_FUNC_IF_NARGGT(i_NPSELECT_RF_STRING_BEFORE,4,__VA_ARGS__)
        #define i_NPSELECT_RF_STRING_BEFORE1(...)  RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_Before() accepts from 3 to 4 arguments\"")
        #define i_NPSELECT_RF_STRING_BEFORE0(...)  RF_SELECT_FUNC(i_SELECT_RF_STRING_BEFORE,__VA_ARGS__)
        #define i_SELECT_RF_STRING_BEFORE3(i_THISSTR_,i_SEARCHSTR_,i_RESULT_) i_rfLMSX_WRAP4(char,i_rfString_Before,i_THISSTR_,i_SEARCHSTR_,i_RESULT_,i_RFI8_(0))
        #define i_SELECT_RF_STRING_BEFORE4(i_THISSTR_,i_SEARCHSTR_,i_RESULT_,i_OPTIONS_) i_rfLMSX_WRAP4(char,i_rfString_Before,i_THISSTR_,i_SEARCHSTR_,i_RESULT_,i_RFI8_(i_OPTIONS_))
        #define i_SELECT_RF_STRING_BEFORE2(...) RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_Before() accepts from 3 to 4 arguments\"")
        #define i_SELECT_RF_STRING_BEFORE1(...) RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_Before() accepts from 3 to 4 arguments\"")
        #define i_SELECT_RF_STRING_BEFORE0(...) RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_Before() accepts from 3 to 4 arguments\"")
    #else
        #define rfString_Before(i_THISSTR_,i_SEARCHSTR_,i_RESULT_,i_OPTIONS_) i_rfLMSX_WRAP4(char,i_rfString_Before,i_THISSTR_,i_SEARCHSTR_,i_RESULT_,i_RFI8_(i_OPTIONS_))
    #endif
#endif

// @memberof RF_String
// @brief Initialize a string after a given substring
//
// @isinherited{StringX}
// Initializes the given String with the substring located after (and not including) the after substring inside the parameter string. If the substring is not located the function returns false.
// @note The given String needs to be deinitialized by the user
// @lmsFunction
// @param[in] thisstr The parameter string from which the substring will be formed. @inhtype{String,StringX}
// @param[in] after The substring to search for inside the parameter string. @inhtype{String,StringX} @tmpSTR
// @param[out] out Pass a reference to a String inside which the substring of the original string after the @c after substring will be placed
// @param options \rfoptional{0} Bitflag options denoting the method with which to search for the substring literals inside the string. Give 0 for the defaults.
// Can have values:
// + @c RF_CASE_IGNORE: If you want to search for any occurence of the substring disregarding CAPS or not.
//     Default search option is to @b match the case. For now this works only for characters of the english language.
// + @c RF_MATCH_WORD: If you to find only exact matches of the substring. For example an exact search for @e "HELLO" in the string
//     @e "HELLOWORLD" would find nothing. Default is with this flag off.
// @return Returns true for success and false if the substring is not found in the parameter string.
#if defined(RF_IAMHERE_FOR_DOXYGEN)
i_DECLIMEX_ char rfString_After(void* thisstr,void* after,RF_String* out,const char options);
#else
i_DECLIMEX_ char i_rfString_After(void* thisstr,void* after,RF_String* out,const char* options);
    #ifdef RF_OPTION_DEFAULT_ARGUMENTS
        #define rfString_After(...) RF_SELECT_FUNC_IF_NARGGT(i_NPSELECT_RF_STRING_AFTER,4,__VA_ARGS__)
        #define i_NPSELECT_RF_STRING_AFTER1(...) RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_After() accepts from 3 to 4 arguments\"")
        #define i_NPSELECT_RF_STRING_AFTER0(...) RF_SELECT_FUNC(i_SELECT_RF_STRING_AFTER,__VA_ARGS__)
        #define i_SELECT_RF_STRING_AFTER3(i_THISSTR_,i_AFTERSTR_,i_OUTSTR_) i_rfLMSX_WRAP4(char,i_rfString_After,i_THISSTR_,i_AFTERSTR_,i_OUTSTR_,i_RFI8_(0))
        #define i_SELECT_RF_STRING_AFTER4(i_THISSTR_,i_AFTERSTR_,i_OUTSTR_,i_OPTIONS_) i_rfLMSX_WRAP4(char,i_rfString_After,i_THISSTR_,i_AFTERSTR_,i_OUTSTR_,i_RFI8_(i_OPTIONS_))
        #define i_SELECT_RF_STRING_AFTER2(...) RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_After() accepts from 3 to 4 arguments\"")
        #define i_SELECT_RF_STRING_AFTER1(...) RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_After() accepts from 3 to 4 arguments\"")
        #define i_SELECT_RF_STRING_AFTER0(...) RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_After() accepts from 3 to 4 arguments\"")
    #else
        #define rfString_After(i_THISSTR_,i_AFTERSTR_,i_OUTSTR_,i_OPTIONS_) i_rfLMSX_WRAP4(char,i_rfString_After,i_THISSTR_,i_AFTERSTR_,i_OUTSTR_,i_RFI8_(i_OPTIONS_))
    #endif
#endif

// @memberof RF_String
// @brief Initialize a string after the first of the given substrings found
//
// @isinherited{StringX}
// Initializes the given String with the substring located after (and not including) the after substring inside the parameter string. If the substring is not located the function returns false.
// The parameters that have to be given as variable argument <b>must</b> be of type RF_String* or RF_StringX* or even
// string initializes with the temporary string macro
// @rfNoDefArgsWarn1
// @warning if the library has been compiled with @c DEFAULT_ARGUMENTS off then arguments @c options and @c parN are actually pointers
// to @c char and @c unsigned char respectively
// @lmsFunction
// @param[in] thisstr The parameter string from which the substring will be formed. @inhtype{String,StringX}
// @param[out] out Pass a reference to a String inside which the substring of the original string
// after the found substring will be placed.
// @param options \rfoptional{0} Bitflag options denoting the method with which to search for the substring literals inside the string. Give 0 for the defaults.
// Can have values:
// + @c RF_CASE_IGNORE: If you want to search for any occurence of the substring disregarding CAPS or not.
//     Default search option is to @b match the case. For now this works only for characters of the english language.
// + @c RF_MATCH_WORD: If you to find only exact matches of the substring. For example an exact search for @e "HELLO" in the string
//     @e "HELLOWORLD" would find nothing. Default is with this flag off.
// @param parN The number of substrings to search for.
// @param ... The substrings to search for. @inhtype{String,StringX} @tmpSTR
// @extraVarArgLim
// @return Returns true for success and false if the substring is not found in the parameter string.
#ifdef RF_IAMHERE_FOR_DOXYGEN
i_DECLIMEX_ char rfString_Afterv(void* thisstr,RF_String* out,const char options,const unsigned char parN,...);
#endif
#ifdef RF_OPTION_DEFAULT_ARGUMENTS
i_DECLIMEX_ char i_rfString_Afterv(void* thisstr,RF_String* out,const char* options,const unsigned char* parN,...);
    #define rfString_Afterv(...)  RF_SELECT_FUNC_IF_NARGGT(i_NPSELECT_RF_STRING_AFTERV,4,__VA_ARGS__)
    #define i_NPSELECT_RF_STRING_AFTERV1(...)  RF_SELECT_FUNC_IF_NARGGT2(i_LIMSELECT_RF_STRING_AFTERV,18,__VA_ARGS__)
    #define i_NPSELECT_RF_STRING_AFTERV0(...) RF_COMPILE_ERROR("message \"Ileggal Arguments Number: Function rfString_Afterv() needs to receive more than 4 arguments\"")
    #define i_LIMSELECT_RF_STRING_AFTERV1(...) RF_COMPILE_ERROR("message \"Extra Arguments Limit Reached: Function rfString_Afterv() received more extra arguments than the limit permits\"")
    #define i_LIMSELECT_RF_STRING_AFTERV0(...) RF_SELECT_FUNC(i_SELECT_RF_STRING_AFTERV,__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV5(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP5(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV6(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP6(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV7(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP7(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV8(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP8(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV9(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP9(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV10(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP10(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV11(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP11(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV12(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP12(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV13(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP13(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV14(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP14(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV15(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP15(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV16(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP16(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV17(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP17(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
    #define i_SELECT_RF_STRING_AFTERV18(i_ARG1_,i_ARG2_,i_ARG3_,i_ARG4_,...) i_rfLMSX_WRAP18(char,i_rfString_Afterv,i_ARG1_,i_ARG2_,i_RFI8_((i_ARG3_),i_RFUI8_(i_ARG4_),__VA_ARGS__)
#else
    char rfString_Afterv(void* thisstr,RF_String* out,const char* options,const unsigned char* parN,...);
#endif



// @}
/*------------------------------------------------------------------------ RF_String manipulation functions-------------------------------------------------------------------------------*/
// @name String Manipulation
// @{

// @memberof RF_String
// @opadd
// @brief Appends a string to this one
//
// @notinherited{StringX}
// @lmsFunction
// @param thisstr The string to append to
// @param other The string to add to this string. @inhtype{String,StringX} @tmpSTR
#if defined(RF_IAMHERE_FOR_DOXYGEN)
i_DECLIMEX_ void rfString_Append(RF_String* thisstr,void* other);
#else
i_DECLIMEX_ void i_rfString_Append(RF_String* thisstr,void* other);
#define rfString_Append(i_THISSTR_,i_OTHERSTR_) i_rfLMS_WRAP2(void,i_rfString_Append,i_THISSTR_,i_OTHERSTR_)
#endif

// @memberof RF_String
// @opadd
// @brief Appends an integer to the string
//
// @notinherited{StringX}
// @param thisstr The string to append to
// @param i The integer to add
i_DECLIMEX_ void rfString_Append_i(RF_String* thisstr,const int32_t i);

// @memberof RF_String
// @opadd
// @brief Appends a float to the string
//
// @notinherited{StringX}
// @param thisstr The string to append to
// @param f The float to add
i_DECLIMEX_ void rfString_Append_f(RF_String* thisstr,const float f);

// @memberof RF_String
// @brief Prepends the parameter String to this string
//
// @notinherited{StringX}
// @lmsFunction
// @param thisstr The string to prepend to
// @param other The string to prepend to this string. @inhtype{String,StringX} @tmpSTR
#if defined(RF_IAMHERE_FOR_DOXYGEN)
i_DECLIMEX_ void rfString_Prepend(RF_String* thisstr,void* other);
#else
i_DECLIMEX_ void i_rfString_Prepend(RF_String* thisstr,void* other);
#define rfString_Prepend(i_THISSTR_,i_OTHERSTR_)    i_rfLMS_WRAP2(void,i_rfString_Prepend,i_THISSTR_,i_OTHERSTR_)
#endif

// @memberof RF_String
// @brief Removes occurences of a substring
//
// @isinherited{StringX}
// Removes a @c number of occurences of a substring from this string, that agree with the given parameters.
// <b>Does not</b> reallocate buffer size
// @lmsFunction
// @param thisstr This string we want to remove from. @inhtype{String,StringX}
// @param rstr The string whose occurences we need to locate and remove from the current string. @inhtype{String,StringX} @tmpSTR
// @param number \rfoptional{0}. The number of occurences to remove. Give @e 0 for all the occurences.
// If the given number is bigger than the actual number of occurences, still all occurences get replaced.
// @param options \rfoptional{0}. Bitflag options denoting some options for the search. Give 0 for the defaults.
// Can have values:
// + @c RF_CASE_IGNORE: If you want the found substring to ignore the case and returns success for any occurence of the string in any case.
//     Default search option is to @b match the case. For now this works only for characters of the english language.
// + @c RF_MATCH_WORD: If you want the found substring to be exact. For example an exact search for @e "HELLO" in the string
//     @e "HELLOWORLD" would return a failure. Default search is to return any found substring.
// @return Returns true in case of success, and false if the substring was not even found inside the string
#if defined(RF_IAMHERE_FOR_DOXYGEN)
i_DECLIMEX_ char rfString_Remove(void* thisstr,void* rstr,uint32_t number,const char options);
#else
    i_DECLIMEX_ char i_rfString_Remove(void* thisstr,void* rstr,uint32_t* number,const char* options);
    #ifdef RF_OPTION_DEFAULT_ARGUMENTS
        #define rfString_Remove(...) RF_SELECT_FUNC_IF_NARGGT(i_NPSELECT_RF_STRING_REMOVE,4,__VA_ARGS__)
        #define i_NPSELECT_RF_STRING_REMOVE1(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Remove() accepts from 2 to 4 arguments\"")
        #define i_NPSELECT_RF_STRING_REMOVE0(...) RF_SELECT_FUNC(i_SELECT_RF_STRING_REMOVE,__VA_ARGS__)
        #define i_SELECT_RF_STRING_REMOVE2(i_THISSTR_,i_REPSTR_) i_rfLMSX_WRAP4(char,i_rfString_Remove,i_THISSTR_,i_REPSTR_,i_RFUI32_(0),i_RFI8_(0))
        #define i_SELECT_RF_STRING_REMOVE3(i_THISSTR_,i_REPSTR_,i_NUMBER_) i_rfLMSX_WRAP4(char,i_rfString_Remove,i_THISSTR_,i_REPSTR_,i_RFUI32_(i_NUMBER_),i_RFI8_(0))
        #define i_SELECT_RF_STRING_REMOVE4(i_THISSTR_,i_REPSTR_,i_NUMBER_,i_OPTIONS_) \
            i_rfLMSX_WRAP4(char,i_rfString_Remove,i_THISSTR_,i_REPSTR_,i_RFUI32_(i_NUMBER_),i_RFI8_(i_OPTIONS_))
        #define i_SELECT_RF_STRING_REMOVE1(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Remove() accepts from 2 to 4 arguments\"")
        #define i_SELECT_RF_STRING_REMOVE0(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Remove() accepts from 2 to 4 arguments\"")
    #else
        #define rfString_Remove(i_THISSTR_,i_REPSTR_,i_NUMBER_,i_OPTIONS_) i_rfLMSX_WRAP4(char,i_rfString_Remove,i_THISSTR_,i_REPSTR_,i_RFUI32_(i_NUMBER_),i_RFI8_(i_OPTIONS_))
    #endif
#endif

// @memberof RF_String
// @brief Removes all of the characters of the string except those specified
//
// This string is scanned for the existence of each characters inside the given
// @c keepstr. Any character found there is kept in the original string. All other
// characters are removed.
// @isinherited{StringX}
// @lmsFunction
// @param thisstr The string to remove from @inhtype{String,StringX}
// @param keepstr A string all of whose characters will be kept in @c thisstr @inhtype{String,StringX} @tmpSTR
#ifdef RF_IAMHERE_FOR_DOXYGEN
i_DECLIMEX_ void rfString_KeepOnly(void* thisstr,void* keepstr);
#else
i_DECLIMEX_ void i_rfString_KeepOnly(void* thisstr,void* keepstr);
#define rfString_KeepOnly(i_THISSTR_,I_KEEPSTR_)    i_rfLMS_WRAP2(void,i_rfString_KeepOnly,i_THISSTR_,I_KEEPSTR_)
#endif

// @memberof RF_String
// @brief Removes the first n characters from the start of the string.
//
// @isinherited{StringX}
// @param thisstr The string to prune from. @inhtype{String,StringX}
// @param n The number of characters to remove. Must be a positive integer.
// @return True if n characters got removed and false if there are not enough characters to remove. (in which case the string becomes empty)
i_DECLIMEX_ char rfString_PruneStart(void* thisstr,uint32_t n);

// @memberof RF_String
// @brief Removes the last n characters from the end of the string
//
// @isinherited{StringX}
// @param thisstr The string to prune from. @inhtype{String,StringX}
// @param n The number of characters to remove. Must be a positive integer.
// @return True if n characters got removed and false if there are not enough characters to remove. (in which case the string becomes empty)
i_DECLIMEX_ char rfString_PruneEnd(void* thisstr,uint32_t n);

// @memberof RF_String
// @brief Removes characters from one point of the string to another going backwards
//
// @isinherited{StringX}
// Removes n characters from the position p (including the character at p) of the string counting backwards. If there is no space to do so, nothing is done and returns false.
// @param thisstr The string to prune from. @inhtype{String,StringX}
// @param p The position to remove the characters from. Must be a positive integer. Indexing starts from zero.
// @param n The number of characters to remove from the position and back.Must be a positive integer.
// @return Returns true in case of succesfull removal and false in any other case.
i_DECLIMEX_ char rfString_PruneMiddleB(void* thisstr,uint32_t p,uint32_t n);
// @memberof RF_String
// @brief Removes characters from one point of the string to another going forward
//
// @isinherited{StringX}
// Removes n characters from the position p (including the character at p) of the string counting forwards. If there is no space, nothing is done and returns false.
// @param thisstr The string to prune from. @inhtype{String,StringX}
// @param p The position to remove the characters from. Must be a positive integer. Indexing starts from zero.
// @param n The number of characters to remove from the position and on. Must be a positive integer.
// @return Returns true in case of succesfull removal and false in any other case.
i_DECLIMEX_ char rfString_PruneMiddleF(void* thisstr,uint32_t p,uint32_t n);


// @memberof RF_String
// @brief Replace all occurences of a String
//
// @notinherited{StringX}
// Replaces all of the specified sstr substring from the String with rstr and reallocates size, unless the new size is smaller
// @lmsFunction
// @param thisstr The string in which to do the replacing
// @param sstr The string to locate and replace from the current string. @inhtype{String,StringX} @tmpSTR
// @param rstr The string with which to replace it. @inhtype{String,StringX} @tmpSTR
// @param number \rfoptional{0}. The number of occurences to replace. Give @e 0 for all the occurences.
// If the given number is bigger than the actual number of occurences, still all occurences get replaced.
// @param options \rfoptional{0}. Bitflag options denoting some options for the string to replace. Give 0 for the defaults. Can have values:
// + @c RF_CASE_IGNORE: If you want to replace any occurence of the substring disregarding CAPS or not.
//     Default search option is to @b match the case. For now this works only for characters of the english language.
// + @c RF_MATCH_WORD: If you to replace only exact matches of the substring. For example an exact replace for @e "HELLO" in the string
//     @e "HELLOWORLD" would replace nothing. Default is with this flag off.
// @return Returns true in case of success, and false if the substring was not even found inside the string
#if defined(RF_IAMHERE_FOR_DOXYGEN)
i_DECLIMEX_ char rfString_Replace(RF_String* thisstr,void* sstr,void* rstr,const uint32_t number,const char options);
#else
i_DECLIMEX_ char i_rfString_Replace(RF_String* thisstr,void* sstr,void* rstr,const uint32_t* number,const char* options);
    #ifdef RF_OPTION_DEFAULT_ARGUMENTS
        #define rfString_Replace(...) RF_SELECT_FUNC_IF_NARGGT(i_NPSELECT_RF_STRING_REPLACE,5,__VA_ARGS__)
        #define i_NPSELECT_RF_STRING_REPLACE1(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Replace() accepts from 3 to 5 arguments\"")
        #define i_NPSELECT_RF_STRING_REPLACE0(...) RF_SELECT_FUNC(i_SELECT_RF_STRING_REPLACE,__VA_ARGS__)
        #define i_SELECT_RF_STRING_REPLACE3(i_THISSTR_,i_SEARCHSTR_,i_REPSTR_) i_rfLMSX_WRAP5(char,i_rfString_Replace,i_THISSTR_,i_SEARCHSTR_,i_REPSTR_,i_RFUI32_(0),i_RFI8_(0))
        #define i_SELECT_RF_STRING_REPLACE4(i_THISSTR_,i_SEARCHSTR_,i_REPSTR_,i_NUMBER_) \
            i_rfLMSX_WRAP5(char,i_rfString_Replace,i_THISSTR_,i_SEARCHSTR_,i_REPSTR_,i_RFUI32_(i_NUMBER_),i_RFI8_(0))
        #define i_SELECT_RF_STRING_REPLACE5(i_THISSTR_,i_SEARCHSTR_,i_REPSTR_,i_NUMBER_,i_OPTIONS_) \
            i_rfLMSX_WRAP5(char,i_rfString_Replace,i_THISSTR_,i_SEARCHSTR_,i_REPSTR_,i_RFUI32_(i_NUMBER_),i_RFI8_(i_OPTIONS_))
        #define i_SELECT_RF_STRING_REPLACE2(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Replace() accepts from 3 to 5 arguments\"")
        #define i_SELECT_RF_STRING_REPLACE1(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Replace() accepts from 3 to 5 arguments\"")
        #define i_SELECT_RF_STRING_REPLACE0(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Replace() accepts from 3 to 5 arguments\"")
    #else
        #define rfString_Replace(i_THISSTR_,i_SEARCHSTR_,i_REPSTR_,i_NUMBER_,i_OPTIONS_) \
            i_rfLMSX_WRAP5(char,i_rfString_Replace,i_THISSTR_,i_SEARCHSTR_,i_REPSTR_,i_RFUI32_(i_NUMBER_),i_RFI8_(i_OPTIONS_))
    #endif
#endif

// @memberof RF_String
// @brief Removes all characters of a substring only from the start of the String
//
// @isinherited{StringX}
// Searches for and removes each individual character inside the @c sub substring from the
// given String @c thisstr starting from the beginning of the String and until it finds any other character
// @lmsFunction
// @param thisstr The string to search in. @inhtype{String,StringX}
// @param sub The substring to search for. @inhtype{String,StringX} @tmpSTR
// @return Returns true for success and false if none of @c sub characters were found inside the given String
#if defined(RF_IAMHERE_FOR_DOXYGEN)
    i_DECLIMEX_ char rfString_StripStart(void* thisstr,void* sub);
#else
    i_DECLIMEX_ char i_rfString_StripStart(void* thisstr,void* sub);
    #define rfString_StripStart(i_THISSTR_,i_SUBSTR_) i_rfLMSX_WRAP2(char,i_rfString_StripStart,i_THISSTR_,i_SUBSTR_)
#endif
// @memberof RF_String
// @brief Removes all characters of a substring starting from the end of the String
//
// @isinherited{StringX}
// Searches for and removes each individual character inside the @c sub substring from the
// given String @c thisstr starting from the end of the String and until it finds any other character
// @lmsFunction
// @param thisstr The string to search in. @inhtype{String,StringX}
// @param sub The substring to search for. @inhtype{String,StringX} @tmpSTR
// @return Returns true for success and false if none of @c sub characters were found inside the given String
#if defined(RF_IAMHERE_FOR_DOXYGEN)
    i_DECLIMEX_ char rfString_StripEnd(void* thisstr,void* sub);
#else
    i_DECLIMEX_ char i_rfString_StripEnd(void* thisstr,void* sub);
    #define rfString_StripEnd(i_THISSTR_,i_SUBSTR_) i_rfLMSX_WRAP2(char,i_rfString_StripEnd,i_THISSTR_,i_SUBSTR_)
#endif

// @memberof RF_String
// @brief Removes all characters of a substring from both ends of the given String
//
// @isinherited{StringX}
// Searches for and removes each individual character inside the @c sub substring from the
// given String @c thisstr starting from both the beginning and the end of the String and until it finds any other character
// @lmsFunction
// @param thisstr The string to search in. @inhtype{String,StringX}
// @param sub The substring to search for. @inhtype{String,StringX} @tmpSTR
// @return Returns true for success and false if none of @c sub characters were found inside the given String
#if defined(RF_IAMHERE_FOR_DOXYGEN)
    i_DECLIMEX_ char rfString_Strip(void* thisstr,void* sub);
#else
    i_DECLIMEX_ char i_rfString_Strip(void* thisstr,void* sub);
    #define rfString_Strip(i_THISSTR_,i_SUBSTR_) i_rfLMSX_WRAP2(char,i_rfString_Strip,i_THISSTR_,i_SUBSTR_)
#endif
// @}
/*------------------------------------------------------------------------ RF_String File Descriptor I/O functions-------------------------------------------------------------------------------*/
// @name String File Descriptor I/O functions
// @{

// @memberof RF_String
// @brief Allocates and returns a string from UTF-8 file parsing
//
// @notinherited{StringX}
// Read the file stream @c f until either a newline character or the EOF is reached and saves it as an RF_String
// The file's encoding must be UTF-8.If for some reason (like EOF reached) no string can be read then null is returned
// Given file character stream must be encoded in UTF-8. A check for valide sequence of bytes is performed.
// @param f A valid and open file pointer in read mode from which to read the string. The file's encoding must be UTF-8.A check for valide sequence of bytes is performed.
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file was reached with this initialization
// @return The initialized string or null pointer in case of failure to read the file, or unexpected data (non-UTF8 encoded string)
i_DECLIMEX_ RF_String* rfString_Create_fUTF8(FILE* f, char* eof);
// @memberof RF_String
// @brief Initializes a string from UTF-8 file parsing
//
// @notinherited{StringX}
// Read the file stream @c f until either a newline character or the EOF is reached and saves it as an RF_String
// The file's encoding must be UTF-8.If for some reason (like EOF reached) no string can be read then null is returned
// Given file character stream must be encoded in UTF-8. A check for valide sequence of bytes is performed.
// @param str The extended string to initialize
// @param f A valid and open file pointer in read mode from which to read the string. The file's encoding must be UTF-8.A check for valide sequence of bytes is performed.
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file was reached with this initialization
// @return Returns either a positive number for succesfull initialization that represents the bytes read from the file.
// If there was a problem an error is returned. Possible errors are any of those that @ref rfFReadLine_UTF8() can produce.
i_DECLIMEX_ int32_t rfString_Init_fUTF8(RF_String* str,FILE* f, char* eof);

// @memberof RF_String
// @brief Assigns to a string from UTF-8 file parsing
//
// @notinherited{StringX}
// Read the file stream @c f until either a newline character or the EOF is reached and assigns it to an RF_StringX
// The file's encoding must be UTF-8.If for some reason (like EOF reached) no string can be read then null is returned
// Given file character stream must be encoded in UTF-8. A check for valide sequence of bytes is performed.
// @param str The extended string to assign to
// @param f A valid and open file pointer in read mode from which to read the string. The file's encoding must be UTF-8.A check for valide sequence of bytes is performed.
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file was reached with this assignment
// @return Returns either a positive number for succesfull assignment that represents the bytes read from the file.
// If there was a problem an error is returned. Possible errors are any of those that @ref rfFReadLine_UTF8() can produce.
i_DECLIMEX_ int32_t rfString_Assign_fUTF8(RF_String* str,FILE* f, char* eof);
// @memberof RF_String
// @brief Appends to a string from UTF-8 file parsing
//
// @notinherited{StringX}
// Read the file stream @c f until either a newline character or the EOF is reached and appends it to an RF_StringX
// The file's encoding must be UTF-8.If for some reason (like EOF reached) no string can be read then null is returned
// Given file character stream must be encoded in UTF-8. A check for valid sequence of bytes is performed.
// @param str The extended string to append to
// @param f A valid and open file pointer in read mode from which to read the string. The file's encoding must be UTF-8.A check for valide sequence of bytes is performed.
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file was reached with this appending
// @return Returns either a positive number for succesfull appending that represents the bytes read from the file.
// If there was a problem an error is returned. Possible errors are any of those that @ref rfFReadLine_UTF8() can produce.
i_DECLIMEX_ int32_t rfString_Append_fUTF8(RF_String* str,FILE* f, char* eof);

// @memberof RF_String
// @cppnotctor
// @brief Allocates and returns a string from UTF-16 file parsing
//
// @notinherited{StringX}
// Read the file stream @c f until either a newline character or the EOF is reached and saves it as an RF_StringX
// The file's encoding must be UTF-16.If for some reason (like EOF reached) no string can be read then null is returned. A check for a valid sequence of bytes is performed.
// @param f A valid and open file pointer in read mode from which to read the string. The file's encoding must be UTF-16.
// @param endianess A flag that determines in what endianess the UTF-16 file is encoded in. Possible values here are
// @c RF_LITTLE_ENDIAN and @c RF_BIG_ENDIAN.
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file was reached with this initialization
// @return The initialized string or null pointer in case of failure to read the file
i_DECLIMEX_ RF_String* rfString_Create_fUTF16(FILE* f, char endianess,char* eof);
// @memberof RF_String
// @brief Initializes a string from UTF-16 file parsing
//
// @notinherited{StringX}
// Read the file stream @c f until either a newline character or the EOF is reached and saves it as an RF_StringX
// The file's encoding must be UTF-16.If for some reason (like EOF reached) no string can be read then null is returned. A check for a valid sequence of bytes is performed.
// @param str The extended string to initialize
// @param f A valid and open file pointer in read mode from which to read the string. The file's encoding must be UTF-16.
// @param endianess A flag that determines in what endianess the UTF-16 file is encoded in. Possible values here are
// @c RF_LITTLE_ENDIAN and @c RF_BIG_ENDIAN.
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file was reached with this initialization
// @return Returns either a positive number for succesfull initialization that represents the bytes read from the file.
// If there was a problem an error is returned. Possible errors are any of those that @ref rfFReadLine_UTF16LE() can produce.
i_DECLIMEX_ int32_t rfString_Init_fUTF16(RF_String* str,FILE* f, char endianess,char* eof);

// @memberof RF_String
// @brief Appends the contents of a UTF-16 file a String
//
// @notinherited{StringX}
// Read the file stream @c f until either a newline character or the EOF is reached and appends it to an RF_StringX
// The file's encoding must be UTF-16.If for some reason (like EOF reached) no string can be read then null is returned. A check for a valid sequence of bytes is performed.
// @param str The extended string to append to
// @param f A valid and open file pointer in read mode from which to read the string. The file's encoding must be UTF-16.
// @param endianess A flag that determines in what endianess the UTF-16 file is encoded in. Possible values here are
// @c RF_LITTLE_ENDIAN and @c RF_BIG_ENDIAN.
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file was reached with this appending
// @return Returns either a positive number for succesfull appending that represents the bytes read from the file.
// If there was a problem an error is returned. Possible errors are any of those that @ref rfFReadLine_UTF16LE() can produce.
i_DECLIMEX_ int32_t rfString_Append_fUTF16(RF_String* str,FILE* f, char endianess,char* eof);
// @memberof RF_String
// @brief Assigns the contents of a UTF-16 file to an already initialized string
//
// @notinherited{StringX}
// Read the file stream @c f until either a newline character or the EOF is reached and assigns it to an RF_StringX
// The file's encoding must be UTF-16.If for some reason (like EOF reached) no string can be read then null is returned. A check for a valid sequence of bytes is performed.
// @param str The extended string to assign to
// @param f A valid and open file pointer in read mode from which to read the string. The file's encoding must be UTF-16.
// @param endianess A flag that determines in what endianess the UTF-16 file is encoded in. Possible values here are
// @c RF_LITTLE_ENDIAN and @c RF_BIG_ENDIAN.
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file was reached with this assignment
// @return Returns either a positive number for succesfull assignment that represents the bytes read from the file.
// If there was a problem an error is returned. Possible errors are any of those that @ref rfFReadLine_UTF16LE() can produce.
i_DECLIMEX_ int32_t rfString_Assign_fUTF16(RF_String* str,FILE* f, char endianess,char* eof);

// @memberof RF_String
// @cppnotctor
// @brief Allocates and returns a string from UTF-32 file parsing
//
// @notinherited{StringX}
// Read the file stream @c f until either a newline character or the EOF is reached and saves it as an RF_StringX
// The file's encoding must be UTF-32.If for some reason (like EOF reached) no string can be read then null is returned. A check for a valid sequence of bytes is performed.
// @param f A valid and open file pointer in read mode from which to read the string. The file's encoding must be UTF-32.
// @param endianess A flag that determines in what endianess the UTF-32 file is encoded in. Possible values here are
// @c RF_LITTLE_ENDIAN and @c RF_BIG_ENDIAN.
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file was reached with this initialization
// @return The initialized string or null pointer in case of failure to read the file
i_DECLIMEX_ RF_String* rfString_Create_fUTF32(FILE* f,char endianess, char* eof);
// @memberof RF_String
// @brief Initializes a string from UTF-32 file parsing
//
// @notinherited{StringX}
// Read the file stream @c f until either a newline character or the EOF is reached and saves it as an RF_StringX
// The file's encoding must be UTF-32.If for some reason (like EOF reached) no string can be read then null is returned. A check for a valid sequence of bytes is performed.
// @param str The extended string to initialize
// @param f A valid and open file pointer in read mode from which to read the string. The file's encoding must be UTF-32.
// @param endianess A flag that determines in what endianess the UTF-32 file is encoded in. Possible values here are
// @c RF_LITTLE_ENDIAN and @c RF_BIG_ENDIAN.
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file was reached with this initialization
// @return Returns either a positive number for succesfull initialization that represents the bytes read from the file.
// If there was a problem an error is returned. Possible errors are any of those that @ref rfFReadLine_UTF32LE() can produce.
i_DECLIMEX_ int32_t rfString_Init_fUTF32(RF_String* str,FILE* f,char endianess, char* eof);
// @memberof RF_String
// @brief Assigns the contents of a UTF-32 file to a string
//
// @notinherited{StringX}
// Read the file stream @c f until either a newline character or the EOF is reached and assigns it as the contents of the given RF_StringX
// The file's encoding must be UTF-32.If for some reason (like EOF reached) no string can be read then null is returned. A check for a valid sequence of bytes is performed.
// @param str The extended string to assign to
// @param f A valid and open file pointer in read mode from which to read the string. The file's encoding must be UTF-32.
// @param endianess A flag that determines in what endianess the UTF-32 file is encoded in. Possible values here are
// @c RF_LITTLE_ENDIAN and @c RF_BIG_ENDIAN.
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file was reached with this assignment
// @return Returns either a positive number for succesfull assignment that represents the bytes read from the file.
// If there was a problem an error is returned. Possible errors are any of those that @ref rfFReadLine_UTF32LE() can produce.
i_DECLIMEX_ int32_t rfString_Assign_fUTF32(RF_String* str,FILE* f,char endianess, char* eof);
// @memberof RF_String
// @brief Appends the contents of a UTF-32 file to a string
//
// @notinherited{StringX}
// Read the file stream @c f until either a newline character or the EOF is reached and appends to the given RF_StringX
// The file's encoding must be UTF-32.If for some reason (like EOF reached) no string can be read then null is returned. A check for a valid sequence of bytes is performed.
// @param str The extended string to append to
// @param f A valid and open file pointer in read mode from which to read the string. The file's encoding must be UTF-32.
// @param endianess A flag that determines in what endianess the UTF-32 file is encoded in. Possible values here are
// @c RF_LITTLE_ENDIAN and @c RF_BIG_ENDIAN.
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file was reached with this appending
// @return Returns either a positive number for succesfull appending that represents the bytes read from the file.
// If there was a problem an error is returned. Possible errors are any of those that @ref rfFReadLine_UTF32LE() can produce.
i_DECLIMEX_ int32_t rfString_Append_fUTF32(RF_String* str,FILE* f,char endianess, char* eof);

// @memberof RF_String
// @brief Writes a string to a file depending on the given encoding
//
// @isinherited{StringX}
// This function shall output the string @c s into the file descriptor @c f in the given @c encoding .
// @lmsFunction
// @param s The string to write to the file @inhtype{String,StringX} @tmpSTR
// @param f A valid and open file pointer into which to write the string.
// @param encoding \rfoptional{@c RF_UTF8} The encoding of the file. Default is @c RF_UTF8. Can be one of:
// + @c RF_UTF8: For Unicode UTF-8 encoding
// + @c RF_UTF16_BE: For Unicode UTF-16 encoding in Big Endian endianess
// + @c RF_UTF16_LE: For Unicode UTF-16 encoding in Little Endian endianess
// + @c RF_UTF32_BE: For Unicode UTF-32 encoding in Big Endian endianess
// + @c RF_UTF32_LE: For Unicode UTF-32 encoding in Little Endian endianess
// @return Returns @c RF_SUCCESS for succesfull writting and error otherwise. Possible errors are:
// + @c RE_FILE_WRITE: There was an unknown write error
// + @c RE_FILE_WRITE_BLOCK: The write failed because the file was occupied by another thread and the no block flag was set
// + @c RE_FILE_BAD: The file descriptor @c f was corrupt
// + @c RE_FILE_TOOBIG: The file's size exceeds the system limiti
// + @c RE_INTERRUPT: Writting failed due to a system interrupt
// + @c RE_FILE_IO: Writting failed because of a physical I/O error
// + @c RE_FILE_NOSPACE: Writting failed because the device containing the file had no free space
// + @c RE_FILE_NOT_FILE: Writting failed because the given file descriptor @c f is either non existen or not a file
#if defined(RF_IAMHERE_FOR_DOXYGEN)
    i_DECLIMEX_ int32_t rfString_Fwrite(void* s,FILE* f,char encoding);
#else
    i_DECLIMEX_ int32_t i_rfString_Fwrite(void* s,FILE* f,char* encoding);
    #ifdef RF_OPTION_DEFAULT_ARGUMENTS
        #define rfString_Fwrite(...) RF_SELECT_FUNC_IF_NARGGT(i_NPSELECT_RF_STRING_FWRITE,3,__VA_ARGS__)
        #define i_NPSELECT_RF_STRING_FWRITE1(...) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Fwrite() accepts from 2 to 3 arguments\"")
        #define i_NPSELECT_RF_STRING_FWRITE0(...) RF_SELECT_FUNC(i_SELECT_RF_STRING_FWRITE,__VA_ARGS__)
        #define i_SELECT_RF_STRING_FWRITE3(i_STR_,i_FILE_,i_ENCODING_) i_rfLMSX_WRAP3(int32_t,i_rfString_Fwrite,i_STR_,i_FILE_,i_RFI8_(i_ENCODING_))
        #define i_SELECT_RF_STRING_FWRITE2(i_STR_,i_FILE_) i_rfLMSX_WRAP3(int32_t,i_rfString_Fwrite,i_STR_,i_FILE_,i_RFI8_(RF_UTF8))
        #define i_SELECT_RF_STRING_FWRITE1(i_STR_,i_FILE_) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Fwrite() accepts from 2 to 3 arguments\"")
        #define i_SELECT_RF_STRING_FWRITE0(i_STR_,i_FILE_) RF_COMPILE_ERROR("message \"Illegal Arguments Number: Function rfString_Fwrite() accepts from 2 to 3 arguments\"")
    #else
        #define rfString_Fwrite_fUTF8(i_STR_,i_FILE_,i_ENCODING_) i_rfLMSX_WRAP3(int32_t,i_rfString_Fwrite,i_STR_,i_FILE_,i_RFI8_(i_ENCODING_))
    #endif
#endif


// @}
// closing the String File I/o functions

#ifdef __cplusplus
}// closing bracket for calling from C++
#endif

#else // end of the strings module include
    #error Attempted to include Refu String manipulation with the String module flag off. Rebuild the library with that option added if you need to include them
#endif

#endif// include guards end

