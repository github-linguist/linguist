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


#ifndef REFU_IO_H
#define REFU_IO_H

#include <rf_setup.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C"
{// opening bracket for calling from C++
#endif

// New line feed
#define RF_LF   0xA
// Carriage Return
#define RF_CR   0xD

#ifdef REFU_WIN32_VERSION
    #define i_PLUSB_WIN32   "b"
#else
    #define i_PLUSB_WIN32   ""
#endif

// This is the type that represents the file offset
#ifdef _MSC_VER
typedef __int64 foff_rft;
#else
#include <sys/types.h>
typedef off64_t foff_rft;
#endif
///Fseek and Ftelll definitions
#ifdef _MSC_VER
    #define rfFseek(i_FILE_,i_OFFSET_,i_WHENCE_)    _fseeki64(i_FILE_,i_OFFSET_,i_WHENCE_)
    #define rfFtell(i_FILE_)                        _ftelli64(i_FILE_)
#else
    #define rfFseek(i_FILE_,i_OFFSET_,i_WHENCE_)    fseeko64(i_FILE_,i_OFFSET_,i_WHENCE_)
    #define rfFtell(i_FILE_)                        ftello64(i_FILE_)
#endif

/**
** @defgroup RF_IOGRP I/O
** @addtogroup RF_IOGRP
** @{
**/

// @brief Reads a UTF-8 file descriptor until end of line or EOF is found and returns a UTF-8 byte buffer
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// When the compile flag @c RF_NEWLINE_CRLF is defined (the default case at Windows) then this function
// shall not be adding any CR character that is found in the file behind a newline character since this is
// the Windows line ending scheme. Beware though that the returned  read bytes value shall still count the CR character inside.
//
// @param[in] f The file descriptor to read
// @param[out] utf8 Give here a refence to an unitialized char* that will be allocated inside the function
// and contain the utf8 byte buffer. Needs to be freed by the caller explicitly later
// @param[out] byteLength Give an @c uint32_t here to receive the length of the @c utf8 buffer in bytes
// @param[out] bufferSize Give an @c uint32_t here to receive the capacity of the @c utf8 buffer in bytes
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file
// with reading this line
// @return Returns either a positive number for success that represents the number of bytes read from @c f and and error in case something goes wrong.
// The possible errors to return are the same as rfFgets_UTF8()
i_DECLIMEX_ int32_t rfFReadLine_UTF8(FILE* f,char** utf8,uint32_t* byteLength,uint32_t* bufferSize,char* eof);
// @brief Reads a Big Endian UTF-16 file descriptor until end of line or EOF is found and returns a UTF-8 byte buffer
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// When the compile flag @c RF_NEWLINE_CRLF is defined (the default case at Windows) then this function
// shall not be adding any CR character that is found in the file behind a newline character since this is
// the Windows line ending scheme. Beware though that the returned  read bytes value shall still count the CR character inside.
//
// @param[in] f The file descriptor to read
// @param[out] utf8 Give here a refence to an unitialized char* that will be allocated inside the function
// and contain the utf8 byte buffer. Needs to be freed by the caller explicitly later
// @param[out] byteLength Give an @c uint32_t here to receive the length of the @c utf8 buffer in bytes
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file
// with reading this line
// @return Returns either a positive number for success that represents the number of bytes read from @c f and and error in case something goes wrong.
// + Any error that can be returned by @ref rfFgets_UTF16BE()
// + @c RE_UTF16_INVALID_SEQUENCE: Failed to decode the UTF-16 byte stream of the file descriptor
// + @c RE_UTF8_ENCODING: Failed to encode the UTF-16 of the file descriptor into UTF-8
i_DECLIMEX_ int32_t rfFReadLine_UTF16BE(FILE* f,char** utf8,uint32_t* byteLength,char* eof);
// @brief Reads a Little Endian UTF-16 file descriptor until end of line or EOF is found and returns a UTF-8 byte buffer
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// When the compile flag @c RF_NEWLINE_CRLF is defined (the default case at Windows) then this function
// shall not be adding any CR character that is found in the file behind a newline character since this is
// the Windows line ending scheme. Beware though that the returned read bytes value shall still count the CR character inside.
//
// @param[in] f The file descriptor to read
// @param[out] utf8 Give here a refence to an unitialized char* that will be allocated inside the function
// and contain the utf8 byte buffer. Needs to be freed by the caller explicitly later
// @param[out] byteLength Give an @c uint32_t here to receive the length of the @c utf8 buffer in bytes
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file
// with reading this line
// @return Returns either a positive number for success that represents the number of bytes read from @c f and and error in case something goes wrong.
// + Any error that can be returned by @ref rfFgets_UTF16LE()
// + @c RE_UTF16_INVALID_SEQUENCE: Failed to decode the UTF-16 byte stream of the file descriptor
// + @c RE_UTF8_ENCODING: Failed to encode the UTF-16 of the file descriptor into UTF-8
i_DECLIMEX_ int32_t rfFReadLine_UTF16LE(FILE* f,char** utf8,uint32_t* byteLength,char* eof);

// @brief Reads a Big Endian UTF-32 file descriptor until end of line or EOF is found and returns a UTF-8 byte buffer
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// When the compile flag @c RF_NEWLINE_CRLF is defined (the default case at Windows) then this function
// shall not be adding any CR character that is found in the file behind a newline character since this is
// the Windows line ending scheme. Beware though that the returned read bytes value shall still count the CR character inside.
//
// @param[in] f The file descriptor to read
// @param[out] utf8 Give here a refence to an unitialized char* that will be allocated inside the function
// and contain the utf8 byte buffer. Needs to be freed by the caller explicitly later
// @param[out] byteLength Give an @c uint32_t here to receive the length of the @c utf8 buffer in bytes
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file
// with reading this line
// @return Returns either a positive number for success that represents the number of bytes read from @c f and and error in case something goes wrong.
// + Any error that can be returned by @ref rfFgets_UTF32BE()
// + @c RE_UTF8_ENCODING: Failed to encode the UTF-16 of the file descriptor into UTF-8
i_DECLIMEX_ int32_t rfFReadLine_UTF32BE(FILE* f,char** utf8,uint32_t* byteLength,char* eof);
// @brief Reads a Little Endian UTF-32 file descriptor until end of line or EOF is found and returns a UTF-8 byte buffer
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// When the compile flag @c RF_NEWLINE_CRLF is defined (the default case at Windows) then this function
// shall not be adding any CR character that is found in the file behind a newline character since this is
// the Windows line ending scheme. Beware though that the returned read bytes value shall still count the CR character inside.
//
// @param[in] f The file descriptor to read
// @param[out] utf8 Give here a refence to an unitialized char* that will be allocated inside the function
// and contain the utf8 byte buffer. Needs to be freed by the caller explicitly later
// @param[out] byteLength Give an @c uint32_t here to receive the length of the @c utf8 buffer in bytes
// @param[out] eof Pass a pointer to a char to receive a true or false value in case the end of file
// with reading this line
// @return Returns either a positive number for success that represents the number of bytes read from @c f and and error in case something goes wrong.
// + Any error that can be returned by @ref rfFgets_UTF32LE()
// + @c RE_UTF8_ENCODING: Failed to encode the UTF-16 of the file descriptor into UTF-8
i_DECLIMEX_ int32_t rfFReadLine_UTF32LE(FILE* f,char** utf8,uint32_t* byteLength,char* eof);

// @brief Gets a number of bytes from a BIG endian UTF-32 file descriptor
//
// This is a function that's similar to c library fgets but it also returns the number of bytes read. Reads in from the file until @c num bytes
// have been read or new line or EOF character has been encountered.
//
// The function will read until @c num characters are read and if @c num
// would take us to the middle of a UTF32 character then the next character shall also be read
// and the function will return the number of bytes read.
// Since the function null terminates the buffer the given @c buff needs to be of at least
// @c num+7 size to cater for the worst case.
//
// The final bytestream stored inside @c buff is in the endianess of the system.
//
// If right after the last character read comes the EOF, the function
// shall detect so and assign @c true to @c eof.
//
// In Windows where file endings are in the form of 2 bytes CR-LF (Carriage return - NewLine) this function
// shall just ignore the carriage returns and not return it inside the return buffer at @c buff.
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// @param[in] buff A buffer to be filled with the contents of the file. Should be of size at least @c num+7
// @param[in] num The maximum number of bytes to read from within the file NOT including the null terminating character(which in itelf is 4 bytes). Should be a multiple of 4
// @param[in] f A valid FILE descriptor from which to read the bytes
// @param[out] eof Pass a reference to a char to receive a true/false value for whether EOF has been reached.
// @return Returns the actual number of bytes read or an error if there was a problem.
// The possible errors are:
// + @c RE_FILE_READ: If during reading the file there was an unknown read error
// + @c RE_FILE_READ_BLOCK: If the read operation failed due to the file descriptor being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the file descriptor's mode was not correctly set for reading
// + @c RE_FILE_POS_OVERFLOW: If during reading, the current file position can't be represented by the system
// + @c RE_INTERRUPT: If during reading, there was a system interrupt
// + @c RE_FILE_IO: If there was a physical I/O error
// + @c RE_FILE_NOSPACE: If reading failed due to insufficient storage space
i_DECLIMEX_ int32_t rfFgets_UTF32BE(char* buff,uint32_t num,FILE* f,char* eof);
// @brief Gets a number of bytes from a Little endian UTF-32 file descriptor
//
// This is a function that's similar to c library fgets but it also returns the number of bytes read. Reads in from the file until @c num bytes
// have been read or new line or EOF character has been encountered.
//
// The function will read until @c num characters are read and if @c num
// would take us to the middle of a UTF32 character then the next character shall also be read
// and the function will return the number of bytes read.
// Since the function null terminates the buffer the given @c buff needs to be of at least
// @c num+7 size to cater for the worst case.
//
// The final bytestream stored inside @c buff is in the endianess of the system.
//
// If right after the last character read comes the EOF, the function
// shall detect so and assign @c true to @c eof.
//
// In Windows where file endings are in the form of 2 bytes CR-LF (Carriage return - NewLine) this function
// shall just ignore the carriage returns and not return it inside the return buffer at @c buff.
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// @param[in] buff A buffer to be filled with the contents of the file. Should be of size at least @c num+7
// @param[in] num The maximum number of bytes to read from within the file NOT including the null terminating character(which in itelf is 4 bytes). Should be a multiple of 4
// @param[in] f A valid FILE descriptor from which to read the bytes
// @param[out] eof Pass a reference to a char to receive a true/false value for whether EOF has been reached.
// @return Returns the actual number of bytes read or an error if there was a problem.
// The possible errors are:
// + @c RE_FILE_READ: If during reading the file there was an unknown read error
// + @c RE_FILE_READ_BLOCK: If the read operation failed due to the file descriptor being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the file descriptor's mode was not correctly set for reading
// + @c RE_FILE_POS_OVERFLOW: If during reading, the current file position can't be represented by the system
// + @c RE_INTERRUPT: If during reading, there was a system interrupt
// + @c RE_FILE_IO: If there was a physical I/O error
// + @c RE_FILE_NOSPACE: If reading failed due to insufficient storage space
i_DECLIMEX_ int32_t rfFgets_UTF32LE(char* buff,uint32_t num,FILE* f,char* eof);

// @brief Gets a number of bytes from a BIG endian UTF-16 file descriptor
//
// This is a function that's similar to c library fgets but it also returns the number of bytes read. Reads in from the file until @c num bytes
// have been read or new line or EOF character has been encountered.
//
// The function will read until @c num characters are read and if @c num
// would take us to the middle of a UTF16 character then the next character shall also be read
// and the function will return the number of bytes read.
// Since the function null terminates the buffer the given @c buff needs to be of at least
// @c num+5 size to cater for the worst case.
//
// The final bytestream stored inside @c buff is in the endianess of the system.
//
// If right after the last character read comes the EOF, the function
// shall detect so and assign @c true to @c eof.
//
// In Windows where file endings are in the form of 2 bytes CR-LF (Carriage return - NewLine) this function
// shall just ignore the carriage returns and not return it inside the return buffer at @c buff.
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// @param[in] buff A buffer to be filled with the contents of the file. Should be of size at least @c num+5
// @param[in] num The maximum number of bytes to read from within the file NOT including the null terminating character(which in itelf is 2 bytes). Should be a multiple of 2
// @param[in] f A valid FILE descriptor from which to read the bytes
// @param[out] eof Pass a reference to a char to receive a true/false value for whether EOF has been reached.
// @return Returns the actual number of bytes read or an error if there was a problem.
// The possible errors are:
// + @c RE_FILE_READ: If during reading the file there was an unknown read error
// + @c RE_FILE_READ_BLOCK: If the read operation failed due to the file descriptor being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the file descriptor's mode was not correctly set for reading
// + @c RE_FILE_POS_OVERFLOW: If during reading, the current file position can't be represented by the system
// + @c RE_INTERRUPT: If during reading, there was a system interrupt
// + @c RE_FILE_IO: If there was a physical I/O error
// + @c RE_FILE_NOSPACE: If reading failed due to insufficient storage space
i_DECLIMEX_ int32_t rfFgets_UTF16BE(char* buff,uint32_t num,FILE* f,char* eof);
// @brief Gets a number of bytes from a Little endian UTF-16 file descriptor
//
// This is a function that's similar to c library fgets but it also returns the number of bytes read. Reads in from the file until @c num bytes
// have been read or new line or EOF character has been encountered.
//
// The function will read until @c num characters are read and if @c num
// would take us to the middle of a UTF16 character then the next character shall also be read
// and the function will return the number of bytes read.
// Since the function null terminates the buffer the given @c buff needs to be of at least
// @c num+5 size to cater for the worst case.
//
// The final bytestream stored inside @c buff is in the endianess of the system.
//
// If right after the last character read comes the EOF, the function
// shall detect so and assign @c true to @c eof.
//
// In Windows where file endings are in the form of 2 bytes CR-LF (Carriage return - NewLine) this function
// shall just ignore the carriage returns and not return it inside the return buffer at @c buff.
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// @param[in] buff A buffer to be filled with the contents of the file. Should be of size at least @c num+2
// @param[in] num The maximum number of bytes to read from within the file NOT including the null terminating character(which in itelf is 2 bytes). Should be a multiple of 2
// @param[in] f A valid FILE descriptor from which to read the bytes
// @param[out] eof Pass a reference to a char to receive a true/false value for whether EOF has been reached.
// @return Returns the actual number of bytes read or an error if there was a problem.
// The possible errors are:
// + @c RE_FILE_READ: If during reading the file there was an unknown read error
// + @c RE_FILE_READ_BLOCK: If the read operation failed due to the file descriptor being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the file descriptor's mode was not correctly set for reading
// + @c RE_FILE_POS_OVERFLOW: If during reading, the current file position can't be represented by the system
// + @c RE_INTERRUPT: If during reading, there was a system interrupt
// + @c RE_FILE_IO: If there was a physical I/O error
// + @c RE_FILE_NOSPACE: If reading failed due to insufficient storage space
i_DECLIMEX_ int32_t rfFgets_UTF16LE(char* buff,uint32_t num,FILE* f,char* eof);
// @brief Gets a number of bytes from a UTF-8 file descriptor
//
// This is a function that's similar to c library fgets but it also returns the number of bytes read. Reads in from the file until @c num characters
// have been read or new line or EOF character has been encountered.
//
// The function  automatically adds a null termination character at the end of
// @c buff but this character is not included in the returned actual number of bytes.
//
// The function will read until @c num characters are read and if @c num
// would take us to the middle of a UTF8 character then the next character shall also be read
// and the function will return the number of bytes read.
// Since the function null terminates the buffer the given @c buff needs to be of at least
// @c num+4 size to cater for the worst case.
//
// If right after the last character read comes the EOF, the function
// shall detect so and assign @c true to @c eof.
//
// In Windows where file endings are in the form of 2 bytes CR-LF (Carriage return - NewLine) this function
// shall just ignore the carriage returns and not return it inside the return buffer at @c buff.
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// @param[in] buff A buffer to be filled with the contents of the file. Should of size at least @c num+4
// @param[in] num The maximum number of bytes to read from within the file NOT including the null terminating character(which in itelf is 1 byte)
// @param[in] f A valid FILE descriptor from which to read the bytes
// @param[out] eof Pass a reference to a char to receive a true/false value for whether EOF has been reached.
// @return Returns the actual number of bytes read or an error if there was a problem.
// The possible errors are:
// + @c RE_UTF8_INVALID_SEQUENCE_INVALID_BYTE: If an invalid UTF-8 byte has been found
// + @c RE_UTF8_INVALID_SEQUENCE_CONBYTE: If during parsing the file we were expecting a continuation
// byte and did not find it
// + @c RE_UTF8_INVALID_SEQUENCE_END: If the null character is encountered in between bytes that should
// have been continuation bytes
// + @c RE_FILE_READ: If during reading the file there was an unknown read error
// + @c RE_FILE_READ_BLOCK: If the read operation failed due to the file descriptor being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the file descriptor's mode was not correctly set for reading
// + @c RE_FILE_POS_OVERFLOW: If during reading, the current file position can't be represented by the system
// + @c RE_INTERRUPT: If during reading, there was a system interrupt
// + @c RE_FILE_IO: If there was a physical I/O error
// + @c RE_FILE_NOSPACE: If reading failed due to insufficient storage space
i_DECLIMEX_ int32_t rfFgets_UTF8(char* buff,uint32_t num,FILE* f,char* eof);

// @brief  Gets a unicode character from a UTF-8 file descriptor
//
// This function attempts to assume a more modern fgetc() role for UTF-8 encoded files.
// Reads bytes from the File descriptor @c f until a full UTF-8 unicode character has been read
//
// After this function the file pointer will have moved either by @c 1, @c 2, @c 3 or @c 4
// bytes if the return value is positive. You can see how much by checking the return value.
//
// You shall need to provide an integer at @c c to contain either the decoded Unicode
// codepoint or the UTF-8 endoced byte depending on the value of the @c cp argument.
//
// @param f A valid FILE descriptor from which to read the bytes
// @param c Pass an int that will receive either the unicode code point value or
// the UTF8 bytes depending on the value of the @c cp flag
// @param cp A boolean flag. If @c true then the int passed at @c c will contain the unicode code point
// of the read character, so the UTF-8 will be decoded.
// If @c false the int passed at @c c will contain the value of the read bytes in UTF-8 without any decoding
// @return Returns the number of bytes read (either @c 1, @c 2, @c 3 or @c 4) or an error if the function
// fails for some reason. Possible error values are:
// + @c RE_FILE_EOF: The end of file has been found while reading. If the end of file is encountered
// in the middle of a UTF-8 encoded character where we would be expecting something different
// and @c RE_UTF8_INVALID_SEQUENCE_END error is also logged
// + @c RE_UTF8_INVALID_SEQUENCE_INVALID_BYTE: If an invalid UTF-8 byte has been found
// + @c RE_UTF8_INVALID_SEQUENCE_CONBYTE: If during parsing the file we were expecting a continuation
// byte and did not find it
// + @c RE_UTF8_INVALID_SEQUENCE_END: If the null character is encountered in between bytes that should
// have been continuation bytes
// + @c RE_FILE_READ: If during reading the file there was an unknown read error
// + @c RE_FILE_READ_BLOCK: If the read operation failed due to the file descriptor being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the file descriptor's mode was not correctly set for reading
// + @c RE_FILE_POS_OVERFLOW: If during reading, the current file position can't be represented by the system
// + @c RE_INTERRUPT: If during reading, there was a system interrupt
// + @c RE_FILE_IO: If there was a physical I/O error
// + @c RE_FILE_NOSPACE: If reading failed due to insufficient storage space
i_DECLIMEX_ int32_t rfFgetc_UTF8(FILE* f,uint32_t *c,char cp);
// @brief  Gets a unicode character from a UTF-16 Big Endian file descriptor
//
// This function attempts to assume a more modern fgetc() role for UTF-16 encoded files.
// Reads bytes from the File descriptor @c f until a full UTF-16 unicode character has been read
//
// After this function the file pointer will have moved either by @c 2 or @c 4
// bytes if the return value is positive. You can see how much by checking the return value.
//
// You shall need to provide an integer at @c c to contain either the decoded Unicode
// codepoint or the Bigendian encoded UTF-16 bytes depending on the value of @c the cp argument.
//
// @param f A valid FILE descriptor from which to read the bytes
// @param c Pass an int that will receive either the unicode code point value or
// the UTF16 bytes depending on the value of the @c cp flag
// @param cp A boolean flag. If @c true then the int passed at @c c will contain the unicode code point
// of the read character, so the UTF-16 will be decoded.
// If @c false the int passed at @c c will contain the value of the read bytes in UTF-16 without any decoding
// @return Returns the number of bytes read (either @c 2 or @c 4) or an error if the function
// fails for some reason. Possible error values are:
// + @c RE_UTF16_INVALID_SEQUENCE: Either the read word or its surrogate pair if 4 bytes were read held illegal values
// + @c RE_UTF16_NO_SURRPAIR: According to the first read word a surrogate pair was expected but none was found
// + @c RE_FILE_EOF: The end of file has been found while reading. If the end of file is encountered
// while we expect a UTF-16 surrogate pair an appropriate error is logged
// + @c RE_FILE_READ: If during reading the file there was an unknown read error
// + @c RE_FILE_READ_BLOCK: If the read operation failed due to the file descriptor being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the file descriptor's mode was not correctly set for reading
// + @c RE_FILE_POS_OVERFLOW: If during reading, the current file position can't be represented by the system
// + @c RE_INTERRUPT: If during reading, there was a system interrupt
// + @c RE_FILE_IO: If there was a physical I/O error
// + @c RE_FILE_NOSPACE: If reading failed due to insufficient storage space
i_DECLIMEX_ int32_t rfFgetc_UTF16BE(FILE* f,uint32_t *c,char cp);
// @brief  Gets a unicode character from a UTF-16 Little Endian file descriptor
//
// This function attempts to assume a more modern fgetc() role for UTF-16 encoded files.
// Reads bytes from the File descriptor @c f until a full UTF-16 unicode character has been read
//
// After this function the file pointer will have moved either by @c 2 or @c 4
// bytes if the return value is positive. You can see how much by checking the return value.
//
// You shall need to provide an integer at @c c to contain either the decoded Unicode
// codepoint or the Bigendian encoded UTF-16 bytes depending on the value of @c the cp argument.
//
// @param f A valid FILE descriptor from which to read the bytes
// @param c Pass an int that will receive either the unicode code point value or
// the UTF16 bytes depending on the value of the @c cp flag
// @param cp A boolean flag. If @c true then the int passed at @c c will contain the unicode code point
// of the read character, so the UTF-16 will be decoded.
// If @c false the int passed at @c c will contain the value of the read bytes in UTF-16 without any decoding
// @return Returns the number of bytes read (either @c 2 or @c 4) or an error if the function
// fails for some reason. Possible error values are:
// + @c RE_UTF16_INVALID_SEQUENCE: Either the read word or its surrogate pair if 4 bytes were read held illegal values
// + @c RE_UTF16_NO_SURRPAIR: According to the first read word a surrogate pair was expected but none was found
// + @c RE_FILE_EOF: The end of file has been found while reading. If the end of file is encountered
// while we expect a UTF-16 surrogate pair an appropriate error is logged
// + @c RE_FILE_READ: If during reading the file there was an unknown read error
// + @c RE_FILE_READ_BLOCK: If the read operation failed due to the file descriptor being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the file descriptor's mode was not correctly set for reading
// + @c RE_FILE_POS_OVERFLOW: If during reading, the current file position can't be represented by the system
// + @c RE_INTERRUPT: If during reading, there was a system interrupt
// + @c RE_FILE_IO: If there was a physical I/O error
// + @c RE_FILE_NOSPACE: If reading failed due to insufficient storage space
i_DECLIMEX_ int32_t rfFgetc_UTF16LE(FILE* f,uint32_t *c,char cp);
// @brief  Gets a unicode character from a UTF-32 Little Endian file descriptor
//
// This function attempts to assume a more modern fgetc() role for UTF-32 encoded files.
// Reads bytes from the File descriptor @c f until a full UTF-32 unicode character has been read
//
// After this function the file pointer will have moved by @c 4
// bytes if the return value is positive.
//
// You shall need to provide an integer at @c to contain the UTF-32 codepoint.
//
// @param f A valid FILE descriptor from which to read the bytes
// @param c Pass an int that will receive either the unicode code point value or
// the UTF16 bytes depending on the value of the @c cp flag
// If @c false the int passed at @c c will contain the value of the read bytes in UTF-16 without any decoding
// @return Returns either @c RF_SUCCESS for succesfull readin or one of the following errors:
// + @c RE_FILE_EOF: The end of file has been found while reading.
// + @c RE_FILE_READ: If during reading the file there was an unknown read error
// + @c RE_FILE_READ_BLOCK: If the read operation failed due to the file descriptor being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the file descriptor's mode was not correctly set for reading
// + @c RE_FILE_POS_OVERFLOW: If during reading, the current file position can't be represented by the system
// + @c RE_INTERRUPT: If during reading, there was a system interrupt
// + @c RE_FILE_IO: If there was a physical I/O error
// + @c RE_FILE_NOSPACE: If reading failed due to insufficient storage space
i_DECLIMEX_ int32_t rfFgetc_UTF32LE(FILE* f,uint32_t *c);
// @brief  Gets a unicode character from a UTF-32 Big Endian file descriptor
//
// This function attempts to assume a more modern fgetc() role for UTF-32 encoded files.
// Reads bytes from the File descriptor @c f until a full UTF-32 unicode character has been read
//
// After this function the file pointer will have moved by @c 4
// bytes if the return value is positive.
//
// You shall need to provide an integer at @c to contain the UTF-32 codepoint.
//
// @param f A valid FILE descriptor from which to read the bytes
// @param c Pass an int that will receive either the unicode code point value or
// the UTF16 bytes depending on the value of the @c cp flag
// If @c false the int passed at @c c will contain the value of the read bytes in UTF-16 without any decoding
// @return Returns either @c RF_SUCCESS for succesfull readin or one of the following errors:
// + @c RE_FILE_EOF: The end of file has been found while reading.
// + @c RE_FILE_READ: If during reading the file there was an unknown read error
// + @c RE_FILE_READ_BLOCK: If the read operation failed due to the file descriptor being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the file descriptor's mode was not correctly set for reading
// + @c RE_FILE_POS_OVERFLOW: If during reading, the current file position can't be represented by the system
// + @c RE_INTERRUPT: If during reading, there was a system interrupt
// + @c RE_FILE_IO: If there was a physical I/O error
// + @c RE_FILE_NOSPACE: If reading failed due to insufficient storage space
i_DECLIMEX_ int32_t rfFgetc_UTF32BE(FILE* f,uint32_t *c);

// @brief Moves a unicode character backwards in a big endian UTF-32 file stream
//
// @param f The file stream
// @param c Returns the character we moved back to as a unicode codepoint
// @return Returns either @c RF_SUCCESS for success or one of the following errors:
// + @c RE_FILE_POS_OVERFLOW: If during trying to read the current file's position it can't be represented by the system
// + @c RE_FILE_BAD: If The file descriptor is corrupt/illegal
// + @c RE_FILE_NOTFILE: If the file descriptor is not a file but something else. e.g. socket.
// + @c RE_FILE_GETFILEPOS: If the file's position could not be retrieved for some unknown reason
// + @c RE_FILE_WRITE_BLOCK: While attempting to move the file pointer, it was occupied by another thread, and the no block flag was set
// + @c RE_INTERRUPT: Operating on the file failed due to a system interrupt
// + @c RE_FILE_IO: There was a physical I/O error
// + @c RE_FILE_NOSPACE: There was no space on the device holding the file
// + @c RE_FILE_NOTFILE: The device we attempted to manipulate is non-existent
// + @c RE_FILE_READ: If during reading the file there was an error
// + @c RE_FILE_READ_BLOCK: If during reading the file the read operation failed due to the file being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the underlying file descriptor's mode was not correctly set for reading
i_DECLIMEX_ int32_t rfFback_UTF32BE(FILE* f,uint32_t *c);
// @brief Moves a unicode character backwards in a little endian UTF-32 file stream
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// @param f The file stream
// @param c Returns the character we moved back to as a unicode codepoint
// @return Returns either @c RF_SUCCESS for success or one of the following errors:
// + @c RE_FILE_POS_OVERFLOW: If during trying to read the current file's position it can't be represented by the system
// + @c RE_FILE_BAD: If The file descriptor is corrupt/illegal
// + @c RE_FILE_NOTFILE: If the file descriptor is not a file but something else. e.g. socket.
// + @c RE_FILE_GETFILEPOS: If the file's position could not be retrieved for some unknown reason
// + @c RE_FILE_WRITE_BLOCK: While attempting to move the file pointer, it was occupied by another thread, and the no block flag was set
// + @c RE_INTERRUPT: Operating on the file failed due to a system interrupt
// + @c RE_FILE_IO: There was a physical I/O error
// + @c RE_FILE_NOSPACE: There was no space on the device holding the file
// + @c RE_FILE_NOTFILE: The device we attempted to manipulate is non-existent
// + @c RE_FILE_READ: If during reading the file there was an error
// + @c RE_FILE_READ_BLOCK: If during reading the file the read operation failed due to the file being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the underlying file descriptor's mode was not correctly set for reading
i_DECLIMEX_ int32_t rfFback_UTF32LE(FILE* f,uint32_t *c);
// @brief Moves a unicode character backwards in a big endian UTF-16 file stream
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// @param f The file stream
// @param c Returns the character we moved back to as a unicode codepoint
// @return Returns either the number of bytes moved backwards (either @c 4 or @c 2) for success or one of the following errors:
// + @c RE_UTF16_INVALID_SEQUENCE: Either the read word or its surrogate pair if 4 bytes were read held illegal values
// + @c RE_FILE_POS_OVERFLOW: If during trying to read the current file's position it can't be represented by the system
// + @c RE_FILE_BAD: If The file descriptor is corrupt/illegal
// + @c RE_FILE_NOTFILE: If the file descriptor is not a file but something else. e.g. socket.
// + @c RE_FILE_GETFILEPOS: If the file's position could not be retrieved for some unknown reason
// + @c RE_FILE_WRITE_BLOCK: While attempting to move the file pointer, it was occupied by another thread, and the no block flag was set
// + @c RE_INTERRUPT: Operating on the file failed due to a system interrupt
// + @c RE_FILE_IO: There was a physical I/O error
// + @c RE_FILE_NOSPACE: There was no space on the device holding the file
// + @c RE_FILE_NOTFILE: The device we attempted to manipulate is non-existent
// + @c RE_FILE_READ: If during reading the file there was an error
// + @c RE_FILE_READ_BLOCK: If during reading the file the read operation failed due to the file being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the underlying file descriptor's mode was not correctly set for reading
i_DECLIMEX_ int32_t rfFback_UTF16BE(FILE* f,uint32_t *c);
// @brief Moves a unicode character backwards in a little endian UTF-16 file stream
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// @param f The file stream
// @param c Returns the character we moved back to as a unicode codepoint
// @return Returns either the number of bytes moved backwards (either @c 4 or @c 2) for success or one of the following errors:
// + @c RE_UTF16_INVALID_SEQUENCE: Either the read word or its surrogate pair if 4 bytes were read held illegal values
// + @c RE_FILE_POS_OVERFLOW: If during trying to read the current file's position it can't be represented by the system
// + @c RE_FILE_BAD: If The file descriptor is corrupt/illegal
// + @c RE_FILE_NOTFILE: If the file descriptor is not a file but something else. e.g. socket.
// + @c RE_FILE_GETFILEPOS: If the file's position could not be retrieved for some unknown reason
// + @c RE_FILE_WRITE_BLOCK: While attempting to move the file pointer, it was occupied by another thread, and the no block flag was set
// + @c RE_INTERRUPT: Operating on the file failed due to a system interrupt
// + @c RE_FILE_IO: There was a physical I/O error
// + @c RE_FILE_NOSPACE: There was no space on the device holding the file
// + @c RE_FILE_NOTFILE: The device we attempted to manipulate is non-existent
// + @c RE_FILE_READ: If during reading the file there was an error
// + @c RE_FILE_READ_BLOCK: If during reading the file the read operation failed due to the file being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the underlying file descriptor's mode was not correctly set for reading
i_DECLIMEX_ int32_t rfFback_UTF16LE(FILE* f,uint32_t *c);
// @brief Moves a unicode character backwards in a UTF-8 file stream
//
// The file descriptor at @c f must have been opened in <b>binary</b> and not text mode. That means that if under
// Windows make sure to call fopen with "wb", "rb" e.t.c. instead of the simple "w", "r" e.t.c. since the initial
// default value under Windows is text mode. Alternatively you can set the initial value using _get_fmode() and
// _set_fmode(). For more information take a look at the msdn pages here:
// http://msdn.microsoft.com/en-us/library/ktss1a9b.aspx
//
// @param f The file stream
// @param c Returns the character we moved back to as a unicode codepoint
// @return Returns either the number of bytes moved backwards for success (either @c 4, @c 3, @c 2 or @c 1) or one of the following errors:
// + @c RE_UTF8_INVALID_SEQUENCE: If during moving bacwards in the file unexpected UTF-8 bytes were found
// + @c RE_FILE_POS_OVERFLOW: If during trying to read the current file's position it can't be represented by the system
// + @c RE_FILE_BAD: If The file descriptor is corrupt/illegal
// + @c RE_FILE_NOTFILE: If the file descriptor is not a file but something else. e.g. socket.
// + @c RE_FILE_GETFILEPOS: If the file's position could not be retrieved for some unknown reason
// + @c RE_FILE_WRITE_BLOCK: While attempting to move the file pointer, it was occupied by another thread, and the no block flag was set
// + @c RE_INTERRUPT: Operating on the file failed due to a system interrupt
// + @c RE_FILE_IO: There was a physical I/O error
// + @c RE_FILE_NOSPACE: There was no space on the device holding the file
// + @c RE_FILE_NOTFILE: The device we attempted to manipulate is non-existent
// + @c RE_FILE_READ: If during reading the file there was an error
// + @c RE_FILE_READ_BLOCK: If during reading the file the read operation failed due to the file being occupied by another thread
// + @c RE_FILE_MODE: If during reading the file the underlying file descriptor's mode was not correctly set for reading
i_DECLIMEX_ int32_t rfFback_UTF8(FILE* f,uint32_t *c);

// @brief Opens another process as a pipe
//
// This function is a cross-platform popen wrapper. In linux it uses popen and in Windows it uses
// _popen.
// @lmsFunction
// @param command The string with the command to execute. Is basically the name of the program/process you want to spawn
// with its full path and its parameters. @inhtype{String,StringX} @tmpSTR
// @param mode The mode you want the pipe to work in. There are two possible values:
// + @c "r" The calling process can read the spawned command's standard output via the returned stream.
// + @c "w" The calling process can write to the spawned command's standard input via the returned stream.
//
// Anything else will result in an error
// @return For success popen will return a FILE descriptor that can be used to either read or write from the pipe.
// If there was an error @c 0 is returned and an error is logged.
#ifdef RF_IAMHERE_FOR_DOXYGEN
i_DECLIMEX_ FILE* rfPopen(void* command,const char* mode);
#else
i_DECLIMEX_ FILE* i_rfPopen(void* command,const char* mode);
#define rfPopen(i_CMD_,i_MODE_) i_rfLMS_WRAP2(FILE*,i_rfPopen,i_CMD_,i_MODE_)
#endif

// @brief Closes a pipe
//
// This function is a cross-platform wrapper for pclose. It closes a file descriptor opened with @ref rfPopen() and
// returns the exit code of the process that was running
// @param stream The file descriptor of the pipe returned by @ref rfPopen() that we want to close
// @return Returns the exit code of the process or -1 if there was an error
i_DECLIMEX_ int rfPclose(FILE* stream);

// @} End of I/O group

#ifdef __cplusplus
}///closing bracket for calling from C++
#endif


#endif//include guards end
