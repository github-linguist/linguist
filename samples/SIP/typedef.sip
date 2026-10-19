/**
 * Copyright (c) 2020 Weitian Leung
 *
 * This file is part of pywpsrpc.
 *
 * This file is distributed under the MIT License.
 * See the LICENSE file for details.
 *
*/

typedef unsigned long ULONG;
typedef ULONG *PULONG;
typedef unsigned short USHORT;
typedef USHORT *PUSHORT;
typedef unsigned char UCHAR;
typedef UCHAR *PUCHAR;
typedef char *PSZ;

typedef void VOID;
typedef char CHAR;
typedef short SHORT;
typedef long LONG;
typedef LONG *PLONG;

typedef char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long long int64_t;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;

%MappedType intptr_t /TypeHint="sip.voidptr"/
{
%ConvertToTypeCode
    intptr_t ptr = (intptr_t)sipConvertToVoidPtr(sipPy);
    if (!sipIsErr)
        return !PyErr_Occurred();
    
    intptr_t *heap = new intptr_t;
    *heap = ptr;

    *sipCppPtr = heap;

    return SIP_TEMPORARY;
%End

%ConvertFromTypeCode
    return sipConvertFromVoidPtr((void *)*sipCpp);
%End
};

%MappedType uintptr_t /TypeHint="sip.voidptr"/
{
%ConvertToTypeCode
    uintptr_t ptr = (uintptr_t)sipConvertToVoidPtr(sipPy);
    if (!sipIsErr)
        return !PyErr_Occurred();
    
    uintptr_t *heap = new uintptr_t;
    *heap = ptr;

    *sipCppPtr = heap;

    return SIP_TEMPORARY;
%End

%ConvertFromTypeCode
    return sipConvertFromVoidPtr((void *)*sipCpp);
%End
};


typedef int8_t				int8;
typedef int16_t				int16;
typedef int32_t				int32;
typedef int64_t				int64;
typedef uint8_t				uint8;
typedef uint16_t			uint16;
typedef uint32_t			uint32;
typedef uint64_t			uint64;
typedef intptr_t			intp;
typedef uintptr_t			uintp;

typedef int8				INT8;
typedef uint8				UINT8;
typedef int16				INT16;
typedef uint16				UINT16;
typedef int32				INT32;
typedef uint32				UINT32;
typedef int64				INT64;
typedef uint64				UINT64;
typedef UINT32				DWORD;
typedef int32				BOOL;
typedef unsigned char		BYTE;
typedef uint16				WORD;
typedef float				FLOAT;
typedef double				DOUBLE;
typedef FLOAT				*PFLOAT;
typedef BOOL    			*PBOOL;
typedef BOOL    			*LPBOOL;
typedef BYTE    			*PBYTE;
typedef BYTE    			*LPBYTE;
typedef int     			*PINT;
typedef int 				*LPINT;
typedef WORD    			*PWORD;
typedef WORD    			*LPWORD;
typedef long    			*LPLONG;
typedef DWORD   			*PDWORD;
typedef DWORD   			*LPDWORD;
typedef void    			*LPVOID;
typedef const void  		*LPCVOID;

typedef int					INT;
typedef unsigned int		UINT;
typedef unsigned int		*PUINT;
typedef long long			LONGLONG;
typedef unsigned long long	ULONGLONG;
typedef DWORD				LCID;
typedef DWORD				LCTYPE;

typedef uintp				UINTP;
typedef intp				INTP;
typedef uintp				KSO_WParam;
typedef intp				KSO_LParam;

typedef int32				size32;
typedef uint32				usize32;
typedef size32				SIZE32;
typedef usize32				USIZE32;

typedef DWORD   COLORREF;
typedef DWORD*	LPCOLORREF;

typedef INT32 HRESULT;

typedef unsigned short WCHAR;

typedef WCHAR* PWCHAR;
typedef WCHAR* LPWCH;
typedef WCHAR* PWCH;
typedef WCHAR* LPWSTR;
typedef WCHAR* PWSTR;
typedef const WCHAR* PCWCHAR;
typedef const WCHAR* LPCWCH;
typedef const WCHAR* PCWCH;
typedef const WCHAR* LPCWSTR;
typedef const WCHAR* PCWSTR;

//typedef LPCWSTR LPCOLESTR;
//typedef LPWSTR LPOLESTR;

typedef CHAR *PCHAR;
typedef CHAR *LPCH;
typedef CHAR *PCH;

typedef const CHAR *LPCCH;
typedef const CHAR *PCCH;
typedef CHAR *NPSTR;
typedef CHAR *LPSTR;
typedef CHAR *PSTR;
typedef const CHAR *LPCSTR;
typedef const CHAR *PCSTR;

typedef void* PVOID;
typedef unsigned short VARTYPE;
typedef long SCODE;

// typedef short VARIANT_BOOL;
%MappedType VARIANT_BOOL /TypeHint="bool"/
{
%ConvertToTypeCode
    if (!sipIsErr)
        return PyBool_Check(sipPy) || PyLong_Check(sipPy);

    VARIANT_BOOL *boolPtr = new VARIANT_BOOL;
    if (PyBool_Check(sipPy))
        *boolPtr = sipPy == Py_True ? VARIANT_TRUE : VARIANT_FALSE;
    else // PyLong_Check
        *boolPtr = PyLong_AsLong(sipPy) == 0 ? VARIANT_FALSE : VARIANT_TRUE;

    *sipCppPtr = boolPtr;

    return SIP_TEMPORARY;
%End

%ConvertFromTypeCode
    return PyBool_FromLong(*sipCpp == VARIANT_FALSE ? 0 : 1);
%End
};