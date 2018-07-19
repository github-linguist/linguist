Windows PowerShell
Copyright (C) 2009 Microsoft Corporation. All rights reserved.

PS Home:\> function f ([string] $string1, [string] $string2, [string] $separator) {
>>     $string1 + $separator * 2 + $string2
>> }
>>
PS Home:\> f 'Rosetta' 'Code' ':'
Rosetta::Code
PS Home:\>
