.bp
.P1 MEMORY
.PP
The EM machine has two distinct address spaces,
one for instructions and one for data.
The data space is divided up into 8-bit bytes.
The smallest addressable unit is a byte.
Bytes are numbered consecutively from 0 to some maximum.
All sizes in EM are expressed in bytes.
.PP
Some EM instructions can transfer objects containing several bytes
to and/or from memory.
The size of all objects larger than a word must be a multiple of
the wordsize.
The size of all objects smaller than a word must be a divisor
of the wordsize.
For example: if the wordsize is 2 bytes, objects of the sizes 1,
2, 4, 6,... are allowed.
The address of such an object is the lowest address of all bytes it contains.
For objects smaller than the wordsize, the
address must be a multiple of the object size.
For all other objects the address must be a multiple of the
wordsize.
For example, if an instruction transfers a 4-byte object to memory at
location \fIm\fP and the wordsize is 2,
\fIm\fP must be a multiple of 2 and the bytes at
locations \fIm\fP, \fIm\fP\|+\|1,\fIm\fP\|+\|2 and
\fIm\fP\|+\|3 are overwritten.
.PP
The size of almost all objects in EM
is an integral number of words.
Only two operations are allowed on
objects whose size is a divisor of the wordsize:
push it onto the stack and pop it from the stack.
The addressing of these objects in memory is always indirect.
If such a small object is pushed onto the stack
it is assumed to be a small integer and stored
in the least significant part of a word.
The rest of the word is cleared to zero,
although
EM provides a way to sign-extend a small integer.
Popping a small object from the stack removes a word
from the stack, stores the least significant byte(s)
of this word in memory and discards the rest of the word.
.PP
The format of pointers into both address spaces is explicitly undefined.
The size of a pointer, however, is fixed for a member of EM, so that
the compiler writer knows how much storage to allocate for a pointer.
.PP
A minor problem is raised by the undefined pointer format.
Some languages, notably Pascal, require a special,
otherwise illegal, pointer value to represent the nil pointer.
The current Pascal-VU compiler uses the
integer value 0 as nil pointer.
This value is also used by many C programs as a normally impossible address.
A better solution would be to have a special
instruction loading an illegal pointer value,
but it is hard to imagine an implementation
for which the current solution is inadequate,
especially because the first word in the EM data space
is special and probably not the target of any pointer.
.PP
The next two chapters describe the EM memory
in more detail.
One describes the instruction address space,
the other the data address space.
.PP
A design goal of EM has been to allow
its implementation on a wide range of existing machines,
as well as allowing a new one to be built in hardware.
To this extent we have tried to minimize the demands
of EM on the memory structure of the target machine.
Therefore, apart from the logical partitioning,
EM memory is divided into 'fragments'.
A fragment consists of consecutive machine
words and has a base address and a size.
Pointer arithmetic is only defined within a fragment.
The only exception to this rule is comparison with the null
pointer.
All fragments must be word aligned.