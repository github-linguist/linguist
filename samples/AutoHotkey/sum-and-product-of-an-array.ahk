numbers = 1,2,3,4,5
product := 1
loop, parse, numbers, `,
{
sum += A_LoopField
product *= A_LoopField
}
msgbox, sum = %sum%`nproduct = %product%
