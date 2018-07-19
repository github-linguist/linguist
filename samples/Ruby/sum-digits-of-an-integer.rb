>> def sumDigits(num, base = 10)
>>     num.to_s(base).split(//).inject(0) {|z, x| z + x.to_i(base)}
>> end
=> nil
>> sumDigits(1)
=> 1
>> sumDigits(12345)
=> 15
>> sumDigits(123045)
=> 15
>> sumDigits(0xfe, 16)
=> 29
>> sumDigits(0xf0e, 16)
=> 29
