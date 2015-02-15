>> class Example
>>   private
>>   def name
>>     "secret"
>>   end
>> end
=> nil
>> example = Example.new
=> #<Example:0x101308408>
>> example.name
NoMethodError: private method `name' called for #<Example:0x101308408>
	from (irb):10
	from :0
>> example.send(:name)
=> "secret"
