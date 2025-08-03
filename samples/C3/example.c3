/*

Source: https://github.com/joshring/argparse/blob/main/src/example.c3

*/

module main;
import std::io;
import args;
import args::testutil;



// Call with arguments
// --required  true -d "description here"
fn void main(String[] args)
{
	io::printfn(
		"\nArgs provided to the program\n"
		"============================="
	);
	foreach(arg : args)
	{
		io::printn(arg);
	}

		
	Option[]? options_read = args::parse(
		args: args,
		arg_sep: " ",
		// arg_sep: "=",
		options: {
			// A long and short name, with a required associated value
			{
				.valid_values={"true", "false"},
				.short_name="r",
				.long_name="required",
				.description="required arg",
				.example_value="EXAMPLE",
				.is_required=true,
				.is_value_required=true,
			},
			// An optional, long and short name, with a required associated value (when provided)
			{
				.valid_values={},
				.short_name="d",
				.long_name="description",
				.description="optional description",
				.example_value=`"description here"`,
				.is_required=false,
				.is_value_required=true,
			},
			// A single value toggle without an associated value
			{
				.short_name="n",
				.long_name="not_required",
				.description="optional arg",
				.example_value="",
				.is_required=false,
				.is_value_required=false,
			},
		}
	);
	if(catch err = options_read)
	{
		io::printn(err);
		return;
	}
	
	io::printfn(
		"\nOptions parsed by the program\n"
		"============================="
	);
	foreach(option : options_read)
	{
		io::printn(option);
		io::printn();
	}
}


fn void test_parse() @test
{
	String[] args = {"./build/argparse", "--required", "true", "-n", "-v", "88"};

	Option[]? options = args::parse(
		args: args,
		arg_sep: " ",
		options: {
			// Value required and has valid values
			{
				.valid_values={"true", "false"},
				.short_name="r",
				.long_name="required",
				.description="required arg",
				.example_value="EXAMPLE",
				.is_required=true,
				.is_value_required=true,
			},
			// Flag without a value
			{
				.short_name="n",
				.long_name="not_required",
				.description="flag argument without a value",
				.example_value="",
				.is_required=false,
				.is_value_required=false,
			},
			// Optional value, value required and has no valid values
			{
				.valid_values={},
				.short_name="v",
				.long_name="value",
				.description="optional, requires value if defined",
				.example_value="88",
				.is_required=false,
				.is_value_required=true,
			},
		}
	);
	if(catch err = options)
	{
		io::printn(err);
		return;
	}
	
	Option[] options_expected = {
		{ 
			.is_found=true, 
			.value_read="true", 
			
			.valid_values={"true", "false"}, 
			.short_name="r", 
			.long_name="required", 
			.description="required arg",
			.example_value="EXAMPLE", 
			.is_required=true, 
			.is_value_required=true, 
		}, 
		{ 
			.is_found=true, 
			.value_read="true", 
			
			.valid_values={}, 
			.short_name="n", 
			.long_name="not_required", 
			.description="flag argument without a value",
			.example_value="", 
			.is_required=false, 
			.is_value_required=false, 
		},
		{ 
			.is_found=true, 
			.value_read="88", 
			
			.valid_values={}, 
			.short_name="v", 
			.long_name="value", 
			.description="optional, requires value if defined",
			.example_value="88", 
			.is_required=false, 
			.is_value_required=true, 
		}
	};

	assert(testutil::equal_value_io(options, options_expected)!! == true);
}