/*

    Source:https://github.com/joshring/argparse/blob/main/src/argsparse.c3

*/


module args;

import std::core::array;
import std::io;
import std::os;

faultdef 
	PARSE_FAILED,
	ARG_NOT_FOUND,
	INVALID_VALUE,
	MISSING_VALUE,
	EMPTY_SHORT_AND_LONG_NAME,
	EMPTY_DESCRIPTION,
	EXAMPLES_NOT_REQUIRED,
	NON_EMPTY_VALUE_READ,
	NON_FALSE_IS_FOUND,
	INVALID_PROGRAM_NAME,
	VALID_VALUES_ONLY_WHEN_VALUE_REQUIRED,
	NEED_VALID_VALUES_WHEN_VALUE_REQUIRED;

struct Option
{
	String[] 	valid_values; 		// for when there are only certain valid values to choose from
	String 		value_read; 		// from args
	String 		short_name;
	String 		long_name;
	String 		description;
	String 		example_value;		// used for errors if is_value_required is set
	bool   		is_required;
	bool 		is_found; 			// is the argument found in args
	bool 		is_value_required; 	// is a value for the argument found in args
}

struct ParseReturn 
{
	bool next_arg_is_value;
	Option option;
}


fn Option[]? parse(
	String[] args, 
	String arg_sep="=",
	Option[] options, 
	bool use_default_help=true,
)
{
	validate_input(options)!;
	bool next_arg_is_value = false;
	
	foreach(arg_idx, arg : args)
	{
		// program name is the first arg
		if(arg_idx == 0) 
		{
			continue;
		}
		// Skipping arg values read from when whitespace is the arg_sep as already read in prev iteration
		if(next_arg_is_value)
		{
			next_arg_is_value = false;
			continue;
		}

		// Allow overrides to this default help
		if(use_default_help && (arg == "--help" || arg == "-h"))
		{
			print_usage(options, args)!;
			return options;
		}

		foreach(&option : options)
		{	
			// Skip input which are not matching the current option
			if (!(arg.starts_with("-") && arg["-".len..].starts_with(option.short_name)) &&
				!(arg.starts_with("--") && arg["--".len..].starts_with(option.long_name))
			)
			{
				continue;
			}

			// is_short eg "-i=4"  or "-j8"
			if (option.short_name != "" && 
				!arg.starts_with("--") && 
				arg.starts_with("-") && 
				arg.len >= "-".len+option.short_name.len &&
				(arg["-".len..].starts_with(option.short_name))
			)
			{
				ParseReturn short_name_parse = parse_short_name(
					args: args,
					arg: arg,
					arg_sep: arg_sep,
					option: *option,
					arg_idx: arg_idx,
					use_default_help: use_default_help,
				)!;
				if (short_name_parse.next_arg_is_value) next_arg_is_value = true;
				*option = short_name_parse.option;
			}

			
			// is_long eg --ncpus=44
			else if (option.long_name != "" && 
				arg.starts_with("--") && 
				arg["--".len..].starts_with(option.long_name)
			)
			{
				ParseReturn long_name_parse = parse_long_name(
					args: args,
					arg: arg,
					arg_sep: arg_sep,
					option: *option,
					arg_idx: arg_idx,
					use_default_help: use_default_help,
				)!;
				if (long_name_parse.next_arg_is_value) next_arg_is_value = true;
				*option = long_name_parse.option;
			}
		}		
	}
	
	check_options_after_parse(options, arg_sep)!;
	return options;
}


fn bool? has_arg_short(Option[] options, String name)
{
	foreach(option : options)
	{
		if (name == option.short_name)
		{
			return option.is_found;
		}
	}

	io::eprintfn("Argument '%s' is not defined", name);
	return ARG_NOT_FOUND?;
}

fn bool? has_arg_long(Option[] options, String name)
{
	foreach(option : options)
	{
		if (name == option.long_name)
		{
			return option.is_found;
		}
	}

	io::eprintfn("Argument '%s' is not defined", name);
	return ARG_NOT_FOUND?;
}


fn String? arg_value(Option[] options, String name)
{
	foreach(option : options)
	{
		if (name == option.long_name || name == option.short_name)
		{
			return option.value_read;
		}
	}

	io::eprintfn("Argument '%s' is not defined", name);
	return ARG_NOT_FOUND?;
}


fn void? validate_input(Option[] options)
{
	foreach(option : options)
	{
		if(option.short_name == "")
		{
			if(option.long_name == "")
			{
				return EMPTY_SHORT_AND_LONG_NAME?;
			}
		}
		
		if(option.description == "")
		{
			return EMPTY_DESCRIPTION?;
		}
		
		// can't have examples when a value isn't required
		if(option.valid_values.len > 0 && !option.is_value_required)
		{
			return VALID_VALUES_ONLY_WHEN_VALUE_REQUIRED?;
		}
		
		if(option.value_read != "")
		{
			return NON_EMPTY_VALUE_READ?;
		}
		
		if(option.is_found == true)
		{
			return NON_FALSE_IS_FOUND?;
		}
			
	}
	return;
}




fn ParseReturn? parse_short_name(
	String[] args,
	String arg,
	String arg_sep="=",
	Option option, 
	usz arg_idx,
	bool use_default_help=true,
)
{
	bool next_arg_is_value;
	
	// args are split on whitespace so check the next argument, if it exists
	// TODO: do we need to cope with values == " " from multiple whitespace chars?
	if (arg_sep == " ")
	{					
		// Check for overflow if next element does not exist, 
		// We skip the first arg as that's the filename, hence +2
		// {0,1,2,3}	// len=4, maxindex=3
		if(arg_idx+2 > args.len)
		{
			option.is_found = true;
			return { .option=option };
		}
		
		// Skip next value because it will be the value for this arg
		if (option.is_value_required)
		{
			// Next arg should be a value
			next_arg_is_value = true;
			
			// Read the value after the separator and before the next whitespace
			if (option.valid_values.len > 0)
			{
				if (array::contains(option.valid_values, args[arg_idx+1]))
				{
					option.value_read = args[arg_idx+1];
					option.is_found = true;
					return { .option=option, .next_arg_is_value=next_arg_is_value };
				}
				else
				{
					io::eprintfn("Invalid value: '%s' valid values: %s", args[arg_idx+1], option.valid_values);
					return INVALID_VALUE?;
				}
			}
			else
			{
				option.value_read = args[arg_idx+1];
				option.is_found = true;
				return { .option=option, .next_arg_is_value=next_arg_is_value };
			}
		}
		// Argument without value
		else
		{
			option.is_found = true;
			return { .option=option };
		}	
	}
	// Non-whitespace arg_sep, eg --n=44
	else if (arg_sep != " ")
	{
		// End of String, No value to read into: value_read
		if (arg.len == (option.short_name.len + "-".len))
		{
			option.is_found = true;
			return { .option=option };
		}
	
		// Split the arg and it's value
		if (option.is_value_required)
		{
			usz sep_end = "-".len + option.short_name.len + arg_sep.len;
			
			// Looking for separator would overflow or reach the end
			if (sep_end >= arg.len)
			{
				return { .option=option };
			}
			
			String value_read = arg[sep_end..];
			
			// Read the value after the separator and before the next whitespace
			if (option.valid_values.len > 0)
			{
				if (array::contains(option.valid_values, value_read))
				{
					option.value_read = value_read;
					option.is_found = true;
					return { .option=option };
				}
				else
				{
					io::eprintfn("Invalid value: '%s' valid values: %s", value_read, option.valid_values);
					return INVALID_VALUE?;
				}
			}
			else
			{
				option.value_read = value_read;
				option.is_found = true;
				return { .option=option };
			}
		}
		// Argument without value
		else
		{
			option.is_found = true;
			return { .option=option };
		}
	}
	
	return { .option=option };
}


fn ParseReturn? parse_long_name(
	String[] args,
	String arg,
	String arg_sep="=",
	Option option, 
	usz arg_idx,
	bool use_default_help=true,
)
{		
	bool next_arg_is_value;
	
	// args are split on whitespace so check the next argument, if it exists
	if(arg_sep == " ")
	{
		// Check for overflow if next element does not exist, 
		// We skip the first arg as that's the filename, hence +2
		// {0,1,2,3}	// len=4, maxindex=3
		if(arg_idx+2 > args.len)
		{
			option.is_found = true;
			return { .option=option };
		}
		
		// Skip next value because it will be the value for this arg
		if (option.is_value_required)
		{
			// Next arg should be a value
			next_arg_is_value = true;						
			
			// Read the value after the separator and before the next whitespace
			if(option.valid_values.len > 0)
			{
				if(array::contains(option.valid_values, args[arg_idx+1]))
				{
					option.value_read = args[arg_idx+1];
					option.is_found = true;
					return { .option=option, .next_arg_is_value=next_arg_is_value };
				}
				else
				{
					io::eprintfn("Invalid value: '%s' valid values: %s", args[arg_idx+1], option.valid_values);
					return INVALID_VALUE?;
				}
			}
			else
			{
				option.value_read = args[arg_idx+1];
				option.is_found = true;
				return { .option=option, .next_arg_is_value=next_arg_is_value };
			}	
		}
		// Argument without value
		else
		{
			option.is_found = true;
			return { .option=option };
		}	
	}
		
	// Non-whitespace arg_sep, eg --ncpus=44
	else if(arg_sep != " ")
	{
		usz sep_end = "--".len + option.long_name.len + arg_sep.len;
		
		// Looking for separator would overflow or reach the end
		if(sep_end >= arg.len)
		{
			option.is_found = true;
			return { .option=option };
		}
		
		if (option.is_value_required)
		{
			String value_read = arg[sep_end..];
			
			// Read the value after the separator and before the next whitespace
			if(option.valid_values.len > 0)
			{
				if(array::contains(option.valid_values, value_read))
				{
					option.value_read = value_read;
					option.is_found = true;
					return { .option=option };
				}
				else
				{
					io::eprintfn("Invalid value: '%s' valid values: %s", value_read, option.valid_values);
					return INVALID_VALUE?;
				}
			}
			else
			{
				option.value_read = value_read;
				option.is_found = true;
				return { .option=option };
			}
		}
		// Argument without value
		else
		{
			option.is_found = true;
			return { .option=option };
		}
		
	}
	return { .option=option };
}


fn void? check_options_after_parse(
	Option[] options,
	String arg_sep,
)
{
	foreach(option : options)
	{
		if(option.is_required && !option.is_found)
		{
			if(option.short_name != "" && option.long_name != "")
			{
				io::eprintfn("Option '--%s' or '-%s' is a required argument", option.long_name, option.short_name);
			}
			else if(option.short_name != "")
			{
				io::eprintfn("Option '-%s' is a required argument", option.short_name);
			}
			else if(option.long_name != "")
			{
				io::eprintfn("Option '--%s' is a required argument", option.long_name);
			}
			
			return PARSE_FAILED?;
		}
		
		if(option.is_required && option.is_value_required && option.value_read == "")
		{	
			if(option.short_name != "" && option.long_name != "")
			{
				io::eprintfn("Option '--%s' or '-%s' must have a value eg '--%s%s%s' or '-%s%s%s'", 
					option.long_name, 
					option.short_name,
					option.long_name,
					arg_sep,
					option.example_value != "" ? option.example_value : "VALUE",
					option.short_name,
					arg_sep,
					option.example_value != "" ? option.example_value : "VALUE",
				);
			}
			else if(option.short_name != "")
			{
				io::eprintfn("Option '-%s' must have a value eg '-%s%s%s'", 
					option.short_name, 
					option.short_name,
					arg_sep,
					option.example_value != "" ? option.example_value : "VALUE",
				);
			}
			else if(option.long_name != "")
			{
				io::eprintfn("Option '--%s' must have a value eg '--%s%s%s'", 
					option.long_name, 
					option.long_name,
					arg_sep,
					option.example_value != "" ? option.example_value : "VALUE",
				);
			}
			
			return PARSE_FAILED?;
		}		
	}
	return;
}


fn String? get_program_name(String[] args)
{
	if(args[0].len == 0)
	{
		return INVALID_PROGRAM_NAME?;
	}
	
	String[] program_split;
	if(args[0].contains("/"))
	{
		program_split = args[0].tsplit("/");
	}
	else if(args[0].contains("\\"))
	{
		program_split = args[0].tsplit("\\");
	}
	else 
	{
		return INVALID_PROGRAM_NAME?;
	}
	
	return program_split[^1];
}


fn void? print_usage(Option[] options, String[] args)
{
	String program_name = get_program_name(args)!;
	
	io::printfn("Usage: %s [OPTIONS]\nOptions:", program_name);

	foreach(option: options)
	{
		if (option.short_name && option.long_name)
		{
			io::printfn("  -%s, --%-10s\t\t%s", option.short_name, option.long_name, option.description);
		} 
		else if (option.long_name)
		{
			io::printfn("      --%-10s\t\t%s", option.long_name, option.description);
		}
		else if (option.short_name)
		{
			io::printfn("  -%s        \t\t\t%s", option.short_name, option.description);
		}
	}
}