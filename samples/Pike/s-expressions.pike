class Symbol(string name)
{
    string _sprintf(int type)
    {
        switch(type)
        {
            case 's': return name;
            case 'O': return sprintf("(Symbol: %s)", name||"");
            case 'q': return name;
            case 't': return "Symbol";
            default:  return sprintf("%"+int2char(type), name);
        }
    }

    mixed cast(string type)
    {
        switch(type)
        {
            case "string": return name;
            default: throw(sprintf("can not cast 'Symbol' to '%s'", type));
        }
    }
}

mixed value(string token)
{
    if ((string)(int)token==token)
        return (int)token;
    array result = array_sscanf(token, "%f%s");
    if (sizeof(result) && floatp(result[0]) && ! sizeof(result[1]))
        return result[0];
    else
        return Symbol(token);
}

array tokenizer(string input)
{
    array output = ({});
    for(int i=0; i<sizeof(input); i++)
    {
        switch(input[i])
        {
            case '(': output+= ({"("}); break;
            case ')': output += ({")"}); break;
            case '"': //"
                      output+=array_sscanf(input[++i..], "%s\"%[ \t\n]")[0..0];
                      i+=sizeof(output[-1]);
                      break;
            case ' ':
            case '\t':
            case '\n': break;
            default: string token = array_sscanf(input[i..], "%s%[) \t\n]")[0];
                     output+=({ value(token) });
                     i+=sizeof(token)-1;
                     break;
        }
    }
    return output;
}

// this function is based on the logic in Parser.C.group() in the pike library;
array group(array tokens)
{
    ADT.Stack stack=ADT.Stack();
    array ret =({});

    foreach(tokens;; string token)
    {
        switch(token)
        {
            case "(": stack->push(ret); ret=({}); break;
            case ")":
                    if (!sizeof(ret) || !stack->ptr)
                    {
                      // Mismatch
                        werror ("unmatched close parenthesis\n");
                        return ret;
                    }
                    ret=stack->pop()+({ ret });
                    break;
            default: ret+=({token}); break;
        }
    }
    return ret;
}

string sexp(array input)
{
    array output = ({});
    foreach(input;; mixed item)
    {
        if (arrayp(item))
            output += ({ sexp(item) });
        else if (intp(item))
            output += ({ sprintf("%d", item) });
        else if (floatp(item))
            output += ({ sprintf("%f", item) });
        else
            output += ({ sprintf("%q", item) });
    }
    return "("+output*" "+")";
}

string input = "((data \"quoted data\" 123 4.5)\n (data (!@# (4.5) \"(more\" \"data)\")))";
array data = group(tokenizer(input))[0];
string output = sexp(data);
