%% Returns a list of lines.
%% Text: an instance of Open.text (a mixin class)
fun {ReadAll Text}
   case {Text getS($)} of false then nil
   [] Line then Line|{ReadAll Text}
   end
end
