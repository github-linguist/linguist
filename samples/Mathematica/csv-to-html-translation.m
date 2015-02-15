a="Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah;he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother;that's who!
The multitude,Behold his mother! Behold his mother!";
(*Naive*)
StringJoin["<table>\n",Map[StringJoin["<tr><td>",#,"</td></tr>\n"]&,
StringSplit[StringReplace[a,{","->"</td><td>","<"->"&lt;",">"->"&gt;"}],"\n"]]
,"</table>"]
(*Extra*)
StringJoin["<table>\n",StringJoin["<tr><th>",#,"</th></tr>\n"]&[
StringSplit[StringReplace[a,{","->"</td><td>","<"->"&lt;",">"->"&gt;"}],"\n"]//First]
,Map[StringJoin["<tr><td>",#,"</td></tr>\n"]&,
StringSplit[StringReplace[a,{","->"</td><td>","<"->"&lt;",">"->"&gt;"}],"\n"]//Rest]
,"</table>"]

Output:

<table>
<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah;he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother;that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>

<table>
<tr><th>Character</td><td>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah;he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother;that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
