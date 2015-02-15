var justification="center",
input=["Given$a$text$file$of$many$lines,$where$fields$within$a$line$",
"are$delineated$by$a$single$'dollar'$character,$write$a$program",
"that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$",
"column$are$separated$by$at$least$one$space.",
"Further,$allow$for$each$word$in$a$column$to$be$either$left$",
"justified,$right$justified,$or$center$justified$within$its$column."],
x,y,cols,max,cols=0,diff,left,right

String.prototype.repeat=function(n){return new Array(1 + parseInt(n)).join(this);}

for(x=0;x<input.length;x++) {
 input[x]=input[x].split("$");
 if(input[x].length>cols) cols=input[x].length;
}
for(x=0;x<cols;x++) {
 max=0;
 for(y=0;y<input.length;y++) if(input[y][x]&&max<input[y][x].length) max=input[y][x].length;
 for(y=0;y<input.length;y++)
  if(input[y][x]) {
   diff=(max-input[y][x].length)/2;
   left=" ".repeat(Math.floor(diff));
   right=" ".repeat(Math.ceil(diff));
   if(justification=="left") {right+=left;left=""}
   if(justification=="right") {left+=right;right=""}
   input[y][x]=left+input[y][x]+right;
  }
}
for(x=0;x<input.length;x++) input[x]=input[x].join(" ");
input=input.join("\n");
document.write(input);
