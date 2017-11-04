// Source - https://github.com/faissaloo/GMLmenus/blob/master/GMLmenus.gml

#define draw_menu
///draw_menu(str,background,foreground,x,y,hpadding,vpadding,height,mouse_button)
//Distributed under the MIT licence:
/////////////////////////////////////////
/*Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.*/
/////////////////////////////////////////
//Height is the height of 1 box
//Menu syntax:
//  |s = seperator (Bug: if |s is placed next to a |n an extra menu item would be added)
//  |n = new item
var str, background, foreground,xx,yy, width, height;
str = " "+argument0 //A hacky thing so that it draws the first item properly, I should probably properly fix this later
background = argument1
foreground = argument2
xx = argument3
yy = argument4
width = 0//argument5
hpadding = argument5
vpadding = argument6
height = argument7
mb=argument8 //This is the main mouse button, I added this to give more choice to the dev
//xx and yy should be corrected if they are placed outside of the room or if they will lead to the menu being cut off by the edge of the room
item_list = ds_list_create()
item_string = ""
///////////////////////////////////
for (i=0;i<string_length(str);i+=1) //Parse the string, it's being set to -1 because it won't read the first character otherwise, yes this is very hacky and stupid, but I can't be bothered right now
{
if string_char_at(str,i)="|" //If it finds a | it means there will be an escape character
{
i+=1 //Move to the next character

switch(string_char_at(str,i)) //Check which escape character it is
{
case "s":
    //i+=1
    ds_list_add(item_list,"|s")
    //ds_list_add(item_list,"") //This is for the meta info

case "n":
    i+=1 //Skip the letter itself as we don't want it to be drawn
    //For some reason this is always 10
    //But it works perfectly fine if an escape code isn't the first character
    //show_message(string(i)+"<"+string(string_length(str))) //Debug
    for (ii=i;ii<=string_length(str);ii+=1)
    {
    //show_message(string_char_at(str,ii))
    if string_char_at(str,ii)="|"
    {
    //i-=1
    break; //We don't want it to go any further if the next character is a backslash
    }
    //If the for loop hasn't been broken then it'll draw the character
    item_string+=string_char_at(str,ii)
    }
    
    if string_width(item_string)>width
    {
        width=string_width(item_string)
    }
    
    
    ds_list_add(item_list,item_string) //Add a new item to the list
    item_string="" //Reset the item_string
}
}
}
draw_set_color(background)
//draw_rectangle(xx,yy,xx+width,yy+(height*ds_list_size(item_list)),false) //Background, temporary?
draw_button(xx,yy,xx+width+(hpadding*2),yy+(height*(ds_list_size(item_list)-1))+vpadding,true) //Background, temporary?

for (i=0;i<ds_list_size(item_list);i+=1) //Go through the list of menu items
{



    draw_set_color(background)
    //draw_rectangle(xx,yy+(height*i),xx+width,yy+(height),false) //Draw the rectange for this one, we're doing this so we can add cool effects to each of them
    draw_set_color(foreground)
    
    if ds_list_find_value(item_list,i)="|s"
    {  
        draw_line(xx+hpadding,yy+((height*(i+0.5))/2)+vpadding,xx+width,yy+((height*(i+0.5))/2)+vpadding) //Draw the seperator
    }
    else
    {
        //draw_text(xx+(width/10),yy+((height/2)*i),ds_list_find_value(item_list,i))
        draw_text(xx+hpadding,yy+((height/2)*i)+vpadding,ds_list_find_value(item_list,i))
        //padding
    }
    
    if (mouse_x>xx) and (mouse_x<xx+width) and (mouse_y<yy+(height/2)*(i+1)) and (mouse_y>yy) and mouse_check_button_released(mb)
    {
        //show_message(i+1) //Debugging
        return i+1; //Returns the number of the item in the list, we're adding 1 because 0 is reserved for if nothing is clicked
    }

    

}

if mouse_check_button_released(mb) and !(mouse_x>xx and mouse_y>yy and mouse_x<xx+width and mouse_y<yy+(height*(ds_list_size(item_list)-1))) //If the mouse was clicked outside
{
    return 0; //Return 0 to indicate that the user chose to exit the menu by clicking outside of it
}

return -1; //If we haven't already returned something, return a -1 to indicate that nothing was clicked

