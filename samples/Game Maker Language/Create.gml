/*
    Originally from /Source/gg2/Objects/Updater.events/Create.xml in Gang Garrison 2

    Copyright (C) 2008-2013 Faucet Software
    http://www.ganggarrison.com

    This program is free software; 
    you can redistribute it and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 3 of the License, or (at your option)
    any later version.
    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
    without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
    See the GNU General Public License for more details.
    You should have received a copy of the GNU General Public License along with this program; if not,
    see <http://www.gnu.org/licenses>.

    Additional permission under GNU GPL version 3 section 7
    If you modify this Program, or any covered work, by linking or combining it with the Game Maker runtime library, 
    the 39dll library/extension, Hobbel's Download Manager DLL, or modified versions of these libraries,
    the licensors of this Program grant you additional permission to convey the resulting work.
*/
// Downloading code. 

var downloadHandle, url, tmpfile, window_oldshowborder, window_oldfullscreen;
timeLeft = 0;
counter = 0;
AudioControlPlaySong(-1, false);
window_oldshowborder = window_get_showborder();
window_oldfullscreen = window_get_fullscreen();
window_set_fullscreen(false);
window_set_showborder(false);

if(global.updaterBetaChannel)
    url = UPDATE_SOURCE_BETA;
else
    url = UPDATE_SOURCE;

tmpfile = temp_directory + "\gg2update.zip";
    
downloadHandle = httpGet(url, -1);

while(!httpRequestStatus(downloadHandle)) 
{ // while download isn't finished
    sleep(floor(1000/30)); // sleep for the equivalent of one frame
    io_handle(); // this prevents GameMaker from appearing locked-up
    httpRequestStep(downloadHandle);
    
    // check if the user cancelled the download with the esc key
    if(keyboard_check(vk_escape)) 
    {
        httpRequestDestroy(downloadHandle);
        window_set_showborder(window_oldshowborder);
        window_set_fullscreen(window_oldfullscreen);
        room_goto_fix(Menu);
        exit;
    }
     
    if(counter == 0 || counter mod 60 == 0)
        timer = random(359)+1;
    draw_sprite(UpdaterBackgroundS,0,0,0);
    draw_set_color(c_white);
    draw_set_halign(fa_left);
    draw_set_valign(fa_center);
    minutes=floor(timer/60);
    seconds=floor(timer-minutes*60);
    draw_text(x,y-20,string(minutes) + " minutes " + string(seconds) + " seconds Remaining...");
    counter+=1;
    var progress, size;
    progress = httpRequestResponseBodyProgress(downloadHandle);
    size = httpRequestResponseBodySize(downloadHandle);
    if (size != -1)
    {
        progressBar = floor((progress/size) * 20);
        offset = 3;
        for(i=0;i<progressBar;i+=1){
            draw_sprite(UpdaterProgressS,0,x+offset,y);
            offset+=12;
        }
    }
    screen_refresh();
}
// Errored
if (httpRequestStatus(downloadHandle) == 2)
{
    show_message("Downloading update failed!#" + httpRequestError(downloadHandle));
    httpRequestDestroy(downloadHandle);
    window_set_showborder(window_oldshowborder);
    window_set_fullscreen(window_oldfullscreen);
    room_goto_fix(Menu);
    exit;
}
// Request failed
if (httpRequestStatusCode(downloadHandle) != 200)
{
    show_message("Downloading update failed!#" + string(httpRequestStatusCode(downloadHandle)) + " " + httpRequestReasonPhrase(downloadHandle));
    httpRequestDestroy(downloadHandle);
    window_set_showborder(window_oldshowborder);
    window_set_fullscreen(window_oldfullscreen);
    room_goto_fix(Menu);
    exit;
}

write_buffer_to_file(httpRequestResponseBody(downloadHandle), tmpfile);
httpRequestDestroy(downloadHandle);

if(!file_exists(tmpfile))
{
    window_set_showborder(window_oldshowborder);
    window_set_fullscreen(window_oldfullscreen);
    show_message("Error updating: Missing gg2update.zip in temp directory, download failed(?)");
    room_goto_fix(Menu);
    exit;
}

// rename existing "Gang Garrison 2.exe" to avoid conflict when extracting
if (file_exists("Gang Garrison 2.exe"))
{
    var newName, n;
    n = 1;
    
    // increment until unused name found
    do
    {
        newName = "gg2-old.delete.me." + string(n);
        n += 1;
    }
    until(!file_exists(newName));

    file_rename("Gang Garrison 2.exe", newName);
}



// let's extract the downloaded file now.
extractzip(tmpfile, working_directory);

// run new version    
execute_program("Gang Garrison 2.exe", "", false);

// exit
game_end();
