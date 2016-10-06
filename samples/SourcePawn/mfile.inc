/*  mFile 1.1 by Minokon
 *
 *  (c) Copyright by Minokon 2010
 *  (c) y_files Copyright by Y_Less 2010
 *
 */

#if defined _mfile_included
	#endinput
#endif
#define _mfile_included

//Defines
#define MAX_LINE_SIZE 80 //Max lenght of one line in file
#define MAX_KEY_SIZE 50 //Max lenght of key in line (to =)
#define MAX_VALUE_SIZE 50 //Max lenght of value in line (past =)
#define TMP_FILE "tmpfile.txt"

//Natives
/*
native mCreateFile(const file[]);
native mRemoveFile(const file[]);
native mClearFile(const file[]);
native bool:mFileExist(const file[]);
native mRenameFile(const file[], const newname[]);
native mFindFile(const name[], dest[], &index, sizename=sizeof dest);
native mCreateDir(const name[]);
native mRenameDir(const dir[], const newname[]);
native mFindDir(const name[], dest[], &index, sizename=sizeof dest);
native mHowLines(const file[]);
native mRemoveNewLine(string[]); //removes "\n" from string
native mGetString(const file[], const key[], bool:ignorecase=false);
native mGetInt(const file[], const key[], bool:ignorecase=false);
native mGetFloat(const file[], const key[], &Float:value);
native mSetString(const file[], const key[], const new_value[]);
native mSetInt(const file[], const key[], new_value);
native mSetFloat(const file[], const key[], Float:new_value);
native mIsSet(const file[], const key[], bool:ignorecase=false);
native mUnSet(const file[], const key[]);
native mIsFileEmpty(const file[]);
native y_files
*/
//y_files by Y_Less Functions
native ffind(const pattern[], filename[], len, &idx);
native frename(const oldname[], const newname[]);
native dfind(const pattern[], filename[], len, &idx);
native dcreate(const name[]);
native drename(const oldname[], const newname[]);

//Forwards
forward OnFileCreated(file[]);
forward OnFileRemoved(file[]);
forward OnFileCleared(file[]);
forward OnFileRenamed(oldname[], newname[]);
forward OnDirCreated(dir[]);
forward OnDirRenamed(oldname[], newname[]);

stock mCreateFile(const file[])
{
	if(fexist(file)) fremove(file);
	new File:mfile = fopen(file, io_write);
 	fclose(mfile);
 	CallLocalFunction("OnFileCreated", "s", file);
 	return 1;
}

stock mRemoveFile(const file[])
{
 	CallLocalFunction("OnFileRemoved", "s", file);
 	return fremove(file);
}

stock mClearFile(const file[])
{
	if(!fremove(file)) return 0;
	new File:mfile = fopen(file, io_write);
	fclose(mfile);
	CallLocalFunction("OnFileCleared", "s", file);
	return 1;
}

stock bool:mFileExist(const file[])
	if(fexist(file)) return true; //dla ciekawych: fexist nie zwraca booleanu
	else return false;

stock mRenameFile(const file[], const newname[])
{
    CallLocalFunction("OnFileRenamed", "ss", file, newname);
	return frename(file, newname);
}

stock mFindFile(const name[], dest[], &index, sizename=sizeof dest)
	return ffind(name, dest, sizename, index);

stock mCreateDir(const name[])
{
    CallLocalFunction("OnDirCreated", "s", name);
	return dcreate(name);
}

stock mRenameDir(const dir[], const newname[])
{
    CallLocalFunction("OnDirRenamed", "ss", dir, newname);
	return drename(dir, newname);
}

stock mFindDir(const name[], dest[], &index, sizename=sizeof dest)
	return dfind(name, dest, sizename, index);

stock mHowLines(const file[])
{
	new lines, str[MAX_LINE_SIZE];
	new File:mfile = fopen(file, io_read);
	while(fread(mfile, str)) lines++;
	fclose(mfile);
	return lines;
}

stock mRemoveNewLine(string[])
{
	new pos = strfind(string, "\n");
	if(pos != -1)
	{
		strdel(string, pos, pos+2);
		return 1;
	}
	return 0;
}

stock mGetString(const file[], const key[], bool:ignorecase=false)
{
	//Create file if not exists
    if(!fexist(file))
	{
	    new File:created = fopen(file, io_write);
	    fclose(created);
	}
	new //Variables
	    File:mfile = fopen(file, io_read), //Open file
	    str[MAX_LINE_SIZE],
	    str2[MAX_KEY_SIZE],
	    str3[MAX_VALUE_SIZE],
	    pos;
	//Find key in file
	while(fread(mfile, str))
	{
	    pos = strfind(str, "=", ignorecase);
	    strmid(str2, str, 0, pos);
		if(!strcmp(str2, key, ignorecase, strlen(key)+1))
		{
		    strmid(str3, str, pos+1, strlen(str));
		    mRemoveNewLine(str3);
		    break;
		}
	}
	fclose(mfile);
	return str3;
}

stock mGetInt(const file[], const key[], bool:ignorecase=false)
	return strval(mGetString(file, key, ignorecase));
	
stock mGetFloat(const file[], const key[], &Float:value, bool:ignorecase=false)
	value = floatstr(mGetString(file, key, ignorecase));

stock mSetString(const file[], const key[], const new_value[])
{
    //Create file if not exists
	if(!fexist(file))
	{
	    new File:created = fopen(file, io_write);
	    fclose(created);
	}
	new //Variables
		str[MAX_LINE_SIZE],
		str2[MAX_KEY_SIZE],
		str3[MAX_LINE_SIZE],
		bool:lFinded = false;
	//Open file
	new File:mfile = fopen(file, io_read);
	//Create cache file
	new File:tmpfile = fopen(TMP_FILE, io_write);
	fclose(tmpfile);
	//Open cache file
	tmpfile = fopen(TMP_FILE, io_append);
	format(str3, sizeof str3, "%s=%s\n", key, new_value);
	while(fread(mfile, str))
	{
	    strmid(str2, str, 0, strfind(str, "="));
	    if(!strcmp(str2, key))
	    {
			fwrite(tmpfile, str3);
			lFinded = true;
	    }
	    else
	        fwrite(tmpfile, str);
	}
	if(!lFinded) //if line not found
		fwrite(tmpfile, str3);
    //Close and remove old file
    fclose(mfile);
	fremove(file);
	//Close cache file and rename it
	fclose(tmpfile);
	frename(TMP_FILE, file);
	return 1;
}

stock mSetInt(const file[], const key[], new_value)
{
	new str[MAX_VALUE_SIZE];
	valstr(str, new_value);
	mSetString(file, key, str);
	return 1;
}

stock mSetFloat(const file[], const key[], Float:new_value)
{
	new str[MAX_VALUE_SIZE];
	format(str, sizeof str, "%f", new_value);
	return mSetString(file, key, str);
}

stock bool:mIsSet(const file[], const key[], bool:ignorecase=false)
{
    //If fexists return false
    if(!fexist(file)) return false;
	new //Variables
	    File:mfile = fopen(file, io_read), //Open file
	    str[MAX_LINE_SIZE],
	    str2[MAX_KEY_SIZE],
		bool:finded = false;
	//Find key in file
	while(fread(mfile, str))
	{
	    strmid(str2, str, 0, strfind(str, "="));
		if(!strcmp(str2, key, ignorecase, strlen(key)+1))
		{
			finded = true;
			break;
   		}
	}
	fclose(mfile);
	return finded;
}

stock mUnSet(const file[], const key[])
{
	if(!fexist(file)) return 0;
	new //Variables
		str[MAX_LINE_SIZE],
		str2[MAX_KEY_SIZE];
	//Open file
	new File:mfile = fopen(file, io_read);
	//Create cache file
	new File:tmpfile = fopen(TMP_FILE, io_write);
	fclose(tmpfile);
	//Open cache file
	tmpfile = fopen(TMP_FILE, io_append);
	while(fread(mfile, str))
	{
	    strmid(str2, str, 0, strfind(str, "="));
	    if(strcmp(str2, key) != 0)
	    	fwrite(tmpfile, str);
	}
    //Close and remove old file
    fclose(mfile);
	fremove(file);
	//Close cache file and rename it
	fclose(tmpfile);
	frename(TMP_FILE, file);
	return 1;
}

stock bool:mIsFileEmpty(const file[])
{
	if(!fexist(file)) return true;
	new File:mfile = fopen(file, io_read);
	if(flength(mfile) <= 1)
	{
	    fclose(mfile);
		return true;
	}
	else
	{
		fclose(mfile);
		return false;
	}
}
