#include <fstream>
#include <iostream>
#include <ctime>
using namespace std;

#define note_file "NOTES.TXT"

int main(int argc, char **argv)
{
	if(argc>1)
	{
		ofstream Notes(note_file, ios::app);
		time_t timer = time(NULL);
		if(Notes.is_open())
		{
			Notes << asctime(localtime(&timer)) << '\t';
			for(int i=1;i<argc;i++)
				Notes << argv[i] << ' ';
			Notes << endl;
			Notes.close();
		}
	}
	else
	{
		ifstream Notes(note_file, ios::in);
		string line;
		if(Notes.is_open())
		{
			while(!Notes.eof())
			{
				getline(Notes, line);
				cout << line << endl;
			}
			Notes.close();
		}
	}
}
