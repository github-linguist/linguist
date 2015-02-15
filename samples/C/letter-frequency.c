/* declare array */
int frequency[26];
int ch;
FILE* txt_file = fopen ("a_text_file.txt", "rt");

/* init the freq table: */
for (ch = 0; ch < 26; ch++)
    frequency[ch] = 0;

while (1) {
    ch = fgetc(txt_file);
    if (ch == EOF) break; /* end of file or read error.  EOF is typically -1 */

    /* assuming ASCII; "letters" means "a to z" */
    if ('a' <= ch && ch <= 'z')      /* lower case */
        frequency[ch-'a']++;
    else if ('A' <= ch && ch <= 'Z') /* upper case */
        frequency[ch-'A']++;
}
