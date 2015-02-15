#include <stdio.h>
#include <string.h>
#include <curl/curl.h>
#include <sys/types.h>
#include <regex.h>

#define BUFSIZE 16384

size_t lr = 0;

size_t filterit(void *ptr, size_t size, size_t nmemb, void *stream)
{
  if ( (lr + size*nmemb) > BUFSIZE ) return BUFSIZE;
  memcpy(stream+lr, ptr, size*nmemb);
  lr += size*nmemb;
  return size*nmemb;
}

int main()
{
  CURL *curlHandle;
  char buffer[BUFSIZE];
  regmatch_t amatch;
  regex_t cregex;

  curlHandle = curl_easy_init();
  curl_easy_setopt(curlHandle, CURLOPT_URL, "http://tycho.usno.navy.mil/cgi-bin/timer.pl");
  curl_easy_setopt(curlHandle, CURLOPT_FOLLOWLOCATION, 1);
  curl_easy_setopt(curlHandle, CURLOPT_WRITEFUNCTION, filterit);
  curl_easy_setopt(curlHandle, CURLOPT_WRITEDATA, buffer);
  int success = curl_easy_perform(curlHandle);
  curl_easy_cleanup(curlHandle);

  buffer[lr] = 0;

  regcomp(&cregex, " UTC", REG_NEWLINE);
  regexec(&cregex, buffer, 1, &amatch, 0);
  int bi = amatch.rm_so;
  while ( bi-- > 0 )
    if ( memcmp(&buffer[bi], "<BR>", 4) == 0 ) break;

  buffer[amatch.rm_eo] = 0;

  printf("%s\n", &buffer[bi+4]);

  regfree(&cregex);
  return 0;
}
