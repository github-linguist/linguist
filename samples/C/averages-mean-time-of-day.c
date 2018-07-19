/*29th August, 2012
Abhishek Ghosh*/

#include<stdlib.h>
#include<math.h>
#include<stdio.h>

typedef struct
{
  int hour, minute, second;
} digitime;

double
timeToDegrees (digitime time)
{
  return (360 * time.hour / 24.0 + 360 * time.minute / (24 * 60.0) +
          360 * time.second / (24 * 3600.0));
}

digitime
timeFromDegrees (double angle)
{
  digitime d;
  double totalSeconds = 24 * 60 * 60 * angle / 360;

  d.second = (int) totalSeconds % 60;
  d.minute = ((int) totalSeconds % 3600 - d.second) / 60;
  d.hour = (int) totalSeconds / 3600;

  return d;
}

double
meanAngle (double *angles, int size)
{
  double y_part = 0, x_part = 0;
  int i;

  for (i = 0; i < size; i++)
    {
      x_part += cos (angles[i] * M_PI / 180);
      y_part += sin (angles[i] * M_PI / 180);
    }

  return atan2 (y_part / size, x_part / size) * 180 / M_PI;
}

int
main ()
{
  digitime *set, meanTime;
  int inputs, i;
  double *angleSet, angleMean;

  printf ("Enter number of inputs : ");
  scanf ("%d", &inputs);
  set = malloc (inputs * sizeof (digitime));
  angleSet = malloc (inputs * sizeof (double));
  printf ("\n\nEnter the data separated by a space between each unit : ");

  for (i = 0; i < inputs; i++)
    {
      scanf ("%d:%d:%d", &set[i].hour, &set[i].minute, &set[i].second);
      angleSet[i] = timeToDegrees (set[i]);
    }

  meanTime = timeFromDegrees (360 + meanAngle (angleSet, inputs));

  printf ("\n\nThe mean time is : %d:%d:%d", meanTime.hour, meanTime.minute,
          meanTime.second);
  return 0;
}
