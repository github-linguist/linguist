move(n, from, to, via)  ;n = # of disks, from = start pole, to = end pole, via = remaining pole
{
  if (n = 1)
  {
    msgbox , Move disk from pole %from% to pole %to%
  }
  else
  {
    move(n-1, from, via, to)
    move(1, from, to, via)
    move(n-1, via, to, from)
  }
}
move(64, 1, 3, 2)
