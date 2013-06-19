int main()
{
  int x;
  chan c;
  par {
    c <: 0;
    c :> x;
  }
  return x;
}
