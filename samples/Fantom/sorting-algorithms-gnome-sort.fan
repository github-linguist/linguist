class Main
{
  Int[] gnomesort (Int[] list)
  {
    i := 1
    j := 2
    while (i < list.size)
    {
      if (list[i-1] <= list[i])
      {
        i = j
        j += 1
      }
      else
      {
        list.swap(i-1, i)
        i -= 1
        if (i == 0)
        {
          i = j
          j += 1
        }
      }
    }

    return list
  }

  Void main ()
  {
    list := [4,1,5,8,2,1,5,7]
    echo ("" + list + " sorted is " + gnomesort (list))
  }
}
