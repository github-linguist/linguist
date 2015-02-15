<?php
function longMult($a, $b)
{
  $as = (string) $a;
  $bs = (string) $b;
  for($pi = 0, $ai = strlen($as) - 1; $ai >= 0; $pi++, $ai--)
    {
      for($p = 0; $p < $pi; $p++)
        {
          $regi[$ai][] = 0;
        }
      for($bi = strlen($bs) - 1; $bi >= 0; $bi--)
        {
          $regi[$ai][] = $as[$ai] * $bs[$bi];
        }
    }
  return $regi;
}

function longAdd($arr)
{
  $outer = count($arr);
  $inner = count($arr[$outer-1]) + $outer;
  for($i = 0; $i <= $inner; $i++)
    {
      for($o = 0; $o < $outer; $o++)
        {
          $val  = isset($arr[$o][$i]) ? $arr[$o][$i] : 0;
          @$sum[$i] += $val;
        }
    }
  return $sum;
}

function carry($arr)
{
  for($i = 0; $i < count($arr); $i++)
    {
      $s = (string) $arr[$i];
      switch(strlen($s))
        {
          case 2:
            $arr[$i] = $s{1};
            @$arr[$i+1] += $s{0};
            break;
          case 3:
            $arr[$i] = $s{2};
            @$arr[$i+1] += $s{0}.$s{1};
            break;
        }
    }
  return ltrim(implode('',array_reverse($arr)),'0');
}

function lm($a,$b)
{
  return carry(longAdd(longMult($a,$b)));
}

if(lm('18446744073709551616','18446744073709551616') == '340282366920938463463374607431768211456')
  {
    echo 'pass!';
  }; // 2^64 * 2^64
