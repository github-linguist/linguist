include fsl-util.f

 3 3 float matrix A{{
 A{{ 3 3 }}fread  1e 2e 3e  4e 5e 6e  7e 8e 9e
 3 3 float matrix B{{
 B{{ 3 3 }}fread  3e 3e 3e  2e 2e 2e  1e 1e 1e
 3 3 float matrix C{{    \ result

 A{{ B{{ C{{ mat*
 C{{ }}print
