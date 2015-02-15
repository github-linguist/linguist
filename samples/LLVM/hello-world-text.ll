@str = internal constant [14 x i8] c"Hello, world!\00"
declare i32 @puts(i8*)
define i32 @main()
{
  call i32 @puts( i8* getelementptr ([14 x i8]* @str, i32 0,i32 0))
  ret i32 0
}
