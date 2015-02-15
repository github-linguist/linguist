echo 'with Ada.text_IO; use Ada.text_IO; procedure X is begin Put("Hello!"); end X;' > x.adb; gnatmake x; ./x; rm x.adb x.ali x.o x
