# Array.make 6 'A' ;;
- : char array = [|'A'; 'A'; 'A'; 'A'; 'A'; 'A'|]

# Array.init 8 (fun i -> i * 10) ;;
- : int array = [|0; 10; 20; 30; 40; 50; 60; 70|]

# let arr = [|0; 1; 2; 3; 4; 5; 6 |] ;;
val arr : int array = [|0; 1; 2; 3; 4; 5; 6|]

# arr.(4) ;;
- : int = 4

# arr.(4) <- 65 ;;
- : unit = ()

# arr ;;
- : int array = [|0; 1; 2; 3; 65; 5; 6|]
