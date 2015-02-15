$desc = 'tH.........
.   .
  ........
.   .
Ht.. ......

      ..
tH.... .......
      ..

      ..
tH..... ......
      ..';

$steps = 30;

//fill in the world with the cells
$world = array(array());
$row = 0;
$col = 0;
foreach(str_split($desc) as $i){
    switch($i){
        case "\n":
            $row++;
            //if($col > $width) $width = $col;
            $col = 0;
            $world[] = array();
            break;
        case '.':
            $world[$row][$col] = 1;//conductor
            $col++;
            break;
        case 'H':
            $world[$row][$col] = 2;//head
            $col++;
            break;
        case 't':
            $world[$row][$col] = 3;//tail
            $col++;
            break;
        default:
            $world[$row][$col] = 0;//insulator/air
            $col++;
            break;
    };
};
function draw_world($world){
    foreach($world as $rowc){
        foreach($rowc as $cell){
            switch($cell){
                case 0:
                    echo ' ';
                    break;
                case 1:
                    echo '.';
                    break;
                case 2:
                    echo 'H';
                    break;
                case 3:
                    echo 't';
            };
        };
        echo "\n";
    };
    //var_export($world);
};
echo "Original world:\n";
draw_world($world);
for($i = 0; $i < $steps; $i++){
    $old_world = $world; //backup to look up where was an electron head
    foreach($world as $row => &$rowc){
        foreach($rowc as $col => &$cell){
            switch($cell){
                case 2:
                    $cell = 3;
                    break;
                case 3:
                    $cell = 1;
                    break;
                case 1:
                    $neigh_heads = (int) @$old_world[$row - 1][$col - 1] == 2;
                    $neigh_heads += (int) @$old_world[$row - 1][$col] == 2;
                    $neigh_heads += (int) @$old_world[$row - 1][$col + 1] == 2;
                    $neigh_heads += (int) @$old_world[$row][$col - 1] == 2;
                    $neigh_heads += (int) @$old_world[$row][$col + 1] == 2;
                    $neigh_heads += (int) @$old_world[$row + 1][$col - 1] == 2;
                    $neigh_heads += (int) @$old_world[$row + 1][$col] == 2;
                    if($neigh_heads == 1 || $neigh_heads == 2){
                        $cell = 2;
                    };
            };
        };
        unset($cell); //just to be safe
    };
    unset($rowc); //just to be safe
    echo "\nStep " . ($i + 1) . ":\n";
    draw_world($world);
};
