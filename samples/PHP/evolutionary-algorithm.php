define('TARGET','METHINKS IT IS LIKE A WEASEL');
define('TBL','ABCDEFGHIJKLMNOPQRSTUVWXYZ ');

define('MUTATE',15);
define('COPIES',30);
define('TARGET_COUNT',strlen(TARGET));
define('TBL_COUNT',strlen(TBL));

// Determine number of different chars between a and b

function unfitness($a,$b)
{
        $sum=0;
        for($i=0;$i<strlen($a);$i++)
                if($a[$i]!=$b[$i]) $sum++;
        return($sum);
}

function mutate($a)
{
        $tbl=TBL;
        for($i=0;$i<strlen($a);$i++) $out[$i]=mt_rand(0,MUTATE)?$a[$i]:$tbl[mt_rand(0,TBL_COUNT-1)];
        return(implode('',$out));
}


$tbl=TBL;
for($i=0;$i<TARGET_COUNT;$i++) $tspec[$i]=$tbl[mt_rand(0,TBL_COUNT-1)];
$parent[0]=implode('',$tspec);
$best=TARGET_COUNT+1;
$iters=0;
do {
        for($i=1;$i<COPIES;$i++) $parent[$i]=mutate($parent[0]);

        for($best_i=$i=0; $i<COPIES;$i++) {
                $unfit=unfitness(TARGET,$parent[$i]);
                if($unfit < $best || !$i) {
                        $best=$unfit;
                        $best_i=$i;
                }
        }
        if($best_i>0) $parent[0]=$parent[$best_i];
        $iters++;
        print("Generation $iters, score $best: $parent[0]\n");
} while($best);
