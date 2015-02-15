error_reporting(E_ALL & ~ ( E_NOTICE | E_WARNING ));

define('MAXSCORE', 100);
define('PLAYERCOUNT', 2);

$confirm = array('Y', 'y', '');

while (true) {
    printf(' Player %d: (%d, %d) Rolling? (Yn) ', $player,
            $safeScore[$player], $score);
    if ($safeScore[$player] + $score < MAXSCORE &&
            in_array(trim(fgets(STDIN)), $confirm)) {
        $rolled = rand(1, 6);
        echo " Rolled $rolled \n";
        if ($rolled == 1) {
            printf(' Bust! You lose %d but keep %d \n\n',
                    $score, $safeScore[$player]);
        } else {
            $score += $rolled;
            continue;
        }
    } else {
        $safeScore[$player] += $score;
        if ($safeScore[$player] >= MAXSCORE)
            break;
        echo ' Sticking with ', $safeScore[$player], '\n\n';
    }
    $score = 0;
    $player = ($player + 1) % PLAYERCOUNT;
}
printf('\n\nPlayer %d wins with a score of %d ',
    $player, $safeScore[$player]);
