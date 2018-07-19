class LZW
{
    function compress($unc) {
        $i;$c;$wc;
        $w = "";
        $dictionary = array();
        $result = array();
        $dictSize = 256;
        for ($i = 0; $i < 256; $i += 1) {
            $dictionary[chr($i)] = $i;
        }
        for ($i = 0; $i < strlen($unc); $i++) {
            $c = $unc[$i];
            if (property_exists($dictionary, $w.$c)) {
                $w = $w.$c;
            } else {
                array_push($result,$dictionary[$w]);
                $dictionary[$wc] = $dictSize++;
                $w = (string)$c;
            }
        }
        if ($w !== "") {
            array_push($result,$dictionary[$w]);
        }
        array_shift($result);
        return implode(",",$result);
    }

    function decompress($com) {
        $com = explode(",",$com);
        $i;$w;$k;$result;
        $dictionary = array();
        $entry = "";
        $dictSize = 256;
        for ($i = 0; $i < 256; $i++) {
            $dictionary[$i] = chr($i);
        }
        $w = chr($com[0]);
        $result = $w;
        for ($i = 1; $i < count($com);$i++) {
            $k = $com[$i];
            if ($dictionary[$k]) {
                $entry = $dictionary[$k];
            } else {
                if ($k === $dictSize) {
                    $entry = $w.$w[0];
                } else {
                    return null;
                }
            }
            $result .= $entry;
            $dictionary[$dictSize++] = $w + $entry[0];
            $w = $entry;
        }
        return $result;
    }
}

//How to use
$str = 'TOBEORNOTTOBEORTOBEORNOT';
$lzw = new LZW();
$com = $lzw->compress($str);
$dec = $lzw->decompress($com);
echo $com . "<br>" . $dec;
