<?php
function caesarEncode( $message, $key ){
    $plaintext = strtolower( $message );
    $ciphertext = "";
    $ascii_a = ord( 'a' );
    $ascii_z = ord( 'z' );
    while( strlen( $plaintext ) ){
        $char = ord( $plaintext );
        if( $char >= $ascii_a && $char <= $ascii_z ){
            $char = ( ( $key + $char - $ascii_a ) % 26 ) + $ascii_a;
        }
        $plaintext = substr( $plaintext, 1 );
        $ciphertext .= chr( $char );
    }
    return $ciphertext;
}

echo caesarEncode( "The quick brown fox Jumped over the lazy Dog", 12 ), "\n";
?>
