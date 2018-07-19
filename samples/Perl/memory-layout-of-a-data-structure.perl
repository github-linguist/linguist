use Bit::Vector::Minimal qw();
my $vec = Bit::Vector::Minimal->new(size => 24);

my %rs232 = reverse (
     1 => 'PG   Protective ground',
     2 => 'TD   Transmitted data',
     3 => 'RD   Received data',
     4 => 'RTS  Request to send',
     5 => 'CTS  Clear to send',
     6 => 'DSR  Data set ready',
     7 => 'SG   Signal ground',
     8 => 'CD   Carrier detect',
     9 => '+ voltage (testing)',
    10 => '- voltage (testing)',
    12 => 'SCD  Secondary CD',
    13 => 'SCS  Secondary CTS',
    14 => 'STD  Secondary TD',
    15 => 'TC   Transmit clock',
    16 => 'SRD  Secondary RD',
    17 => 'RC   Receiver clock',
    19 => 'SRS  Secondary RTS',
    20 => 'DTR  Data terminal ready',
    21 => 'SQD  Signal quality detector',
    22 => 'RI   Ring indicator',
    23 => 'DRS  Data rate select',
    24 => 'XTC  External clock',
);

$vec->set($rs232{'RD   Received data'}, 1);
$vec->get($rs232{'TC   Transmit clock'});
