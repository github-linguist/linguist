class Chromosome {
  has Seq $.chromosome is rw;
  has $.fitness is rw;

}

my $len = 32;
my $this-chromosome = Chromosome.new( chromosome => map( { rand >= 0.5 ?? True !! False }, 1..$len )  );
say $this-chromosome.chromosome();
