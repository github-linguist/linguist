# for our tantrums
my class X::TypeCheck { ... }
my role Supply { ... }

my sub combinations($n, $k) {
    my @result;
    my @stack;

    return ([],) unless $k;

    @stack.push(0);
    gather while @stack {
        my $index = @stack - 1;
        my $value = @stack.pop;

        while $value < $n {
            @result[$index++] = $value++;
            @stack.push($value);
            if $index == $k {
                take [@result];
                $value = $n;  # fake a last
            }
        }
    }
}

my sub permutations(Int $n) {
    $n == 1 ?? ( [0,] ) !!
    gather for ^$n -> $i {
        my @i = grep none($i), ^$n;
        take [$i, @i[@$_]] for permutations($n - 1);
    }
}

my class List does Positional { # declared in BOOTSTRAP
    # class List is Iterable is Cool
    #   has Mu $!items;        # VM's array of our reified elements
    #   has Mu $!flattens;     # true if this list flattens its parcels
    #   has Mu $!nextiter;     # iterator for generating remaining elements

    method new(|) {
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);

        nqp::p6list($args, self.WHAT, Mu);
    }

    multi method Bool(List:D:)    { self.gimme(1).Bool }
    multi method Int(List:D:)     { self.elems }
    multi method end(List:D:)     { self.elems - 1 }
    multi method Numeric(List:D:) { self.elems }
    multi method Str(List:D:)     { self.join(' ') }

    # Pretend we're a Match assuming we're a list of Matches
    method to()         { self.elems ?? self[self.end].to !! Nil }
    method from()       { self.elems ?? self[0].from !! Nil }

    method fmt($format = '%s', $separator = ' ') {
        self.map({ .fmt($format) }).join($separator);
    }

    method flat() { self.flattens
                    ?? self
                    !! nqp::p6list(nqp::list(self), List, Bool::True)
    }
    method list() { self }
    method lol() {
        self.gimme(0);
        my Mu $rpa := nqp::clone($!items);
        nqp::push($rpa, $!nextiter) if $!nextiter.defined;
        nqp::p6list($rpa, LoL, Mu);
    }

    method flattens() { $!flattens }

    method Capture() {
        self.gimme(*);
        my $cap := nqp::create(Capture);
        nqp::bindattr($cap, Capture, '$!list', $!items);
        $cap
    }

    method Parcel() {
        my Mu $rpa := nqp::clone(nqp::p6listitems(self));
        nqp::push($rpa, $!nextiter) if $!nextiter.defined;
        nqp::p6parcel($rpa, Any);
    }

    method Supply(List:D:) { Supply.from-list(self) }

    multi method at_pos(List:D: int \pos) is rw {
        fail X::OutOfRange.new(:what<Index>,:got(pos),:range<0..Inf>)
          if nqp::islt_i(pos,0);
        self.exists_pos(pos) ?? nqp::atpos($!items,pos) !! Nil;
    }
    multi method at_pos(List:D: Int:D \pos) is rw {
        my int $pos = nqp::unbox_i(pos);
        fail X::OutOfRange.new(:what<Index>,:got(pos),:range<0..Inf>)
          if nqp::islt_i($pos,0);
        self.exists_pos($pos) ?? nqp::atpos($!items,$pos) !! Nil;
    }

    method eager() { self.gimme(*); self }

    method elems() {
        return 0 unless self.DEFINITE;
        return nqp::elems(nqp::p6listitems(self)) unless nqp::defined($!nextiter);
        # Get as many elements as we can.  If gimme stops before
        # reaching the end of the list, assume the list is infinite.
        my $n := self.gimme(*);
        nqp::defined($!nextiter) ?? Inf !! $n
    }

    multi method exists_pos(List:D: int $pos) {
        return False if nqp::islt_i($pos,0);
        self.gimme($pos + 1);
        nqp::p6bool(
          nqp::not_i(nqp::isnull(nqp::atpos($!items,$pos)))
        );
    }
    multi method exists_pos(List:D: Int:D $pos) {
        return False if $pos < 0;
        self.gimme($pos + 1);
        nqp::p6bool(
          nqp::not_i(nqp::isnull(nqp::atpos($!items,nqp::unbox_i($pos))))
        );
    }

    method gimme($n, :$sink) {
        return unless self.DEFINITE;
        # loop through iterators until we have at least $n elements
        my int $count = nqp::elems(nqp::p6listitems(self));
        if nqp::istype($n, Whatever) || nqp::istype($n, Num) && nqp::istrue($n == Inf) {
            while $!nextiter.DEFINITE && !$!nextiter.infinite {
                $!nextiter.reify(*, :$sink);
                $count = nqp::elems($!items);
            }
        }
        else {
            my int $target = $n.Int;
            while nqp::isconcrete($!nextiter) && $count < $target {
                $!nextiter.reify($target - $count, :$sink);
                $count = nqp::elems($!items);
            }
        }

        # return the number of elements we have now
        $count
    }

    multi method infinite(List:D:) { $!nextiter.infinite }

    method iterator() {
        # Return a reified ListIter containing our currently reified elements
        # and any subsequent iterator.
        my $iter := nqp::create(ListIter);
        nqp::bindattr($iter, ListIter, '$!nextiter', $!nextiter);
        nqp::bindattr($iter, ListIter, '$!reified', self.Parcel());
        $iter;
    }

    method munch($n is copy) {
        $n = 0 if $n < 0;
        $n = self.gimme($n) if nqp::not_i(nqp::istype($n, Int))
                               || nqp::not_i(nqp::islist($!items))
                               || nqp::islt_i(nqp::elems($!items), nqp::unbox_i($n));
        nqp::p6parcel(
            nqp::p6shiftpush(nqp::list(), $!items, nqp::unbox_i($n)),
            Any
        )
    }

    proto method pick(|) { * }
    multi method pick() {
        fail "Cannot .pick from infinite list" if self.infinite;
        my $elems = self.elems;
        $elems ?? self.at_pos($elems.rand.floor) !! Nil;
    }
    multi method pick($n is copy) {
        fail "Cannot .pick from infinite list" if self.infinite;
        ## We use a version of Fisher-Yates shuffle here to
        ## replace picked elements with elements from the end
        ## of the list, resulting in an O(n) algorithm.
        my $elems = self.elems;
        return unless $elems;
        $n = Inf if nqp::istype($n, Whatever);
        $n = $elems if $n > $elems;
        return self.at_pos($elems.rand.floor) if $n == 1;
        my Mu $rpa := nqp::clone($!items);
        my $i;
        my Mu $v;
        gather while $n > 0 {
            $i = nqp::rand_I(nqp::decont($elems), Int);
            $elems--; $n--;
            $v := nqp::atpos($rpa, nqp::unbox_i($i));
            # replace selected element with last unpicked one
            nqp::bindpos($rpa, nqp::unbox_i($i),
                         nqp::atpos($rpa, nqp::unbox_i($elems)));
            take-rw $v;
        }
    }

    method pop() is parcel {
        my $elems = self.gimme(*);
        fail 'Cannot .pop from an infinite list' if $!nextiter.defined;
        $elems > 0
          ?? nqp::pop($!items)
          !! fail 'Element popped from empty list';
    }

    method shift() is parcel {
        # make sure we have at least one item, then shift+return it
        nqp::islist($!items) && nqp::existspos($!items, 0) || self.gimme(1)
          ?? nqp::shift($!items)
          !! fail 'Element shifted from empty list';
    }

    my &list_push = multi method push(List:D: *@values) {
        fail 'Cannot .push an infinite list' if @values.infinite;
        nqp::p6listitems(self);
        my $elems = self.gimme(*);
        fail 'Cannot .push to an infinite list' if $!nextiter.DEFINITE;

        # push is always eager
        @values.gimme(*);

        # need type checks?
        my $of := self.of;

        unless $of =:= Mu {
            X::TypeCheck.new(
              operation => '.push',
              expected  => $of,
              got       => $_,
            ).throw unless nqp::istype($_, $of) for @values;
        }

        nqp::splice($!items,
                nqp::getattr(@values, List, '$!items'),
                $elems, 0);

        self;
    }

    multi method push(List:D: \value) {
        if nqp::iscont(value) || nqp::not_i(nqp::istype(value, Iterable)) && nqp::not_i(nqp::istype(value, Parcel)) {
            $!nextiter.DEFINITE && self.gimme(*);
            fail 'Cannot .push to an infinite list' if $!nextiter.DEFINITE;
            nqp::p6listitems(self);
            nqp::istype(value, self.of)
                ?? nqp::push($!items, nqp::assign(nqp::p6scalarfromdesc(nqp::null), value))
                !! X::TypeCheck.new(
                      operation => '.push',
                      expected  => self.of,
                      got       => value,
                    ).throw;
            self
        }
        else {
            list_push(self, value)
        }
    }

    multi method unshift(List:D: \value) {
        if nqp::iscont(value) || !(nqp::istype(value, Iterable) || nqp::istype(value, Parcel)) {
            nqp::p6listitems(self);
            value.gimme(*) if nqp::istype(value, List); # fixes #121994
            nqp::istype(value, self.of)
                ?? nqp::unshift($!items, my $ = value)
                !! X::TypeCheck.new(
                      operation => '.push',
                      expected  => self.of,
                      got       => value,
                    ).throw;
            self
        }
        else {
            callsame();
        }
    }

    multi method unshift(List:D: *@values) {
        fail 'Cannot .unshift an infinite list' if @values.infinite;
        nqp::p6listitems(self);

        # don't bother with type checks
        my $of := self.of;
        if ( $of =:= Mu ) {
            nqp::unshift($!items, @values.pop) while @values;
        }

        # we must check types
        else {
            while @values {
                my $value := @values.pop;
                if nqp::istype($value, $of) {
                    nqp::unshift($!items, $value);
                }

                # huh?
                else {
                    X::TypeCheck.new(
                      operation => '.unshift',
                      expected  => $of,
                      got       => $value,
                    ).throw;
                }
            }
        }

        self
    }

    method plan(List:D: |args) {
        nqp::p6listitems(self);
        my $elems = self.gimme(*);
        fail 'Cannot add plan to an infinite list' if $!nextiter.defined;

#        # need type checks?
#        my $of := self.of;
#
#        unless $of =:= Mu {
#            X::TypeCheck.new(
#              operation => '.push',
#              expected  => $of,
#              got       => $_,
#            ).throw unless nqp::istype($_, $of) for @values;
#        }

        nqp::bindattr(self, List, '$!nextiter', nqp::p6listiter(nqp::list(args.list), self));
        Nil;
    }

    proto method roll(|) { * }
    multi method roll() {
        fail "Cannot .roll from infinite list" if self.infinite;
        my $elems = self.elems;
        $elems ?? self.at_pos($elems.rand.floor) !! Nil;
    }
    multi method roll($n is copy) {
        fail "Cannot .roll from infinite list" if self.infinite;
        my $elems = self.elems;
        return unless $elems;
        $n = Inf if nqp::istype($n, Whatever);
        return self.at_pos($elems.rand.floor) if $n == 1;

        gather while $n > 0 {
            take nqp::atpos($!items, nqp::unbox_i($elems.rand.floor.Int));
            $n--;
        }
    }

    method reverse() {
        self.gimme(*);
        fail 'Cannot .reverse from an infinite list' if $!nextiter.defined;
        my Mu $rev  := nqp::list();
        my Mu $orig := nqp::clone($!items);
        nqp::push($rev, nqp::pop($orig)) while $orig;
        my $rlist := nqp::create(self.WHAT);
        nqp::bindattr($rlist, List, '$!items', $rev);
        $rlist;
    }

    method rotate(Int $n is copy = 1) {
        self.gimme(*);
        fail 'Cannot .rotate an infinite list' if $!nextiter.defined;
        my $items = nqp::p6box_i(nqp::elems($!items));
        return self if !$items;

        $n %= $items;
        return self if $n == 0;

        my Mu $res := nqp::clone($!items);
        if $n > 0 {
            nqp::push($res, nqp::shift($res)) while $n--;
        }
        elsif $n < 0 {
            nqp::unshift($res, nqp::pop($res)) while $n++;
        }
        my $rlist := nqp::create(self.WHAT);
        nqp::bindattr($rlist, List, '$!items', $res);
        $rlist;
    }

    method splice($offset = 0, $size?, *@values) {
        self.gimme(*);
        my $o = $offset;
        my $s = $size;
        my $elems = self.elems;
        $o = $o($elems) if nqp::istype($o, Callable);
        X::OutOfRange.new(
            what => 'offset argument to List.splice',
            got  => $offset,
            range => (0..^self.elems),
        ).fail if $o < 0;
        $s //= self.elems - ($o min $elems);
        $s = $s(self.elems - $o) if nqp::istype($s, Callable);
        X::OutOfRange.new(
            what => 'size argument to List.splice',
            got  => $size,
            range => (0..^(self.elems - $o)),
        ).fail if $s < 0;

        my @ret = self[$o..($o + $s - 1)];
        nqp::splice($!items,
                    nqp::getattr(@values.eager, List, '$!items'),
                    $o.Int, $s.Int);
        @ret;
    }

    method sort($by = &infix:<cmp>) {
        fail 'Cannot .sort an infinite list' if self.infinite; #MMD?

        # Instead of sorting elements directly, we sort a Parcel of
        # indices from 0..^$list.elems, then use that Parcel as
        # a slice into self. This is for historical reasons: on
        # Parrot we delegate to RPA.sort. The JVM implementation
        # uses a Java collection sort. MoarVM has its sort algorithm
        # implemented in NQP.

        # nothing to do here
        my $elems := self.elems;
        return self if $elems < 2;

        # Range is currently optimized for fast Parcel construction.
        my $index := Range.new(0, $elems, :excludes-max).reify(*);
        my Mu $index_rpa := nqp::getattr($index, Parcel, '$!storage');

        # if $by.arity < 2, then we apply the block to the elements
        # for sorting.
        if ($by.?count // 2) < 2 {
            my $list = self.map($by).eager;
            nqp::p6sort($index_rpa, -> $a, $b { $list.at_pos($a) cmp $list.at_pos($b) || $a <=> $b });
        }
        else {
            my $list = self.eager;
            nqp::p6sort($index_rpa, -> $a, $b { $by($list.at_pos($a), $list.at_pos($b)) || $a <=> $b });
        }
        self[$index];
    }

    multi method ACCEPTS(List:D: $topic) { self }

    method uniq(|c) {
        DEPRECATED('unique', |<2014.11 2015.11>);
        self.unique(|c);
    }

    proto method unique(|) {*}
    multi method unique() {
        my $seen := nqp::hash();
        my str $target;
        gather for @.list {
            $target = nqp::unbox_s($_.WHICH);
            unless nqp::existskey($seen, $target) {
                nqp::bindkey($seen, $target, 1);
                take $_;
            }
        }
    }
    multi method unique( :&as!, :&with! ) {
        my @seen = "should be Mu, but doesn't work in settings :-("
        my Mu $target;
        gather for @.list {
            $target = &as($_);
            if first( { with($target,$_) }, @seen ) =:= Nil {
                @seen.push($target);
                take $_;
            }
        };
    }
    multi method unique( :&as! ) {
        my $seen := nqp::hash();
        my str $target;
        gather for @.list {
            $target = &as($_).WHICH;
            unless nqp::existskey($seen, $target) {
                nqp::bindkey($seen, $target, 1);
                take $_;
            }
        }
    }
    multi method unique( :&with! ) {
        nextwith() if &with === &[===]; # use optimized version

        my @seen;  # should be Mu, but doesn't work in settings :-(
        my Mu $target;
        gather for @.list {
            $target := $_;
            if first( { with($target,$_) }, @seen ) =:= Nil {
                @seen.push($target);
                take $_;
            }
        }
    }

    my @secret;
    proto method squish(|) {*}
    multi method squish( :&as!, :&with = &[===] ) {
        my $last = @secret;
        my str $which;
        gather for @.list {
            $which = &as($_).Str;
            unless with($which,$last) {
                $last = $which;
                take $_;
            }
        }
    }
    multi method squish( :&with = &[===] ) {
        my $last = @secret;
        gather for @.list {
            unless with($_,$last) {
                $last = $_;
                take $_;
            }
        }
    }

    proto method rotor(|) {*}
    multi method rotor(1, 0) { self }
    multi method rotor($elems = 2, $overlap = 1) {
        X::OutOfRange.new(
            what => 'Overlap argument to List.rotor',
            got  => $overlap,
            range => (0 .. $elems - 1),
        ).fail unless 0 <= $overlap < $elems;
        X::OutOfRange.new(
            what => 'Elements argument to List.rotor',
            got  => $elems,
            range => (0 .. *),
        ).fail unless 0 <= $elems;

        my $finished = 0;
        gather while $finished + $overlap < self.gimme($finished + $elems) {
            take item self[$finished ..^ $finished + $elems];
            $finished += $elems - $overlap
        }
    }

    multi method gist(List:D:) {
        @(self).map( -> $elem {
            given ++$ {
                when 101 { '...' }
                when 102 { last }
                default  { $elem.gist }
            }
        } ).join: ' ';
    }
    multi method perl(List:D \SELF:) {
        self.gimme(*);
        self.Parcel.perl ~ '.list'
          ~ (nqp::iscont(SELF) ?? '.item' !! '')
    }

    method REIFY(Parcel \parcel, Mu \nextiter) {
        nqp::splice($!items, nqp::getattr(parcel, Parcel, '$!storage'),
                    nqp::elems($!items), 0);
        nqp::bindattr(self, List, '$!nextiter', nextiter);
        parcel
    }

    method FLATTENABLE_LIST() { self.gimme(*); $!items }
    method FLATTENABLE_HASH() { nqp::hash() }

    multi method DUMP(List:D: :$indent-step = 4, :%ctx?) {
        return DUMP(self, :$indent-step) unless %ctx;

        my $flags    := ("\x221e" if self.infinite);
        my Mu $attrs := nqp::list();
        nqp::push($attrs, '$!flattens');
        nqp::push($attrs,  $!flattens );
        nqp::push($attrs, '$!items'   );
        nqp::push($attrs,  $!items    );
        nqp::push($attrs, '$!nextiter');
        nqp::push($attrs,  $!nextiter );
        self.DUMP-OBJECT-ATTRS($attrs, :$indent-step, :%ctx, :$flags);
    }

    multi method keys(List:D:) {
        self.values.map: { (state $)++ }
    }
    multi method kv(List:D:) {
        gather for self.values {
            take (state $)++;
            take-rw $_;
        }
    }
    multi method values(List:D:) {
        my Mu $rpa := nqp::clone(nqp::p6listitems(self));
        nqp::push($rpa, $!nextiter) if $!nextiter.defined;
        nqp::p6list($rpa, List, self.flattens);
    }
    multi method pairs(List:D:) {
        self.values.map: {; (state $)++ => $_ }
    }

    method reduce(List: &with) {
        fail('can only reduce with arity 2')
            unless &with.arity <= 2 <= &with.count;
        return unless self.DEFINITE;
        my \vals = self.values;
        my Mu $val = vals.shift;
        $val = with($val, $_) for vals;
        $val;
    }

    method sink() {
        self.gimme(*, :sink) if self.DEFINITE && $!nextiter.DEFINITE;
        Nil;
    }

    # this is a remnant of a previous implementation of .push(), which
    # apparently is used by LoL.  Please remove when no longer necessary.
    method STORE_AT_POS(Int \pos, Mu \v) is rw {
        nqp::bindpos($!items, nqp::unbox_i(pos), v)
    }

    proto method combinations($?) {*}
    multi method combinations( Int $of ) {
        ([self[@$_]] for combinations(self.elems, $of).eager)
    }
    multi method combinations( Range $of = 0 .. * ) {
        gather for @$of {
            last if $_ > self.elems;
            take self.combinations($_);
        }
    }

    method permutations() {
        # need block on Moar because of RT#121830
        gather { take [self[@$_]] for permutations(self.elems).eager }
    }
}

sub eager(|) {
    nqp::p6parcel(nqp::p6argvmarray(), Any).eager
}

sub flat(|) {
    nqp::p6list(nqp::p6argvmarray(), List, Bool::True)
}

sub list(|) {
    nqp::p6list(nqp::p6argvmarray(), List, Mu)
}

proto sub infix:<xx>(|)       { * }
multi sub infix:<xx>()        { fail "No zero-arg meaning for infix:<xx>" }
multi sub infix:<xx>(Mu \x)   {x }
multi sub infix:<xx>(Mu \x, $n is copy, :$thunked!) {
    $n = nqp::p6bool(nqp::istype($n, Whatever)) ?? Inf !! $n.Int;
    GatherIter.new({ take x.() while --$n >= 0; }, :infinite($n == Inf)).list
}
multi sub infix:<xx>(Mu \x, Whatever, :$thunked!) {
    GatherIter.new({ loop { take x.() } }, :infinite(True)).flat
}
multi sub infix:<xx>(Mu \x, Whatever) {
    GatherIter.new({ loop { take x } }, :infinite(True)).flat
}
multi sub infix:<xx>(Mu \x, $n) {
    my int $size = $n.Int;

    my Mu $rpa := nqp::list();
    if $size > 0 {
        nqp::setelems($rpa, $size);
        nqp::setelems($rpa, 0);

        $size = $size + 1;
        nqp::push($rpa,x) while $size = $size - 1;
    }

    nqp::p6parcel($rpa, Any);
}

proto sub pop(@) {*}
multi sub pop(@a) { @a.pop }

proto sub shift(@) {*}
multi sub shift(@a) { @a.shift }

proto sub unshift(|) {*}
multi sub unshift(\a, \elem) { a.unshift: elem }
multi sub unshift(\a, *@elems) { a.unshift: @elems }

proto sub push(|) {*}
multi sub push(\a, \elem) { a.push: elem }
multi sub push(\a, *@elems) { a.push: @elems }

sub reverse(*@a)            { @a.reverse }
sub rotate(@a, Int $n = 1)  { @a.rotate($n) }
sub reduce (&with, *@list)  { @list.reduce(&with) }
sub splice(@arr, $offset = 0, $size?, *@values) {
    @arr.splice($offset, $size, @values)
}

multi sub infix:<cmp>(@a, @b) { (@a Zcmp @b).first(&prefix:<?>) || @a <=> @b }

# vim: ft=perl6 expandtab sw=4
