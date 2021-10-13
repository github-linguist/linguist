my class Failure { ... }
my role X::Comp { ... }
my class X::ControlFlow { ... }

my class Exception {
    has $!ex;

    method backtrace() { Backtrace.new(self) }

    multi method Str(Exception:D:) {
        self.?message.Str // 'Something went wrong'
    }

    multi method gist(Exception:D:) {
        my $str = try self.?message;
        return "Error while creating error string: $!" if $!;
        $str ~= "\n";
        try $str ~= self.backtrace;
        return "$str\nError while creating backtrace: $!.message()\n$!.backtrace.full();" if $!;
        return $str;
    }

    method throw() is hidden_from_backtrace {
        nqp::bindattr(self, Exception, '$!ex', nqp::newexception())
            unless nqp::isconcrete($!ex);
        nqp::setpayload($!ex, nqp::decont(self));
        my $msg := self.?message;
        nqp::setmessage($!ex, nqp::unbox_s($msg.Str))
            if $msg.defined;
        nqp::throw($!ex)
    }
    method rethrow() is hidden_from_backtrace {
        nqp::setpayload($!ex, nqp::decont(self));
        nqp::rethrow($!ex)
    }

    method resumable() {
        nqp::p6bool(nqp::istrue(nqp::atkey($!ex, 'resume')));
    }

    method resume() {
        my Mu $resume := nqp::atkey($!ex, 'resume');
        if $resume {
            $resume();
        }
        else {
            die "Exception is not resumable";
        }
    }

    method fail(Exception:D:) {
        try self.throw;
        my $fail := Failure.new($!);
        my Mu $return := nqp::getlexcaller('RETURN');
        $return($fail) unless nqp::isnull($return);
        $fail
    }

    method is-compile-time { False }
}

my class X::AdHoc is Exception {
    has $.payload;
    method message() { $.payload.Str     }
    method Numeric() { $.payload.Numeric }
}

my class X::Method::NotFound is Exception {
    has $.method;
    has $.typename;
    has Bool $.private = False;
    method message() {
        $.private
            ?? "No such private method '$.method' for invocant of type '$.typename'"
            !! "No such method '$.method' for invocant of type '$.typename'";
    }
}

my class X::Method::InvalidQualifier is Exception {
    has $.method;
    has $.invocant;
    has $.qualifier-type;
    method message() {
          "Cannot dispatch to method $.method on {$.qualifier-type.^name} "
        ~ "because it is not inherited or done by {$.invocant.^name}";
    }
}


sub EXCEPTION(|) {
    my Mu $vm_ex   := nqp::shift(nqp::p6argvmarray());
    my Mu $payload := nqp::getpayload($vm_ex);
    if nqp::p6bool(nqp::istype($payload, Exception)) {
        nqp::bindattr($payload, Exception, '$!ex', $vm_ex);
        $payload;
    } else {
        my int $type = nqp::getextype($vm_ex);
        my $ex;
#?if parrot
        if $type == pir::const::EXCEPTION_METHOD_NOT_FOUND &&
#?endif
#?if !parrot
        if
#?endif
            nqp::p6box_s(nqp::getmessage($vm_ex)) ~~ /"Method '" (.*?) "' not found for invocant of class '" (.+)\'$/ {

            $ex := X::Method::NotFound.new(
                method   => ~$0,
                typename => ~$1,
            );
        }
        else {

            $ex := nqp::create(X::AdHoc);
            nqp::bindattr($ex, X::AdHoc, '$!payload', nqp::p6box_s(nqp::getmessage($vm_ex)));
        }
        nqp::bindattr($ex, Exception, '$!ex', $vm_ex);
        $ex;
    }
}

my class X::Comp::AdHoc { ... }
sub COMP_EXCEPTION(|) {
    my Mu $vm_ex   := nqp::shift(nqp::p6argvmarray());
    my Mu $payload := nqp::getpayload($vm_ex);
    if nqp::p6bool(nqp::istype($payload, Exception)) {
        nqp::bindattr($payload, Exception, '$!ex', $vm_ex);
        $payload;
    } else {
        my $ex := nqp::create(X::Comp::AdHoc);
        nqp::bindattr($ex, Exception, '$!ex', $vm_ex);
        nqp::bindattr($ex, X::AdHoc, '$!payload', nqp::p6box_s(nqp::getmessage($vm_ex)));
        $ex;
    }
}


do {
    sub is_runtime($bt) {
        for $bt.keys {
            try {
                my Mu $sub := nqp::getattr(nqp::decont($bt[$_]<sub>), ForeignCode, '$!do');
                my Mu $codeobj := nqp::ifnull(nqp::getcodeobj($sub), Mu);
                my $is_nqp = $codeobj && $codeobj.HOW.name($codeobj) eq 'NQPRoutine';
                return True if nqp::iseq_s(nqp::getcodename($sub), 'eval') && $is_nqp;
                return False if nqp::iseq_s(nqp::getcodename($sub), 'compile') && $is_nqp;
            }
        }
        return False;
    }


    sub print_exception(|) is hidden_from_backtrace {
        my Mu $ex := nqp::atpos(nqp::p6argvmarray(), 0);
        try {
            my $e := EXCEPTION($ex);
            my Mu $err := nqp::getstderr();

#?if parrot
            if $e.is-compile-time || is_runtime($ex.backtrace) {
#?endif
#?if !parrot
            if $e.is-compile-time || is_runtime(nqp::backtrace($ex)) {
#?endif
                nqp::printfh($err, $e.gist);
                nqp::printfh($err, "\n");
            }
            else {
                nqp::printfh($err, "===SORRY!===\n");
                nqp::printfh($err, $e.Str);
                nqp::printfh($err, "\n");
            }
            $_() for nqp::hllize(nqp::getcurhllsym('@END_PHASERS'));
        }
        if $! {
#?if parrot
            pir::perl6_based_rethrow__0PP(nqp::getattr(nqp::decont($!), Exception, '$!ex'), $ex);
#?endif
#?if !parrot
            nqp::rethrow(nqp::getattr(nqp::decont($!), Exception, '$!ex'));
            $ex
#?endif
        }
    }

    sub print_control(|) is hidden_from_backtrace {
        my Mu $ex := nqp::atpos(nqp::p6argvmarray(), 0);
        my int $type = nqp::getextype($ex);
        if ($type == nqp::const::CONTROL_WARN) {
            my Mu $err := nqp::getstderr();
            my $msg = nqp::p6box_s(nqp::getmessage($ex));
            nqp::printfh($err, $msg ?? "$msg" !! "Warning");
#?if parrot
            nqp::printfh($err, Backtrace.new($ex.backtrace, 0).nice(:oneline));
#?endif
#?if jvm
#            XXX Backtraces busted
#            nqp::printfh($err, Backtrace.new(nqp::backtrace($ex), 0).nice(:oneline));
#?endif
            nqp::printfh($err, "\n");
#?if parrot
            my $resume := nqp::atkey($ex, 'resume');
            if ($resume) {
                $resume();
            }
#?endif
#?if !parrot
            nqp::resume($ex)
#?endif
        }
        if ($type == nqp::const::CONTROL_LAST) {
            X::ControlFlow.new(illegal => 'last', enclosing => 'loop construct').throw;
        }
        if ($type == nqp::const::CONTROL_NEXT) {
            X::ControlFlow.new(illegal => 'next', enclosing => 'loop construct').throw;
        }
        if ($type == nqp::const::CONTROL_REDO) {
            X::ControlFlow.new(illegal => 'redo', enclosing => 'loop construct').throw;
        }
        if ($type == nqp::const::CONTROL_PROCEED) {
            X::ControlFlow.new(illegal => 'proceed', enclosing => 'when clause').throw;
        }
        if ($type == nqp::const::CONTROL_SUCCEED) {
            # XXX: should work like leave() ?
            X::ControlFlow.new(illegal => 'succeed', enclosing => 'when clause').throw;
        }
        if ($type == nqp::const::CONTROL_TAKE) {
            X::ControlFlow.new(illegal => 'take', enclosing => 'gather').throw;
        }
    }
            
    my Mu $comp := nqp::getcomp('perl6');
    $comp.HOW.add_method($comp, 'handle-exception',
        method (|) {
            my Mu $ex := nqp::atpos(nqp::p6argvmarray(), 1);
#?if parrot
            pir::perl6_invoke_catchhandler__vPP(&print_exception, $ex);
#?endif
#?if !parrot
            print_exception($ex);
#?endif
            nqp::exit(1);
            0;
        }
    );
    $comp.HOW.add_method($comp, 'handle-control',
        method (|) {
            my Mu $ex := nqp::atpos(nqp::p6argvmarray(), 1);
#?if parrot
            pir::perl6_invoke_catchhandler__vPP(&print_control, $ex);
#?endif
#?if !parrot
            print_control($ex);
#?endif
            nqp::rethrow($ex);
        }
    );

}

my role X::OS {
    has $.os-error;
}

my role X::IO does X::OS { };

my class X::IO::Rename does X::IO is Exception {
    has $.from;
    has $.to;
    method message() {
        "Failed to rename '$.from' to '$.to': $.os-error"
    }
}

my class X::IO::Copy does X::IO is Exception {
    has $.from;
    has $.to;
    method message() {
        "Failed to copy '$.from' to '$.to': $.os-error"
    }
}

my class X::IO::Symlink does X::IO is Exception {
    has $.target;
    has $.name;
    method message() {
        "Failed to create symlink called '$.name' on target '$.target': $.os-error"
    }
}

my class X::IO::Link does X::IO is Exception {
    has $.target;
    has $.name;
    method message() {
        "Failed to create link called '$.name' on target '$.target': $.os-error"
    }
}

my class X::IO::Mkdir does X::IO is Exception {
    has $.path;
    has $.mode;
    method message() {
        "Failed to create directory '$.path' with mode '0o{$.mode.fmt("%03o")}': $.os-error"
    }
}

my class X::IO::Chdir does X::IO is Exception {
    has $.path;
    method message() {
        "Failed to change the working directory to '$.path': $.os-error"
    }
}

my class X::IO::Dir does X::IO is Exception {
    has $.path;
    method message() {
        "Failed to get the directory contents of '$.path': $.os-error"
    }
}

my class X::IO::Cwd does X::IO is Exception {
    method message() {
        "Failed to get the working directory: $.os-error"
    }
}

my class X::IO::Rmdir does X::IO is Exception {
    has $.path;
    method message() {
        "Failed to remove the directory '$.path': $.os-error"
    }
}

my class X::IO::Unlink does X::IO is Exception {
    has $.path;
    method message() {
        "Failed to remove the file '$.path': $.os-error"
    }
}

my class X::IO::Chmod does X::IO is Exception {
    has $.path;
    has $.mode;
    method message() {
        "Failed to set the mode of '$.path' to '0o{$.mode.fmt("%03o")}': $.os-error"
    }
}

my role X::Comp is Exception {
    has $.filename;
    has $.line;
    has $.column;
    has @.modules;
    has $.is-compile-time = False;
    has $.pre;
    has $.post;
    has @.highexpect;
    multi method gist(::?CLASS:D: :$sorry = True, :$expect = True) {
        if $.is-compile-time {
            my $color = %*ENV<RAKUDO_ERROR_COLOR> // $*OS ne 'MSWin32';
            my ($red, $green, $yellow, $clear) = $color
                ?? ("\e[31m", "\e[32m", "\e[33m", "\e[0m")
                !! ("", "", "", "");
            my $eject = $*OS eq 'MSWin32' ?? "<HERE>" !! "\x[23CF]";
            my $r = $sorry ?? self.sorry_heading() !! "";
            $r ~= "$.message\nat $.filename():$.line\n------> ";
            $r ~= "$green$.pre$yellow$eject$red$.post$clear" if defined $.pre;
            if $expect && @.highexpect {
                $r ~= "\n    expecting any of:";
                for @.highexpect {
                    $r ~= "\n        $_";
                }
            }
            for @.modules.reverse[1..*] {
                $r ~= $_<module>.defined
                        ?? "\n  from module $_<module> ($_<filename>:$_<line>)"
                        !! "\n  from $_<filename>:$_<line>";
            }
            $r;
        }
        else {
            self.Exception::gist;
        }
    }
    method sorry_heading() {
        my $color = %*ENV<RAKUDO_ERROR_COLOR> // $*OS ne 'MSWin32';
        my ($red, $clear) = $color ?? ("\e[31m", "\e[0m") !! ("", "");
        "$red==={$clear}SORRY!$red===$clear Error while compiling $.filename\n"
    }
    method SET_FILE_LINE($file, $line) {
        $!filename = $file;
        $!line     = $line;
        $!is-compile-time = True;
    }
}

my class X::Comp::Group is Exception {
    has $.panic;
    has @.sorrows;
    has @.worries;

    method is-compile-time() { True }

    multi method gist(::?CLASS:D:) {
        my $r = "";
        if $.panic || @.sorrows {
            my $color = %*ENV<RAKUDO_ERROR_COLOR> // $*OS ne 'MSWin32';
            my ($red, $clear) = $color ?? ("\e[31m", "\e[0m") !! ("", "");
            $r ~= "$red==={$clear}SORRY!$red===$clear\n";
            for @.sorrows {
                $r ~= .gist(:!sorry, :!expect) ~ "\n";
            }
            if $.panic {
                $r ~= $.panic.gist(:!sorry) ~ "\n";
            }
        }
        if @.worries {
            $r ~= $.panic || @.sorrows
                ?? "Other potential difficulties:\n"
                !! "Potential difficulties:\n";
            for @.worries {
                $r ~= .gist(:!sorry, :!expect).indent(4) ~ "\n";
            }
        }
        $r
    }

    method message() {
        my @m;
        for @.sorrows {
            @m.push(.message);
        }
        if $.panic {
            @m.push($.panic.message);
        }
        for @.worries {
            @m.push(.message);
        }
        @m.join("\n")
    }
}

# XXX a hack for getting line numbers from exceptions from the metamodel
my class X::Comp::AdHoc is X::AdHoc does X::Comp {
    method is-compile-time() { True }
}

my role X::Syntax does X::Comp { }
my role X::Pod                 { }

my class X::NYI is Exception {
    has $.feature;
    method message() { "$.feature not yet implemented. Sorry. " }
}
my class X::Comp::NYI is X::NYI does X::Comp { };

my class X::Trait::Unknown is Exception {
    has $.type;       # is, will, of etc.
    has $.subtype;    # wrong subtype being tried
    has $.declaring;  # variable, sub, parameter, etc.
    method message () {
        "Can't use unknown trait '$.type $.subtype' in a$.declaring declaration."
    }
}
my class X::Comp::Trait::Unknown is X::Trait::Unknown does X::Comp { };

my class X::Trait::NotOnNative is Exception {
    has $.type;       # is, will, of etc.
    has $.subtype;    # wrong subtype being tried
    has $.native;     # type of native (optional)
    method message () {
        "Can't use trait '$.type $.subtype' on a native"
          ~ ( $.native ?? " $.native." !! "." );
    }
}
my class X::Comp::Trait::NotOnNative is X::Trait::NotOnNative does X::Comp { };

my class X::OutOfRange is Exception {
    has $.what = 'Argument';
    has $.got = '<unknown>';
    has $.range = '<unknown>';
    has $.comment;
    method message() {
        $.comment.defined 
           ?? "$.what out of range. Is: $.got, should be in $.range.gist(); $.comment"
           !! "$.what out of range. Is: $.got, should be in $.range.gist()"
    }
}

my class X::Buf::AsStr is Exception {
    has $.method;
    method message() {
        "Cannot use a Buf as a string, but you called the $.method method on it";
    }
}
my class X::Buf::Pack is Exception {
    has $.directive;
    method message() {
        "Unrecognized directive '$.directive'";
    }
}

my class X::Buf::Pack::NonASCII is Exception {
    has $.char;
    method message() {
        "non-ASCII character '$.char' while processing an 'A' template in pack";
    }
}

my class X::Signature::Placeholder does X::Comp {
    has $.placeholder;
    method message() {
        "Placeholder variable '$.placeholder' cannot override existing signature";
    }
}

my class X::Placeholder::Block does X::Comp {
    has $.placeholder;
    method message() {
        "Placeholder variable $.placeholder may not be used here because the surrounding block takes no signature";
    }
}

my class X::Placeholder::Mainline is X::Placeholder::Block {
    method message() {
        "Cannot use placeholder parameter $.placeholder in the mainline"
    }
}

my class X::Undeclared does X::Comp {
    has $.what = 'Variable';
    has $.symbol;
    has @.suggestions;
    method message() {
        my $message := "$.what '$.symbol' is not declared";
        if +@.suggestions == 1 {
            $message := "$message. Did you mean '@.suggestions[0]'?";
        } elsif +@.suggestions > 1 {
            $message := "$message. Did you mean any of these?\n    { @.suggestions.join("\n    ") }\n";
        }
        $message;
    }
}

my class X::Attribute::Undeclared is X::Undeclared {
    has $.package-kind;
    has $.package-name;

    method message() {
        "Attribute $.symbol not declared in $.package-kind $.package-name";
    }
}

my class X::Undeclared::Symbols does X::Comp {
    has %.post_types;
    has %.unk_types;
    has %.unk_routines;
    has %.routine_suggestion;
    has %.type_suggestion;
    multi method gist(:$sorry = True) {
        ($sorry ?? self.sorry_heading() !! "") ~ self.message
    }
    method message() {
        sub l(@l) {
            my @lu = @l.map({ nqp::hllize($_) }).uniq.sort;
            'used at line' ~ (@lu == 1 ?? ' ' !! 's ') ~ @lu.join(', ')
        }
        sub s(@s) {
            "Did you mean '{ @s.join("', '") }'?";
        }
        my $r = "";
        if %.post_types {
            $r ~= "Illegally post-declared type" ~ (%.post_types.elems == 1 ?? "" !! "s") ~ ":\n";
            for %.post_types.sort(*.key) {
                $r ~= "    $_.key() &l($_.value)\n";
            }
        }
        if %.unk_types {
            $r ~= "Undeclared name" ~ (%.unk_types.elems == 1 ?? "" !! "s") ~ ":\n";
            for %.unk_types.sort(*.key) {
                $r ~= "    $_.key() &l($_.value)";
                if +%.type_suggestion{$_.key()} {
                    $r ~= ". " ~ s(%.type_suggestion{$_.key()});
                }
                $r ~= "\n";
            }
        }
        if %.unk_routines {
            $r ~= "Undeclared routine" ~ (%.unk_routines.elems == 1 ?? "" !! "s") ~ ":\n";
            for %.unk_routines.sort(*.key) {
                $r ~= "    $_.key() &l($_.value)";
                if +%.routine_suggestion{$_.key()} {
                    $r ~= ". " ~ s(%.routine_suggestion{$_.key()});
                }
                $r ~= "\n";
            }
        }
        $r
    }
}

my class X::Redeclaration does X::Comp {
    has $.symbol;
    has $.postfix = '';
    has $.what    = 'symbol';
    method message() {
        "Redeclaration of $.what $.symbol$.postfix";
    }
}

my class X::Redeclaration::Outer does X::Comp {
    has $.symbol;
    method message() {
        "Lexical symbol '$.symbol' is already bound to an outer symbol;\n" ~
        "the implicit outer binding must be rewritten as OUTER::<$.symbol>\n" ~
        "before you can unambiguously declare a new '$.symbol' in this scope";
    }
}

my class X::Import::Redeclaration does X::Comp {
    has @.symbols;
    has $.source-package-name;
    method message() {
        @.symbols == 1
            ?? "Cannot import symbol @.symbols[0] from $.source-package-name, because it already exists in this lexical scope"
            !! ("Cannot import the following symbols from $.source-package-name, because they already exist in this lexical scope: ", @.symbols.join(', '));
    }
}

my class X::Import::OnlystarProto does X::Comp {
    has @.symbols;
    has $.source-package-name;
    method message() {
        @.symbols == 1
            ?? "Cannot import symbol @.symbols[0] from $.source-package-name, only onlystar-protos can be merged"
            !! ("Cannot import the following symbols from $.source-package-name, only onlystar-protos can be merged: ", @.symbols.join(', '));
    }
}

my class X::Phaser::Multiple does X::Comp {
    has $.block;
    method message() { "Only one $.block block is allowed" }
}

my class X::Obsolete does X::Comp {
    has $.old;
    has $.replacement; # can't call it $.new, collides with constructor
    has $.when = 'in Perl 6';
    method message() { "Unsupported use of $.old; $.when please use $.replacement" }
}

my class X::Parameter::Default does X::Comp {
    has $.how;
    has $.parameter;
    method message() {
        $.parameter
            ?? "Cannot put default on $.how parameter $.parameter"
            !! "Cannot put default on anonymous $.how parameter";
    }
}

my class X::Parameter::Placeholder does X::Comp {
    has $.parameter;
    has $.right;
    method message() {
        "In signature parameter, placeholder variables like $.parameter are illegal\n"
        ~ "you probably meant a named parameter: '$.right'";
    }
}

my class X::Parameter::Twigil does X::Comp {
    has $.parameter;
    has $.twigil;
    method message() {
        "In signature parameter $.parameter, it is illegal to use the $.twigil twigil";
    }
}

my class X::Parameter::MultipleTypeConstraints does X::Comp {
    has $.parameter;
    method message() {
        ($.parameter ?? "Parameter $.parameter" !! 'A parameter')
        ~ " may only have one prefix type constraint";
    }
}

my class X::Parameter::WrongOrder does X::Comp {
    has $.misplaced;
    has $.parameter;
    has $.after;
    method message() {
        "Cannot put $.misplaced parameter $.parameter after $.after parameters";
    }
}

my class X::Parameter::InvalidType does X::Comp {
    has $.typename;
    has @.suggestions;
    method message() {
        my $msg := "Invalid typename '$.typename' in parameter declaration.";
        if +@.suggestions > 0 {
            $msg := $msg ~ " Did you mean '" ~ @.suggestions.join("', '") ~ "'?";
        }
        return $msg;
    }
}

my class X::Signature::NameClash does X::Comp {
    has $.name;
    method message() {
        "Name $.name used for more than one named parameter";
    }
}

my class X::Method::Private::Permission does X::Comp {
    has $.method;
    has $.source-package;
    has $.calling-package;
    method message() {
        "Cannot call private method '$.method' on package $.source-package because it does not trust $.calling-package";
    }
}

my class X::Method::Private::Unqualified does X::Comp {
    has $.method;
    method message() {
        "Private method call to $.method must be fully qualified with the package containing the method";
    }
}

my class X::Bind is Exception {
    has $.target;
    method message() {
        $.target.defined
            ?? "Cannot bind to $.target"
            !! 'Cannot use bind operator with this left-hand side'
    }
}
my class X::Bind::NativeType does X::Comp {
    has $.name;
    method message() {
        "Cannot bind to natively typed variable '$.name'; use assignment instead"
    }
}
my class X::Bind::Slice is Exception  {
    has $.type;
    method message() {
        "Cannot bind to {$.type.^name} slice";
    }
}
my class X::Bind::ZenSlice is X::Bind::Slice {
    method message() {
        "Cannot bind to {$.type.^name} zen slice";
    }
}

my class X::Value::Dynamic does X::Comp {
    has $.what;
    method message() { "$.what value must be known at compile time" }
}

my class X::Syntax::Name::Null does X::Syntax {
    method message() { 'Name component may not be null'; }
}

my class X::Syntax::UnlessElse does X::Syntax {
    method message() { '"unless" does not take "else", please rewrite using "if"' }
}

my class X::Syntax::KeywordAsFunction does X::Syntax {
    has $.word;
    has $.needparens;
    method message {
        "Word '$.word' interpreted as '{$.word}()' function call; please use whitespace "
            ~ ($.needparens ?? 'around the parens' !! 'instead of parens')
    }
}

my class X::Syntax::Malformed::Elsif does X::Syntax {
    has $.what = 'else if';
    method message() { qq{In Perl 6, please use "elsif' instead of "$.what"} }
}

my class X::Syntax::Reserved does X::Syntax {
    has $.reserved;
    has $.instead = '';
    method message() { "The $.reserved is reserved$.instead" }
}

my class X::Syntax::P5 does X::Syntax {
    method message() { 'This appears to be Perl 5 code' }
}

my class X::Syntax::NegatedPair does X::Syntax {
    has $.key;
    method message() { "Argument not allowed on negated pair with key '$.key'" }
}

my class X::Syntax::Variable::Numeric does X::Syntax {
    has $.what = 'variable';
    method message() { "Cannot declare a numeric $.what" }
}

my class X::Syntax::Variable::Match does X::Syntax {
    method message() { 'Cannot declare a match variable' }
}

my class X::Syntax::Variable::Twigil does X::Syntax {
    has $.twigil;
    has $.scope;
    method message() { "Cannot use $.twigil twigil on $.scope variable" }
}

my class X::Syntax::Variable::IndirectDeclaration does X::Syntax {
    method message() { 'Cannot declare a variable by indirect name (use a hash instead?)' }
}

my class X::Syntax::Augment::WithoutMonkeyTyping does X::Syntax {
    method message() { "augment not allowed without 'use MONKEY_TYPING'" };
}

my class X::Syntax::Augment::Illegal does X::Syntax {
    has $.package;
    method message() { "Cannot augment $.package because it is closed" };
}

my class X::Syntax::Argument::MOPMacro does X::Syntax {
    has $.macro;
    method message() { "Cannot give arguments to $.macro" };
}

my class X::Does::TypeObject is Exception {
    method message() { "Cannot use 'does' operator with a type object." }
}

my class X::Role::Initialization is Exception {
    has $.role;
    method message() { "Can only supply an initialization value for a role if it has a single public attribute, but this is not the case for '{$.role.^name}'" }
}

my class X::Syntax::Comment::Embedded does X::Syntax {
    method message() { "Opening bracket required for #` comment" }
}

my class X::Syntax::Pod::BeginWithoutIdentifier does X::Syntax does X::Pod {
    method message() {
        '=begin must be followed by an identifier; (did you mean "=begin pod"?)'
    }
}

my class X::Syntax::Pod::BeginWithoutEnd does X::Syntax does X::Pod {
    method message() { '=begin without matching =end' }
}

my class X::Syntax::Confused does X::Syntax {
    has $.reason = 'unknown';
    method message() { $.reason eq 'unknown' ?? 'Confused' !! $.reason }
}

my class X::Syntax::Malformed does X::Syntax {
    has $.what;
    method message() { "Malformed $.what" }
}
my class X::Syntax::Missing does X::Syntax {
    has $.what;
    method message() { "Missing $.what" }
}

my class X::Syntax::Perl5Var does X::Syntax {
    has $.name;
    my %m =
      '$*'  => '^^ and $$',
      '$"'  => '.join() method',
      '$$'  => '$*PID',
      '$('  => '$*GID',
      '$)'  => '$*EGID',
      '$<'  => '$*UID',
      '$>'  => '$*EUID',
      '$;'  => 'real multidimensional hashes',
      '$&'  => '$<>',
      '$`'  => 'explicit pattern before <(',
      '$\'' => 'explicit pattern after )>',
      '$,'  => '$*OUT.output_field_separator()',
      '$.'  => "the filehandle's .line method",
      '$\\' => "the filehandle's .ors attribute",
      '$|'  => ':autoflush on open',
      '$?'  => '$! for handling child errors also',
      '$@'  => '$!',
      '$#'  => '.fmt',
      '$['  => 'user-defined array indices',
      '$]'  => '$*PERL_VERSION',

      '$^C' => 'COMPILING namespace',
      '$^D' => '$*DEBUGGING',
      '$^E' => '$!.extended_os_error',
      '$^F' => '$*SYSTEM_FD_MAX',
      '$^H' => '$?FOO variables',
      '$^I' => '$*INPLACE',
      '$^M' => 'a global form such as $*M',
      '$^N' => '$/[*-1]',
      '$^O' => '$?OS or $*OS',
      '$^R' => 'an explicit result variable',
      '$^S' => 'context function',
      '$^T' => '$*BASETIME',
      '$^V' => '$*PERL_VERSION',
      '$^W' => '$*WARNING',
      '$^X' => '$*EXECUTABLE_NAME',

      '$:'  => 'Form module',
      '$-'  => 'Form module',
      '$+'  => 'Form module',
      '$='  => 'Form module',
      '$%'  => 'Form module',
      '$^'  => 'Form module',
      '$~'  => 'Form module',
      '$^A' => 'Form module',
      '$^L' => 'Form module',

      '@-'  => '.from method',
      '@+'  => '.to method',

      '%-'  => '.from method',
      '%+'  => '.to method',
      '%^H' => '$?FOO variables',
    ;
    method message() {
        my $v = $.name ~~ m/ <[ $ @ % & ]> [ \^ <[ A..Z ]> | \W ] /;
        $v
          ?? %m{~$v}
            ?? "Unsupported use of $v variable; in Perl 6 please use {%m{~$v}}"
            !! "Unsupported use of $v variable"
          !! 'Non-declarative sigil is missing its name';
    }
}

my class X::Syntax::Self::WithoutObject does X::Syntax {
    method message() { "'self' used where no object is available" }
}
my class X::Syntax::VirtualCall does X::Syntax {
    has $.call;
    method message() { "Virtual call $.call may not be used on partially constructed objects" }
}
my class X::Syntax::NoSelf does X::Syntax {
    has $.variable;
    method message() { "Variable $.variable used where no 'self' is available" }
}

my class X::Syntax::Number::RadixOutOfRange does X::Syntax {
    has $.radix;
    method message() { "Radix $.radix out of range (allowed: 2..36)" }
}

my class X::Syntax::NonAssociative does X::Syntax {
    has $.left;
    has $.right;
    method message() {
        "Operators '$.left' and '$.right' are non-associative and require parenthesis";
    }
}

my class X::Syntax::Regex::Adverb does X::Syntax {
    has $.adverb;
    has $.construct;
    method message() { "Adverb $.adverb not allowed on $.construct" }
}

my class X::Syntax::Regex::UnrecognizedMetachar does X::Syntax {
    has $.metachar;
    method message() { "Unrecognized regex metacharacter $.metachar (must be quoted to match literally)" }
}

my class X::Syntax::Regex::NullRegex does X::Syntax {
    method message() { 'Null regex not allowed' }
}

my class X::Syntax::Signature::InvocantMarker does X::Syntax {
    method message() {
        "Can only use : as invocant marker in a signature after the first parameter"
    }
}

my class X::Syntax::Extension::Category does X::Syntax {
    has $.category;
    method message() {
        "Cannot add tokens of category '$.category'";
    }
}

my class X::Syntax::Extension::Null does X::Syntax {
    method message() {
        "Null operator is not allowed";
    }
}

my class X::Syntax::InfixInTermPosition does X::Syntax {
    has $.infix;
    method message() {
        "Preceding context expects a term, but found infix $.infix instead";
    }
}

my class X::Attribute::Package does X::Comp {
    has $.package-kind;
    has $.name;
    method message() { "A $.package-kind cannot have attributes, but you tried to declare '$.name'" }
}
my class X::Attribute::NoPackage does X::Comp {
    has $.name;
    method message() { "You cannot declare attribute '$.name' here; maybe you'd like a class or a role?" }
}
my class X::Declaration::Scope does X::Comp {
    has $.scope;
    has $.declaration;
    method message() { "Cannot use '$.scope' with $.declaration declaration" }
}

my class X::Declaration::Scope::Multi is X::Declaration::Scope {
    method message() {
        "Cannot use '$.scope' with individual multi candidates. Please declare an {$.scope}-scoped proto instead";
    }
}

my class X::Anon::Multi does X::Comp {
    has $.multiness;
    has $.routine-type = 'routine';
    method message() { "Cannot put $.multiness on anonymous $.routine-type" }
}
my class X::Anon::Augment does X::Comp {
    has $.package-kind;
    method message() { "Cannot augment anonymous $.package-kind" }
}
my class X::Augment::NoSuchType does X::Comp {
    has $.package-kind;
    has $.package;
    method message() { "You tried to augment $.package-kind $.package, but it does not exist" }
}

my class X::Routine::Unwrap is Exception {
    method message() { "Cannot unwrap routine: invalid wrap handle" }
}

my class X::Constructor::Positional is Exception {
    has $.type;
    method message() { "Default constructor for '" ~ $.type.^name ~ "' only takes named arguments" }
}

my class X::Hash::Store::OddNumber is Exception {
    method message() { "Odd number of elements found where hash expected" }
}

my class X::Package::Stubbed does X::Comp {
    has @.packages;
    # TODO: suppress display of line number
    method message() {
        "The following packages were stubbed but not defined:\n    "
        ~ @.packages.join("\n    ");
    }
}

my class X::Phaser::PrePost is Exception {
    has $.phaser = 'PRE';
    has $.condition;
    method message {
        my $what = $.phaser eq 'PRE' ?? 'Precondition' !! 'Postcondition';
        $.condition.defined
            ?? "$what '$.condition.trim()' failed"
            !! "$what failed";
    }
}

my class X::Str::Numeric is Exception {
    has $.source;
    has $.pos;
    has $.reason;
    method source-indicator {
        constant marker = chr(0x23CF);
        join '', "in '",
                $.source.substr(0, $.pos),
                marker,
                $.source.substr($.pos),
                "' (indicated by ",
                marker,
                ")",
                ;
    }
    method message() {
        "Cannot convert string to number: $.reason $.source-indicator";
    }
}

my class X::Str::Match::x is Exception {
    has $.got;
    method message() {
        "in Str.match, got invalid value of type {$.got.^name} for :x, must be Int or Range"
    }
}

my class X::Str::Trans::IllegalKey is Exception {
    has $.key;
    method message {
        "in Str.trans, got illegal substitution key of type {$.key.^name} (should be a Regex or Str)"
    }
}
my class X::Str::Trans::InvalidArg is Exception {
    has $.got;
    method message() {
        "Only Pair objects are allowed as arguments to Str.trans, got {$.got.^name}";
    }
}

my class X::Range::InvalidArg is Exception {
    has $.got;
    method message() {
        "{$.got.^name} objects are not valid endpoints for Ranges";
    }
}

my class X::Sequence::Deduction is Exception {
    method message() { 'Unable to deduce sequence' }
}

my class X::Backslash::UnrecognizedSequence does X::Syntax {
    has $.sequence;
    method message() { "Unrecognized backslash sequence: '\\$.sequence'" }
}

my class X::Backslash::NonVariableDollar does X::Syntax {
    method message() { "Non-variable \$ must be backslashed" }
}

my class X::ControlFlow is Exception {
    has $.illegal;   # something like 'next'
    has $.enclosing; # ....  outside a loop

    method message() { "$.illegal without $.enclosing" }
}
my class X::ControlFlow::Return is X::ControlFlow {
    method illegal()   { 'return'  }
    method enclosing() { 'Routine' }
    method message()   { 'Attempt to return outside of any Routine' }
}

my class X::Composition::NotComposable does X::Comp {
    has $.target-name;
    has $.composer;
    method message() {
        $.composer.^name ~ " is not composable, so $.target-name cannot compose it";
    }
}

my class X::TypeCheck is Exception {
    has $.operation;
    has $.got;
    has $.expected;
    method message() {
        "Type check failed in $.operation; expected '{$.expected.^name}' but got '{$.got.^name}'";

    }
}

my class X::TypeCheck::Binding is X::TypeCheck {
    method operation { 'binding' }
}
my class X::TypeCheck::Return is X::TypeCheck {
    method operation { 'returning' }
    method message() {
        "Type check failed for return value; expected '{$.expected.^name}' but got '{$.got.^name}'";
    }
}
my class X::TypeCheck::Assignment is X::TypeCheck {
    has $.symbol;
    method operation { 'assignment' }
    method message {
        $.symbol.defined
            ?? "Type check failed in assignment to '$.symbol'; expected '{$.expected.^name}' but got '{$.got.^name}'"
            !! "Type check failed in assignment; expected '{$.expected.^name}' but got '{$.got.^name}'";
    }
}
my class X::TypeCheck::Argument is X::TypeCheck {
    has $.protoguilt;
    has @.arguments;
    has $.objname;
    has $.signature;
    method message { 
            ($.protoguilt ?? "Calling proto of '" !! "Calling '") ~
            $.objname ~ "' " ~
            (+@.arguments == 0
              ?? "requires arguments\n"
              !! "will never work with argument types (" ~ join(', ', @.arguments) ~ ")\n") 
            ~ $.signature 
    }
}

my class X::TypeCheck::Splice is X::TypeCheck does X::Comp {
    has $.action;
    method message {
        "Type check failed in {$.action}; expected {$.expected.^name} but got {$.got.^name}";
    }

}

my class X::Assignment::RO is Exception {
    method message {
        "Cannot modify an immutable value";
    }
}

my class X::Immutable is Exception {
    has $.typename;
    has $.method;
    method message {
        "Cannot call '$.method' on an immutable '$.typename'";
    }
}

my class X::NoDispatcher is Exception {
    has $.redispatcher;
    method message() {
        "$.redispatcher is not in the dynamic scope of a dispatcher";
    }
}

my class X::Localizer::NoContainer is Exception {
    has $.localizer;
    method message() {
        "Can only use '$.localizer' on a container";
    }
}

my class X::Mixin::NotComposable is Exception {
    has $.target;
    has $.rolish;
    method message() {
        "Cannot mix in non-composable type {$.rolish.^name} into object of type {$.target.^name}";
    }
}

my class X::Inheritance::Unsupported does X::Comp {
    # note that this exception is thrown before the child type object
    # has been composed, so it's useless to carry it around. Use the
    # name instead.
    has $.child-typename;
    has $.parent;
    method message {
        $.parent.^name ~ ' does not support inheritance, so '
        ~ $.child-typename ~ ' cannot inherit from it';
    }
}

my class X::Inheritance::UnknownParent is Exception {
    has $.child;
    has $.parent;
    has @.suggestions is rw;

    method message {
        my $message := "'" ~ $.child ~ "' cannot inherit from '" ~ $.parent ~ "' because it is unknown.";
        if +@.suggestions > 1 {
            $message := $message ~ "\nDid you mean one of these?\n    '" ~ @.suggestions.join("'\n    '") ~ "'\n";
        } elsif +@.suggestions == 1 {
            $message := $message ~ "\nDid you mean '" ~ @.suggestions[0] ~ "'?\n";
        }
        return $message;
    }
}

my class X::Inheritance::SelfInherit is Exception {
    has $.name;

    method message {
        "'$.name' cannot inherit from itself."
    }
}

my class X::Export::NameClash does X::Comp {
    has $.symbol;
    method message() {
        "A symbol '$.symbol' has already been exported";
    }
}

my class X::HyperOp::NonDWIM is Exception {
    has &.operator;
    has $.left-elems;
    has $.right-elems;
    method message() {
        "Lists on both side of non-dwimmy hyperop of &.operator.name() are not of the same length\n"
        ~ "left: $.left-elems elements, right: $.right-elems elements"; 
    }
}

my class X::Set::Coerce is Exception {
    has $.thing;
    method message {
        "Cannot coerce object of type {$.thing.^name} to Set. To create a one-element set, pass it to the 'set' function";
    }
}


my role X::Temporal is Exception { }
my class X::Temporal::InvalidFormat does X::Temporal {
    has $.invalid-str;
    has $.target = 'Date';
    has $.format;
    method message() {
        "Invalid $.target string '$.invalid-str'; use $.format instead";
    }
}
my class X::DateTime::TimezoneClash does X::Temporal {
    method message() {
        'DateTime.new(Str): :timezone argument not allowed with a timestamp offset';
    }
}
my class X::DateTime::InvalidDeltaUnit does X::Temporal {
    has $.unit;
    method message() {
        "Cannnot use unit $.unit with Date.delta";
    }
}

my class X::Eval::NoSuchLang is Exception {
    has $.lang;
    method message() {
        "No compiler available for language '$.lang'";
    }
}

my class X::Import::MissingSymbols is Exception {
    has $.from;
    has @.missing;
    method message() {
        "Trying to import from '$.from', but the following symbols are missing: "
            ~ @.missing.join(', ');
    }
}

my class X::Numeric::Real is Exception {
    has $.target;
    has $.reason;
    has $.source;

    method message() {
        "Can not convert $.source to {$.target.^name}: $.reason";
    }
}

my class X::Numeric::DivideByZero is Exception {
    has $.using;
    method message() {
        "Divide by zero" ~ ( $.using ?? " using $.using" !! '' );
    }
}

my class X::PseudoPackage::InDeclaration does X::Comp {
    has $.pseudo-package;
    has $.action;
    method message() {
        "Cannot use pseudo package $.pseudo-package in $.action";
    }
}

my class X::NoSuchSymbol is Exception {
    has $.symbol;
    method message { "No such symbol '$.symbol'" }
}

my class X::Item is Exception {
    has $.aggregate;
    has $.index;
    method message { "Cannot index {$.aggregate.^name} with $.index" }
}

my class X::Multi::Ambiguous is Exception {
    has $.dispatcher;
    has @.ambiguous;
    method message {
        join "\n",
            "Ambiguous call to '$.dispatcher.name()'; these signatures all match:",
            @.ambiguous.map(*.signature.perl)
    }
}

my class X::Multi::NoMatch is Exception {
    has $.dispatcher;
    method message {
        join "\n",
            "Cannot call '$.dispatcher.name()'; none of these signatures match:",
            $.dispatcher.dispatchees.map(*.signature.perl)
    }
}

my class X::Caller::NotDynamic is Exception {
    has $.symbol;
    method message() {
        "Cannot access '$.symbol' through CALLER, because it is not declared as dynamic";
    }
}

{
    my %c_ex;
    %c_ex{'X::TypeCheck::Binding'} := sub ($got, $expected) is hidden_from_backtrace {
            X::TypeCheck::Binding.new(:$got, :$expected).throw;
        };
    %c_ex<X::TypeCheck::Assignment> := sub ($symbol, $got, $expected) is hidden_from_backtrace {
            X::TypeCheck::Assignment.new(:$symbol, :$got, :$expected).throw;
        };
    %c_ex{'X::TypeCheck::Return'} := sub ($got, $expected) is hidden_from_backtrace {
            X::TypeCheck::Return.new(:$got, :$expected).throw;
        };
    %c_ex<X::Assignment::RO> := sub () is hidden_from_backtrace {
            X::Assignment::RO.new.throw;
        };
    %c_ex{'X::ControlFlow::Return'} := sub () is hidden_from_backtrace {
            X::ControlFlow::Return.new().throw;
        };
    %c_ex{'X::NoDispatcher'} := sub ($redispatcher) is hidden_from_backtrace {
            X::NoDispatcher.new(:$redispatcher).throw;
        };
    %c_ex{'X::Multi::Ambiguous'} := sub ($dispatcher, @ambiguous) is hidden_from_backtrace {
            X::Multi::Ambiguous.new(:$dispatcher, :@ambiguous).throw
        };
    %c_ex{'X::Multi::NoMatch'} := sub ($dispatcher) is hidden_from_backtrace {
            X::Multi::NoMatch.new(:$dispatcher).throw
        };
    my Mu $parrot_c_ex := nqp::getattr(%c_ex, EnumMap, '$!storage');
    nqp::bindcurhllsym('P6EX', $parrot_c_ex);
    
    0;
}


# vim: ft=perl6
