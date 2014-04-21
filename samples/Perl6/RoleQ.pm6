role q {
    token stopper { \' }

    token escape:sym<\\> { <sym> <item=.backslash> }

    token backslash:sym<qq> { <?before 'q'> <quote=.LANG('MAIN','quote')> }
    token backslash:sym<\\> { <text=.sym> }
    token backslash:sym<stopper> { <text=.stopper> }

    token backslash:sym<miscq> { {} . }

    method tweak_q($v) { self.panic("Too late for :q") }
    method tweak_qq($v) { self.panic("Too late for :qq") }
}

role qq does b1 does c1 does s1 does a1 does h1 does f1 {
    token stopper { \" }
    token backslash:sym<unrec> { {} (\w) { self.throw_unrecog_backslash_seq: $/[0].Str } }
    token backslash:sym<misc> { \W }

    method tweak_q($v) { self.panic("Too late for :q") }
    method tweak_qq($v) { self.panic("Too late for :qq") }
}
