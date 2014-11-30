theory HelloWorld
imports Main
begin

section{*Playing around with Isabelle*}

text{* creating a lemma with the name hello_world*}
lemma hello_world: "True" by simp

(*inspecting it*)
thm hello_world

text{* defining a string constant HelloWorld *}

definition HelloWorld :: "string" where
  "HelloWorld \<equiv> ''Hello World!''"

(*reversing HelloWorld twice yilds HelloWorld again*)
theorem "rev (rev HelloWorld) = HelloWorld"
  by (fact List.rev_rev_ident)

text{*now we delete the already proven List.rev_rev_ident lema and show it by hand*}
declare List.rev_rev_ident[simp del]
hide_fact List.rev_rev_ident

(*It's trivial since we can just 'execute' it*)
corollary "rev (rev HelloWorld) = HelloWorld"
  apply(simp add: HelloWorld_def)
  done

text{*does it hold in general?*}
theorem rev_rev_ident:"rev (rev l) = l"
  proof(induction l)
  case Nil thus ?case by simp
  next
  case (Cons l ls)
    assume IH: "rev (rev ls) = ls"
    have "rev (l#ls) = (rev ls) @ [l]" by simp
    hence "rev (rev (l#ls)) = rev ((rev ls) @ [l])" by simp
    also have "\<dots> = [l] @ rev (rev ls)" by simp
    finally show "rev (rev (l#ls)) = l#ls" using IH by simp
  qed

corollary "\<forall>(l::string). rev (rev l) = l" by(fastforce intro: rev_rev_ident)

end
