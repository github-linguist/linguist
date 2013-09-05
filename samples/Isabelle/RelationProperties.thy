(*
Auction Theory Toolbox (http://formare.github.io/auctions/)

Authors:
* Marco B. Caminati <marco.caminati@gmail.com>
* Christoph Lange <math.semantic.web@gmail.com>

Dually licenced under
* Creative Commons Attribution (CC-BY) 3.0
* ISC License (1-clause BSD License)
See LICENSE file for details
(Rationale for this dual licence: http://arxiv.org/abs/1107.3212)
*)

theory RelationProperties
imports Main SetUtils
begin

section {* restriction *}

text {* restriction of a relation to a set (usually resulting in a relation with a smaller domain) *}
definition restrict
(* TODO MC: compare with restr in SchorrWaite.thy
   CL@MC: doesn't seem helpful, as its type "('a \<times> 'a) set \<Rightarrow> ('a \<Rightarrow> bool) \<Rightarrow> ('a \<times> 'a) set" is 
   more specific than what we need. *)
:: "('a \<times> 'b) set \<Rightarrow> 'a set \<Rightarrow> ('a \<times> 'b) set" (infix "||" 75)
where "R || X = X \<times> (Range R) \<inter> R"

text {* Restricting a relation to the empty set yields the empty set. *}
lemma restrict_empty: "P || {} = {}"
unfolding restrict_def by simp

text {* A restriction is a subrelation of the original relation. *}
lemma restriction_is_subrel: "P || X \<subseteq> P"
using restrict_def by blast

text {* Restricting a relation only has an effect within its domain. *}
lemma restriction_within_domain: "P || X = P || (X \<inter> (Domain P))"
unfolding restrict_def by fast

text {* alternative characterisation of the restriction of a relation to a singleton set *}
lemma restrict_to_singleton: "P || {x} = {x} \<times> P `` {x}"
unfolding restrict_def by fast

section {* relation outside some set *}

text {* For a set-theoretical relation @{term R} and an ``exclusion'' set @{term X}, return those
  tuples of @{term R} whose first component is not in @{term X}.  In other words, exclude @{term X}
  from the domain of @{term R}. *}
definition Outside :: "('a \<times> 'b) set \<Rightarrow> 'a set \<Rightarrow> ('a \<times> 'b) set" (infix "outside" 75) (* MC: 75 or whatever, for what I know *)
where "Outside R X = R - (X \<times> Range R)"

text {* Considering a relation outside some set @{term X} reduces its domain by @{term X}. *}
lemma outside_reduces_domain: "Domain (P outside X) = Domain P - X"
unfolding Outside_def by fast

text {* For any set, a relation equals the union of its restriction to that set and its
  pairs outside that set. *}
lemma outside_union_restrict: "P = P outside X \<union> P || X"
unfolding Outside_def restrict_def by fast

text {* The range of a relation @{term R} outside some exclusion set @{term X} is a 
  subset of the image of the domain of @{term R}, minus @{term X}, under @{term R}. *}
lemma Range_outside_sub_Image_Domain: "Range (R outside X) \<subseteq> R `` (Domain R - X)"
using Outside_def Image_def Domain_def Range_def by blast

section {* evaluation as a function *}

text {* Evaluates a relation @{term R} for a single argument, as if it were a function.
  This will only work if @{term R} is a total function, i.e. if the image is always a singleton set. *}
fun eval_rel :: "('a \<times> 'b) set \<Rightarrow> 'a \<Rightarrow> 'b" (infix ",," 75) (* . (Mizar's notation) confuses Isar *)
where "eval_rel R a = the_elem (R `` {a})"

section {* Image *}

text {* The image of a relation is only effective within the domain of that relation *}
lemma Image_within_domain: "R `` X = R `` (X \<inter> Domain R)"
by fast

text {* An alternative phrasing of @{thm Image_within_domain} *}
lemma Image_within_domain': fixes x R shows "x \<in> Domain R \<longleftrightarrow> R `` {x} \<noteq> {}"
using Image_within_domain by blast

text {* The image of a set outside a relation's domain under that domain is empty. *}
lemma Image_outside_domain:
  fixes X::"'a set"
    and R::"('a \<times> 'b) set"
shows "X \<inter> Domain R = {} \<longleftrightarrow> R `` X = {}"
using Image_within_domain by blast

section {* right-uniqueness *}

text {* right-uniqueness of a relation (in other words: the relation is a function on its domain) *}
definition runiq :: "('a \<times> 'b) set \<Rightarrow> bool" where
(*"runiq R = (\<forall> x . R `` {x} \<subseteq> {R ,, x})"*)
"runiq R = (\<forall> x \<in> Domain R . trivial (R `` {x}))"

text {* an alternative definition of right-uniqueness in terms of @{const eval_rel} *}
lemma runiq_wrt_eval_rel:
  fixes R :: "('a \<times> 'b) set"
  shows "runiq R = (\<forall>x . R `` {x} \<subseteq> {R ,, x})"
using assms unfolding runiq_def trivial_def
by (metis (lifting) Domain_iff Image_singleton_iff eval_rel.simps subsetI)

text {* A subrelation of a right-unique relation is right-unique. *}
lemma subrel_runiq:
  fixes Q::"('a \<times> 'b) set"
    and R::"('a \<times> 'b) set"
  assumes "runiq Q"
      and "R \<subseteq> Q"
shows "runiq R"
proof -
  {
    fix a assume "a \<in> Domain R"
    then have "trivial (Q `` {a}) \<and> R `` {a} \<subseteq> (Q `` {a})" 
      using assms unfolding runiq_def trivial_def by fast
    then have "trivial (R `` {a})" using trivial_subset by (rule conjE)
  }
  then show ?thesis using runiq_def by blast
qed

text {* A singleton relation is right-unique. *}
lemma runiq_singleton_rel: "runiq {(x, y)}" (is "runiq ?R")
unfolding runiq_def
proof
  fix z assume "z \<in> Domain ?R"
  then have "z = x" by simp
  then have "?R `` {z} = {y}" by fastforce
  then show "trivial (?R `` {z})" unfolding trivial_def by (rule equalityE) simp
qed

text {* A trivial relation is right-unique *}
lemma runiq_trivial_rel:
  assumes "trivial R"
  shows "runiq R"
using assms runiq_singleton_rel trivial_def
by (metis prod.exhaust subrel_runiq)

text {* alternative characterisation of the fact that, if a relation @{term R} is right-unique,
  its evaluation @{term "R,,x"} on some argument @{term x} in its domain, occurs in @{term R}'s
  range. *}
lemma eval_runiq_rel:
  assumes domain: "x \<in> Domain R"
      and runiq: "runiq R" 
  shows "(x, R,,x) \<in> R"
proof -
  have "trivial (R `` {x})" using domain runiq unfolding runiq_def by fast
  then have "R ,, x \<in> R `` {x}" using domain
    by (metis Image_within_domain' RelationProperties.eval_rel.simps subset_empty subset_insert trivial_def)
  then show ?thesis by fast 
qed

text {* The image of a singleton set under a right-unique relation is a singleton set. *}
lemma Image_runiq_eq_eval:
  assumes "x \<in> Domain R"
      and "runiq R" 
  shows "R `` {x} = {R ,, x}"
using assms runiq_wrt_eval_rel
by (metis Image_within_domain' subset_singletonD)

text {* If the images of two sets @{term X} and @{term Y} under a relation @{term R} are 
  disjoint, @{term X} and @{term Y} are disjoint on the domain of @{term R}. *}
lemma disj_Image_imp_disj_Domain:
  assumes "R `` X \<inter> R `` Y = {}" 
  shows "Domain R \<inter> X \<inter> Y = {}"
using assms by auto

section {* paste *}

text {* the union of two binary relations @{term P} and @{term Q}, where pairs from @{term Q}
  override pairs from @{term P} when their first components coincide *}
definition paste (infix "+*" 75)
where "P +* Q = (P outside Domain Q) \<union> Q"
(* Avoids possible conflicts btw P & Q using `outside', 
thus giving precedence to Q. This is particularly useful when 
P, Q are functions, and we want to preserve that property. *)

text {* If a relation @{term P} is a subrelation of another relation @{term Q} on @{term Q}'s
  domain, pasting @{term Q} on @{term P} is the same as forming their union. *}
lemma paste_subrel: assumes "P || Domain Q \<subseteq> Q" shows "P +* Q = P \<union> Q"
unfolding paste_def using assms outside_union_restrict by blast

text {* Pasting two relations with disjoint domains is the same as forming their union. *}                                                                                                
lemma paste_disj_domains: assumes "Domain P \<inter> Domain Q = {}" shows "P +* Q = P \<union> Q"
unfolding paste_def Outside_def
using assms
by fast

text {* A relation @{term P} is equivalent to pasting its restriction to some set @{term X} on 
  @{term "P outside X"}. *}
lemma paste_outside_restrict: "P = (P outside X) +* (P || X)"
proof -
  have "Domain (P outside X) \<inter> Domain (P || X) = {}"
    unfolding Outside_def restrict_def by fast
  moreover have "P = P outside X \<union> P || X" by (rule outside_union_restrict)
  ultimately show ?thesis using paste_disj_domains by metis
qed

text {* Pasting @{term Q} on @{term P} yields a right-unique relation if @{term Q} is 
  right-unique, and @{term P} is right-unique outside @{term Q}'s domain. *}
lemma runiq_paste1:
  fixes P::"('a \<times> 'b) set"
    and Q::"('a \<times> 'b) set"
  assumes "runiq Q"
      and "runiq (P outside Domain Q)" (is "runiq ?PoutsideQ")
  shows "runiq (P +* Q)"
proof - 
  have disjoint_domains: "Domain ?PoutsideQ \<inter> Domain Q = {}"
    using outside_reduces_domain by (metis Diff_disjoint inf_commute)
  {
    fix a assume "a \<in> Domain (?PoutsideQ \<union> Q)"
    then have triv: "trivial (?PoutsideQ `` {a}) \<and> trivial (Q `` {a})"
      using assms by (metis Image_within_domain' runiq_def trivial_empty)
    then have "?PoutsideQ `` {a} = {} \<or> Q `` {a} = {}" using disjoint_domains by blast
    then have "(?PoutsideQ \<union> Q) `` {a} = Q `` {a} \<or> (?PoutsideQ \<union> Q) `` {a} = ?PoutsideQ `` {a}" by blast
    then have "trivial ((?PoutsideQ \<union> Q) `` {a})" using triv by presburger
  }
  then have "runiq (?PoutsideQ \<union> Q)" unfolding runiq_def by blast
  then show ?thesis unfolding paste_def .
qed

text {* Pasting two right-unique relations yields a right-unique relation. *}
corollary runiq_paste2:
  assumes "runiq Q"
      and "runiq P" 
shows "runiq (P +* Q)"
using assms runiq_paste1 subrel_runiq
by (metis Diff_subset Outside_def)

text {* Pasting a singleton relation on some other right-unique relation @{term R} yields a
  right-unique relation if the single element of the singleton's domain is not yet in the 
  domain of @{term R}. *}
lemma runiq_paste3:
  assumes "runiq R"
      and "x \<notin> Domain R" 
  shows "runiq (R +* {(x, y)})"
using assms runiq_paste2 runiq_singleton_rel by metis

text {* The domain of two pasted relations equals the union of their domains. *}
lemma paste_Domain: "Domain (P +* Q) = Domain P \<union> Domain Q"
unfolding paste_def Outside_def by blast

text {* Pasting two relations yields a subrelation of their union. *}
lemma paste_sub_Un: "P +* Q \<subseteq> P \<union> Q"
unfolding paste_def Outside_def by fast

text {* The range of two pasted relations is a subset of the union of their ranges. *}
lemma paste_Range: "Range (P +* Q) \<subseteq> Range P \<union> Range Q"
using paste_sub_Un by blast

section {* Converse *}

text {* The definition of @{const converse} isn't suitable for generating code, so we provide
  a code equation using an alternative definition. *}
lemma [code_unfold]: "converse R = { (y, x) . (x, y) \<in> R }" by (rule converse_unfold)

text {* If two relations are subrelations of each other, so are their converse relations. *}
lemma converse_subrel: assumes "P \<subseteq> Q" shows "P\<inverse> \<subseteq> Q\<inverse>"
using assms by fast

text {* The domain of the inverse of a relation is the relation's range. *}
lemma Domain_conv_Range: "Domain (R\<inverse>)=Range R"
by simp

text {* alternative characterisation of the intersection of a relation's domain with some set, in
  terms of the converse relation *}
lemma Domain_Int_wrt_converse: "Domain R \<inter> X \<subseteq> R\<inverse> `` (R `` X)"
by fast

text {* The inverse image of the image of a singleton set under some relation is the same
  singleton set, if both the relation and its converse are right-unique and the singleton set
  is in the relation's domain. *}
lemma converse_Image_singleton_Domain:
  assumes runiq: "runiq R"
      and runiq_conv: "runiq (R\<inverse>)"
      and domain: "x \<in> Domain R"
shows "R\<inverse> `` R `` {x} = {x}"
proof -
  have sup: "{x} \<subseteq> R\<inverse> `` R `` {x}" using Domain_Int_wrt_converse domain by fast
  have "trivial (R `` {x})" using runiq domain unfolding runiq_def by fast
  then have "trivial (R\<inverse> `` R `` {x})"
    using assms
    by (metis Image_runiq_eq_eval RelationProperties.eval_rel.simps runiq_wrt_eval_rel trivial_def)
  then show ?thesis
    using sup by (metis singleton_sub_trivial_uniq subset_antisym trivial_def)
qed

text {* The inverse image of the image of a singleton set under some relation is the same
  singleton set or empty, if both the relation and its converse are right-unique. *}
lemma converse_Image_singleton:
  assumes "runiq R"
      and "runiq (R\<inverse>)"
  shows "R\<inverse> `` R `` {x} \<subseteq> {x}"
using assms converse_Image_singleton_Domain
by (metis Image_empty Image_within_domain' empty_subsetI set_eq_subset)

text {* The inverse image of the image of a set under some relation is a subset of that set,
  if both the relation and its converse are right-unique. *}
lemma converse_Image: 
  assumes runiq: "runiq R"
      and runiq_conv: "runiq (R\<inverse>)"
shows "R\<inverse> `` R `` X \<subseteq> X"
proof -
  have "(R O R\<inverse>) `` X = (\<Union>x \<in> X . (R O R\<inverse>) `` {x})" by (rule Image_eq_UN)
  also have "\<dots> = (\<Union>x\<in>X. R\<inverse> `` R `` {x})" by blast
  also have "\<dots> \<subseteq> (\<Union>x \<in> X. {x})" using converse_Image_singleton assms by fast
  also have "\<dots> = X" by simp
  finally show ?thesis by fast
qed

text {* The inverse statement of @{thm disj_Image_imp_disj_Domain} holds when both the 
  relation and its converse are right-unique. *}
lemma disj_Domain_imp_disj_Image: assumes "Domain R \<inter> X \<inter> Y = {}" 
  assumes "runiq R"
      and "runiq (R\<inverse>)"
  shows "R `` X \<inter> R `` Y = {}"
proof -
  let ?X_on_Dom = "Domain R \<inter> X"
  let ?Y_on_Dom = "Domain R \<inter> Y"
  have "R\<inverse> `` (R `` ?X_on_Dom) \<subseteq> ?X_on_Dom" using converse_Image assms by fast
  moreover have "R\<inverse> `` (R `` ?Y_on_Dom) \<subseteq> ?Y_on_Dom" using converse_Image assms by metis
  ultimately have "R\<inverse> `` R `` ?X_on_Dom \<inter> R\<inverse> `` R `` ?Y_on_Dom \<subseteq> {}" using assms by blast
  moreover have "?X_on_Dom \<inter> ?Y_on_Dom = {}" using assms by blast
  ultimately
  have "{} = Domain (R\<inverse>) \<inter> R `` ?X_on_Dom \<inter> R `` ?Y_on_Dom"
    using disj_Image_imp_disj_Domain by fast
  also have "\<dots> = Range R \<inter> R `` ?X_on_Dom \<inter> R `` ?Y_on_Dom" using Domain_conv_Range by metis
  also have "\<dots> = R `` ?X_on_Dom \<inter> R `` ?Y_on_Dom" by blast
  finally show ?thesis by auto
qed

text {* The converse relation of two pasted relations is right-unique, if 
  the relations have disjoint domains and ranges, and if their converses are both
  right-unique. *}
lemma runiq_converse_paste:
  assumes runiq_P_conv: "runiq (P\<inverse>)"
      and runiq_Q_conv: "runiq (Q\<inverse>)"
      and disj_D: "Domain P \<inter> Domain Q = {}"
      and disj_R: "Range P \<inter> Range Q = {}"
  shows "runiq ((P +* Q)\<inverse>)"
proof -
  have "P +* Q = P \<union> Q" using disj_D by (rule paste_disj_domains)
  then have "(P +* Q)\<inverse> = P\<inverse> \<union> Q\<inverse>" by auto
  also have "\<dots> = P\<inverse> +* Q\<inverse>" using disj_R paste_disj_domains Domain_conv_Range by metis
  finally show ?thesis using runiq_P_conv runiq_Q_conv runiq_paste2 by auto
qed

text {* The converse relation of a singleton relation pasted on some other relation @{term R} is right-unique,
  if the singleton pair is not in @{term "Domain R \<times> Range R"}, and if @{term "R\<inverse>"} is right-unique. *}
lemma runiq_converse_paste_singleton:
  assumes runiq: "runiq (R\<inverse>)" 
      and y_notin_R: "y \<notin> Range R"
      and x_notin_D: "x \<notin> Domain R"
  shows "runiq ((R +* {(x,y)})\<inverse>)"
proof -
  have "{(x,y)}\<inverse> = {(y,x)}" by fastforce
  then have "runiq ({(x,y)}\<inverse>)" using runiq_singleton_rel by metis
  moreover have "Domain R \<inter> Domain {(x,y)} = {}" and "Range R \<inter> (Range {(x,y)})={}"
    using y_notin_R x_notin_D by simp_all
  ultimately show ?thesis using runiq runiq_converse_paste by blast
qed

section {* Injective functions *}

text {* Given a relation @{term R}, an element @{term x} of the relation's domain type and
  a set @{term Y} of the relation's range type, this function constructs the list of all 
  superrelations of @{term R} that extend @{term R} by a pair @{term "(x,y)"} for some
  @{term y} not yet covered by @{term R}. *}
fun sup_rels_from :: "('a \<times> 'b\<Colon>linorder) set \<Rightarrow> 'a \<Rightarrow> 'b set \<Rightarrow> ('a \<times> 'b) set list"
where 
"sup_rels_from R x Y = [ R +* {(x,y)} . y \<leftarrow> sorted_list_of_set (Y - Range R) ]"
(* Y or Y-Range R ? *)

text {* the list of all injective functions (represented as relations) from one set 
  (represented as a list) to another set *}
fun injections :: "'a list \<Rightarrow> 'b\<Colon>linorder set \<Rightarrow> ('a \<times> 'b) set list"
where "injections [] Y = [{}]" |
      "injections (x # xs) Y = concat [ sup_rels_from R x Y . R \<leftarrow> injections xs Y ]"
(* We need this as a list in order to be able to iterate over it.  It would be easy to provide 
   an alternative of type ('a \<times> 'b) set set, by using \<Union> and set comprehension. *)

(* TODO CL: Maybe introduce a variant of injections that can also generate partial functions.
   This would have to be done by recursing not just to "xs", but to all sublists of "x # xs" of length n - 1. *)

(* TODO CL: check how much of the following we still need *)
section {* Christoph's old stuff *}

definition left_total_on :: "('a \<times> 'b) set \<Rightarrow> 'a set \<Rightarrow> bool"
where "left_total_on R A \<longleftrightarrow> (\<forall> x \<in> A . \<exists> y . (x, y) \<in> R)"

definition function_on :: "('a \<times> 'b) set \<Rightarrow> 'a set \<Rightarrow> bool"
where "function_on R A \<longleftrightarrow> left_total_on R A \<and> runiq R"

fun as_part_fun :: "('a \<times> 'b) set \<Rightarrow> 'a \<rightharpoonup> 'b"
where "as_part_fun R a = (let im = R `` {a} in 
        if card im = 1 then Some (the_elem im)
        else None)"

fun eval_rel_or :: "('a \<times> 'b) set \<Rightarrow> 'a \<Rightarrow> 'b \<Rightarrow> 'b"
where "eval_rel_or R a z = (let im = R `` {a} in if card im = 1 then the_elem im else z)"

definition to_relation :: "('a \<Rightarrow> 'b) \<Rightarrow> ('a set) \<Rightarrow> ('a \<times> 'b) set"
where "to_relation f X = {(x, f x) | x . x \<in> X}"

definition injective :: "('a \<times> 'b) set \<Rightarrow> bool"
where "injective R \<longleftrightarrow> (\<forall> a \<in> Domain R . \<forall> b \<in> Domain R . R `` {a} = R `` {b} \<longrightarrow> a = b)"

end
