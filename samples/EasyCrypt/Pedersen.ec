(*
 * A formal verification of the Pedersen commitment scheme
 *
 * Pedersen, Torben Pryds
 * "Non-interactive and information-theoretic secure verifiable secret sharing"
 *)
require import Real.
require (****) DLog.

clone DLog as DL.
import DL.G DL.GP DL.FD DL.GP.ZModE.

require (*--*) Commitment.

(* Pedersen protocol types *)
theory PedersenTypes.
  type value        = group.
  type message      = exp.
  type commitment   = group.
  type openingkey   = exp.
end PedersenTypes.
export PedersenTypes.

(* Instantiate the Commitment scheme with the above types *)
clone import Commitment as CM with
  type CommitmentProtocol.value      <- value,
  type CommitmentProtocol.message    <- message,
  type CommitmentProtocol.commitment <- commitment,
  type CommitmentProtocol.openingkey <- openingkey.
export CommitmentProtocol.

module Pedersen : CommitmentScheme = {
  proc gen() : value = {
    var x, h;
    x <$ dt;
    h <- g ^ x;
    return h;
  }

  proc commit(h: value, m: message) : commitment * openingkey = {
    var c, d;
    d <$ dt;
    c <- (g ^ d) * (h ^ m);
    return (c, d);
  }

  proc verify(h: value, m: message, c: commitment, d: openingkey) : bool = {
    var c';
    c' <- (g ^ d) * (h ^ m);
    return (c = c');
  }
}.

module DLogAttacker(B:Binder) : DL.DLog.Adversary = {
  proc guess (h: group) : exp option = {

    var x, c, m, m', d, d';
    (c, m, d, m', d') <@ B.bind(h);
    if ((c = g ^ d * h ^ m) /\ (c = g ^ d' * h ^ m') /\ (m <> m'))
      x <- Some((d - d') * inv (m' - m));
    else
      x <- None;

    return x;
  }
}.

section PedersenSecurity.

  (* Correctness *)
  lemma pedersen_correctness:
    hoare[Correctness(Pedersen).main: true ==> res].
  proof. proc; inline *;auto. qed.

  local module FakeCommit(U:Unhider) = {
    proc main() : bool = {
      var b, b', x, h, c, d;
      var m0, m1 : exp;

      (* Clearly, there are many useless lines, but their presence helps for the proofs *)
      x <$ dt;
      h <- g^x;
      (m0, m1) <@ U.choose(h);
      b <$ {0,1};
      d <$ dt;
      c <- g^d; (* message independent - fake commitment *)
      b' <@ U.guess(c);

      return (b = b');
    }
  }.

  local lemma hi_ll (U<:Unhider):
    islossless U.choose =>
    islossless U.guess =>
    islossless FakeCommit(U).main.
  proof.
    by move => uc_ll ug_ll; islossless; (apply dt_ll || apply DBool.dbool_ll).
  qed.

  (* Perfect hiding *)
  local lemma fakecommit_half (U<:Unhider) &m:
    islossless U.choose =>
    islossless U.guess =>
    Pr[FakeCommit(U).main() @ &m : res] = 1%r/2%r.
  proof.
    move => uc_ll ug_ll; byphoare => //.
    proc; wp.
    swap 4 3.
    rnd (pred1 b'); call ug_ll; wp; rnd; call uc_ll; auto => />.
    by rewrite dt_ll /= => v _ result; rewrite DBool.dbool1E.
  qed.

  local lemma phi_hi (U<:Unhider) &m:
    Pr[HidingExperiment(Pedersen,U).main() @ &m : res] =
    Pr[FakeCommit(U).main() @ &m : res].
  proof.
    byequiv => //.
    proc; inline*.
    call (_:true); wp.
    rnd (fun d, (d + x * (b?m1:m0)){2})
        (fun d, (d - x * (b?m1:m0)){2}).
    by wp; rnd; call (_: true); auto => />; progress; algebra.
  qed.

  (* Perfect hiding - QED *)
  lemma pedersen_perfect_hiding (U<:Unhider) &m:
    islossless U.choose =>
    islossless U.guess =>
    Pr[HidingExperiment(Pedersen,U).main() @ &m : res] = 1%r/2%r.
  proof.
    by move => uc_ll ug_ll; rewrite (phi_hi U &m) (fakecommit_half U &m).
  qed.

  (* Computational binding - QED *)
  lemma pedersen_computational_binding (B<:Binder) &m:
    Pr[BindingExperiment(Pedersen, B).main() @ &m : res] =
    Pr[DL.DLog.DLogExperiment(DLogAttacker(B)).main() @ &m : res].
  proof.
    byequiv => //.
    proc; inline*.
    wp; call (_: true); auto => /> x _ [ c m d m' d'] /= comm comm' m_neq_m'.
    rewrite eq_sym eqT.
    have ->: (d - d') * inv (m' - m) = x <=> (d - d') = x * (m' - m).
    + by split => [<- | ->]; field; apply: contra m_neq_m' => heq;ring heq.
    have -> : d - d' = x * (m' - m) <=> d + x * m = d' + x * m'.
    + by split => heq; ring heq.
    by rewrite pow_bij !(expD, expM, -comm, -comm').
  qed.

  (*
     The following two are to compare probability directly with book discrete
     logarithm experiment. Not strictly necessary though, only for completeness.
  *)
  local lemma std_red_dl_bridge (B<:Binder) &m:
    Pr[DL.DLog.DLogExperiment(DLogAttacker(B)).main() @ &m : res] <=
    Pr[DL.DLog.DLogStdExperiment(DL.StdRedAdversary(DLogAttacker(B))).main() @ &m : res].
  proof.
    byequiv => //.
    proc; wp; inline{2} DL.StdRedAdversary(DLogAttacker(B)).guess; wp.
    seq 2 3: (x'{1} = lx{2} /\ x{1} = x{2}).
    + by inline*; wp; call (_: true); auto.
    by if{2}; auto => />; apply dt_ll.
  qed.

  lemma pedersen_std_computational_binding (B<:Binder) &m:
    Pr[BindingExperiment(Pedersen, B).main() @ &m : res] <=
    Pr[DL.DLog.DLogStdExperiment(DL.StdRedAdversary(DLogAttacker(B))).main() @ &m : res]
  by rewrite(pedersen_computational_binding B &m); apply (std_red_dl_bridge B &m).

end section PedersenSecurity.

print pedersen_correctness.
print pedersen_perfect_hiding.
print pedersen_computational_binding.
