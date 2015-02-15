let sqr x = x *. x

let chi2UniformDistance distrib =
  let count, len = Array.fold_left (fun (s, c) e -> s + e, succ c)
  				   (0, 0) distrib in
  let expected = float count /. float len in
  let distance = Array.fold_left (fun s e ->
    s +. sqr (float e -. expected) /. expected
  ) 0. distrib in
  let dof = float (pred len) in
  dof, distance

let chi2Proba dof distance =
  Gsl_sf.gamma_inc_Q (0.5 *. dof) (0.5 *. distance)

let chi2IsUniform distrib significance =
  let dof, distance = chi2UniformDistance distrib in
  let likelihoodOfRandom = chi2Proba dof distance in
  likelihoodOfRandom > significance

let _ =
  List.iter (fun distrib ->
    let dof, distance = chi2UniformDistance distrib in
    Printf.printf "distribution ";
    Array.iter (Printf.printf "\t%d") distrib;
    Printf.printf "\tdistance %g" distance;
    Printf.printf "\t[%g > 0.05]" (chi2Proba dof distance);
    if chi2IsUniform distrib 0.05 then Printf.printf " fair\n"
    else Printf.printf " unfair\n"
  )
  [
    [| 199809; 200665; 199607; 200270; 199649 |];
    [| 522573; 244456; 139979; 71531; 21461 |]
  ]
