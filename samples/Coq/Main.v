Require Import FunctionNinjas.All.
Require Import ListString.All.
Require Import Computation.

Import C.Notations.

Definition error (message : LString.t) : C.t :=
  do_call! Command.ShowError message in
  ret.

Definition main : C.t :=
  call! card_is_valid := Command.AskCard in
  if card_is_valid then
    call! pin := Command.AskPIN in
    match pin with
    | None => error @@ LString.s "No PIN given."
    | Some pin =>
      call! pin_is_valid := Command.CheckPIN pin in
      if pin_is_valid then
        call! ask_amount := Command.AskAmount in
        match ask_amount with
        | None => error @@ LString.s "No amount given."
        | Some amount =>
          call! amount_is_valid := Command.CheckAmount amount in
          if amount_is_valid then
            call! card_is_given := Command.GiveCard in
            if card_is_given then
              call! amount_is_given := Command.GiveAmount amount in
              if amount_is_given then
                ret
              else
                error @@ LString.s "Cannot give you the amount. Please contact your bank."
            else
              error @@ LString.s "Cannot give you back the card. Please contact your bank."
          else
            error @@ LString.s "Invalid amount."
        end
      else
        error @@ LString.s "Invalid PIN."
    end
  else
    error @@ LString.s "Invalid card.".
