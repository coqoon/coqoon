(* Sleep.ml4
   A time-wasting tactic for Coq 8.5

   This file is free software; its author grants unlimited permission to copy
   and/or distribute it, with or without modifications. *)

(* This tactic is, surprisingly, not completely useless: it lets you create
   arbitrary delays to simulate long-running computations without having to
   do any real work. (This is particularly useful when debugging Coqoon:
   running two JVMs, one of them in debug mode, and /then/ doing something
   complex in Coq is almost guaranteed to bring your computer grinding to a
   halt...) *)

DECLARE PLUGIN "Sleep"

let sleep n =
  let _ = Unix.sleep n in
    Proofview.tclUNIT ()

TACTIC EXTEND sleep
| [ "sleep" natural_opt(n) ] -> [
    match n with
    | Some(n) when n == 0 -> Errors.error "Not enough time to get comfy."
    | Some(n) -> sleep n
    | _ -> sleep 5 ]
END
