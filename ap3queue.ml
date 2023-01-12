(*
 À compiler avec ocamlc -c ap3queue.ml
 *)

(* Le type des files représentées par une paire de listes *)
type 'a t_queue = 'a list * 'a list
;;

let empty () : 'a t_queue = ([], [])
;;

let isEmpty (f : ' a t_queue) = (f = ([], [])) 
(* ou alors ([], _) on peut aussi mettre ça *)
;;

let enter (x , q : 'a * 'a t_queue) : 'a t_queue =
  let (l1, l2) = q
  in
    if l1 = [] then ([x], l2) else (l1, x::l2)
;;

let qhd (q : 'a t_queue) : 'a =
  if isEmpty(q) then invalid_arg "qhd : the queue is empty"
  else
    let (l1, l2) = q
    in
      List.hd l1
	(* quelle que soit la longueur 
	   de la liste List.hd est en O(1)
	   => qhd est en O(1) car les files 
	   sont en forme normale
	*)
;;

let qrest (q : 'a t_queue) : 'a t_queue =
  if isEmpty q then invalid_arg "qrest : the queue is empty"
  else
    let (l1, l2) = q
    in
    let l1rest = List.tl l1
      (* List.tl est en O(1) *)
    in
      if l1rest = [] then 
	(List.rev l2, [])
      else
	(l1rest, l2)
(* dans le cas le pire qrest en O(n) avec n longueur de l2
   en moyenne qrest est en O(1) car pour 
   une opération en O(n) on a n opération en O(1)
*)
;;

