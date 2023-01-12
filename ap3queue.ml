(*
 Compil with ocamlc -c ap3queue.ml
 *)

(*Queues Type with list*)
type 'a t_queue = 'a list * 'a list
;;

let empty () : 'a t_queue = ([], [])
;;

let isEmpty (f : ' a t_queue) = (f = ([], [])) 
                               (* or ([], _) *)
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
	(* no matter the list length
	   List.hd is O(1)
	   => qhd is O(1) because queues
	   are in NF
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
(* in worst case qrest is O(n) with n the l2 length
   on average qrest is O(1) because for
   an O(n) operation we have n O(1) operations
*)
;;

