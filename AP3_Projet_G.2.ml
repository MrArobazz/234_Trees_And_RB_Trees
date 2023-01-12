(*****************************)
(*    AP3 PROJET Groupe-2    *)
(*      Al Natour Mazen      *)
(*       Caillaud Tom        *)
(*****************************)

(*    Vous trouverez dans ce document toutes les fonctions et types définissant les arbres 2-3-4 et les arbres rouge et noir.
    Concernant les démonstrations ainsi que les exemples des arbres, ils se trouvent dans le rapport.pdf.                   *)




(*================================================= 2-3-4 TREE ==================================================*)

(* Question 3 *)
type 'a t_234tree = EMPTY
                 | TWO_ROOT of 'a * 'a t_234tree * 'a t_234tree
                 | THREE_ROOT of 'a * 'a * 'a t_234tree * 'a t_234tree * 'a t_234tree
                 | FOUR_ROOT of 'a * 'a * 'a * 'a t_234tree * 'a t_234tree * 'a t_234tree * 'a t_234tree
;;

let empty_234tree () : 'a t_234tree =
  EMPTY
;;

let two_rooting(r1, l, r : 'a * 'a t_234tree * 'a t_234tree) : 'a t_234tree =
  TWO_ROOT(r1,l,r)
;;

let three_rooting(r1, r2, l, m, r : 'a * 'a * 'a t_234tree * 'a t_234tree * 'a t_234tree)
    : 'a t_234tree =
  THREE_ROOT(r1, r2, l, m, r)
;;

let four_rooting(r1, r2, r3, l, lm, rm, r : 'a * 'a * 'a * 'a t_234tree * 'a t_234tree * 'a t_234tree * 'a t_234tree) : 'a t_234tree =
  FOUR_ROOT(r1,r2,r3,l,lm,rm,r)
;;

let isEmpty_234tree (tree : 'a t_234tree) : bool =
  match tree with
  | EMPTY -> true
  | _ -> false
;;

let isTwoRoots (tree : 'a t_234tree) : bool =
  match tree with
  | TWO_ROOT(_,_,_) -> true
  | _ -> false
;;

let isThreeRoots (tree : 'a t_234tree) : bool =
  match tree with
  | THREE_ROOT(_,_,_,_,_) -> true
  | _ -> false
;;

let isFourRoots (tree : 'a t_234tree) : bool =
  match tree with
  | FOUR_ROOT(_,_,_,_,_,_,_) -> true
  | _ -> false
;;


let isLeaf_234tree (tree : 'a t_234tree) : bool =
  match tree with
  | TWO_ROOT(_,l,r) -> isEmpty_234tree(l) && isEmpty_234tree(r)
  | THREE_ROOT(_,_,l,m,r) -> isEmpty_234tree(l) && isEmpty_234tree(m) && isEmpty_234tree(r)
  | FOUR_ROOT(_,_,_,l,lm,rm,r) -> isEmpty_234tree(l) && isEmpty_234tree(lm) && isEmpty_234tree(rm) && isEmpty_234tree(r)
  | _ -> false
;;

let isEquals_234tree (tree, value : 'a t_234tree * 'a) : bool =
  match tree with
  | TWO_ROOT(v,_,_) -> v=value
  | THREE_ROOT(v1,v2,_,_,_) -> v1 = value || v2 = value
  | FOUR_ROOT(v1,v2,v3,_,_,_,_) -> v1 = value || v2 = value || v3 = value
  | _ -> false
;;


let test : 'a t_234tree = empty_234tree();;
let test2 : 'a t_234tree = three_rooting(5,7,empty_234tree(),two_rooting(7,empty_234tree(),empty_234tree()),empty_234tree());;
let test3 : 'a t_234tree = four_rooting(8,9,12, test2, empty_234tree(), empty_234tree(), test2);;
let exemple1_234tree : 'a t_234tree = four_rooting(12,15,18,
                                          three_rooting(4,8,
                                                        three_rooting(1,2,
                                                                      empty_234tree(),
                                                                      empty_234tree(),
                                                                      empty_234tree()
                                                                      ),
                                                        three_rooting(5,6,
                                                                      empty_234tree(),
                                                                      empty_234tree(),
                                                                      empty_234tree()
                                                                      ),
                                                        three_rooting(9,10,
                                                                      empty_234tree(),
                                                                      empty_234tree(),
                                                                      empty_234tree()
                                                                      )
                                                        ),
                                          two_rooting(13,
                                                      empty_234tree(),
                                                      empty_234tree()
                                                      ),
                                          two_rooting(17,
                                                      empty_234tree(),
                                                      empty_234tree()
                                                      ),
                                          three_rooting(20,25,
                                                        two_rooting(19,
                                                                    empty_234tree(),
                                                                    empty_234tree()
                                                                    ),
                                                        two_rooting(22,
                                                                    empty_234tree(),
                                                                    empty_234tree()
                                                                    ),
                                                        four_rooting(27,30,35,
                                                                     empty_234tree(),
                                                                     empty_234tree(),
                                                                     empty_234tree(),
                                                                     empty_234tree()
                                                                     )
                                                        )

                                )
;;


(* Question 4 *)
let rec t234_search (tree, value : 'a t_234tree * 'a) : bool =
  if isEmpty_234tree(tree)
  then false
  else
    if isEquals_234tree(tree, value)
    then true
    else
      if isLeaf_234tree(tree)
      then false
      else match tree with
           | TWO_ROOT(v,l,r) ->
              if value < v
              then t234_search(l, value)
              else t234_search(r, value)
           | THREE_ROOT(v1,v2,l,m,r) ->
              if value < v1
              then t234_search(l, value)
              else
                if (v1 < value && value < v2)
                then t234_search(m, value)
                else t234_search(r, value)
           | FOUR_ROOT(v1,v2,v3,l,lm,rm,r) ->
              if value < v1
              then t234_search(l, value)
              else
                if (v1 < value && value < v2)
                then t234_search(lm, value)
                else
                  if (v2 < value && value < v3)
                  then t234_search(rm, value)
                  else t234_search(r, value)
           | _ -> false
;;

t234_search(exemple1_234tree,30);;
t234_search(exemple1_234tree,32);;
t234_search(empty_234tree(), 27);;

(* Question 6 *)
let rec t234_insert (tree, value : 'a t_234tree * 'a) : 'a t_234tree =
  if isEmpty_234tree(tree)
  then two_rooting(value,empty_234tree(),empty_234tree())
  else
    if isLeaf_234tree(tree)
    then match tree with
         | TWO_ROOT(v,_,_) ->
            if v < value
            then three_rooting(v,value,empty_234tree(),empty_234tree(),empty_234tree())
            else three_rooting(value,v,empty_234tree(),empty_234tree(),empty_234tree())
         | THREE_ROOT(v1,v2,_,_,_) ->
            if value < v1
            then four_rooting(value,v1,v2,empty_234tree(),empty_234tree(),empty_234tree(),empty_234tree())
            else
              if v2 < value
              then four_rooting(v1,v2,value,empty_234tree(),empty_234tree(),empty_234tree(),empty_234tree())
              else four_rooting(v1,value,v2,empty_234tree(),empty_234tree(),empty_234tree(),empty_234tree())
         | FOUR_ROOT(v1,v2,v3,_,_,_,_) ->
            let left_tree : 'a t_234tree = two_rooting(v1,empty_234tree(),empty_234tree())
            and right_tree : 'a t_234tree = two_rooting(v3,empty_234tree(),empty_234tree()) in
            if value < v2
            then let new_tree : 'a t_234tree =
                   two_rooting(v2,t234_insert(left_tree,value),right_tree) in
                 new_tree
            else let new_tree : 'a t_234tree =
                   two_rooting(v2,left_tree, t234_insert(right_tree,value)) in
                 new_tree
         | _ -> failwith "error t_234 insert"
    else match tree with
         | TWO_ROOT(v,l,r) ->
            if v < value
            then two_rooting(v,l,t234_insert(r,value))
            else two_rooting(v,t234_insert(l,value),r)
         | THREE_ROOT(v1,v2,l,m,r) ->
            if v2 < value
            then three_rooting(v1,v2,l,m,t234_insert(r,value))
            else
              if value < v1
              then three_rooting(v1,v2,t234_insert(l,value),m,r)
              else three_rooting(v1,v2,l,t234_insert(m,value),r)
         | FOUR_ROOT(v1,v2,v3,l,lm,rm,r) ->
            if v3 < value
            then four_rooting(v1,v2,v3,l,lm,rm,t234_insert(r, value))
            else
              if value < v1
              then four_rooting(v1,v2,v3,t234_insert(l, value),lm,rm,r)
              else
                if v1 < value && value < v2
                then four_rooting(v1,v2,v3,l,t234_insert(lm,value),rm,r)
                else four_rooting(v1,v2,v3,l,lm,t234_insert(rm,value),r)
         | _ -> failwith "error t234_insert"
;;

(* Question 7 *)
t234_insert(t234_insert(t234_insert(t234_insert(empty_234tree(),3),1),4),5);;
let testfinal : 'a t_234tree = empty_234tree();;
let b : 'a t_234tree = t234_insert(testfinal,4);;
let c : 'a t_234tree = t234_insert(b,35);;
let d : 'a t_234tree = t234_insert(c,10);;
let e : 'a t_234tree = t234_insert(d,13);;
let f : 'a t_234tree = t234_insert(e,3);;
let g : 'a t_234tree = t234_insert(f,30);;
let h : 'a t_234tree = t234_insert(g,15);;
let i : 'a t_234tree = t234_insert(h,12);;
let j : 'a t_234tree = t234_insert(i,7);;
let k : 'a t_234tree = t234_insert(j,40);;
let l : 'a t_234tree = t234_insert(k,20);;
let m : 'a t_234tree = t234_insert(l,11);;
let n : 'a t_234tree = t234_insert(m,6);;

(* resultat *)
# let n : 'a t_234tree = t234_insert(m,6);;
val n : int t_234tree =
  TWO_ROOT (10,
   TWO_ROOT (4, TWO_ROOT (3, EMPTY, EMPTY),
    THREE_ROOT (6, 7, EMPTY, EMPTY, EMPTY)),
   TWO_ROOT (30,
    TWO_ROOT (13, THREE_ROOT (11, 12, EMPTY, EMPTY, EMPTY),
     THREE_ROOT (15, 20, EMPTY, EMPTY, EMPTY)),
    THREE_ROOT (35, 40, EMPTY, EMPTY, EMPTY)))
#

(*================================================= RED-BLACK TREE ==================================================*)
type color = Red | Black ;;
type 'a t_rbtree = EMPTY
                 | ROOTED of color * 'a * 'a t_rbtree * 'a t_rbtree
;;

let rbt_empty(): 'a t_rbtree = EMPTY
;;

let rbt_rooting(c,x, g, d : color* 'a * 'a t_rbtree * 'a t_rbtree) : 'a t_rbtree =
      ROOTED(c,x, g, d)
;;

let rbt_root(bt : 'a t_rbtree) : 'a =
  match bt with
   EMPTY -> failwith"Arbre vide"
  | ROOTED(_,x,_,_) -> x
;;

let rbt_subleft(bt : 'a t_rbtree) :  'a t_rbtree =
  match bt with
    EMPTY -> failwith"Arbre vide"
  | ROOTED(_,_,x,_) -> x
;;

let rbt_subright(bt : 'a t_rbtree) :  'a t_rbtree =
  match bt with
    EMPTY -> failwith"Arbre vide"
  | ROOTED(_,_,_,x) -> x
;;

let rbt_isempty(bt :'a t_rbtree ) : bool =
  match bt with
    EMPTY -> true
  | ROOTED(_,_,_,_) -> false
;;

let rbt_getcolornode(bt :'a t_rbtree ) : color =
  match bt with
    EMPTY -> failwith"Arbre vide"
  | ROOTED(c,_,_,_) -> c
;;

(****************** Exemple 1 ********************)

(* f : feuille*)
let fd1_ex1 : int t_rbtree = rbt_rooting(Red,14,rbt_empty(),rbt_empty());;
let fd2_ex1 : int t_rbtree = rbt_rooting(Black,20,rbt_empty(),rbt_empty());;

let nd1_ex1 : int t_rbtree = rbt_rooting(Black,15,fd1_ex1,rbt_empty());;

let nd2_ex1 : int t_rbtree = rbt_rooting(Red,17,nd1_ex1,fd2_ex1);;



let fg1_ex1 : int t_rbtree = rbt_rooting(Black,7,rbt_empty(),rbt_empty());;
let fg2_ex1 : int t_rbtree = rbt_rooting(Black,9,rbt_empty(),rbt_empty());;

let nf1_ex1 : int t_rbtree = rbt_rooting(Red,8,fg1_ex1,fg2_ex1);;


let racine_ex1 : int t_rbtree = rbt_rooting(Black,13,nf1_ex1,nd2_ex1);;


(****************** Exemple 2 ********************)


let fd1_ex2 : int t_rbtree = rbt_rooting(Black,18,rbt_empty(),rbt_empty());;

let fg1_ex2 : int t_rbtree =  rbt_rooting(Red,1,rbt_empty(),rbt_empty());;
let fg2_ex2 : int t_rbtree =  rbt_rooting(Red,14,rbt_empty(),rbt_empty());;

let ng1_ex2 : int t_rbtree =  rbt_rooting(Black,2,fg1_ex2,fg2_ex2);;


let racine_ex2 : int t_rbtree =  rbt_rooting(Black,15,ng1_ex2,fd1_ex2);;



(****************** Exemple 3 ********************)


let fd1_ex3 : int t_rbtree = rbt_rooting(Black,18,rbt_empty(),rbt_empty());;
let fd2_ex3 : int t_rbtree = rbt_rooting(Black,30,rbt_empty(),rbt_empty());;

let fd3_ex3 : int t_rbtree = rbt_rooting(Black,14,rbt_empty(),rbt_empty());;
let nd1_ex3 : int t_rbtree = rbt_rooting(Red,20,fd1_ex3,fd2_ex3);;


let nd2_ex3 : int t_rbtree = rbt_rooting(Black,15,fd3_ex3,nd1_ex3);;

let fg1_ex3 : int t_rbtree = rbt_rooting(Black,2,rbt_empty(),rbt_empty());;
let fg2_ex3 : int t_rbtree = rbt_rooting(Black,11,rbt_empty(),rbt_empty());;

let ng2_ex3 : int t_rbtree = rbt_rooting(Black,10,fg1_ex3,fg2_ex3);;

let racine_ex3 : int t_rbtree = rbt_rooting(Black,12, ng2_ex3,nd2_ex3);; 



(* Question 3 *)
let rb_balance(col,v,fg,fd : color * 'a * 'a t_rbtree * 'a t_rbtree) : 'a t_rbtree =
  match col with
    Red -> ROOTED(Red,v,fg,fd)
  | _ -> match fg with
         | ROOTED (Red, y, ROOTED (Red, x, a, b), c) -> ROOTED(Red,y,ROOTED(Black,x,a,b),ROOTED(Black,v,c,fd))
         | ROOTED (Red, x, a, ROOTED (Red, y, b, c)) -> ROOTED(Red,y,ROOTED(Black,x,a,b),ROOTED(Black,v,c,fd))
         | _ -> match fd with
                | ROOTED(Red,z,ROOTED(Red,y,b,c),d) -> ROOTED(Red,y,ROOTED(Black,v,fg,b),ROOTED(Black,z,c,d))
                | ROOTED(Red,y,b,ROOTED(Red,z,c,d)) -> ROOTED(Red,y,ROOTED(Black,v,fg,b),ROOTED(Black,z,c,d))
                | _ -> ROOTED(Black,v,fg,fd)
;;


(* Question 4 *)            
let rec rb_insert_aux(rbt,v : 'a t_rbtree* 'a) : 'a t_rbtree =
  match rbt with
    EMPTY -> ROOTED(Red,v,EMPTY,EMPTY)
  |  ROOTED(col,x,fg,fd) ->
      if v < x
      then rb_balance(col,x,rb_insert_aux(fg,v),fd)
      else rb_balance(col,x,fg,rb_insert_aux(fd,v))
;;


let make_black_root(t : 'a t_rbtree) : 'a t_rbtree =
  match t with
    EMPTY -> EMPTY
  | ROOTED(_,x,fg,fd) -> ROOTED(Black,x,fg,fd)
;;

let rb_insert (rbt,v : 'a t_rbtree* 'a) : 'a t_rbtree =
  make_black_root(rb_insert_aux(rbt,v))
;;

 
(* Question 5 *)     
let arbre1 = rb_insert( rbt_empty(),4);;
let arbre2 = rb_insert(arbre1,10);;
let arbre3 = rb_insert(arbre2,13);;
let arbre4 = rb_insert(arbre3,3);;
let arbre5 = rb_insert(arbre4,30);;
let arbre6 = rb_insert(arbre5,15);;
let arbre7 = rb_insert(arbre6,12);;
let arbre8 = rb_insert(arbre7,7);;
let arbre9 = rb_insert(arbre8,40);;
let arbre10 = rb_insert(arbre9,20);;
let arbre11 = rb_insert(arbre10,11);;
let arbretest = rb_insert(arbre11,6);;

(* resultat *)
# let arbretest = rb_insert(arbre11,6);;
val arbretest : int t_rbtree =
  ROOTED (Black, 12,
   ROOTED (Black, 10,
    ROOTED (Red, 6, ROOTED (Black, 4, ROOTED (Red, 3, EMPTY, EMPTY), EMPTY),
     ROOTED (Black, 7, EMPTY, EMPTY)),
    ROOTED (Black, 11, EMPTY, EMPTY)),
   ROOTED (Black, 15, ROOTED (Black, 13, EMPTY, EMPTY),
    ROOTED (Black, 30, ROOTED (Red, 20, EMPTY, EMPTY),
     ROOTED (Red, 40, EMPTY, EMPTY))));;



(* Question 6 *)
let takeValuesFromNode(tree : 'a t_234tree) : 'a list =
  match tree with
  | EMPTY -> []
  | TWO_ROOT(v,_,_) -> v::[]
  | THREE_ROOT(v1,v2,_,_,_) -> v1::v2::[]
  | FOUR_ROOT(v1,v2,v3,_,_,_,_) -> v1::v2::v3::[]
;;


let rec parcours_234tree(tree : 'a t_234tree) : 'a list =
  if isEmpty_234tree(tree)
  then []
  else match tree with
       | TWO_ROOT(_,l,r) ->
          takeValuesFromNode(tree)@parcours_234tree(l)@parcours_234tree(r)
       | THREE_ROOT(_,_,l,m,r) ->
          takeValuesFromNode(tree)@parcours_234tree(l)@parcours_234tree(m)@parcours_234tree(r)
       | FOUR_ROOT(_,_,_,l,lm,rm,r) ->
          takeValuesFromNode(tree)@parcours_234tree(l)@parcours_234tree(lm)@parcours_234tree(rm)@parcours_234tree(r)
       | _ -> failwith "error parcours_234tree"
;;

parcours_234tree(exemple1_234tree);;

let to_rb( tree : 'a t_234tree  ) : 'a t_rbtree =
  if isEmpty_234tree(tree)
  then rbt_empty()
  else
    let listvalbis : 'a list ref=ref (parcours_234tree(tree))
    and rbtreebis : 'a t_rbtree ref=ref  (rbt_empty())
    and fst : int ref=ref 0
    in
    while (List.length(!listvalbis) > 0)
    do
      fst := List.hd(!listvalbis);
      listvalbis := List.tl(!listvalbis);
      rbtreebis := rb_insert(!rbtreebis,!fst);
    done;
    !rbtreebis;
;;

to_rb(exemple1_234tree);;




(* Question 7 *)
#load "ap3queue.cmo";;
#show Ap3queue;;
open Ap3queue;;

let rec parcours_largeur_aux(myq,myl : ('a t_rbtree) t_queue * 'a list) : 'a list =
  if isEmpty(myq)
  then myl
  else
    let t : 'a t_rbtree = qhd(myq) in
    if  rbt_isempty(t)
    then  parcours_largeur_aux(qrest(myq),myl)
    else
      let (v,ltree,rtree) : (int * 'a t_rbtree * 'a t_rbtree) = (rbt_root(t),rbt_subleft(t),rbt_subright(t)) in
      let mynextq :('a t_rbtree) t_queue = enter(rtree,enter(ltree,qrest(myq))) in
      parcours_largeur_aux(mynextq,v::myl)
;;


let parcours_largeur(bt : 'a t_rbtree) : 'a list  =
  List.rev(parcours_largeur_aux(enter(bt,empty()),[]))
;;

parcours_largeur(arbretest);;

let to_234( tree : 'a t_rbtree ) : 'a t_234tree =
  if rbt_isempty(tree)
  then empty_234tree()
  else
    let listvalbis : 'a list ref=ref (parcours_largeur(tree))
    and tree234bis : 'a t_234tree ref=ref  (empty_234tree())
    and fst : int ref=ref 0
    in
    while (List.length(!listvalbis) > 0)
    do
      fst := List.hd(!listvalbis);
      listvalbis := List.tl(!listvalbis);
      tree234bis := t234_insert(!tree234bis,!fst);
    done;
    !tree234bis;
;;

to_234(arbretest);;








(*==============================Question bonus===========================================*)
(*========================Algorithmes de suppression=====================================*)


(*------------------------Abres Rouge-Noir-----------------------*)


(* Equilibrage � gauche apres suppresion*)
let balance_left_del (c,l,x,r: color * 'a t_rbtree* 'a* 'a t_rbtree) : 'a t_rbtree =
  match r with
  | ROOTED (Red, y, rl, rr) -> ROOTED (c, y, ROOTED (Red, x, l, rl), rr)
  | ROOTED (Black, z, ROOTED (Red, y, rl, rr), rrr) -> ROOTED (Black, y, ROOTED (c, x, l, rl), ROOTED (Black, z, rr, rrr))
  | _ -> ROOTED (c, x, l, r)
;;

(* Equilibrage � droite apres suppresion*)
let balance_right_del (c,l,x,r: color * 'a t_rbtree* 'a* 'a t_rbtree) : 'a t_rbtree =
  match l with
  | ROOTED (Red, y, ll, lr) -> ROOTED (c, y, ll, ROOTED (Red, x, lr, r))
  | ROOTED (Black, z, lll, ROOTED (Red, y, ll, lr)) -> ROOTED (Black, y, ROOTED (Black, z, lll, ll), ROOTED (Red, x, lr, r))
  | _ -> ROOTED (c, x, l, r)
;;

(* Fusion de deux arbres rouge-noir *)
let rec merge (t1,t2: 'a t_rbtree * 'a t_rbtree) : 'a t_rbtree =
  match (t1,t2) with
  | (EMPTY, t) | (t, EMPTY) -> t
  | (ROOTED (c1, x1, l1, r1), ROOTED (c2, x2, l2, r2)) ->
     if x1 <= x2
     then balance_right_del (c2, merge(r1, l2), x2, r2)
     else balance_left_del (c1, l1, x1, merge(r1, t2))
;;

(* Suppression de l'�l�ment de valeur minimale dans un arbre rouge-noir *)
let rec delete_min(t : 'a t_rbtree) : 'a * 'a t_rbtree =
  match t with
  | EMPTY -> failwith("err : delete_min sur un arbre vide")
  | ROOTED (Black, v, l, r) ->
      (
        match l with
        | EMPTY -> (v, r)
        | ROOTED (Red, vl, ll, lr) -> (vl, ROOTED (Black, v, lr, r))
        | ROOTED (Black, vl, ll, lr) ->
            let (v1, l1) : ('a * 'a t_rbtree) = delete_min(l) in (v1, balance_left_del (Black, l1, v, r))
      )
  | ROOTED (Red, v, l, r) ->
      (
        match l with
        | EMPTY -> (v, r)
        | ROOTED (Red, vl, ll, lr) -> (vl, ROOTED (Red, v, lr, r))
        | ROOTED (Black, vl, ll, lr) ->
            let (v1, l1) : ('a * 'a t_rbtree) = delete_min(l) in (v1, balance_left_del (Red, l1, v, r))
      )
;;

(* Suppression d'un �l�ment dans un arbre rouge-noir *)
let rec delete_aux (value,t : 'a * 'a t_rbtree) : 'a t_rbtree =
  match t with
  | EMPTY -> EMPTY
  | ROOTED (color, v, l, r) ->
     (* Cas o� la valeur � supprimer est la racine de l'arbre *)
     if value = v
     then
     (
       match (l, r) with
       | EMPTY, EMPTY -> EMPTY
       | ROOTED (lc, lv, ll, lr), EMPTY -> ROOTED (lc, lv, ll, lr)
       | EMPTY, ROOTED (rc, rv, rl, rr) -> ROOTED (rc, rv, rl, rr)
       | _, _ -> let (v1, r1) : ('a * 'a t_rbtree) = delete_min(r) in  ROOTED (color, v1, l, r1)
     )
     else (* Sinon *)
        match t with
        | EMPTY -> EMPTY
        | ROOTED (Red, v, l, r) ->
           if value < v
           then ROOTED (Red, v, delete_aux(value, l), r)
           else
             if value > v
             then ROOTED (Red, v, l, delete_aux(value, r))
             else
               (
                 match (l,r) with
                 | EMPTY, _ -> r
                 | _, EMPTY -> l
                 | _,_ -> let (v1, r1) : ('a * 'a t_rbtree) = delete_min(r) in  ROOTED (Red, v1, l, r1)
               )
        | ROOTED (Black, v, l, r) ->
           if value < v
           then balance_left_del (Black, delete_aux(value, l), v, r)
           else
             if value > v
             then balance_right_del (Black, l, v, delete_aux(value, r))
             else
               (
                 match (l,r) with
                 | EMPTY, _ -> r
                 | _, EMPTY -> l
                 | _,_ ->
                    let (v1, r1) : ('a * 'a t_rbtree) = delete_min(r) in
                    let t : 'a t_rbtree = ROOTED(Black, v1, l, r1) in merge (l, t)
               )
;;

let delete_val(value,t : 'a * 'a t_rbtree): 'a t_rbtree = 
  make_black_root(delete_aux(value,t))
;;

arbretest;;
let newtree : 'a t_rbtree = delete_val(12,arbretest);;
# let newtree : 'a t_rbtree = delete_val(12,arbretest);;
val newtree : int t_rbtree =
  ROOTED (Black, 13,
   ROOTED (Black, 10,
    ROOTED (Red, 6, ROOTED (Black, 4, ROOTED (Red, 3, EMPTY, EMPTY), EMPTY),
     ROOTED (Black, 7, EMPTY, EMPTY)),
    ROOTED (Black, 11, EMPTY, EMPTY)),
   ROOTED (Black, 20, ROOTED (Black, 15, EMPTY, EMPTY),
    ROOTED (Black, 30, EMPTY, ROOTED (Red, 40, EMPTY, EMPTY))))


    
arbretest;;
let newtree2 : 'a t_rbtree = delete_val(15,arbretest);;
# let newtree2 : 'a t_rbtree = delete_val(15,arbretest);;
val newtree2 : int t_rbtree =
  ROOTED (Black, 12,
   ROOTED (Black, 10,
    ROOTED (Red, 6, ROOTED (Black, 4, ROOTED (Red, 3, EMPTY, EMPTY), EMPTY),
     ROOTED (Black, 7, EMPTY, EMPTY)),
    ROOTED (Black, 11, EMPTY, EMPTY)),
   ROOTED (Black, 20, ROOTED (Black, 13, EMPTY, EMPTY),
           ROOTED (Black, 30, EMPTY, ROOTED (Red, 40, EMPTY, EMPTY))))


      
arbretest;;
let newtree2 : 'a t_rbtree = delete_val(7,arbretest);;
# let newtree2 : 'a t_rbtree = delete_val(7,arbretest);;
val newtree2 : int t_rbtree =
  ROOTED (Black, 12,
   ROOTED (Black, 10,
    ROOTED (Red, 6, ROOTED (Black, 4, ROOTED (Red, 3, EMPTY, EMPTY), EMPTY),
     EMPTY),
    ROOTED (Black, 11, EMPTY, EMPTY)),
   ROOTED (Black, 15, ROOTED (Black, 13, EMPTY, EMPTY),
    ROOTED (Black, 30, ROOTED (Red, 20, EMPTY, EMPTY),
     ROOTED (Red, 40, EMPTY, EMPTY))))


(*------------------------Arbres 2-3-4-----------------------*)

(*Une fonction pour récupérer la plus grande valeur de l'arbre pour l'opération de délétion,
Suppose l'arbre correctement fait, càd avec la plus grande valeur à droite*)
let right_tree(tree : 'a t_234tree) : 'a t_234tree =
  match tree with
  | TWO_ROOT(_,_,r) -> r
  | THREE_ROOT(_,_,_,_,r) -> r
  | FOUR_ROOT(_,_,_,_,_,_,r) -> r
  | _ -> failwith "error unknown tree l673"
;;

let rec t234_highest_value (tree : 'a t_234tree) : 'a =
  if isEmpty_234tree(tree)
  then failwith "error empty tree"
  else
    if isLeaf_234tree(tree)
    then
      match tree with
      | TWO_ROOT(v,_,_) -> v
      | THREE_ROOT(_,v2,_,_,_) -> v2
      | FOUR_ROOT(_,_,v3,_,_,_,_) -> v3
      | _ -> failwith "error unknown tree l686"
    else t234_highest_value(right_tree(tree))
;;

(*Fonction pour modifier une valeur d'un noeud, on suppose que la valeur ne changera pas
l'équilibre de l'arbre, à eulement utiliser dans la fonction de délétion*)
let rec modify_value (tree,value,new_value : 'a t_234tree * 'a * 'a) : 'a t_234tree =
  match tree with
  | EMPTY -> tree
  | TWO_ROOT(v,l,r) -> if v = value
                       then two_rooting(new_value,l,r)
                       else if value < v
                       then two_rooting(v,modify_value(l,value,new_value),r)
                       else two_rooting(v,l,modify_value(r,value,new_value))
  | THREE_ROOT(v1,v2,l,m,r) -> if v1 = value
                               then three_rooting(new_value,v2,l,m,r)
                               else if v2 = value
                               then three_rooting(v1,new_value,l,m,r)
                               else if value < v1
                               then three_rooting(v1,v2,modify_value(l,value,new_value),m,r)
                               else if value < v2
                               then three_rooting(v1,v2,l,modify_value(m,value,new_value),r)
                               else three_rooting(v1,v2,l,m,modify_value(r,value,new_value))
  | FOUR_ROOT(v1,v2,v3,l,ml,mr,r) -> if v1 = value
                                     then four_rooting(new_value,v2,v3,l,ml,mr,r)
                                     else if v2 = value
                                     then four_rooting(v1,new_value,v3,l,ml,mr,r)
                                     else if v3 = value
                                     then four_rooting(v1,v2,new_value,l,ml,mr,r)
                                     else if value < v1
                                     then four_rooting(v1,v2,v3,modify_value(l,value,new_value),ml,mr,r)
                                     else if value < v2
                                     then four_rooting(v1,v2,v3,l,modify_value(ml,value,new_value),mr,r)
                                     else if value < v3
                                     then four_rooting(v1,v2,v3,l,ml,modify_value(mr,value,new_value),r)
                                     else four_rooting(v1,v2,v3,l,ml,mr,modify_value(r,value,new_value))
;;


(*Pour les arbres à 2-noeud pour un cas spécial*)
let getTheRoot (tree : 'a t_234tree) : 'a =
  match tree with
  | TWO_ROOT(v,_,_) -> v
  | _ -> failwith "error not a two rooted tree"
;;

let getTheLeft (tree : 'a t_234tree) : 'a t_234tree =
  match tree with
  | TWO_ROOT(_,l,_) -> l
  | _ -> failwith "error not a two rooted tree"
;;

let getTheRight (tree : 'a t_234tree) : 'a t_234tree =
  match tree with
  | TWO_ROOT(_,_,r) -> r
  | _ -> failwith "error not a two rooted tree"
;;



let rec t234_delete (tree, value : 'a t_234tree * 'a) : 'a t_234tree =
  match tree with
  | EMPTY -> tree
  | TWO_ROOT(v,l,r) ->
     if isLeaf_234tree(tree)
     then if v = value then empty_234tree() else tree
     else
       if isTwoRoots(l) && isTwoRoots(r)
       then let new_tree : 'a t_234tree =
              four_rooting(getTheRoot(l),v,getTheRoot(r),
                           getTheLeft(l),getTheRight(l),
                           getTheLeft(r),getTheRight(r)) in
            t234_delete(new_tree,value)
       else
       if value < v
       then if isEquals_234tree(l,value)
            then if isLeaf_234tree(l)
                 then (*Si le pere est un 2-noeud et que ses fils sont des feuilles*)
                   match l with
                   | TWO_ROOT(lv,_,_) ->
                      (
                        match r with
                        | TWO_ROOT(rv,_,_) ->
                           three_rooting(v,rv,
                                         empty_234tree(),
                                         empty_234tree(),
                                         empty_234tree())
                        | THREE_ROOT(rv1,rv2,_,_,_) ->
                           two_rooting(rv1,
                                       two_rooting(v,
                                                   empty_234tree(),
                                                   empty_234tree()),
                                       two_rooting(rv2,
                                                   empty_234tree(),
                                                   empty_234tree()))
                        | FOUR_ROOT(rv1,rv2,rv3,_,_,_,_) ->
                           two_rooting(rv1,
                                       two_rooting(v,
                                                   empty_234tree(),
                                                   empty_234tree()),
                                       three_rooting(rv2,
                                                     rv3,
                                                     empty_234tree(),
                                                     empty_234tree(),
                                                     empty_234tree()))
                        | _ -> failwith "error unknown tree l791"
                      )
                   | THREE_ROOT(lv1,lv2,_,_,_) ->
                      (
                        if lv1 = value
                        then let new_left : 'a t_234tree =
                               two_rooting(lv2,empty_234tree(),empty_234tree()) in
                             two_rooting(v,new_left,r)
                        else let new_left : 'a t_234tree =
                               two_rooting(lv1,empty_234tree(),empty_234tree()) in
                             two_rooting(v,new_left,r)
                      )
                   | FOUR_ROOT(lv1,lv2,lv3,_,_,_,_) ->
                      (
                        if lv1 = value
                        then let new_left : 'a t_234tree =
                               three_rooting(lv2,lv3,
                                             empty_234tree(),empty_234tree(),empty_234tree()) in
                             two_rooting(v,new_left,r)
                        else

                        if lv2 = value
                        then let new_left : 'a t_234tree =
                               three_rooting(lv1,lv3,
                                             empty_234tree(),empty_234tree(),empty_234tree()) in
                             two_rooting(v,new_left,r)

                        else let new_left : 'a t_234tree =
                               three_rooting(lv1,lv2,
                                             empty_234tree(),empty_234tree(),empty_234tree()) in
                             two_rooting(v,new_left,r)
                      )
                   | _ -> failwith "error unknown tree l823"
                 else (*Si le père est un 2-noeud et que ses fils ne sont pas des feuilles*)
                   two_rooting(v,t234_delete(l,value),r)
            else two_rooting(v,t234_delete(l,value),r)
       else
         if v < value
         then if isEquals_234tree(r,value)
              then if isLeaf_234tree(r)
                   then (*Si le père est un 2-noeud et que ses fils sont des feuilles*)
                     match r with
                     | TWO_ROOT(rv,_,_) ->
                        (
                          match l with
                          | TWO_ROOT(lv,_,_) ->
                             three_rooting(lv,v,
                                           empty_234tree(),
                                           empty_234tree(),
                                           empty_234tree())

                          | THREE_ROOT(lv1,lv2,_,_,_) ->
                             two_rooting(lv2,
                                         two_rooting(lv1,
                                                     empty_234tree(),
                                                     empty_234tree()),
                                         two_rooting(v,
                                                     empty_234tree(),
                                                     empty_234tree()))

                          | FOUR_ROOT(lv1,lv2,lv3,_,_,_,_) ->
                             two_rooting(lv3,
                                         three_rooting(lv1,
                                                       lv2,
                                                       empty_234tree(),
                                                       empty_234tree(),
                                                       empty_234tree()),
                                         two_rooting(v,
                                                     empty_234tree(),
                                                     empty_234tree()))

                          | _ -> failwith "error unknown tree l862"
                        )

                     | THREE_ROOT(rv1,rv2,_,_,_) ->
                        (
                          if rv1 = value
                          then let new_right : 'a t_234tree =
                                 two_rooting(rv2,empty_234tree(),empty_234tree()) in
                               two_rooting(v,l,new_right)
                          else let new_right : 'a t_234tree =
                                 two_rooting(rv1,empty_234tree(),empty_234tree()) in
                               two_rooting(v,l,new_right)
                        )

                     | FOUR_ROOT(rv1,rv2,rv3,_,_,_,_) ->
                        (
                          if rv1 = value
                          then
                            let new_right : 'a t_234tree =
                              three_rooting(rv2,rv3,
                                            empty_234tree(),empty_234tree(),empty_234tree()) in
                            two_rooting(v,l,new_right)

                          else
                            if rv2 = value
                            then
                              let new_right : 'a t_234tree =
                                three_rooting(rv1,rv3,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                              two_rooting(v,l,new_right)

                            else
                              let new_right : 'a t_234tree =
                                three_rooting(rv1,rv2,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                              two_rooting(v,l,new_right)
                        )

                     | _ -> failwith "error unknown tree l900"

                   else (*Si le père est un 2-noeud et que ses fils ne sont pas des feuilles*)
                     two_rooting(v,l,t234_delete(r,value))

              else two_rooting(v,l,t234_delete(r,value))

         else let new_root_value : 'a = t234_highest_value(l) in
              let new_tree : 'a t_234tree = t234_delete(tree,new_root_value) in
              modify_value(new_tree,v,new_root_value)

  | THREE_ROOT(v1,v2,l,m,r) ->
     if isLeaf_234tree(tree)
     then
       if v1 = value then two_rooting(v2,empty_234tree(),empty_234tree())
       else
         if v2 = value then two_rooting(v1,empty_234tree(),empty_234tree())
         else tree
     else
       if value < v1
       then if isEquals_234tree(l,value)
            then if isLeaf_234tree(l)
                 then (*Si le pere est un 3-noeud et que ses fils sont des feuilles*)
                   (
                     match l with
                     | TWO_ROOT(lv,_,_) ->
                        (
                          match m with
                          | TWO_ROOT(mv,_,_) ->
                             let new_left : 'a t_234tree =
                               three_rooting(v1,mv,
                                             empty_234tree(),empty_234tree(),empty_234tree()) in
                             two_rooting(v2,new_left,r)

                          | THREE_ROOT(mv1,mv2,_,_,_) ->
                             let new_left : 'a t_234tree =
                               two_rooting(v1,empty_234tree(),empty_234tree())
                             and new_mid : 'a t_234tree =
                               two_rooting(mv2,empty_234tree(),empty_234tree()) in
                             three_rooting(mv1,v2,new_left,new_mid,r)

                          | FOUR_ROOT(mv1,mv2,mv3,_,_,_,_) ->
                             let new_left : 'a t_234tree =
                               two_rooting(v1,empty_234tree(),empty_234tree())
                             and new_mid : 'a t_234tree =
                               three_rooting(mv2,mv3,
                                             empty_234tree(),empty_234tree(),empty_234tree()) in
                             three_rooting(mv1,v2,new_left,new_mid,r)

                          | _ -> failwith "error unknown tree l949"
                        )
                     | THREE_ROOT(lv1,lv2,_,_,_) ->
                        (
                          if lv1 = value
                          then let new_left : 'a t_234tree =
                                 two_rooting(lv2,empty_234tree(),empty_234tree()) in
                               three_rooting(v1,v2,new_left,m,r)
                          else let new_left : 'a t_234tree =
                                 two_rooting(lv1,empty_234tree(),empty_234tree()) in
                               three_rooting(v1,v2,new_left,m,r)
                        )
                     | FOUR_ROOT(lv1,lv2,lv3,_,_,_,_) ->
                        (
                          if lv1 = value
                          then
                            let new_left : 'a t_234tree =
                              three_rooting(lv2,lv3,
                                            empty_234tree(),empty_234tree(),empty_234tree()) in
                            three_rooting(v1,v2,new_left,m,r)

                          else
                            if lv2 = value
                            then
                              let new_left : 'a t_234tree =
                                three_rooting(lv1,lv3,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                              three_rooting(v1,v2,new_left,m,r)
                            else
                              let new_left : 'a t_234tree =
                                three_rooting(lv1,lv2,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                              three_rooting(v1,v2,new_left,m,r)
                        )
                     | _ -> failwith "error unknown tree l983"
                   )
                 else (*Si le père est un 3-noeud et que ses fils ne sont pas des feuilles*)
                   three_rooting(v1,v2,t234_delete(l,value),m,r)
            else three_rooting(v1,v2,t234_delete(l,value),m,r)
       else
         if v1 < value && value < v2 (*donc fils du milieu*)
         then if isEquals_234tree(m,value)
              then if isLeaf_234tree(m)
                   then
                     match m with
                     | TWO_ROOT(mv,_,_) ->
                        (
                          match l with
                          | TWO_ROOT(lv,_,_) ->
                             let new_left : 'a t_234tree =
                               three_rooting(lv,v1,
                                             empty_234tree(),empty_234tree(),empty_234tree()) in
                             two_rooting(v2,new_left,r)

                          | THREE_ROOT(lv1,lv2,_,_,_) ->
                             let new_mid : 'a t_234tree =
                               two_rooting(v1,empty_234tree(),empty_234tree())
                             and new_left : 'a t_234tree =
                               two_rooting(lv1,empty_234tree(),empty_234tree()) in
                             three_rooting(lv2,v2,new_left,new_mid,r)

                          | FOUR_ROOT(lv1,lv2,lv3,_,_,_,_) ->
                             let new_mid : 'a t_234tree =
                               two_rooting(v1,empty_234tree(),empty_234tree())
                             and new_left : 'a t_234tree =
                               three_rooting(lv1,lv2,
                                             empty_234tree(),empty_234tree(),empty_234tree()) in
                             three_rooting(lv3,v2,new_left,new_mid,r)

                          | _ -> failwith "error unknown tree l1018"
                        )
                     | THREE_ROOT(mv1,mv2,_,_,_) ->
                        (
                          if mv1 = value
                          then let new_mid : 'a t_234tree =
                                 two_rooting(mv2,empty_234tree(),empty_234tree()) in
                               three_rooting(v1,v2,l,new_mid,r)
                          else let new_mid : 'a t_234tree =
                                 two_rooting(mv1,empty_234tree(),empty_234tree()) in
                               three_rooting(v1,v2,l,new_mid,r)
                        )
                     | FOUR_ROOT(mv1,mv2,mv3,_,_,_,_) ->
                        (
                          if mv1 = value
                          then
                            let new_mid : 'a t_234tree =
                              three_rooting(mv2,mv3,
                                            empty_234tree(),empty_234tree(),empty_234tree()) in
                            three_rooting(v1,v2,l,new_mid,r)

                          else
                            if mv2 = value
                            then
                              let new_mid : 'a t_234tree =
                                three_rooting(mv1,mv3,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                              three_rooting(v1,v2,l,new_mid,r)

                            else
                              let new_mid : 'a t_234tree =
                                three_rooting(mv1,mv2,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                              three_rooting(v1,v2,l,new_mid,r)
                        )
                     | _ -> failwith "error unknown tree l1053"

                   else three_rooting(v1,v2,l,t234_delete(m,value),r)

              else three_rooting(v1,v2,l,t234_delete(m,value),r)
         else
           if v2 < value (*donc fils de droite*)
           then if isEquals_234tree(r,value)
                then if isLeaf_234tree(r)
                     then
                       match r with
                       | TWO_ROOT(rv,_,_) ->
                          (
                            match m with
                            | TWO_ROOT(mv,_,_) ->
                               let new_right : 'a t_234tree =
                                 three_rooting(mv,v2,
                                               empty_234tree(),empty_234tree(),empty_234tree()) in
                               two_rooting(v1,l,new_right)

                            | THREE_ROOT(mv1,mv2,_,_,_) ->
                               let new_right : 'a t_234tree =
                                 two_rooting(v2,empty_234tree(),empty_234tree())
                               and new_mid : 'a t_234tree =
                                 two_rooting(mv1,empty_234tree(),empty_234tree()) in
                               three_rooting(v1,mv2,l,new_mid,new_right)

                            | FOUR_ROOT(mv1,mv2,mv3,_,_,_,_) ->
                               let new_right : 'a t_234tree =
                                 two_rooting(v2,empty_234tree(),empty_234tree())
                               and new_mid : 'a t_234tree =
                                 three_rooting(mv1,mv2,
                                               empty_234tree(),empty_234tree(),empty_234tree()) in
                               three_rooting(v1,mv3,l,new_mid,new_right)

                            | _ -> failwith "error unknown tree l1088"
                          )
                       | THREE_ROOT(rv1,rv2,_,_,_) ->
                          (
                            if rv1 = value
                            then let new_right : 'a t_234tree =
                                   two_rooting(rv2,empty_234tree(),empty_234tree()) in
                                 three_rooting(v1,v2,l,m,new_right)
                            else let new_right : 'a t_234tree =
                                   two_rooting(rv1,empty_234tree(),empty_234tree()) in
                                 three_rooting(v1,v2,l,m,new_right)
                          )
                       | FOUR_ROOT(rv1,rv2,rv3,_,_,_,_) ->
                          (
                            if rv1 = value
                            then
                              let new_right : 'a t_234tree =
                                three_rooting(rv2,rv3,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                              three_rooting(v1,v2,l,m,new_right)
                            else

                            if rv2 = value
                            then
                              let new_right : 'a t_234tree =
                                three_rooting(rv1,rv3,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                              three_rooting(v1,v2,l,m,new_right)

                            else
                              let new_right : 'a t_234tree =
                                three_rooting(rv1,rv2,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                              three_rooting(v1,v2,l,m,new_right)

                          )
                       | _ -> failwith "error unknown tree l1124"

                   else three_rooting(v1,v2,l,m,t234_delete(r,value))

              else three_rooting(v1,v2,l,m,t234_delete(r,value))

           else

             if v1 = value
             then let new_root_value : 'a = t234_highest_value(l) in
                  let new_tree : 'a t_234tree = t234_delete(tree,new_root_value) in
                  modify_value(new_tree,v1,new_root_value)

             else let new_root_value : 'a = t234_highest_value(m) in
                  let new_tree : 'a t_234tree = t234_delete(tree,new_root_value) in
                  modify_value(new_tree,v2,new_root_value)

  | FOUR_ROOT(v1,v2,v3,l,ml,mr,r) ->
     if isLeaf_234tree(tree)
     then
       if v1 = value then three_rooting(v2,v3,empty_234tree(),empty_234tree(),empty_234tree())
       else
         if v2 = value then three_rooting(v1,v3,empty_234tree(),empty_234tree(),empty_234tree())
         else
           if v3 = value then three_rooting(v1,v2,empty_234tree(),empty_234tree(),empty_234tree())
           else tree
     else
       if value < v1
       then if isEquals_234tree(l,value)
            then if isLeaf_234tree(l)
                 then (*Si le pere est un 4-noeud et que ses fils sont des feuilles*)
                   (
                     match l with
                     | TWO_ROOT(lv,_,_) ->
                        (
                          match ml with
                          | TWO_ROOT(mlv,_,_) ->
                             let new_left : 'a t_234tree =
                               three_rooting(v1,mlv,
                                             empty_234tree(),empty_234tree(),empty_234tree()) in
                             three_rooting(v2,v3,new_left,mr,r)

                          | THREE_ROOT(mlv1,mlv2,_,_,_) ->
                             let new_left : 'a t_234tree =
                               two_rooting(v1,empty_234tree(),empty_234tree())
                             and new_midleft : 'a t_234tree =
                               two_rooting(mlv2,empty_234tree(),empty_234tree()) in
                             four_rooting(mlv1,v2,v3,new_left,new_midleft,mr,r)

                          | FOUR_ROOT(mlv1,mlv2,mlv3,_,_,_,_) ->
                             let new_left : 'a t_234tree =
                               two_rooting(v1,empty_234tree(),empty_234tree())
                             and new_midleft : 'a t_234tree =
                               three_rooting(mlv2,mlv3,
                                             empty_234tree(),empty_234tree(),empty_234tree()) in
                             four_rooting(mlv1,v2,v3,new_left,new_midleft,mr,r)

                          | _ -> failwith "error unknown tree l1181"
                        )
                     | THREE_ROOT(lv1,lv2,_,_,_) ->
                        (
                          if lv1 = value
                          then let new_left : 'a t_234tree =
                                 two_rooting(lv2,empty_234tree(),empty_234tree()) in
                               four_rooting(v1,v2,v3,new_left,ml,mr,r)
                          else let new_left : 'a t_234tree =
                                 two_rooting(lv1,empty_234tree(),empty_234tree()) in
                               four_rooting(v1,v2,v3,new_left,ml,mr,r)
                        )
                     | FOUR_ROOT(lv1,lv2,lv3,_,_,_,_) ->
                        (
                          if lv1 = value
                          then
                            let new_left : 'a t_234tree =
                              three_rooting(lv2,lv3,
                                            empty_234tree(),empty_234tree(),empty_234tree()) in
                            four_rooting(v1,v2,v3,new_left,ml,mr,r)

                          else
                            if lv2 = value
                            then
                              let new_left : 'a t_234tree =
                                three_rooting(lv1,lv3,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                              four_rooting(v1,v2,v3,new_left,ml,mr,r)

                            else
                              let new_left : 'a t_234tree =
                                three_rooting(lv1,lv2,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                              four_rooting(v1,v2,v3,new_left,ml,mr,r)
                        )
                     | _ -> failwith "error unknown tree l1216"
                   )
                 else (*Si le père est un 3-noeud et que ses fils ne sont pas des feuilles*)
                   four_rooting(v1,v2,v3,t234_delete(l,value),ml,mr,r)
            else four_rooting(v1,v2,v3,t234_delete(l,value),ml,mr,r)
       else
         if v1 < value && value < v2 (*donc fils du milieu gauche*)
         then if isEquals_234tree(ml,value)
              then if isLeaf_234tree(ml)
                   then
                     match ml with
                     | TWO_ROOT(mlv,_,_) ->
                        (
                          match l with
                          | TWO_ROOT(lv,_,_) ->
                             let new_left : 'a t_234tree =
                               three_rooting(lv,v1,
                                             empty_234tree(),empty_234tree(),empty_234tree()) in
                             three_rooting(v2,v3,new_left,mr,r)

                          | THREE_ROOT(lv1,lv2,_,_,_) ->
                             let new_midleft : 'a t_234tree =
                               two_rooting(v1,empty_234tree(),empty_234tree())
                             and new_left : 'a t_234tree =
                               two_rooting(lv1,empty_234tree(),empty_234tree()) in
                             four_rooting(lv2,v2,v3,new_left,new_midleft,mr,r)

                          | FOUR_ROOT(lv1,lv2,lv3,_,_,_,_) ->
                             let new_midleft : 'a t_234tree =
                               two_rooting(v1,empty_234tree(),empty_234tree())
                             and new_left : 'a t_234tree =
                               three_rooting(lv1,lv2,
                                             empty_234tree(),empty_234tree(),empty_234tree()) in
                             four_rooting(lv3,v2,v3,new_left,new_midleft,mr,r)

                          | _ -> failwith "error unknown tree l1251"
                        )
                     | THREE_ROOT(mlv1,mlv2,_,_,_) ->
                        (
                          if mlv1 = value
                          then let new_midleft : 'a t_234tree =
                                 two_rooting(mlv2,empty_234tree(),empty_234tree()) in
                               four_rooting(v1,v2,v3,l,new_midleft,mr,r)
                          else let new_midleft : 'a t_234tree =
                                 two_rooting(mlv1,empty_234tree(),empty_234tree()) in
                               four_rooting(v1,v2,v3,l,new_midleft,mr,r)
                        )
                     | FOUR_ROOT(mlv1,mlv2,mlv3,_,_,_,_) ->
                        (
                          if mlv1 = value
                          then
                            let new_midleft : 'a t_234tree =
                              three_rooting(mlv2,mlv3,
                                            empty_234tree(),empty_234tree(),empty_234tree()) in
                            four_rooting(v1,v2,v3,l,new_midleft,mr,r)

                          else
                            if mlv2 = value
                            then
                              let new_midleft : 'a t_234tree =
                                three_rooting(mlv1,mlv3,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                                 four_rooting(v1,v2,v3,l,new_midleft,mr,r)

                            else
                              let new_midleft : 'a t_234tree =
                                three_rooting(mlv1,mlv2,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                              four_rooting(v1,v2,v3,l,new_midleft,mr,r)
                        )
                     | _ -> failwith "error unknown tree l1286"
                   else four_rooting(v1,v2,v3,l,t234_delete(ml,value),mr,r)
              else four_rooting(v1,v2,v3,l,t234_delete(ml,value),mr,r)
         else
           if v2 < value && value < v3 (*donc fils milieu droite*)
           then if isEquals_234tree(mr,value)
            then if isLeaf_234tree(mr)
                 then (*Si le pere est un 4-noeud et que ses fils sont des feuilles*)
                   (
                     match mr with
                     | TWO_ROOT(mrv,_,_) ->
                        (
                          match ml with
                          | TWO_ROOT(mlv,_,_) ->
                             let new_mid : 'a t_234tree =
                               three_rooting(mlv,v2,
                                             empty_234tree(),empty_234tree(),empty_234tree()) in
                             three_rooting(v1,v3,l,new_mid,r)

                          | THREE_ROOT(mlv1,mlv2,_,_,_) ->
                             let new_midleft : 'a t_234tree =
                               two_rooting(mlv1,empty_234tree(),empty_234tree())
                             and new_midright : 'a t_234tree =
                               two_rooting(v2,empty_234tree(),empty_234tree()) in
                             four_rooting(v1,mlv2,v3,l,new_midleft,new_midright,r)

                          | FOUR_ROOT(mlv1,mlv2,mlv3,_,_,_,_) ->
                             let new_midleft : 'a t_234tree =
                               three_rooting(mlv1,mlv2,
                                             empty_234tree(),empty_234tree(),empty_234tree())
                             and new_midright : 'a t_234tree =
                               two_rooting(v2,empty_234tree(),empty_234tree()) in
                             four_rooting(v1,mlv3,v3,l,new_midleft,new_midright,r)

                          | _ -> failwith "error unknown tree l1320"
                        )
                     | THREE_ROOT(mrv1,mrv2,_,_,_) ->
                        (
                          if mrv1 = value
                          then let new_midright : 'a t_234tree =
                                 two_rooting(mrv2,empty_234tree(),empty_234tree()) in
                               four_rooting(v1,v2,v3,l,ml,new_midright,r)
                          else let new_midright : 'a t_234tree =
                                 two_rooting(mrv1,empty_234tree(),empty_234tree()) in
                               four_rooting(v1,v2,v3,l,ml,new_midright,r)
                        )
                     | FOUR_ROOT(mrv1,mrv2,mrv3,_,_,_,_) ->
                        (
                          if mrv1 = value
                          then
                            let new_midright : 'a t_234tree =
                              three_rooting(mrv2,mrv3,
                                            empty_234tree(),empty_234tree(),empty_234tree()) in
                            four_rooting(v1,v2,v3,l,ml,new_midright,r)

                          else
                            if mrv2 = value
                            then
                              let new_midright : 'a t_234tree =
                                three_rooting(mrv1,mrv3,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                                 four_rooting(v1,v2,v3,l,ml,new_midright,r)

                            else
                              let new_midright : 'a t_234tree =
                                three_rooting(mrv1,mrv2,
                                              empty_234tree(),empty_234tree(),empty_234tree()) in
                              four_rooting(v1,v2,v3,l,ml,new_midright,r)
                        )
                     | _ -> failwith "error unknown tree l1355"
                   )
                 else (*Si le père est un 3-noeud et que ses fils ne sont pas des feuilles*)
                   four_rooting(v1,v2,v3,l,ml,t234_delete(mr,value),r)
            else four_rooting(v1,v2,v3,l,ml,t234_delete(mr,value),r)
           else
             if v3 < value (*donc fils de droite*)
             then if isEquals_234tree(r,value)
                  then if isLeaf_234tree(r)
                       then
                         match r with
                         | TWO_ROOT(rv,_,_) ->
                            (
                              match mr with
                              | TWO_ROOT(mrv,_,_) ->
                                 let new_right : 'a t_234tree =
                                   three_rooting(mrv,v3,
                                                 empty_234tree(),empty_234tree(),empty_234tree()) in
                                 three_rooting(v1,v2,l,ml,new_right)

                              | THREE_ROOT(mrv1,mrv2,_,_,_) ->
                                 let new_right : 'a t_234tree =
                                   two_rooting(v3,empty_234tree(),empty_234tree())
                                 and new_midright : 'a t_234tree =
                                   two_rooting(mrv1,empty_234tree(),empty_234tree()) in
                                 four_rooting(v1,v2,mrv2,l,ml,new_midright,new_right)

                              | FOUR_ROOT(mrv1,mrv2,mrv3,_,_,_,_) ->
                                 let new_right : 'a t_234tree =
                                   two_rooting(v3,empty_234tree(),empty_234tree())
                                 and new_midright : 'a t_234tree =
                                   three_rooting(mrv1,mrv2,
                                                 empty_234tree(),empty_234tree(),empty_234tree()) in
                                 four_rooting(v1,v2,mrv3,l,ml,new_midright,new_right)

                              | _ -> failwith "error unknown tree l1390"
                            )
                         | THREE_ROOT(rv1,rv2,_,_,_) ->
                            (
                              if rv1 = value
                              then let new_right : 'a t_234tree =
                                     two_rooting(rv2,empty_234tree(),empty_234tree()) in
                                   four_rooting(v1,v2,v3,l,ml,mr,new_right)
                              else let new_right : 'a t_234tree =
                                     two_rooting(rv1,empty_234tree(),empty_234tree()) in
                                   four_rooting(v1,v2,v3,l,ml,mr,new_right)
                            )
                         | FOUR_ROOT(rv1,rv2,rv3,_,_,_,_) ->
                            (
                              if rv1 = value
                              then
                                let new_right : 'a t_234tree =
                                  three_rooting(rv2,rv3,
                                                empty_234tree(),empty_234tree(),empty_234tree()) in
                                four_rooting(v1,v2,v3,l,ml,mr,new_right)
                              else
                                if rv2 = value
                                then
                                  let new_right : 'a t_234tree =
                                    three_rooting(rv1,rv3,
                                                  empty_234tree(),empty_234tree(),empty_234tree()) in
                                     four_rooting(v1,v2,v3,l,ml,mr,new_right)

                                else
                                  let new_right : 'a t_234tree =
                                    three_rooting(rv1,rv2,
                                                  empty_234tree(),empty_234tree(),empty_234tree()) in
                                  four_rooting(v1,v2,v3,l,ml,mr,new_right)
                            )
                         | _ -> failwith "error unknown tree l1424"
                       else four_rooting(v1,v2,v3,l,ml,mr,t234_delete(r,value))
                  else four_rooting(v1,v2,v3,l,ml,mr,t234_delete(r,value))
             else
               if v1 = value
               then let new_root_value : 'a = t234_highest_value(l) in
                    let new_tree : 'a t_234tree = t234_delete(tree,new_root_value) in
                    modify_value(new_tree,v1,new_root_value)
               else
                 if v2 = value
                 then let new_root_value : 'a = t234_highest_value(ml) in
                      let new_tree : 'a t_234tree = t234_delete(tree,new_root_value) in
                      modify_value(new_tree,v2,new_root_value)
                 else let new_root_value : 'a = t234_highest_value(mr) in
                      let new_tree : 'a t_234tree = t234_delete(tree,new_root_value) in
                      modify_value(new_tree,v3,new_root_value)
;;

(*Vous pouvez suivre ce site : j'y reprend l'exemple et on retrouve les même résultats*)
(*https://azrael.digipen.edu/~mmead/www/Courses/CS280/Trees-2-3-4-delete.html*)

let a : 'a t_234tree = three_rooting(1,2,empty_234tree(),empty_234tree(),empty_234tree());;
let b : 'a t_234tree = three_rooting(5,6,empty_234tree(),empty_234tree(),empty_234tree());;
let c : 'a t_234tree = two_rooting(14,empty_234tree(),empty_234tree());;
let d : 'a t_234tree = three_rooting(18,19,empty_234tree(),empty_234tree(),empty_234tree());;
let e : 'a t_234tree = four_rooting(24,25,26,empty_234tree(),empty_234tree(),empty_234tree(),empty_234tree());;
let abc : 'a t_234tree = three_rooting(3,8,a,b,c);;
let de : 'a t_234tree = two_rooting(22,d,e);;
let abcde : 'a t_234tree = two_rooting(16,abc,de);;

let c1 : 'a t_234tree = t234_delete(abcde,1);;
let c2 : 'a t_234tree = t234_delete(c1,14);;
let c3 : 'a t_234tree = t234_delete(c2,8);;
let c4 : 'a t_234tree = t234_delete(c3,18);;

let c5 : 'a t_234tree = t234_delete(c4,3);;
let c6 : 'a t_234tree = t234_delete(c5,16);;(*ne donne pas le même résultat car la façon dont
                                             j'ai opéré est différente que la façon dont
                                             l'auteur le fait mais on a bien un 3-noeud avec
                                             en fils un 2-noeud et un 3-noeud*)
let c7 : 'a t_234tree = t234_delete(c6,5);;(*on retrouve pareil au final vous voyez*)
let c8 : 'a t_234tree = t234_delete(c7,6);;
let c9 : 'a t_234tree = t234_delete(c8,22);;
let c10 : 'a t_234tree = t234_delete(c9,2);;
let c11 : 'a t_234tree = t234_delete(c10,24);;
let c12 : 'a t_234tree = t234_delete(c11,25);;
let c13 : 'a t_234tree = t234_delete(c12,19);;
let c14 : 'a t_234tree = t234_delete(c13,26);;

(*On a réussi à supprimer tous les éléments en passant par la majorité des cas possibles
  Vous êtes libres d'essayer avec un ordre différent et un arbre différent*)
