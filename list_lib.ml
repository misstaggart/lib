include Base
include Core
include Core_kernel
(*some useful list functions*)

let cons x l = x::l

let cons_b x l = l @ [x]

let rec repeat l n = if n = 0 then [] else l @ (repeat l (n-1))

(* subset: int -> 'a list -> 'a list list
computes all subsets of l allowing for n duplicates of
  a value per list*)
let subset ?dups:(n = 0) l =
  let lwithdups = repeat l (n + 1) in
  List.fold lwithdups ~init:[[]] ~f:(fun acc-> fun x -> acc @ (List.map acc ~f:(cons x)))

(*computes a list of lists comprising every possible way x can be inserted into l*)
let all_insertions x l =
  let rec all_insertions_help n acc = if (n < 0) then acc else
      let (front, back) = List.split_n l n in all_insertions_help (n-1) ((front @ (x::back))::acc) in
  all_insertions_help (List.length l) []

let rec permute l = match l with
    [] -> [[]]
  | y::ys -> List.concat (List.map (permute ys) ~f:(all_insertions y))

let sublist ~compare:cmp ?dups:(n = 0) l = List.dedup_and_sort ~compare:(List.compare cmp) (List.concat (List.map (subset ~dups:n l) ~f:permute))


(* : 'a list list -> 'a list -> 'a list list
prod_list acc l1 is all possible ways to cons one element of l1 to the front of one element of acc *)
let rec prod_list l1 acc = let add_to_each_front = fun x -> fun acc -> List.map acc ~f:(fun l -> x::l ) in
  match l1 with
    [] -> []
  | x::xs -> (add_to_each_front x acc) @ (prod_list xs acc)

(*Pi_{0< i < |l|} l[i]
: 'a list list -> 'a list list *)
let iterated_prod l = match (List.drop_last l) with
    None -> [[]]
  | Some front -> List.fold_right front ~f:(fun l' -> fun prod -> prod_list l' prod)
                    ~init:(List.map (List.last_exn l) ~f:(fun x -> [x]))
                   


(*: 'a list -> 'a -> int*)
let rec set_exn l v i = let tail = (List.tl_exn l) in
  if (i == 0) then v::tail else
    (List.hd_exn l) :: (set_exn tail v (i - 1))

(*: 'a list -> ('a -> 'a) -> ('a -> bool) -> 'a list
find_set_exn l f p replaces each (a \in l st p a = true) with f a *)
let find_set_exn l f p =
  let setme = List.filter_mapi l ~f:(fun i -> fun a -> if (p a) then Some (i, a) else None) in
  List.fold setme ~init:l ~f:(fun l_old -> fun (i, a) -> set_exn l_old (f a) i) 

let (/*) = List.cartesian_product

(*finite functions, represented as a list of ordered pairs, from l1 -> l2
|l2 ^ l1|*)
let exponential l1 l2 =
  let mappings_poss = (List.map l1 ~f:(fun x -> List.cartesian_product [x] l2)) in (*: ('a * 'b) list list
                                                                                  each element is all possible mappings of one domain value into the range*)
  iterated_prod mappings_poss


(*computes l1' x l2', where
li' = li if li <> []
[v] otherwise
let rec /*_ne ~filler:v l1 l2 = match (l1, l2) with
    ([], _) -> /*_ne ~filler:v [v] l2
  | (_, []) -> /*_ne ~filler:v l1 [v]
  | _ -> List.cartesian_product l1 l2 *)
