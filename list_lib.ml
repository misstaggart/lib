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
prod_list acc l1 is all possible ways to add one element of l1 to the back of one element of acc *)
let rec prod_list acc l1 = let add_to_each_back = fun x -> fun acc -> List.map acc ~f:(fun l -> l @ [x]) in
  match l1 with
    [] -> []
  | x::xs -> (add_to_each_back x acc) @ (prod_list acc xs)

(*Pi_{0< i < |l|} l[i]
: 'a list list -> 'a list list *)
let iterated_prod l = match (List.tl l) with
    None -> []
  | Some tail -> List.fold tail ~init:(List.map (List.hd_exn l) ~f:(fun x -> [x]))
                   ~f:(fun prod -> fun l' -> prod_list prod l')
