open Sys;;

type floating = {value: float ref};;
type ens_gen = {content: float list ref};;
let ens_A = {content = ref []};;
let ens_B = {content = ref []};;
let ens_C = {content = ref []};;
let res = {value = ref 0.};;
let res_final = {value = ref 0.};;

#load "str.cma";;
open Str;;


(*To load a file and transform its data into a int list list*)
let load file =
  try
    let ich = open_in file in
    let len = in_channel_length ich in
    let buf = Buffer.create len in
    Buffer.add_channel buf ich len;
    close_in ich;
    Some buf
  with _ -> None
;;
              
let string_of_file filename =
  let ic = load filename in
  match ic with
  | None -> failwith "Error reading the file " ^ filename
  | Some buf -> Buffer.contents buf
;;

let list_of_file filename =
  Str.split (Str.regexp "\n") (string_of_file filename)
;;
  
let instance_of_file filename =
  let tmp = (List.map (Str.split (Str.regexp " ")) (list_of_file filename)) in
  match tmp with
  | [] -> []
  | head::tail -> transpose_list tail
;;

(*
 * Fonction transpose_list issue de
 * https://stackoverflow.com/questions/3989776/transpose-of-a-list-of-lists
 *)

let rec transpose_list list = match list with
  | []             -> []
  | []   :: xss    -> transpose_list xss
  | (x::xs) :: xss ->
     (x :: List.map List.hd xss) :: transpose_list (xs :: List.map List.tl xss)
;;

let int_list_of_string_list l =
  (List.map int_of_string l)
;;

let int_list_list_of_string_list_list l =
  (List.map int_list_of_string_list l)
;;

(***********************************************************************)

let comp a b = if a < b then a else b;;

(*We only consider machine A and B*)
let minimum list = comp (List.nth list 0) (List.nth list 1);;

(*Fusion_sorting : faster than traditionnal one*)
let rec divide_l list = match list with
  | [] -> ([], [])
  | [alone] -> ([alone], [])
  | head :: second :: tail -> let (list1,list2) = divide_l tail in (head :: list1, second :: list2);;


let rec fusion list1 list2 = match (list1,list2) with
  | ([], []) -> []
  | (_, []) -> list1
  | ([], _) -> list2
  | (a :: list1bis, b :: list2bis) -> if minimum a < minimum b then a :: (fusion list1bis list2)
    else b :: (fusion list1 list2bis);;

let rec fusion_sorting list = match list with
  | [] -> []
  | [alone] -> [alone]
  | _ -> let (list1,list2) = divide_l list in fusion (fusion_sorting list1) (fusion_sorting list2);;



(*Johnson's algorithm*)
let rec johnson l g d = let b = fusion_sorting l in
  match b with
  | [] -> []
  | [alone] -> [alone] 
  | head :: tail -> if minimum head = List.hd head  then [head] @ (johnson tail (g @ head) d)
    else (johnson tail g (head @ d)) @ [head]
;;

(*************************************************************************)

(*To iterate a several times the algorithm because time () returns "seconds"*)
let triv_johnson a =
  let c = johnson a [] [] in
    ()
;;

let test j a =
  for i = 0 to j do
    triv_johnson a;
  done
;;

(*For class A, generate ratio for each number of tasks, in ens_A *)
let calculate_ratio_A n i=
  for k = 1 to n do
    res.value := 0.;
    res_final.value := 0.;
    let kbis = k * 10 in
    for j = 0 to i-1 do
      let tmp = instance_of_file ("a"^(string_of_int kbis)^"_"^(string_of_int j)) in
      let ltmp = int_list_list_of_string_list_list tmp in
      let t1 = time() in
      test 10 ltmp;
      let t2 = ((time() -. t1) /. 10. ) in
      res.value := !(res.value) +. t2
    done;
    res_final.value := !(res.value) /. (float_of_int i);
    ens_A.content := !(ens_A.content) @ [!(res_final.value)]
  done;
  !(ens_A.content)
;;

(*For class B, generate ratio for each number of tasks, in ens_B *)
let calculate_ratio_B n i =
  for k = 1 to n do
    res.value := 0.;
    res_final.value := 0.;
    let kbis = k * 10 in
    for j = 0 to i-1 do
      let tmp = instance_of_file ("b"^(string_of_int kbis)^"_"^(string_of_int j)) in
      let ltmp = int_list_list_of_string_list_list tmp in 
      let t1 = time() in
      test 10 ltmp;
      let t2 = ((time() -. t1) /. 10. ) in
      res.value := !(res.value) +. t2
    done;
    res_final.value := !(res.value) /. (float_of_int i);
    ens_B.content := !(ens_B.content) @ [!(res_final.value)]
  done;
  !(ens_B.content)
;;

(*For class B, generate ratio for each number of tasks, in ens_B *)
let calculate_ratio_C n i =
  for k = 1 to n do
    res.value := 0.;
    res_final.value := 0.;
    let kbis = k * 10 in
    for j = 0 to i-1 do
      let tmp = instance_of_file ("c"^(string_of_int kbis)^"_"^(string_of_int j)) in
      let ltmp = int_list_list_of_string_list_list tmp in
      let t1 = time() in
      test 10 ltmp;
      let t2 = ((time() -. t1) /. 10. ) in
      res.value := !(res.value) +. t2
    done;
    res_final.value := !(res.value) /. (float_of_int i);
    ens_C.content := !(ens_C.content) @ [!(res_final.value)] 
  done;
  !(ens_C.content)
;;
(*Execution times*)
(*
let a = calculate_ratio_A 20 20;;
let b = calculate_ratio_B 20 20;;
let c = calculate_ratio_C 20 20;;
*)

(*To create a file contaning execution time's values for gnuplot, called only when all arguments have the same length*) 
let rec construct_plot li lf1 lf2 lf3 =
  match li,lf1,lf2,lf3 with
  | [],[],[],[] -> ""
  | (h0::t0),(h1::t1),(h2::t2),(h3::t3) -> (string_of_int h0)^" "^(string_of_float h1)^" "^(string_of_float h2)^" "^(string_of_float h3)^"\n"^(construct_plot t0 t1 t2 t3)
;;

let create_file_plot file li lf1 lf2 lf3 =
  let oc = open_out file in
  let s = construct_plot li lf1 lf2 lf3 in
  output_string oc s;
  close_out oc
;;

let rec range i j =  if i>j then [] else (i*10) :: range (i+1) j;;

(*
create_file_plot "johnson_gnuplot.txt" (range 1 20) a b c;; 
*)
