#load "unix.cma";;
  #load "str.cma";;
    
open Str;;
open Unix;;
open Stack;;

(*We're using these to calculate execution times*)
type floating = {quant: float ref};;
type ens_gen = {quant_cont: float list ref};;
let ens_A = {quant_cont = ref []};;
let ens_B = {quant_cont = ref []};;
let ens_C = {quant_cont = ref []};;
let res_calc = {quant = ref 0.};;
let res_final = {quant = ref 0.};


(* "Global variables" used to lighten the exectution stack*)
type integer = {value: int ref};;
type llint = {content: int list list ref};;
let sumAC = {value = ref 0};;
let sumCA = {value = ref 0};;
let sumBC = {value = ref 0};;
let sumCB = {value = ref 0};;
let sumAB = {value = ref 0};;
let sumBA = {value = ref 0};;  
let binf = {value = ref 0};;
let bsup = {value = ref (-1)};;
let b1 = {value = ref 0};;
let b2 = {value = ref 0};;
let b3 = {value = ref 0};;
let b4 = {value = ref 0};;
let bpia = {value = ref 0};;
let bpib = {value = ref 0};;
let bpic = {value = ref 0};;
let b2 = {value = ref 0};;
let b3 = {value = ref 0};;
let tpia = {value = ref 0};;
let tpib = {value = ref 0};;
let tpic = {value = ref 0};;
let sum_pip_a = {value = ref 0};;
let sum_pip_b = {value = ref 0};;
let sum_pip_c = {value = ref 0};;
let min_pip_bc = {value = ref 0};;   (* à initialiser comme il faut dans f1*)
let min_pip_c = {value = ref 0};;
let res = {content = ref []};;

(*Initializing the right value*)
let initialize l pip =
  (* "real" initialization *)
  sumAC.value := 0; sumCA.value := 0;
  sumBC.value := 0; sumCB.value := 0;
  sumAB.value := 0; sumBA.value := 0;
  bsup.value := -1;
  tpic.value := 0; tpib.value := 0; tpia.value := 0;
  res.content := [];
  sum_pip_a.value := sum (sum_pile pip [0]);
  sum_pip_b.value := sum (sum_pile pip [1]);
  sum_pip_c.value := sum (sum_pile pip [2]);
  min_pip_bc.value := minimum (sum_pile pip [1;2]);
  min_pip_c.value := minimum (sum_pile pip [2]);
  sumAC.value := sum_ordered_XY l 0 2;
  sumCA.value := sum_orderedQ_XY l 2 0;
  sumBC.value := sum_ordered_XY l 1 2;
  sumCB.value := sum_orderedQ_XY l 2 1;
  sumAB.value := sum_ordered_XY l 0 1;
  sumBA.value := sum_orderedQ_XY l 1 0;
  binf.value := 0;
  bpia.value := !(tpia.value) + !(sum_pip_a.value) + !(min_pip_bc.value);
  bpib.value := !(tpib.value) + !(sum_pip_b.value) + !(min_pip_c.value);
  bpic.value := !(tpic.value) + !(sum_pip_c.value);
  b1.value := (max !(bpia.value) (max !(bpib.value) !(bpic.value)));
  b2.value := maximum (sumk_AC pip !(sumAC.value) !(sumCA.value) !(tpia.value));
  b3.value := maximum (sumk_BC pip !(sumBC.value) !(sumCB.value) !(tpib.value));
  b4.value := maximum (sumk_AB pip !(sumAB.value) !(sumBA.value) !(tpia.value));
  binf.value := (max (max !(b1.value) !(b2.value)) (max !(b3.value) !(b4.value)));
  pip
;;


(*BOTH VERIFIED To transform a stack into a list and in return*)
let rec transformPL p =
  let pbis = copy p in
  if is_empty pbis then []
  else
    let a = pop pbis in
    (transformPL pbis) @ [a];;

let rec transformLP l =
  match l with
  | [] -> Stack.create ()
  | head :: tail -> let p = (transformLP tail) in
		    push head p;p;
;;

(*VERIFIED To verify if an element is in a given list*)
let rec is_in_aux l e i =
  if i >= List.length l then false
  else if List.nth l i = e then true
  else
    is_in_aux l e (i+1);;

let is_in e l = match l with
  | [] -> false
  | _ -> is_in_aux l e 0;;

(*ALL VERIFIED To calculate the sum of elements in a a' list stack based on given parameters*)
let sum l = match l with
  | [] -> 0
  | [alone] -> alone
  | head :: tail -> List.fold_left (+) head tail;;

let rec sum_params_aux l params i res =
  if i>= List.length params then
    res
  else if (List.nth params i) >= (List.length l) || (List.nth params i) < 0 then
    sum_params_aux l params (i+1) res
  else
    sum_params_aux l params (i+1) (res + List.nth l (List.nth params i))
;;

let sum_params l params = match l,params with
  | [],_ -> 0
  | _,[] -> sum l
  | _,_ -> sum_params_aux l params 0 0
;;

(*BOTH VERIFIED Useful functions for initialisation*)
let rec sum_ordered_XY l x y = match l with
  | [] -> 0
  | [alone] -> if List.nth alone x <= List.nth alone y then List.nth alone x
    else 0
  | head :: tail -> if List.nth head x <= List.nth head y then (List.nth head x) + sum_ordered_XY tail x y
    else 0 + sum_ordered_XY tail x y
;;

let rec sum_orderedQ_XY l x y = match l with
  | [] -> 0
  | [alone] -> if List.nth alone x < List.nth alone y then List.nth alone x
    else 0
  | head :: tail -> if List.nth head x < List.nth head y then (List.nth head x) + sum_ordered_XY tail x y
    else 0 + sum_ordered_XY tail x y
;;

(*To create a list, containing the sum of elements in a list, extracted from a pile, on given parameters*)
let rec sum_pile p params =
  let copyp = copy p in
  if is_empty copyp then []
  else
    let tmp = pop copyp in
    sum_params tmp params :: sum_pile copyp params
;;


(*VERIFIED To push an element at the bottom of a stack*)
let rec push_bottom bot p =
  if is_empty p then
    begin
      push bot p;
      p;
    end
  else
    begin
      let tmp = pop p in
      push tmp (push_bottom bot p);
      p;
    end
;;

(*VERIFIED  To find the minimum in a' list Stack*)
let minimum l = match l with
  | [] -> -1
  | [alone] -> alone
  | head :: tail -> List.fold_left min head tail
;;

(*VERIFIED To find the max in a list l*)
let maximum l = match l with
  | [] -> !(b1.value) + !(b2.value) + !(b3.value) + !(b4.value) + !(binf.value)
  | head :: tail -> List.fold_left max head tail;;


(*To calculate the second minor born b2*)
let rec sumk_AC p ac ca tpia = (* l'argument p doit être pip *)
  let pipb = copy p in
  if is_empty pipb then []
  else
    begin
      let ack = {value = ref 0} in
      let cak = {value = ref 0} in
      let a = pop pipb in
      if List.nth a 0 <= List.nth a 2 then
	ack.value := ac - List.nth a 0
      else if List.nth a 2 < List.nth a 0 then
	cak.value := ca - List.nth a 2;
      [tpia + (sum a) + !(ack.value) + !(cak.value)] @ sumk_AC pipb ac ca tpia
    end
;;

(*To calculate the third minor born b3*)
let rec sumk_BC p bc cb tpib =
  let pipb = copy p in
  if is_empty pipb then []
  else
    begin
      let bck = {value = ref 0} in
      let cbk = {value = ref 0} in
      let a = pop pipb in
      if List.nth a 1 <= List.nth a 2 then
	bck.value := bc - List.nth a 1
      else if List.nth a 2 < List.nth a 1 then
	cbk.value := cb - List.nth a 2;
      [tpib + ((List.nth a 1) + (List.nth a 2)) + !(bck.value) + !(cbk.value)] @ sumk_BC pipb bc cb tpib
    end
;;

(*To calculate a fourth minor born b4, question 9*)
let rec sumk_AB p ab ba tpia =
  let pipb = copy p in
  if is_empty pipb then []
  else
    begin
      let abk = {value = ref 0} in
      let bak = {value = ref 0} in
      let a = pop pipb in
      if List.nth a 0 <= List.nth a 1 then
	abk.value := ab - List.nth a 0
      else if List.nth a 1 < List.nth a 0 then
	bak.value := ba - List.nth a 1;
      [tpia + ((List.nth a 1) + (List.nth a 2)) + !(abk.value) + !(bak.value)] @ sumk_AB pipb ab ba tpia
    end
;;

(*To udpate values and limits*)
let update pi pip = 
  let tmp = pop pip in
  let tmpDa = List.nth tmp 0 in
  let tmpDb = List.nth tmp 1 in
  let tmpDc = List.nth tmp 2 in
  if tmpDa <= tmpDc then
    sumAC.value :=  !(sumAC.value) - tmpDa
  else
    sumCA.value := !(sumCA.value) - tmpDc;
  if tmpDb <= tmpDc then
    sumBC.value :=  !(sumBC.value) - tmpDb
  else
    sumCB.value :=  !(sumCB.value) - tmpDc;
  if tmpDa <= tmpDb then
    sumAB.value :=  !(sumAB.value) - tmpDa
  else
    sumBA.value :=  !(sumBA.value) - tmpDb;
  push tmp pi;
  (*Values and limits update*)
  sum_pip_a.value := !(sum_pip_a.value) - tmpDa;
  sum_pip_b.value := !(sum_pip_b.value) - tmpDb;
  sum_pip_c.value := !(sum_pip_c.value) - tmpDc;
  tpia.value := !(tpia.value) + tmpDa;
  tpib.value := (max !(tpia.value) !(tpib.value)) + tmpDb;
  tpic.value := (max !(tpib.value) !(tpic.value)) + tmpDc;
  if tmpDc = minimum (sum_pile pip [2]) then
    min_pip_bc.value := minimum (sum_pile pip [2]);
  if (tmpDb + tmpDc) = minimum (sum_pile pip [1;2]) then
    min_pip_bc.value := minimum (sum_pile pip [1;2]);
  bpia.value := !(tpia.value) + !(sum_pip_a.value) + !(min_pip_bc.value);
  bpib.value := !(tpib.value) + !(sum_pip_b.value) + !(min_pip_c.value);
  bpic.value := !(tpic.value) + !(sum_pip_c.value);
  b1.value := max !(bpia.value) (max !(bpib.value) !(bpic.value));
  b2.value := maximum (sumk_AC pip !(sumAC.value) !(sumCA.value) !(tpia.value));
  b3.value := maximum (sumk_BC pip !(sumBC.value) !(sumCB.value) !(tpib.value));
  b4.value := maximum (sumk_AB pip !(sumAB.value) !(sumBA.value) !(tpia.value));
  (* TODO: mettre à jour correctement bint en cas d'ajout/suppression de borne *)
  binf.value := (max (max !(b1.value) !(b2.value)) (max !(b3.value) !(b4.value)));
;;

(*To prune the tree,updating values and limits consequently*)
let reverse_update pi pip tmp_tpib tmp_tpic tmp_bpia tmp_bpib tmp_bpic =
  let tmp = pop pi in
      let tmpDa = List.nth tmp 0 in
      let tmpDb = List.nth tmp 1 in
      let tmpDc = List.nth tmp 2 in
      if tmpDa <= tmpDc then
	sumAC.value := !(sumAC.value) + tmpDa
      else
	sumCA.value := !(sumCA.value) + tmpDc;
      if tmpDb <= tmpDc then
	sumBC.value := !(sumBC.value) + tmpDb
      else
	sumCB.value := !(sumCB.value) + tmpDc;
      if tmpDa <= tmpDb then
	sumAB.value := !(sumAB.value) + tmpDa
      else
	sumBA.value := !(sumBA.value) + tmpDb;
      unit_of_whatever (push_bottom tmp pip);
      (*Values and limits update*)
      sum_pip_a.value := !(sum_pip_a.value) + tmpDa;
      sum_pip_b.value := !(sum_pip_b.value) + tmpDb;
      sum_pip_c.value := !(sum_pip_c.value) + tmpDc;
      tpia.value := !(tpia.value) - tmpDa;
      tpib.value := tmp_tpib;
      tpic.value := tmp_tpic;
      if tmpDc < minimum (sum_pile pip [2]) then
	min_pip_c.value := tmpDc;
      if (tmpDb + tmpDc) < minimum (sum_pile pip [1;2]) then
	min_pip_bc.value := tmpDb + tmpDc;
      bpia.value := tmp_bpia;
      bpib.value := tmp_bpib;
      bpic.value := tmp_bpic;
      b1.value := max !(bpia.value) (max !(bpib.value) !(bpic.value));
      b2.value := maximum (sumk_AC pip !(sumAC.value) !(sumCA.value) !(tpia.value));
      b3.value := maximum (sumk_BC pip !(sumBC.value) !(sumCB.value) !(tpib.value));
      b4.value := maximum (sumk_AB pip !(sumAB.value) !(sumBA.value) !(tpia.value));
      (* TODO: mettre à jour correctement bint en cas d'ajout/suppression de borne *)
      binf.value := (max (max !(b1.value) !(b2.value)) (max !(b3.value) !(b4.value)));
      false
;;

(*Main functions*)
let rec f2 pi pip =
  if Stack.is_empty pip then (* LEAF *)
    begin
      if !(bsup.value) = -1 || !(bsup.value) > !(tpic.value) then
	begin
	  res.content := transformPL pi;
	  bsup.value := !(tpic.value);
          false;
	end
      else
	begin
	  false;
	end;
    end
  else (* NOT LEAF *)
    begin
      (* TODO: mettre à jour binf correctement en cas d'ajout de borne *)
      if !(bsup.value) = -1  || !(binf.value) <= !(bsup.value) then
	let flag = f3 pi pip 0 (length pip) in
	if flag then true
	else false
      else (* ÉLAGAGE *)
	false
    end
  and
    f3 pi pip i j =
  if i > j then
    false
  else
    begin
      let tmp_tpib = !(tpib.value) in
      let tmp_tpic = !(tpic.value) in
      let tmp_bpia = !(bpia.value) in
      let tmp_bpib = !(bpib.value) in
      let tmp_bpic = !(bpic.value) in
      update pi pip;
      unit_of_whatever (f2 pi pip);
      unit_of_whatever (reverse_update pi pip tmp_tpib tmp_tpic tmp_bpia tmp_bpib tmp_bpic);
      f3 pi pip (i+1) j
    end
;;

(* TODO: A APPELER SUR F3 *)
let exact_method l =
  f3 (create ()) (initialize l (transformLP l)) 0 (List.length l);
;;

let unit_of_whatever p = ();;

let p = !(res.content);;

(* utilities to print lists and associated objects *)

let rec print_list_aux l = match l with
  |[] -> print_string "]"
  |e::f -> print_int e; print_string " "; print_list_aux f;;

let rec print_list l = match l with
  | [] -> ()
  | [alone] -> print_string "[ "; print_list_aux alone
  | head :: tail -> print_string "[ "; print_list_aux head; print_string ";";  print_list tail
;;
  
let rec string_of_int_list_local l =
  match l with
  | [] -> ""
  | head::tail -> " " ^ (string_of_int head) ^ ";" ^ string_of_int_list_local tail
;;

let string_of_int_list l =
  match l with
  | [] -> "[]"
  | _ -> let s = string_of_int_list_local l in
         Bytes.set s 0 '[';
         Bytes.set s (String.length s -1) ']';
         s
;;

let rec string_of_int_list_list_local l =
  match l with
  | [] -> ""
  | head::tail -> (string_of_int_list head) ^ "\n" ^ string_of_int_list_list_local tail
;;

let string_of_int_list_list l =
  match l with
  | [] -> ""
  | _ -> "\n" ^ string_of_int_list_list_local l
;;


let int_list_of_string_list l =
  (List.map int_of_string l)
;;

let int_list_list_of_string_list_list l =
  (List.map int_list_of_string_list l)
;;
  
(* parse utilities *)

  (* fonction load et utilisation de cette dernière tirées de
   * https://openclassrooms.com/forum/sujet/ocaml-lire-un-fichier-94653
   *)
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

(*
let u1 = [[2;3;4];[2;3;5];[9;4;1]];;
let u2 = [[2;3;5];[2;3;4];[9;4;1]];;
let u3 = [[2;3;5];[9;4;1];[2;3;4]];;
let u4 = [[36;15;18];[47;89;31];[5;11;7];[78;61;62]];;
let u5 = [[35;26;13];[12;15;19];[25;41;21];[6;15;25];[76;65;51];[56;68;42];[54;42;28];[25;32;31]];;

Str.split (Str.regexp " ") "bla bli blou";;
  
let string_instance = instance_of_file "test";;

let instance = int_list_list_of_string_list_list string_instance;;

  exact_method u5;;
    print_string (string_of_int_list_list !(res.content));;
*)

(*To iterate a several times the algorithm because time () returns "seconds"*)
let triv_exact l =
  let c = exact_method l in
    ()
;;

let test j l =
  for i = 0 to j do
    triv_exact l;
  done
;;

(*For class A, generate ratio for each number of tasks, in ens_A *)
let calculate_ratio_A n i=
  for k = 1 to n do
    res_calc.quant := 0.;
    res_final.quant := 0.;
    let kbis = k * 3 in
    for j = 0 to i-1 do
      let tmp = instance_of_file ("a"^(string_of_int kbis)^"_"^(string_of_int j)) in
      let ltmp = int_list_list_of_string_list_list tmp in
      let t1 = time() in
      test 30 ltmp;
      let t2 = ((time() -. t1) /. 30. ) in
      res_calc.quant := !(res_calc.quant) +. t2
    done;
    res_final.quant := !(res_calc.quant) /. (float_of_int i);
    ens_A.quant_cont := !(ens_A.quant_cont) @ [!(res_final.quant)]
  done;
  !(ens_A.quant_cont)
;;

(*For class B, generate ratio for each number of tasks, in ens_B *)
let calculate_ratio_B n i =
  for k = 1 to n do
    res_calc.quant := 0.;
    res_final.quant := 0.;
    let kbis = k * 3 in
    for j = 0 to i-1 do
      let tmp = instance_of_file ("b"^(string_of_int kbis)^"_"^(string_of_int j)) in
      let ltmp = int_list_list_of_string_list_list tmp in 
      let t1 = time() in
      test 30 ltmp;
      let t2 = ((time() -. t1) /. 30. ) in
      res_calc.quant := !(res_calc.quant) +. t2
    done;
    res_final.quant := !(res_calc.quant) /. (float_of_int i);
    ens_B.quant_cont := !(ens_B.quant_cont) @ [!(res_final.quant)]
  done;
  !(ens_B.quant_cont)
;;

(*For class B, generate ratio for each number of tasks, in ens_C *)
let calculate_ratio_C n i =
  for k = 1 to n do
    res_calc.quant := 0.;
    res_final.quant := 0.;
    let kbis = k * 3 in
    for j = 0 to i-1 do
      let tmp = instance_of_file ("c"^(string_of_int kbis)^"_"^(string_of_int j)) in
      let ltmp = int_list_list_of_string_list_list tmp in
      let t1 = time() in
      test 30 ltmp;
      let t2 = ((time() -. t1) /. 30. ) in
      res_calc.quant := !(res_calc.quant) +. t2
    done;
    res_final.quant := !(res_calc.quant) /. (float_of_int i);
    ens_C.quant_cont := !(ens_C.quant_cont) @ [!(res_final.quant)] 
  done;
  !(ens_C.quant_cont)
;;

(*
(*Execution times*)
let a = calculate_ratio_A 3 4;;
let b = calculate_ratio_B 3 4;;
let c = calculate_ratio_C 3 4;;


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

let rec range i j =  if i>j then [] else (i*3) :: range (i+1) j;;

create_file_plot "exact_fourb_gnuplot.txt" (range 1 9) a b c;; 
*)
