open Core

let year = 2024
let day = 5

type rules = (int, int list) Hashtbl.t
type pages = int list

let parse (input : string) : rules * pages list =
  let lines = String.split_lines input in
  let split_idx, _ = List.findi_exn lines ~f:(fun _ x -> String.length x = 0) in
  let rule_strl = List.slice lines 0 split_idx in
  let pages_strl = List.slice lines (split_idx + 1) 0 in
  let rule_tbl = Hashtbl.create (module Int) in
  let parse_rule line =
    match String.split line ~on:'|' with
    | [ xs; ys ] ->
      let x = int_of_string xs in
      let y = int_of_string ys in
      Hashtbl.update rule_tbl x ~f:(fun o ->
        match o with
        | None -> [ y ]
        | Some lst -> y :: lst)
    | _ -> failwith ""
  in
  let parse_pages line : pages =
    String.split line ~on:',' |> List.map ~f:(fun x -> int_of_string x)
  in
  let () = List.iter ~f:parse_rule rule_strl in
  rule_tbl, List.map ~f:parse_pages pages_strl
;;

let is_right_order rules pagelst : bool =
  let seen = Hash_set.create (module Int) in
  let is_valid x : bool =
    let pages_after = Hashtbl.find rules x in
    let ret =
      match pages_after with
      | None -> true
      | Some plst ->
        (* See if any pages that should appear after have been seen *)
        List.map plst ~f:(fun p -> Hash_set.mem seen p) |> List.exists ~f:Fn.id |> not
    in
    Hash_set.add seen x;
    ret
  in
  List.map pagelst ~f:is_valid |> List.for_all ~f:Fn.id
;;

let get_mid_page pagelst : int = List.nth_exn pagelst (List.length pagelst / 2)

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let rulelst, pageslst = parse input in
    let mid_pages =
      List.map pageslst ~f:(fun p ->
        if is_right_order rulelst p then Some (get_mid_page p) else None)
    in
    let res = List.filter_map mid_pages ~f:Fn.id |> List.sum (module Int) ~f:Fn.id in
    Ok (string_of_int res)
  ;;
end

let compare p1 p2 (h : rules) : int =
  let p1v = Hashtbl.find h p1 |> Option.value ~default:[] in
  let p2v = Hashtbl.find h p2 |> Option.value ~default:[] in
  if List.exists p1v ~f:(fun x -> x = p2)
  then -1 (* -1 : p2 after p1 *)
  else if List.exists p2v ~f:(fun x -> x = p1)
  then 1 (* 1 : p1 after p2 *)
  else 0 (* 0 : No restriction *)
;;

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let rulelst, pageslst = parse input in
    let invalid_orders =
      List.map pageslst ~f:(fun p -> if is_right_order rulelst p then None else Some p)
      |> List.filter_map ~f:Fn.id
    in
    let sorted =
      List.map invalid_orders ~f:(fun l ->
        List.sort l ~compare:(fun x y -> compare x y rulelst))
    in
    let mid_pages = List.map sorted ~f:get_mid_page in
    let res = List.sum (module Int) mid_pages ~f:Fn.id in
    Ok (string_of_int res)
  ;;
end