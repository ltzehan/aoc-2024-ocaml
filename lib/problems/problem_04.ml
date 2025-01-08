open Core

let year = 2024
let day = 4

type grid =
  { data : char list
  ; ncol : int
  ; nrow : int
  }

let get g x y =
  if x < 0 || x >= g.ncol || y < 0 || y >= g.nrow
  then None
  else (
    let idx = x + (g.ncol * y) in
    List.nth g.data idx)
;;

let xy g i = i % g.ncol, i / g.ncol

(* Flatten grid *)
let parse (input : string) : grid =
  let lines = String.split_lines input in
  let nrow = List.length lines in
  let ncol = String.length (List.nth_exn lines 0) in
  { data = String.concat lines |> String.to_list; ncol; nrow }
;;

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let dirs = [ -1, -1; -1, 0; -1, 1; 0, 1; 1, 1; 1, 0; 1, -1; 0, -1 ] in
    let g = parse input in
    let join i dx dy : string option =
      let x, y = xy g i in
      List.range ~stop:`inclusive 0 3
      |> List.map ~f:(fun n -> x + (dx * n), y + (dy * n))
      |> List.map ~f:(fun (nx, ny) -> get g nx ny)
      |> Option.all
      |> Option.map ~f:String.of_char_list
    in
    let search i : string option list =
      let x, y = xy g i in
      let o = get g x y in
      match o with
      (* Optimize by checking first letter separately *)
      | Some c when Char.equal c 'X' -> List.map dirs ~f:(fun (dx, dy) -> join i dx dy)
      | _ -> []
    in
    let check (o : string option) : bool =
      match o with
      | None -> false
      | Some s -> String.equal s "XMAS"
    in
    let count_xmas i : int = search i |> List.map ~f:check |> List.count ~f:Fn.id in
    let res =
      List.range 0 (List.length g.data)
      |> List.map ~f:count_xmas
      |> List.sum (module Int) ~f:Fn.id
    in
    Ok (string_of_int res)
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let g = parse input in
    (* DL; UL; UR; DR *)
    let dirs = [ -1, -1; -1, 1; 1, 1; 1, -1 ] in
    let search i : char list option =
      let x, y = xy g i in
      match get g x y with
      | Some 'A' ->
        List.map ~f:(fun (dx, dy) -> get g (x + dx) (y + dy)) dirs |> Option.all
      | _ -> None
    in
    let check_mas (lst : char list) =
      match lst with
      | [ 'M'; 'M'; 'S'; 'S' ]
      | [ 'S'; 'M'; 'M'; 'S' ]
      | [ 'S'; 'S'; 'M'; 'M' ]
      | [ 'M'; 'S'; 'S'; 'M' ] -> true
      | _ -> false
    in
    let check (o : char list option) =
      match o with
      | None -> false
      | Some lst -> check_mas lst
    in
    let res =
      List.range 0 (List.length g.data)
      |> List.map ~f:search
      |> List.map ~f:check
      |> List.count ~f:Fn.id
    in
    Ok (string_of_int res)
  ;;
end