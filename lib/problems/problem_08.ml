open Core

let year = 2024
let day = 8

type antennas_ht = (char, int list) Hashtbl.t

type grid =
  { data : char array
  ; nrow : int
  ; ncol : int
  }

let idx g x y = x + (y * g.ncol)
let xy g idx = idx % g.ncol, idx / g.ncol

let parse input : grid =
  let lines = String.split_lines input in
  let nrow = List.length lines in
  let ncol = String.length (List.hd_exn lines) in
  let data = String.split_lines input |> String.concat |> String.to_list in
  { data = Array.of_list data; nrow; ncol }
;;

let find_antennas g : antennas_ht =
  let ht = Hashtbl.create (module Char) in
  let () =
    Array.iteri g.data ~f:(fun idx x ->
      match x with
      | '.' -> ()
      | _ ->
        Hashtbl.update ht x ~f:(fun o ->
          match o with
          | None -> [ idx ]
          | Some lst -> idx :: lst))
  in
  ht
;;

(* Cartesian product except for self-products *)
let pairwise lst = List.cartesian_product lst lst |> List.filter ~f:(fun (x, y) -> x <> y)

let find_antinodes g idx1 idx2 ~mode : int list =
  let in_grid x y : bool = x >= 0 && x < g.ncol && y >= 0 && y < g.nrow in
  let x1, y1 = xy g idx1 in
  let x2, y2 = xy g idx2 in
  let dx, dy = x2 - x1, y2 - y1 in
  let xy_lst =
    match mode with
    | `Limited ->
      List.filter [ x2 + dx, y2 + dy; x1 - dx, y1 - dy ] ~f:(fun (x, y) -> in_grid x y)
    | `Infinite ->
      let rec aux x y dx dy acc =
        let nx, ny = x + dx, y + dy in
        if in_grid nx ny then aux nx ny dx dy ((nx, ny) :: acc) else acc
      in
      List.concat [ aux x1 y1 (-dx) (-dy) [ x1, y1 ]; aux x2 y2 dx dy [ x2, y2 ] ]
  in
  List.map xy_lst ~f:(fun (x, y) -> idx g x y)
;;

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let g = parse input in
    let antennas = find_antennas g in
    let antinodes =
      Hashtbl.data antennas
      |> List.map ~f:(fun lst ->
        let pairs = pairwise lst in
        List.map pairs ~f:(fun (p1, p2) -> find_antinodes g p1 p2 ~mode:`Limited)
        |> List.concat)
      |> List.concat
    in
    let res = Hash_set.length (Hash_set.of_list (module Int) antinodes) in
    Ok (string_of_int res)
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let g = parse input in
    let antennas = find_antennas g in
    let antinodes =
      Hashtbl.data antennas
      |> List.map ~f:(fun lst ->
        let pairs = pairwise lst in
        List.map pairs ~f:(fun (p1, p2) -> find_antinodes g p1 p2 ~mode:`Infinite)
        |> List.concat)
      |> List.concat
    in
    let res = Hash_set.length (Hash_set.of_list (module Int) antinodes) in
    Ok (string_of_int res)
  ;;
end