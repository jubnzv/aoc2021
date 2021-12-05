#require "core.top"
open Core

module IntPair = struct
  module T = struct
    type t = int * int
    let mk (i1 : int) (i2 : int) : t = (i1, i2)
    let to_s (i1,i2) = Printf.sprintf "(%d,%d)" i1 i2
    let compare x y = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare x y
    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
  end
  include T
  include Comparable.Make(T)
end

module Part1 = struct
  let run () =
    let cnt = ref 0 in
    In_channel.with_file "input.txt" ~f:(fun ic ->
        In_channel.fold_lines ic
          ~init:(Map.empty (module IntPair))
          ~f:(fun m l ->
              String.split l ~on:' '
              |> (fun (p1::_::[p2]) ->
                let sp s = String.split s ~on:','
                           |> (fun (v1::[v2]) -> (Int.of_string v1, Int.of_string v2))
                in (sp p1, sp p2))
              |> (fun ((x1,y1),(x2,y2)) ->
                let add ?(x=true) m v r1 r2 =
                  List.range r1 @@ r2 + 1
                  |> List.fold_left
                       ~init:m
                       ~f:(fun m vv -> let p = if x then IntPair.mk v vv else IntPair.mk vv v in
                           if Map.mem m p then
                             if phys_equal 0 @@ Map.find_exn m p then (cnt := !cnt + 1; Map.set m p 1)
                             else m
                           else Map.add_exn m p 0)
                in
                let max x y = if x >= y then x else y and min x y = if x <= y then x else y in
                if      x1 == x2 then (add ~x:false m x1 (min y1 y2) (max y1 y2))
                else if y1 == y2 then (add ~x:true  m y1 (min x1 x2) (max x1 x2))
                else m)));
        Printf.printf "Part 1: %d\n" !cnt
end
