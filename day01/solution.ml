#require "core.top"
open Core

module Part1_core = struct
  let run () = 
    In_channel.with_file "input.txt" ~f:(fun ic ->
        In_channel.fold_lines ic
          ~init:[0;max_int]
          ~f:(fun (c::[p]) l ->
            let v = int_of_string l in if v > p then [(c+1);v] else [c;v])
        |> List.hd_exn
        |> Printf.printf "Part 1: %d\n")
end

module Part2_core = struct
  let run () =
    In_channel.with_file "input.txt" ~f:(fun ic ->
        let get_0th (a,_) = a in
        In_channel.fold_lines ic
          ~init:(0,[max_int;max_int;max_int])
          ~f:(fun (c,[p1;p2;p3]) l ->
            let v = int_of_string l in
            if List.reduce_exn ~f:(+) [p2;p3;v] > List.reduce_exn ~f:(+) [p1;p2;p3]
            then ((c+1),[p2;p3;v]) else (c,[p2;p3;v]))
        |> get_0th
        |> Printf.printf "Part 2: %d\n")
end
