#require "core.top"
open Core

module Part1 = struct
  let run () =
    In_channel.with_file "input.txt" ~f:(fun ic ->
      In_channel.fold_lines ic
        ~init:(0,0)
        ~f:(fun (h,d) l ->
          match String.split ~on:' ' l with
          | "forward"::[v] -> (h+(int_of_string v),d)
          | "down"::[v] -> (h,d+(int_of_string v))
          | "up"::[v] -> (h,d-(int_of_string v)))
      |> (fun (h,d) -> h * d)
      |> Printf.printf "Part 1: %d\n")
end

module Part2 = struct
  let run () =
    In_channel.with_file "input.txt" ~f:(fun ic ->
      In_channel.fold_lines ic
        ~init:(0,0,0)
        ~f:(fun (h,d,a) l ->
          match String.split ~on:' ' l with
          | "down"::[v] -> (h,d,a+(int_of_string v))
          | "up"::[v] -> (h,d,a-(int_of_string v))
          | "forward"::[v] -> let v = (int_of_string v) in (h+v,d+a*v,a))
      |> (fun (h,d,_) -> h * d)
      |> Printf.printf "Part 2: %d\n")
end
