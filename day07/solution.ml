#require "core.top"
open Core

let r (ps : int list) =
 let max = Caml.Option.value ~default:0 @@ List.max_elt ~compare:Int.compare ps
 and min = Caml.Option.value ~default:0 @@ List.min_elt ~compare:Int.compare ps
 in List.range min max

module Part1 = struct
  let run () =
    In_channel.read_all "input.txt"
    |> (fun s -> String.sub s 0 ((String.length s) - 1))
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
    |> (fun ps -> List.map (r ps) ~f:(fun d -> List.fold_left ps ~init:0 ~f:(fun acc p -> acc + Int.abs (p - d))))
    |> List.min_elt ~compare:Int.compare
    |> Caml.Option.value ~default:0
    |> Printf.printf "Part 1: %d\n"
end
