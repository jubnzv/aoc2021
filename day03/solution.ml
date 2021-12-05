#require "core.top"
open Core

module Part1 = struct
  let run () =
    In_channel.with_file "input.txt" ~f:(fun ic ->
        In_channel.fold_lines ic
          ~init:[]
          ~f:(fun acc l ->
            List.mapi
              (List.init
                (String.length l)
                ~f:(fun i -> String.get l i |> (fun i -> if Char.equal i '0' then (0,1) else (1,0))))
              ~f:(fun i (c1,c0) ->
                  if List.is_empty acc then (c1,c0)
                  else let (ac1,ac0) = List.nth_exn acc i in (ac1+c1,ac0+c0)))
        |> List.fold_left
             ~init:([],[])
             ~f:(fun (gr,er) (c0,c1) ->
               if c0 > c1 then (gr@['0'],er@['1'])
               else            (gr@['1'],er@['0']))
        |> (fun (gr,er) ->
             let conv seq =
               Int32.of_string ("0b" ^ (Caml.String.of_seq @@ Caml.List.to_seq seq))
               |> Int32.to_int_exn
             in
            (conv gr) * (conv er))
        |> Printf.printf "Part 1: %d\n")
end

module Part2 = struct
  let run () =
    In_channel.with_file "input.txt" ~f:(fun ic ->
      In_channel.fold_lines ic
        ~init:[]
        ~f:(fun acc l ->
      ))
end
