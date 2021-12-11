#require "core.top"
open Core

module Part1 = struct
  let run () =
    In_channel.with_file "input.txt" ~f:(fun ic ->
        In_channel.fold_lines ic
          ~init:0
          ~f:(fun cnt l ->
            let is_o = function '(' | '[' | '{' | '<' -> true | _ -> false
            and is_p = function ('(',')')|('[',']')|('{','}')|('<','>') -> true | _ -> false
            and v = function ')' -> 3 | ']' -> 57 | '}' -> 1197 | '>' -> 25137 | _ -> failwith "invalid input" in
            let s = Stack.create () in
            List.init (String.length l) (String.get l)
            |> List.fold_left ~init:(0,false) ~f:(fun (cnt,stop) c ->
                   if stop then (cnt,true) else
                   (if is_o c then (Stack.push s c; (cnt,false))
                   else (match Stack.pop s with | Some(cs) when is_p (cs,c) -> (cnt,false)
                                                | Some(cs) -> (cnt+(v c),true)
                                                | None -> (cnt+(v c),true))))
            |> (fun (lcnt,_) -> cnt + lcnt)
          )
        |> Printf.printf "Part 1: %d\n")
end
