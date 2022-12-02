open Gfile
open Tools
open FonctionFF

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in
  let graph = gmap graph int_of_string in 
  (*let () = export outfile graph in *)
  (*let int_of_intoption = function None -> [] | Some n -> n in 
    let chemin = trouver_chemin graph 0 5 in 
    let rec print_tuples =
    function
    | [] -> ()
    | (a, b) :: rest ->
      Printf.printf "(%i, %i); " a b;
      print_tuples rest in 

    let () =
    print_tuples (int_of_intoption chemin) in 
    ()*)


  (*let () =
    Printf.printf "%d\n" (int_of_intoption (trouver_chemin graph 0 5))*)

  (* we'd rather do something different on None *)
  let l = trouver_chemin graph 0 5 in 
  let l= Option.get l in 
  let ()= List.iter (fun (a,b) -> Printf.printf "(%i, %i); " a b ) l in 



  (*let graph = add_arc graph 0 2 2 in *)
  (*let graph = int_to_flot graph in *)
  (*test flot update ok*)
  (*let graph = flot_update graph [(4,5)] 10 in 
    let graph = construire_res graph in*) 
  (*let graph = gmap graph string_of_int in*)
  (*let graph = flot_to_string graph in*) 

  (*let graph = construire_res graph in 
    let graph = gmap graph string_of_int in*)
  let graph = gmap graph string_of_int in 

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph in
  ()

