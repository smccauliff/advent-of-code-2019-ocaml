open Base
open Stdio

(*                       0  1   2  3  4  5   6  7   8   9  10  11 *)
let test_program_0  = [| 1; 9; 10; 3; 2; 3; 11; 0; 99; 30; 40; 50 |];;
let final_test_program_0 = [| 3500; 9; 10; 70; 2; 3; 11; 0; 99; 30; 40; 50 |];;


let test_program_1 = [| 1; 0; 0; 0; 99 |];;
let final_test_program_1 = [| 2; 0; 0; 0; 99 |];;
let test_program_2 = [| 2; 3; 0; 3; 99|];;
let final_test_program_2 = [| 2; 3; 0; 6; 99 |];;
let test_program_3 = [| 2; 4; 4; 5; 99; 0 |];;
let final_test_program_3 = [| 2; 4; 4; 5; 99; 9801 |];;
let test_program_4 = [| 1; 1; 1; 4; 99; 5; 6; 0; 99|];;
let final_test_program_4 = [| 30; 1; 1; 4; 2; 5; 6; 0; 99 |];;


let run_opcode prog pc op = 
    prog.(prog.(pc + 3)) <- op prog.(prog.(pc + 1)) prog.(prog.(pc + 2));;

let function_for_opcode opcode =
    if opcode = 1 then
        ( + )
    else if opcode = 2 then
        ( * )
    else
        raise (Invalid_argument (Printf.sprintf "Bad opcode %d" opcode));;

let rec run_program prog pc =
    if pc >= Array.length prog || prog.(pc) = 99 then
        ()
    else
        let op = function_for_opcode prog.(pc) in
        let _ = run_opcode prog pc op in (* I don't know why this let statement makes this compile *)
        run_program prog (pc + 4);;

let print_program prog =
    Array.iter prog ~f:(fun opcode -> printf "%d," opcode);
    printf "\n";;

let run_and_test prog final =
    run_program prog 0;
    print_program prog;
    let ok = Array.equal ( = ) prog final in
        printf " ok %b\n" ok;;

let restore_working_state prog =
    prog.(1) <- 12;
    prog.(2) <- 2;;

let test_me =
   run_and_test test_program_0 final_test_program_0;
   run_and_test test_program_1 final_test_program_1;
   run_and_test test_program_2 final_test_program_2;
   run_and_test test_program_3 final_test_program_3;
   run_and_test test_program_4 final_test_program_4;;

let stdin_to_program () = 
    let line_opt = Stdio.In_channel.input_line In_channel.stdin in
        match line_opt with
        | None -> raise (Invalid_argument "No line to read from stdin.")
        | Some(line) -> Array.map (Array.of_list (String.split_on_chars line ~on:[','])) ~f:Int.of_string;;

let () =
    test_me;
    let prog = stdin_to_program () in
        restore_working_state prog;
        run_program prog 0;
        print_program prog;;
