open Base

(* https://adventofcode.com/2019/day/3 *)

let test1_path1 = "R8,U5,L5,D3";;
let test1_path2 = "U7,R6,D4,L4";;

(* moves the path of the wire relative to the previous coordinate *)
let execute_command prev_coordinate c =
    let x, y = prev_coordinate in
    let magnitude = Int.of_string (String.sub c ~pos:1 ~len:((String.length c) - 1)) in
    match (String.sub c ~pos:0 ~len:1) with
    | "U" -> (x, y + magnitude)
    | "D" -> (x, y - magnitude)
    | "L" -> (x - magnitude, y)
    | "R" -> (x + magnitude, y)
    | _   -> raise (Invalid_argument "Bad command");;

let manhatten_distance (x1, y1) (x2, y2) =
    abs(x1 - x2) + abs(y1 - y2);;

(* l is the list of commands U, D, L, R *)
let convert_to_cartesian l =
    List.fold_left l ~init:[(0, 0)] ~f:(fun cartesian_list relative_command -> 
        match List.hd cartesian_list with
        | None -> raise (Invalid_argument "Empty list")
        | Some(prev_coordinate) -> execute_command prev_coordinate relative_command :: cartesian_list
    );;

(* This is a super stupid algorithm, but it does not really have any corner cases. *)
let points_in_line_segment m start endd f=
    let x1, y1 = start in 
    let x2, y2 = endd in
    if x1 != x2 then (* horizonal line *)
        for x = x1 to x2 do
            f m x y1
        done
    else 
        for y = y1 to y2 do
            f m x1 y
        done
    ;;

let rec render_points_in_wire m wire_path =
    match wire_path with
    | [] -> ()
    | head :: tail -> match (List.hd tail) with 
        | None -> ()
        | Some(next) ->
            points_in_line_segment m head next (fun m x y -> Hashtbl.set m ~key:(x,y) ~data:1);
            render_points_in_wire m tail;;




let () =
    let path_commands = String.split_on_chars test1_path1 ~on:[','] in
    let wire_path = convert_to_cartesian path_commands in
    let my_table = Hashtbl.create in
        Hashtbl.set my_table ~key:(0, 0) ~data:1;
        render_points_in_wire my_table wire_path;;
   (*| _ -> ();; (* I don't know how else I can convert the return type type to unit *) *)