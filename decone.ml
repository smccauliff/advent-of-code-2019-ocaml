open Base;;
open Stdio;;

let fuel_requirement module_mass = module_mass / 3  - 2;;

let rec fuel_fuel_requirement total_fuel last_fuel =
  let next_fuel = fuel_requirement last_fuel in
  if (next_fuel <= 0) then
    total_fuel
  else
    fuel_fuel_requirement (total_fuel + next_fuel) next_fuel;;
  

let module_fuel_requirement () =
  In_channel.fold_lines In_channel.stdin ~init:0 ~f:(fun total_fuel line ->
    let module_mass = Int.of_string line in
    let module_fuel = fuel_requirement module_mass in
    (*printf "%d %d\n" module_mass module_fuel *)
    total_fuel + fuel_fuel_requirement module_fuel module_fuel
  );;


  
let () =
  let total_fuel = (module_fuel_requirement ()) in
  printf "%d\n" total_fuel;;
