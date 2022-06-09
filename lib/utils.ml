module Todo = struct
  let unimplimented _ = failwith "unimplimented"
end

module Debug = struct
  let rec print_list printer = function
    | [] -> ()
    | x :: xs ->
        print_endline (printer x);
        print_list printer xs
end

module Branch = struct
  let unreachable _ = failwith "Unreachable code block"
end
