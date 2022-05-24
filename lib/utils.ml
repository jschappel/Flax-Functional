module Todo = struct
  let unimplimented _ = failwith "unimplimented"
end

module Branch = struct
  let unreachable _ = failwith "Unreachable code block"
end
