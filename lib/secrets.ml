let format = "protoss-sealed-secret-v1"

let handle_ref ~scope ~typ ~handle =
  Kernel.hash_string
    ("protoss-secret-handle-v1\nscope=" ^ scope ^ "\ntype=" ^ Kernel.type_to_canonical typ
   ^ "\nhandle=" ^ handle ^ "\n")

let seal_json ~scope ~typ ~handle ~value:_ =
  Kernel.json_obj
    [
      Kernel.json_field "format" (Kernel.json_string format);
      Kernel.json_field "scope" (Kernel.json_string scope);
      Kernel.json_field "type" (Kernel.json_string (Ast.string_of_typ typ));
      Kernel.json_field "handleRef" (Kernel.json_string (handle_ref ~scope ~typ ~handle));
      Kernel.json_field "valueHashed" (Kernel.json_bool false);
      Kernel.json_field "valueStored" (Kernel.json_bool false);
    ]
  ^ "\n"
