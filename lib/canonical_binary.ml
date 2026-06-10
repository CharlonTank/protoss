let magic = "PROTOSS-PTB\000\001"

let uint32_be n =
  if n < 0 || n > 0x3fffffff then Kernel.fail "canonical binary payload too large";
  String.init 4 (function
    | 0 -> Char.chr ((n lsr 24) land 0xff)
    | 1 -> Char.chr ((n lsr 16) land 0xff)
    | 2 -> Char.chr ((n lsr 8) land 0xff)
    | _ -> Char.chr (n land 0xff))

let read_uint32_be input offset =
  (Char.code input.[offset] lsl 24)
  lor (Char.code input.[offset + 1] lsl 16)
  lor (Char.code input.[offset + 2] lsl 8)
  lor Char.code input.[offset + 3]

let encode_canonical canonical = magic ^ uint32_be (String.length canonical) ^ canonical

let decode_canonical input =
  let magic_len = String.length magic in
  let len = String.length input in
  if len < magic_len + 4 then Kernel.fail "canonical binary too short";
  if not (String.equal (String.sub input 0 magic_len) magic) then
    Kernel.fail "canonical binary magic/version mismatch";
  let payload_len = read_uint32_be input magic_len in
  let payload_start = magic_len + 4 in
  if len <> payload_start + payload_len then
    Kernel.fail "canonical binary payload length mismatch";
  String.sub input payload_start payload_len

let checked_to_binary checked =
  encode_canonical (Kernel.serialize_checked_program checked)

let checked_of_binary input =
  let canonical = decode_canonical input in
  let caps, defs = Kernel.parse_serialized_program canonical in
  Kernel.checked_of_canonical caps defs
