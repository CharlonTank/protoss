let byte s i = Char.code (String.get s i)

let is_cont b = b >= 0x80 && b <= 0xBF

let char_width s i =
  let len = String.length s in
  let b0 = byte s i in
  if b0 < 0x80 then 1
  else if b0 >= 0xC2 && b0 <= 0xDF && i + 1 < len && is_cont (byte s (i + 1)) then
    2
  else if b0 >= 0xE0 && b0 <= 0xEF && i + 2 < len && is_cont (byte s (i + 1))
          && is_cont (byte s (i + 2))
  then 3
  else if b0 >= 0xF0 && b0 <= 0xF4 && i + 3 < len && is_cont (byte s (i + 1))
          && is_cont (byte s (i + 2)) && is_cont (byte s (i + 3))
  then 4
  else 1

let length s =
  let len = String.length s in
  let rec loop i count =
    if i >= len then count else loop (i + char_width s i) (count + 1)
  in
  loop 0 0

let byte_offset s index =
  let len = String.length s in
  let rec loop i count =
    if i >= len || count >= index then i else loop (i + char_width s i) (count + 1)
  in
  loop 0 0

let slice s start count =
  if count <= 0 then ""
  else
    let start_offset = byte_offset s start in
    let suffix = String.sub s start_offset (String.length s - start_offset) in
    let end_offset = start_offset + byte_offset suffix count in
    String.sub s start_offset (end_offset - start_offset)
