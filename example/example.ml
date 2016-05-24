type file =
  { filename: string;
    opened: bool;
    permissions: int; } [@@ to_prod]

let file_to_string file = file.filename
