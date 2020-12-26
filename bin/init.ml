open Core

let init dir theme =
  let home = match Sys.getenv("HOME") with | Some a -> a | None -> "" in
  let theme_dir =  (home^"/.config/alembic/themes/"^theme) in
  if phys_equal (Sys.is_directory theme_dir) `Yes then
    FileUtil.cp ~recurse:true [theme_dir] ("./"^dir)
  else
    Printf.printf "Theme \"%s\" does not exists!\n\r" theme