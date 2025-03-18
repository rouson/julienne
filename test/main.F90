  use julienne_string_m, only : string_t
  implicit none
  type(string_t) array(1)
  array = string_t(["do"])
  print *, array%bracket() == [string_t("[do]")]
end
