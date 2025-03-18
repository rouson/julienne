  use julienne_string_m, only : string_t
  implicit none
  type(string_t), allocatable :: array(:)
  array = string_t(["do", "re", "mi"])
  print *, array%bracket()        == [string_t("[do]"), string_t("[re]"), string_t("[mi]")]
end
