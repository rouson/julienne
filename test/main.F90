  use julienne_string_m
  implicit none
  type(string_t) array(1)
  array(1)%string_ = "do"
  print *, string_t_eq_string_t(array%bracket() , [from_characters("[do]")])
end
