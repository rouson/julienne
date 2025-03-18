  use string_test_m ,only : string_test_t
  implicit none
  type(string_test_t) string_test
  integer :: passes=0, tests=0
  call string_test%report(passes, tests)
end 
