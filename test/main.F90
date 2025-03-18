  use vector_test_description_test_m ,only : vector_test_description_test_t
  implicit none

  integer :: passes=0, tests=0

  type(vector_test_description_test_t) vector_test_description_test

  call vector_test_description_test%report(passes,tests)

end
