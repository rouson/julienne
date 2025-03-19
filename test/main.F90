  use vector_test_description_test_m ,only : vector_test_description_test_t
  implicit none
  type(vector_test_description_test_t) vector_test_description_test
  associate(results => vector_test_description_test%results())
  end associate
end
