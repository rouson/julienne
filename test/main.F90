  use julienne_m,only : vector_test_description_t, string_t, test_diagnosis_t
  implicit none

  associate(vector_test_description => vector_test_description_t([string_t::], diagnoses))
  end associate

contains

   function diagnoses()
    type(test_diagnosis_t), allocatable :: diagnoses(:)
    diagnoses = [test_diagnosis_t::] 
  end function

end
