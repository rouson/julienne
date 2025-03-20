  implicit none

  type string_t
  end type

  type vector_test_description_t
    type(string_t), allocatable :: descriptions_(:)
    procedure(diagnoses), pointer, nopass :: vector_diagnosis_function_
  end type

  associate(vector_test_description => construct_from_strings([string_t::], diagnoses))
  end associate

contains

  function diagnoses()
    logical, allocatable :: diagnoses(:)
    diagnoses = [logical::]
  end function

  function construct_from_strings(descriptions, vector_diagnosis_function) result(vector_test_description)
    type(string_t), intent(in) :: descriptions(:)
    procedure(diagnoses), intent(in), pointer :: vector_diagnosis_function
    type(vector_test_description_t) vector_test_description
    vector_test_description%descriptions_ = descriptions
    vector_test_description%vector_diagnosis_function_ => vector_diagnosis_function
  end function

end
