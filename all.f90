  implicit none

  type string_t
  end type

  type vector_test_description_t
    type(string_t), allocatable :: description_
    procedure(diagnoses), pointer, nopass :: vector_diagnosis_function_
  end type

  associate(vector_test_description => construct_from_strings(string_t(), diagnoses))
  end associate

contains

  function diagnoses()
    logical, allocatable :: diagnoses(:)
    diagnoses = [logical::]
  end function

  function construct_from_strings(description, vector_diagnosis_function) result(vector_test_description)
    type(string_t), intent(in) :: description
    procedure(diagnoses), intent(in), pointer :: vector_diagnosis_function
    type(vector_test_description_t) vector_test_description
    vector_test_description%description_ = description
    vector_test_description%vector_diagnosis_function_ => vector_diagnosis_function
  end function

end
