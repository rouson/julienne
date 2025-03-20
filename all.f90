  implicit none

  type string_t
  end type

  type test_description_t
    type(string_t), allocatable :: description_
    procedure(diagnoses), pointer, nopass :: diagnosis_function_
  end type

  associate(test_description => construct_from_strings(string_t(), diagnoses))
  end associate

contains

  function diagnoses()
    logical, allocatable :: diagnoses
    diagnoses = .true.
  end function

  function construct_from_strings(description, diagnosis_function) result(test_description)
    type(string_t), intent(in) :: description
    procedure(diagnoses), intent(in), pointer :: diagnosis_function
    type(test_description_t) test_description
    test_description%description_ = description
    test_description%diagnosis_function_ => diagnosis_function
  end function

end
