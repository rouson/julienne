  implicit none

  type string_t
  end type

  type test_description_t
    type(string_t), allocatable :: description_
    procedure(diagnoses), pointer, nopass :: diagnosis_function_
  end type

  associate(test_description => test_description(string_t(), diagnoses))
  end associate

contains

  function diagnoses()
    logical, allocatable :: diagnoses
    diagnoses = .true.
  end function

  type(test_description_t) function test_description(description, diagnosis_function)
    type(string_t), intent(in) :: description
    procedure(diagnoses), intent(in), pointer :: diagnosis_function
    test_description%description_ = description
    test_description%diagnosis_function_ => diagnosis_function
  end function

end
