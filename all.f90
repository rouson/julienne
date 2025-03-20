  implicit none

  type test_description_t
    character(len=:), allocatable :: description_
    procedure(diagnoses), pointer, nopass :: diagnosis_function_
  end type

  associate(test_description => test_description("", diagnoses))
  end associate

contains

  function diagnoses()
    logical, allocatable :: diagnoses
    diagnoses = .true.
  end function

  type(test_description_t) function test_description(description, diagnosis_function)
    character(len=*) description
    procedure(diagnoses), pointer :: diagnosis_function
    test_description%description_ = description
    test_description%diagnosis_function_ => diagnosis_function
  end function

end
