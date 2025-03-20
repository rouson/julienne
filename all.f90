  implicit none

  type test_description_t
    character(len=:), allocatable :: description_
    procedure(diagnosis), pointer, nopass :: diagnosis_function_
  end type

  associate(test_description => test_description(diagnosis))
  end associate

contains

  function diagnosis()
    logical, allocatable :: diagnosis
    diagnosis = .true.
  end function

  type(test_description_t) function test_description(diagnosis_function)
    procedure(diagnosis), pointer :: diagnosis_function
    test_description%description_ = ""
    test_description%diagnosis_function_ => diagnosis_function
  end function

end
