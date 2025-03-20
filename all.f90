  implicit none

  type test_description_t
    character(len=:), allocatable :: description_
    procedure(diagnosis), pointer, nopass :: diagnosis_function_
  end type

  associate(test_description => test_description_t("",diagnosis))
  end associate

contains

  function diagnosis()
    logical, allocatable :: diagnosis
    diagnosis = .true.
  end function

end
