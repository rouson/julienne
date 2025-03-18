  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  end type

  print *, always_true([some_string()])

contains

  type(string_t) function some_string()
    some_string%string_ = ""
  end function

  logical elemental function always_true(rhs)
    class(string_t), intent(in) :: rhs
    always_true = .true.
  end function

end
