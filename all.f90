  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  end type

  print *, true(string_t("["), [new_string()])

contains

  type(string_t) function new_string()
    new_string%string_ = "["
  end function

  logical elemental function true(lhs, rhs)
    type(string_t), intent(in) :: lhs
    class(string_t), intent(in) :: rhs
    true = .true.
  end function

end
