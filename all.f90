  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  end type

  print *, equals(bracket(), [new_string()])

contains

  type(string_t) function new_string()
    new_string%string_ = "["
  end function

  logical elemental function equals(lhs, rhs)
    type(string_t), intent(in) :: lhs
    class(string_t), intent(in) :: rhs
    equals = lhs%string_ == rhs%string_
  end function

  type(string_t) function bracket()
    bracket%string_ = "["
  end function

end
