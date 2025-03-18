  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  end type

  type(string_t) array(1)

  array(1)%string_ = "do"
  print *, equals(bracket(array), [new_string("[do]")])

contains

  type(string_t) function new_string(string)
    character(len=*) string
    new_string%string_ = string
  end function

  logical elemental function equals(lhs, rhs)
    class(string_t), intent(in) :: lhs, rhs
    equals = lhs%string_ == rhs%string_
  end function

  type(string_t) elemental function bracket(self)
    type(string_t), intent(in) :: self
    bracket%string_ = "[" // self%string_ // "]"
  end function

end
