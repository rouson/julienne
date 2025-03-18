  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  end type

  type(string_t) array(1)

  array(1)%string_ = "do"
  print *, string_t_eq_string_t(bracket(array), [from_characters("[do]")])

contains

  function from_characters(string) result(new_string)
    character(len=*), intent(in) :: string
    type(string_t) new_string
    new_string%string_ = string
  end function

  elemental function string_t_eq_string_t(lhs, rhs) result(lhs_eq_rhs)
    class(string_t), intent(in) :: lhs, rhs
    logical lhs_eq_rhs
    lhs_eq_rhs = lhs%string_ == rhs%string_
  end function

  elemental function bracket(self) result(bracketed_self)
    type(string_t), intent(in) :: self
    type(string_t) bracketed_self
    bracketed_self = string_t("[" // self%string_ // "]")
  end function

end

