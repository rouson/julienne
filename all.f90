module julienne_string_m
  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  end type

contains

  elemental function from_characters(string) result(new_string)
    character(len=*), intent(in) :: string
    type(string_t) new_string
    new_string%string_ = string
  end function

  elemental function string_t_eq_string_t(lhs, rhs) result(lhs_eq_rhs)
    class(string_t), intent(in) :: lhs, rhs
    logical lhs_eq_rhs
    lhs_eq_rhs = lhs%string_ == rhs%string_
  end function

  elemental function bracket(self, opening, closing) result(bracketed_self)
    class(string_t), intent(in) :: self
    character(len=*), intent(in), optional :: opening, closing
    type(string_t) bracketed_self
  
    character(len=:), allocatable :: actual_opening, actual_closing

    associate(opening_present => present(opening))

      if (opening_present) then
        actual_opening = opening
      else
        actual_opening = "[" 
      end if

      if (present(closing)) then
        actual_closing = closing
      else if(opening_present) then
        actual_closing = actual_opening
      else
        actual_closing = "]" 
      end if

    end associate

    bracketed_self = string_t(actual_opening // self%string_ // actual_closing)

  end function

end module
  use julienne_string_m
  implicit none
  type(string_t) array(1)
  array(1)%string_ = "do"
  print *, string_t_eq_string_t(bracket(array), [from_characters("[do]")])
end
