module julienne_string_m
  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  contains
    procedure :: bracket
  end type

  interface

    elemental module function from_characters(string) result(new_string)
      implicit none
      character(len=*), intent(in) :: string
      type(string_t) new_string
    end function

    elemental module function string_t_eq_string_t(lhs, rhs) result(lhs_eq_rhs)
      implicit none
      class(string_t), intent(in) :: lhs, rhs
      logical lhs_eq_rhs
    end function

    elemental module function bracket(self, opening, closing) result(bracketed_self)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in), optional :: opening, closing
      type(string_t) bracketed_self
    end function

  end interface
  
end module julienne_string_m
