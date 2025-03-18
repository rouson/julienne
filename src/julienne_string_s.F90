submodule(julienne_string_m) julienne_string_s
  implicit none

contains

  module procedure from_characters
    new_string%string_ = string
  end procedure

  module procedure string_t_eq_string_t
    lhs_eq_rhs = lhs%string_ == rhs%string_
  end procedure
   
  module procedure bracket
  
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

  end procedure
   
end submodule julienne_string_s
