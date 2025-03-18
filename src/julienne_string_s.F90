submodule(julienne_string_m) julienne_string_s
  implicit none

  integer, parameter :: integer_width_supremum = 11, default_real_width_supremum = 20, double_precision_width_supremum = 25
  integer, parameter :: logical_width=2, comma_width = 1, parenthesis_width = 1, space=1
  
contains

  module procedure as_character
    raw_string = self%string_
  end procedure

  module procedure from_characters
    new_string%string_ = string
  end procedure

  module procedure string_t_eq_string_t
    lhs_eq_rhs = lhs%string() == rhs%string()
  end procedure
   
  module procedure string_t_eq_character
    lhs_eq_rhs = lhs%string() == rhs
  end procedure

  module procedure character_eq_string_t
    lhs_eq_rhs = lhs == rhs%string()
  end procedure
   
  module procedure assign_character_to_string_t
    lhs%string_ = rhs
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
