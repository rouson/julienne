submodule(julienne_string_m) julienne_string_s
  implicit none

  integer, parameter :: integer_width_supremum = 11, default_real_width_supremum = 20, double_precision_width_supremum = 25
  integer, parameter :: logical_width=2, comma_width = 1, parenthesis_width = 1, space=1
  
contains

  module procedure as_character
    raw_string = self%string_
  end procedure

  module procedure is_allocated
    string_allocated = allocated(self%string_)
  end procedure

  module procedure from_characters
    new_string%string_ = string
  end procedure

  module procedure from_default_integer
    allocate(character(len=integer_width_supremum) :: string%string_)
    write(string%string_, '(g0)') i
    string%string_ = trim(adjustl(string%string_))
  end procedure

  module procedure from_default_real
    allocate(character(len=double_precision_width_supremum) :: string%string_)
    write(string%string_, '(g20.13)') x
    string%string_ = trim(adjustl(string%string_))
  end procedure

  module procedure from_double_precision
    allocate(character(len=double_precision_width_supremum) :: string%string_)
    write(string%string_, '(g20.13)') x
    string%string_ = trim(adjustl(string%string_))
  end procedure

  module procedure from_default_logical
    allocate(character(len=logical_width) :: string%string_)
    write(string%string_, '(g0)') b
    string%string_ = trim(adjustl(string%string_))
  end procedure

  module procedure from_default_complex
    allocate(character(len=2*default_real_width_supremum + 2*parenthesis_width + comma_width) :: string%string_)
    write(string%string_, '("(",g20.13,",",g20.13,")")') z
    string%string_ = trim(adjustl(string%string_))
  end procedure

  module procedure from_double_precision_complex
    allocate(character(len=space + 2*double_precision_width_supremum + 2*parenthesis_width + comma_width) :: string%string_)
    write(string%string_, '("(",g20.13,",",g20.13,")")') z
    string%string_ = trim(adjustl(string%string_))
  end procedure

  module procedure concatenate_elements
    integer s 

    concatenated_strings = ""
    do s = 1, size(strings)
      concatenated_strings = concatenated_strings // strings(s)%string()
    end do
  end procedure

  module procedure strings_with_comma_separator
    csv = strings_with_string_t_separator(strings, string_t(","))
  end procedure 

  module procedure characters_with_comma_separator
    csv = strings_with_string_t_separator(string_t(strings), string_t(","))
  end procedure 

  module procedure characters_with_character_separator
    sv = strings_with_string_t_separator(string_t(strings), string_t(separator))
  end procedure 

  module procedure characters_with_string_separator
    sv = strings_with_string_t_separator(string_t(strings), separator)
  end procedure 

  module procedure strings_with_character_separator
    sv = strings_with_string_t_separator(strings, string_t(separator))
  end procedure 

  module procedure strings_with_string_t_separator

    integer s 

    associate(num_elements => size(strings))

      sv = ""

      do s = 1, num_elements - 1
        sv = sv // strings(s) // separator
      end do

      sv = sv // strings(num_elements)

    end associate

  end procedure

  module procedure array_of_strings
    character(len=:), allocatable :: remainder, next_string
    integer next_delimiter, string_end

    remainder = trim(adjustl(delimited_strings))
    allocate(strings_array(0))

    do  
      next_delimiter = index(remainder, delimiter)
      string_end = merge(len(remainder), next_delimiter-1, next_delimiter==0)
      next_string = trim(adjustl(remainder(:string_end)))
      if (len(next_string)==0) exit
      strings_array = [strings_array, string_t(next_string)]
      if (next_delimiter==0) then
        remainder = ""
      else
        remainder = trim(adjustl(remainder(next_delimiter+1:)))
      end if
    end do

  end procedure

  module procedure file_extension
    character(len=:), allocatable :: name_

    name_ = trim(adjustl(self%string()))

    associate( dot_location => index(name_, '.', back=.true.) )
      if (dot_location < len(name_)) then
        extension = trim(adjustl(name_(dot_location+1:)))
      else
        extension = ""
      end if
    end associate
  end procedure

  module procedure base_name
    character(len=:), allocatable :: name_

    name_ = self%string()
    
    associate(dot_location => index(name_, '.', back=.true.) )
      if (dot_location < len(name_)) then
        base = trim(adjustl(name_(1:dot_location-1)))
      else
        base = ""
      end if
    end associate
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
   
  module procedure string_t_ne_string_t
    lhs_ne_rhs = lhs%string() /= rhs%string()
  end procedure
   
  module procedure string_t_ne_character
    lhs_ne_rhs = lhs%string() /= rhs
  end procedure

  module procedure character_ne_string_t
    lhs_ne_rhs = lhs /= rhs%string()
  end procedure
   
  module procedure assign_string_t_to_character
    lhs = rhs%string()
  end procedure
   
  module procedure assign_character_to_string_t
    lhs%string_ = rhs
  end procedure

  module procedure string_t_cat_string_t
    lhs_cat_rhs = string_t(lhs%string_ // rhs%string_)
  end procedure
   
  module procedure string_t_cat_character
    lhs_cat_rhs = string_t(lhs%string_ // rhs)
  end procedure

  module procedure character_cat_string_t
    lhs_cat_rhs = string_t(lhs // rhs%string_)
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
