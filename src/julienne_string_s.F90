! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
#include "assert_macros.h"

submodule(julienne_string_m) julienne_string_s
  use assert_m
  implicit none

  integer, parameter :: integer_width_supremum = 11, default_real_width_supremum = 20, double_precision_width_supremum = 25
  integer, parameter :: logical_width=2, comma_width = 1, parenthesis_width = 1, space=1
  
contains

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

  module procedure from_logical_c_bool
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
