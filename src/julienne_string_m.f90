! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_string_m
  use assert_m, only : characterizable_t
  use iso_c_binding, only : c_bool
  implicit none
  
  private
  public :: string_t
  public :: array_of_strings
  public :: operator(.cat.) ! element-wise concatenation unary operator
  public :: operator(.csv.) ! comma-separated values unary operator
  public :: operator(.sv.)  ! separated-values binary operator

  type, extends(characterizable_t) :: string_t
    private
    character(len=:), allocatable :: string_
  contains
    procedure :: as_character
    generic :: string => as_character
    procedure :: is_allocated
    procedure :: file_extension
    procedure :: base_name
    procedure :: bracket
    generic :: operator(//)   => string_t_cat_string_t, string_t_cat_character, character_cat_string_t
    generic :: operator(/=)   => string_t_ne_string_t, string_t_ne_character, character_ne_string_t
    generic :: operator(==)   => string_t_eq_string_t, string_t_eq_character, character_eq_string_t
    generic :: assignment(= ) => assign_string_t_to_character, assign_character_to_string_t
    procedure, private :: string_t_ne_string_t, string_t_ne_character
    procedure, private :: string_t_eq_string_t, string_t_eq_character
    procedure, private :: assign_character_to_string_t
    procedure, private :: string_t_cat_string_t, string_t_cat_character
    procedure, private, pass(rhs) :: character_cat_string_t
    procedure, private, pass(rhs) :: character_ne_string_t
    procedure, private, pass(rhs) :: character_eq_string_t
    procedure, private, pass(rhs) :: assign_string_t_to_character
  end type

  interface string_t

    elemental module function from_characters(string) result(new_string)
      implicit none
      character(len=*), intent(in) :: string
      type(string_t) new_string
    end function

    elemental module function from_default_integer(i) result(string)
      implicit none
      integer, intent(in) :: i
      type(string_t) string
    end function

    elemental module function from_default_real(x) result(string)
      implicit none
      real, intent(in) :: x
      type(string_t) string
    end function

    elemental module function from_double_precision(x) result(string)
      implicit none
      double precision, intent(in) :: x
      type(string_t) string
    end function

    elemental module function from_default_logical(b) result(string)
      implicit none
      logical, intent(in) :: b
      type(string_t) string
    end function

    elemental module function from_logical_c_bool(b) result(string)
      implicit none
      logical(c_bool), intent(in) :: b
      type(string_t) string
    end function

    elemental module function from_default_complex(z) result(string)
      implicit none
      complex, intent(in) :: z
      type(string_t) string
    end function

    elemental module function from_double_precision_complex(z) result(string)
      implicit none
      complex(kind=kind(1D0)), intent(in) :: z
      type(string_t) string
    end function

  end interface

  interface operator(.cat.)

   pure  module function concatenate_elements(strings) result(concatenated_strings)
      implicit none
      type(string_t), intent(in) :: strings(:)
      type(string_t) concatenated_strings
    end function

  end interface

  interface operator(.csv.)

    pure module function strings_with_comma_separator(strings) result(csv)
      implicit none
      type(string_t), intent(in) :: strings(:)
      type(string_t) csv
    end function

    pure module function characters_with_comma_separator(strings) result(csv)
      implicit none
      character(len=*), intent(in) :: strings(:)
      type(string_t) csv
    end function

  end interface

  interface operator(.sv.)

    pure module function strings_with_character_separator(strings, separator) result(sv)
      implicit none
      type(string_t)  , intent(in) :: strings(:)
      character(len=*), intent(in) :: separator
      type(string_t) sv
    end function

    pure module function characters_with_character_separator(strings, separator) result(sv)
      implicit none
      character(len=*), intent(in) :: strings(:), separator
      type(string_t) sv
    end function

    pure module function characters_with_string_separator(strings, separator) result(sv)
      implicit none
      character(len=*), intent(in) :: strings(:)
      type(string_t)  , intent(in) :: separator
      type(string_t) sv
    end function

    pure module function strings_with_string_t_separator(strings, separator) result(sv)
      implicit none
      type(string_t), intent(in) :: strings(:), separator
      type(string_t) sv 
    end function

  end interface

  interface

    pure module function as_character(self) result(raw_string)
      implicit none
      class(string_t), intent(in) :: self
      character(len=:), allocatable :: raw_string
    end function

    pure module function array_of_strings(delimited_strings, delimiter) result(strings_array)
      implicit none
      character(len=*), intent(in) :: delimited_strings, delimiter
      type(string_t), allocatable :: strings_array(:)
    end function

    elemental module function is_allocated(self) result(string_allocated)
      implicit none
      class(string_t), intent(in) :: self
      logical string_allocated
    end function

    elemental module function file_extension(self) result(extension)
      !! result contains all characters in file_name after the last dot (.)
      class(string_t), intent(in) :: self
      type(string_t) extension
    end function

    pure module function base_name(self) result(base)
      !! result contains all characters in file_name before the last dot (.)
      class(string_t), intent(in) :: self
      type(string_t) base
    end function

    elemental module function string_t_eq_string_t(lhs, rhs) result(lhs_eq_rhs)
      implicit none
      class(string_t), intent(in) :: lhs, rhs
      logical lhs_eq_rhs
    end function

    elemental module function string_t_eq_character(lhs, rhs) result(lhs_eq_rhs)
      implicit none
      class(string_t), intent(in) :: lhs
      character(len=*), intent(in) :: rhs
      logical lhs_eq_rhs
    end function

    elemental module function character_eq_string_t(lhs, rhs) result(lhs_eq_rhs)
      implicit none
      class(string_t), intent(in) :: rhs
      character(len=*), intent(in) :: lhs
      logical lhs_eq_rhs
    end function

    elemental module function string_t_ne_string_t(lhs, rhs) result(lhs_ne_rhs)
      implicit none
      class(string_t), intent(in) :: lhs, rhs
      logical lhs_ne_rhs
    end function

    elemental module function string_t_ne_character(lhs, rhs) result(lhs_ne_rhs)
      implicit none
      class(string_t), intent(in) :: lhs
      character(len=*), intent(in) :: rhs
      logical lhs_ne_rhs
    end function

    elemental module function character_ne_string_t(lhs, rhs) result(lhs_ne_rhs)
      implicit none
      class(string_t), intent(in) :: rhs
      character(len=*), intent(in) :: lhs
      logical lhs_ne_rhs
    end function

    pure module function string_t_cat_string_t(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      class(string_t), intent(in) :: lhs, rhs
      type(string_t) lhs_cat_rhs
    end function

    pure module function string_t_cat_character(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      class(string_t), intent(in) :: lhs
      character(len=*), intent(in) :: rhs
      type(string_t) lhs_cat_rhs
    end function

    pure module function character_cat_string_t(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      character(len=*), intent(in) :: lhs
      class(string_t), intent(in) :: rhs
      type(string_t) lhs_cat_rhs
    end function

    pure module subroutine assign_character_to_string_t(lhs, rhs)
      implicit none
      class(string_t), intent(inout) :: lhs
      character(len=*), intent(in) :: rhs
    end subroutine

    pure module subroutine assign_string_t_to_character(lhs, rhs)
      implicit none
      class(string_t), intent(in) :: rhs
      character(len=:), intent(out), allocatable :: lhs
    end subroutine

    elemental module function bracket(self, opening, closing) result(bracketed_self)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in), optional :: opening, closing
      type(string_t) bracketed_self
    end function

  end interface
  
end module julienne_string_m
