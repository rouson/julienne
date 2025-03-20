! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_string_m
  use assert_m, only : characterizable_t
  use iso_c_binding, only : c_bool
  implicit none
  
  private
  public :: string_t
  public :: array_of_strings

  type, extends(characterizable_t) :: string_t
    private
    character(len=:), allocatable :: string_
  contains
    procedure :: as_character
    generic :: string => as_character
    procedure :: is_allocated
    procedure :: bracket
    generic :: operator(//)   => string_t_cat_string_t, string_t_cat_character, character_cat_string_t
    procedure, private :: string_t_cat_string_t, string_t_cat_character
    procedure, private, pass(rhs) :: character_cat_string_t
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

    elemental module function bracket(self, opening, closing) result(bracketed_self)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in), optional :: opening, closing
      type(string_t) bracketed_self
    end function

  end interface
  
end module julienne_string_m
