! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module string_test_m
  use assert_m, only : assert
  use iso_c_binding, only : c_bool

  use julienne_test_m, only : test_t, test_description_substring
  use julienne_test_result_m, only : test_result_t
  use julienne_test_description_m, only : test_description_t, diagnosis_function_i
  use julienne_test_diagnosis_m, only : test_diagnosis_t
  use julienne_string_m, only : string_t, operator(.cat.) , operator(.csv.) , operator(.sv.)
  implicit none

  private
  public :: string_test_t

  type, extends(test_t) :: string_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The string_t type"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ &
       test_description_t('constructing bracketed strings', brackets_strings)&
    ]
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(diagnosis_function_i), pointer :: brackets_strings_ptr
    brackets_strings_ptr => brackets_strings

    test_descriptions = [ &
       test_description_t('constructing bracketed strings', brackets_strings_ptr)&
    ]
#endif
    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0 .or. &
      test_descriptions%contains_text(string_t(test_description_substring)))
    test_results = test_descriptions%run()
  end function

  function brackets_strings() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(scalar => string_t("do re mi"))
       
#ifndef __GFORTRAN__
      associate(array  => string_t(["do", "re", "mi"]))
#else
      block
        type(string_t), allocatable :: array(:)
        array = string_t(["do", "re", "mi"])
#endif
      test_diagnosis = test_diagnosis_t( &
        test_passed = scalar%bracket()        == string_t("[do re mi]")                                  &
             .and. all(array%bracket()        == [string_t("[do]"), string_t("[re]"), string_t("[mi]")]) &
             .and. all(array%bracket('"')     == [string_t('"do"'), string_t('"re"'), string_t('"mi"')]) &
             .and. all(array%bracket("{","}") == [string_t('{do}'), string_t('{re}'), string_t('{mi}')]) &
        ,diagnostics_string = "" &
      )
#ifndef __GFORTRAN__
      end associate
#else
      end block
#endif
    end associate
  end function

end module string_test_m
