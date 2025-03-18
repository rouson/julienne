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
       test_description_t("is_allocated() result .true. if & only if the string_t component(s) is/are allocated", check_allocation)&
      ,test_description_t('assigning a string_t object to a character variable',                     assigns_string_t_to_character)&
      ,test_description_t('assigning a character variable to a string_t object',                     assigns_character_to_string_t)&
      ,test_description_t('constructing from a default integer',                                   constructs_from_default_integer)&
      ,test_description_t('constructing from a default real value',                                   constructs_from_default_real)&
      ,test_description_t('constructing from a double-precision value',                           constructs_from_double_precision)&
      ,test_description_t('constructing from a default-precision complex value',                   constructs_from_default_complex)&
      ,test_description_t('constructing from a default-kind logical value',                        constructs_from_default_logical)&
      ,test_description_t('constructing from a logical(c_bool) value',                              constructs_from_logical_c_bool)&
      ,test_description_t('constructing bracketed strings',                                                       brackets_strings)&
      ,test_description_t('constructing (comma-)separated values from character or string_t arrays',   constructs_separated_values)&
      ,test_description_t('constructing from a double-precision complex value',           constructs_from_double_precision_complex)&
    ]
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(diagnosis_function_i), pointer :: &
       check_allocation_ptr                     &
      ,assigns_string_t_to_character_ptr        &
      ,assigns_character_to_string_t_ptr        &
      ,constructs_from_default_integer_ptr      &
      ,constructs_from_default_real_ptr         &
      ,constructs_from_double_precision_ptr     &
      ,constructs_from_default_complex_ptr      &
      ,constructs_from_default_logical_ptr      &
      ,constructs_from_logical_c_bool_ptr       &
      ,brackets_strings_ptr                     &
      ,constructs_separated_values_ptr          &
      ,constructs_from_double_precision_complex_ptr

      check_allocation_ptr                         => check_allocation
      assigns_string_t_to_character_ptr            => assigns_string_t_to_character
      assigns_character_to_string_t_ptr            => assigns_character_to_string_t
      constructs_from_default_integer_ptr          => constructs_from_default_integer
      constructs_from_default_real_ptr             => constructs_from_default_real
      constructs_from_double_precision_ptr         => constructs_from_double_precision       
      constructs_from_default_complex_ptr          => constructs_from_default_complex
      constructs_from_default_logical_ptr          => constructs_from_default_logical
      constructs_from_logical_c_bool_ptr           => constructs_from_logical_c_bool
      brackets_strings_ptr                         => brackets_strings
      constructs_separated_values_ptr              => constructs_separated_values
      constructs_from_double_precision_complex_ptr => constructs_from_double_precision_complex

    test_descriptions = [ &
       test_description_t("is_allocated() result .true. if & only if the string_t component(s) is/are allocated", check_allocation_ptr)&
      ,test_description_t('assigning a string_t object to a character variable',                     assigns_string_t_to_character_ptr)&
      ,test_description_t('assigning a character variable to a string_t object',                     assigns_character_to_string_t_ptr)&
      ,test_description_t('constructing from a default integer',                                   constructs_from_default_integer_ptr)&
      ,test_description_t('constructing from a default real value',                                   constructs_from_default_real_ptr)&
      ,test_description_t('constructing from a double-precision value',                           constructs_from_double_precision_ptr)&
      ,test_description_t('constructing from a default-precision complex value',                   constructs_from_default_complex_ptr)&
      ,test_description_t('constructing from a default-kind logical value',                        constructs_from_default_logical_ptr)&
      ,test_description_t('constructing from a logical(c_bool) value',                              constructs_from_logical_c_bool_ptr)&
      ,test_description_t('constructing bracketed strings',                                                       brackets_strings_ptr)&
      ,test_description_t('constructing (comma-)separated values from character or string_t arrays',   constructs_separated_values_ptr)&
      ,test_description_t('constructing from a double-precision complex value',           constructs_from_double_precision_complex_ptr)&
    ]
#endif
    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0 .or. &
      test_descriptions%contains_text(string_t(test_description_substring)))
    test_results = test_descriptions%run()
  end function

  pure function check_allocation() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(string_t) :: scalar_not_allocated, scalar_allocated, array_allocated(2), array_not_allocated(2)

    scalar_allocated = string_t("")
    array_allocated = [string_t("yada yada"), string_t("blah blah blah")]

    associate(not_any_allocated => .not. any([scalar_not_allocated%is_allocated(), array_not_allocated%is_allocated()]))
      associate(all_allocated => all([scalar_allocated%is_allocated(), array_allocated%is_allocated()]))
        test_diagnosis = test_diagnosis_t( &
           test_passed = not_any_allocated .and. all_allocated &
          ,diagnostics_string = "expected .true., true.; actual " // string_t(not_any_allocated) // string_t(all_allocated) &
        )
      end associate 
    end associate 
  end function

  function assigns_string_t_to_character() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    character(len=:), allocatable :: lhs

    associate(rhs => string_t("ya don't say"))
      lhs = rhs
      test_diagnosis = test_diagnosis_t( &
         test_passed = lhs == rhs &
        ,diagnostics_string = "expected lhs == rhs; actual lhs = " // lhs // ", rhs = " // rhs &
      )
    end associate
  end function

  function assigns_character_to_string_t() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    character(len=*), parameter :: rhs = "well, alrighty then"
    type(string_t) lhs

    lhs = rhs
    test_diagnosis = test_diagnosis_t( &
       test_passed = lhs == rhs &
      ,diagnostics_string = "expected lhs == rhs; actual lhs = " // lhs // ", rhs = " // rhs &
    )
  end function

  function constructs_from_default_integer() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_value = 1234567890

    associate(string => string_t(expected_value))
      test_diagnosis = test_diagnosis_t( &
         test_passed = adjustl(trim(string%string())) == "1234567890" &
        ,diagnostics_string = "expected '"// string_t(expected_value) // "', actual " // string%string() &
      )
    end associate
  end function

  function constructs_from_default_real() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: real_value = -1./1024. ! use a negative power of 2 for an exactly representable rational number
    real read_value
    character(len=:), allocatable :: character_representation

    associate(string => string_t(real_value))
      character_representation = string%string()
      read(character_representation, *) read_value
      test_diagnosis = test_diagnosis_t( &
         test_passed = read_value == real_value &
        ,diagnostics_string = "expected '"// string_t(real_value) // "', actual " // string_t(read_value) &
      )
    end associate
  end function

  function constructs_from_double_precision() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: double_precision_value = -1D0/1024D0 ! use a negative power of 2 for an exactly representable rational number
    real read_value
    character(len=:), allocatable :: character_representation

    associate(string => string_t(double_precision_value))
      character_representation = string%string()
      read(character_representation, *) read_value
      test_diagnosis = test_diagnosis_t( &
         test_passed = read_value == double_precision_value &
        ,diagnostics_string = "expected '"// string_t(double_precision_value) // "', actual " // string_t(read_value) &
      )
    end associate
  end function

  function constructs_from_default_complex() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: real_value = -1./1024. ! use a negative power of 2 for an exactly representable rational number
    real, parameter :: tolerance = 1E-08
    complex, parameter :: z = (real_value, real_value)
    complex read_value
    character(len=:), allocatable :: character_representation

    associate(string => string_t(z))
      character_representation = string%string()
      read(character_representation, *) read_value
      test_diagnosis = test_diagnosis_t( &
         test_passed = abs(read_value - z) < tolerance &
        ,diagnostics_string = "expected '"// string_t(z) // "', actual " // string_t(read_value) &
      )
    end associate
  end function

  function constructs_from_double_precision_complex() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: double_precision_value = -1D0/1024D0 ! use a negative power of 2 for an exactly representable rational number
    double precision, parameter :: tolerance = 1E-16
    complex(kind(1D0)), parameter :: z = (double_precision_value, double_precision_value)
    complex(kind(1D0)) read_value
    character(len=:), allocatable :: character_representation

    associate(string => string_t(z))
      character_representation = string%string()
      read(character_representation, *) read_value
      test_diagnosis = test_diagnosis_t( &
         test_passed = abs(read_value - z) < tolerance &
        ,diagnostics_string = "expected '"// string_t(z) // "', actual " // string_t(read_value) &
      )
    end associate
  end function

  function constructs_from_default_logical() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(true => string_t(.true.), false => string_t(.false.))
      test_diagnosis = test_diagnosis_t( &
         test_passed = all([true%string() == "T", false%string() == "F"]) &
        ,diagnostics_string = "expected T, F; actual '"// true%string() // ", " // false%string() &
      )
    end associate
  end function

  function constructs_from_logical_c_bool() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(true => string_t(.true._c_bool), false => string_t(.false._c_bool))
      test_diagnosis = test_diagnosis_t( &
         test_passed = true%string() == "T" .and. false%string() == "F" &
        ,diagnostics_string = "expected T, F; actual '"// true%string() // ", " // false%string() &
      )
    end associate
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

  function constructs_separated_values() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    test_diagnosis = test_diagnosis_t( &
      test_passed = &
              "a,bc,def" == .csv. [string_t("a"), string_t("bc"), string_t("def")]    &
        .and. "abc,def"  == .csv. ["abc", "def"]                                      &
        .and. "do|re|mi" == (string_t(["do", "re", "mi"])         .sv.          "|" ) &
        .and. "dore|mi"  == (([string_t("dore"), string_t("mi")]) .sv. string_t("|")) &
        .and. "do|re|mi" == (         ["do", "re", "mi"]          .sv.          "|" ) &
        .and. "do|re|mi" == (         ["do", "re", "mi"]          .sv. string_t("|")) &
      ,diagnostics_string = "" &
    )
  end function

end module string_test_m
