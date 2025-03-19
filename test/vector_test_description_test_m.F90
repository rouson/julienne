! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module vector_test_description_test_m
  !! Verify test_description_t object behavior
  use julienne_m, only : &
     diagnosis_function_i &
    ,string_t &
    ,test_result_t &
    ,test_description_t &
    ,test_description_substring &
    ,test_diagnosis_t &
    ,test_t &
    ,vector_test_description_t
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    use julienne_vector_test_description_m, only : vector_diagnosis_function_i
#endif
#ifdef __GFORTRAN__
    use julienne_vector_test_description_m, only : run
#endif
  implicit none

  private
  public :: vector_test_description_test_t

  type vector_test_description_test_t
  contains
    procedure, nopass :: results
  end type

contains

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:), results_with_matches(:)
    type(vector_test_description_t), allocatable :: matching_vector_tests(:), vector_test_descriptions(:)
    logical, allocatable :: substring_in_description_vector(:)
    integer i

    print '(a)',"  skips  on testing vector_test_description_t due to a compiler bug "

    vector_test_descriptions = [vector_test_description_t([string_t(""),string_t("")], check_substring_search)]
    substring_in_description_vector = &
      [(any(vector_test_descriptions(i)%contains_text(test_description_substring)), i=1,size(vector_test_descriptions))]
    matching_vector_tests = pack(vector_test_descriptions, substring_in_description_vector)
    results_with_matches = [(run(matching_vector_tests(i)), i=1,size(matching_vector_tests))]
    test_results = pack(results_with_matches, results_with_matches%description_contains(test_description_substring))
  end function

  function check_substring_search() result(diagnoses)
    type(test_diagnosis_t), allocatable :: diagnoses(:)
    procedure(diagnosis_function_i), pointer :: unused

    unused => null()

    associate(doing_something => test_description_t("doing something", unused))
      diagnoses = [ &
         test_diagnosis_t(test_passed =       doing_something%contains_text("something"),    diagnostics_string="expected .true.") &
        ,test_diagnosis_t(test_passed = .not. doing_something%contains_text("missing text"), diagnostics_string="expected .true.") &
      ]
    end associate
  end function

end module vector_test_description_test_m
