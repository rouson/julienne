  use julienne_m,only : vector_test_description_t, string_t, test_diagnosis_t, diagnosis_function_i, test_description_t
  implicit none

  associate(vector_test_descriptions => [vector_test_description_t([string_t("")], check_substring_search)])
  end associate

contains

   function check_substring_search() result(diagnoses)
    type(test_diagnosis_t), allocatable :: diagnoses(:)
    type(test_description_t) some_test
    procedure(diagnosis_function_i), pointer :: unused

    unused => null()
    some_test = test_description_t("do something", unused)
    diagnoses = [test_diagnosis_t(test_passed = some_test%contains_text("something"), diagnostics_string="")] 
  end function

end
