  use julienne_m,only : vector_test_description_t, string_t, test_diagnosis_t, diagnosis_function_i, test_description_t
  implicit none
  type(vector_test_description_t), allocatable :: vector_test_descriptions(:)

  vector_test_descriptions = [vector_test_description_t([string_t(""),string_t("")], check_substring_search)]

contains
   function check_substring_search() result(diagnoses)
    type(test_diagnosis_t), allocatable :: diagnoses(:)
    type(test_description_t) doing_something
    procedure(diagnosis_function_i), pointer :: unused

    unused => null()

    doing_something = test_description_t("doing something", unused)
    diagnoses = [ & 
       test_diagnosis_t(test_passed =       doing_something%contains_text("something"),    diagnostics_string="expected .true.") &
      ,test_diagnosis_t(test_passed = .not. doing_something%contains_text("missing text"), diagnostics_string="expected .true.") &
    ]   
  end function
end
