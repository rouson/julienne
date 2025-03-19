module julienne_vector_test_description_m
  use julienne_string_m, only : string_t
  use julienne_test_diagnosis_m, only : test_diagnosis_t
  implicit none

  abstract interface
    function vector_diagnosis_function_i() result(diagnoses)
      import test_diagnosis_t
      implicit none
      type(test_diagnosis_t), allocatable :: diagnoses(:)
    end function
  end interface

  type vector_test_description_t
    private
    type(string_t), allocatable :: descriptions_(:)
    procedure(vector_diagnosis_function_i), pointer, nopass :: vector_diagnosis_function_
  end type

  interface vector_test_description_t
    module procedure construct_from_strings
  end interface

contains

  function construct_from_strings(descriptions, vector_diagnosis_function) result(vector_test_description)
    type(string_t), intent(in) :: descriptions(:)
    procedure(vector_diagnosis_function_i), intent(in), pointer :: vector_diagnosis_function
    type(vector_test_description_t) vector_test_description
    vector_test_description%descriptions_ = descriptions
    vector_test_description%vector_diagnosis_function_ => vector_diagnosis_function
  end function

end module
