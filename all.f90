  implicit none

  type string_t
    character(len=:), allocatable :: string_
  end type

  abstract interface
    function vector_diagnosis_function_i() result(diagnoses)
      implicit none
      logical, allocatable :: diagnoses(:)
    end function
  end interface

  type vector_test_description_t
    type(string_t), allocatable :: descriptions_(:)
    procedure(vector_diagnosis_function_i), pointer, nopass :: vector_diagnosis_function_
  end type

  associate(vector_test_description => construct_from_strings([string_t::], diagnoses))
  end associate

contains

 function diagnoses()
    logical, allocatable :: diagnoses(:)
    diagnoses = [logical::]
  end function

  function construct_from_strings(descriptions, vector_diagnosis_function) result(vector_test_description)
    type(string_t), intent(in) :: descriptions(:)
    procedure(vector_diagnosis_function_i), intent(in), pointer :: vector_diagnosis_function
    type(vector_test_description_t) vector_test_description
    vector_test_description%descriptions_ = descriptions
    vector_test_description%vector_diagnosis_function_ => vector_diagnosis_function
  end function

end
