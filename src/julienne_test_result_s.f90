submodule(julienne_test_result_m) julienne_test_result_s
  implicit none

contains

    module procedure construct_from_string_and_diagnosis
      test_result%description_ = description
      test_result%diagnostics_ = diagnosis
    end procedure

    module procedure construct_from_character_and_diagnosis
      test_result%description_ = description
      test_result%diagnostics_ = diagnosis
    end procedure

    module procedure characterize
      characterization = &
        trim(merge("passes on ", "FAILS on  ", self%diagnostics_%test_passed())) // " " // trim(self%description_%string()) // "."
      if (.not. self%diagnostics_%test_passed()) &
        characterization = characterization // new_line('') // "      diagnostics: " // self%diagnostics_%diagnostics_string()
    end procedure

    module procedure passed
      test_passed = self%diagnostics_%test_passed()
    end procedure

    module procedure description_contains_string
      substring_found = self%description_contains_characters(substring%string())
    end procedure

    module procedure description_contains_characters
      substring_found = index(self%description_%string(), substring) /= 0
    end procedure

end submodule julienne_test_result_s
