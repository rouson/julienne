module test_m
  implicit none

  type test_t
    integer, allocatable :: i
  end type

  interface
    pure module function construct_test() result(test)
      implicit none
      type(test_t) test
    end function
  end interface

contains
  module procedure construct_test
    allocate(test%i, source = 0)
  end procedure
end module
