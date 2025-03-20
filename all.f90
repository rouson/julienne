  implicit none

  type foo_t
    integer, allocatable :: i_
    procedure(f), pointer, nopass :: f_
  end type

  associate(foo => foo_t(1,f))
  end associate

contains

  function f()
    logical, allocatable :: f
    f = .true.
  end function

end
