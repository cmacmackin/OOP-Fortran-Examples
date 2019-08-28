module oop_example_mod
  type :: foo
    integer :: i = 5 !Initial value
  contains
    procedure :: get_val=>get_fval
  end type foo

  type, extends(foo) :: bar
  contains
    procedure :: get_val=>get_bval
  end type bar
contains
  integer function get_fval(this)
    class(foo), intent(in) :: this
    get_fval = this%i
  end function

  integer function get_bval(this)
    class(bar), intent(in) :: this
    get_bval = 2*this%i - 1
  end function
end module oop_example_mod


program polymorphism_demo
  use oop_example_mod
  type(foo) :: f
  type(bar) :: b

  call print_val(f) ! Prints 5
  call print_val(b) ! Prints 9

contains

  subroutine print_val(obj)
    class(foo), intent(in) :: obj
    print*, obj%get_val()
  end subroutine print_val

end program polymorphism_demo
