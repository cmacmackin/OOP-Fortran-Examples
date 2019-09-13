module oop_example_mod
  type :: parent
    integer, private :: i = 5
  contains
    procedure :: get_val=>get_parent
  end type parent

  type, extends(parent) :: child
  contains
    procedure :: get_val=>get_child
  end type child
contains
  integer function get_parent(this)
    class(parent), intent(in):: this
    get_parent = this%i
  end function

  integer function get_child(this)
    class(child), intent(in):: this
    get_child = 2*this%i - 1
  end function
end module oop_example_mod

program polymorphism_demo
  use oop_example_mod
  type(parent) :: p
  type(child) :: c

  call print_val(p) ! Prints 5
  call print_val(c) ! Prints 9

contains

  subroutine print_val(obj)
    class(parent), intent(in) :: obj
    print*, obj%get_val()
  end subroutine print_val

end program polymorphism_demo
