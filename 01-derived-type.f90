module type_foo_mod
  type :: foo
    real, private :: int_comp
    integer, public :: real_comp
  end type foo
end module type_foo_mod

program foo_example
  use type_foo_mod
  type(foo) :: f1
  
  f1%int_comp = 42
  f1%real_comp = 3.14 ! ILLEGAL
  ! Can only access private
  ! component in defining module
end program foo_example
