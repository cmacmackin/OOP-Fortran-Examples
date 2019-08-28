module interface_example_mod

  type, abstract :: differentiator
  contains
    procedure(diff), deferred :: derivative
  end type differentiator

  abstract interface
    function diff(this, f, dx)
      import differentiator
      class(differentiator), intent(in) :: this 
      real, intent(in) :: f(:)
      real, intent(in) :: dx
      real, dimension(size(f)) :: diff
    end function diff
  end interface

end module interface_example_mod



module interface_implementation_mod
  use interface_example_mod

  type, extends(differentiator) :: centered_2nd_order
   contains
     procedure :: derivative
  end type centered_2nd_order

contains

  function derivative(this, f, dx)
    class(centered_2nd_order), intent(in) :: this
    real, intent(in) :: f(:)
    real, intent(in) :: dx
    real, dimension(size(f)) :: derivative

    integer :: i, up, down

    do i = 1, size(f)
       up = min(i+1, size(f))
       down = max(i-1, 1)
       derivative(i) = 0.5*(f(up) - f(down))/dx
    end do
  end function derivative
  
end module interface_implementation_mod
