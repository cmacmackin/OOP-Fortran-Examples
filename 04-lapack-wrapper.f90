module square_matrix_mod
  implicit none
  private

  integer, parameter, public :: dp = selected_real_kind(15, 307)
  
  type, abstract, public :: square_matrix
   contains
     procedure(getm), deferred :: get_matrix
     procedure(gets), deferred :: get_size
     procedure, private :: mat_mult
     generic :: operator(*) => mat_mult
     procedure(solve), deferred :: inv_mat_mult
  end type square_matrix

  abstract interface
     pure function getm(this) result(m)
       import square_matrix
       import dp
       class(square_matrix), intent(in) :: this
       real(dp), dimension(this%get_size(), this%get_size()) :: m
     end function getm

     pure function gets(this) result(s)
       import square_matrix
       class(square_matrix), intent(in) :: this
       integer :: s
     end function gets

     function solve(this, rhs) result(solution)
       ! TODO: Consider returning a derived type with information on
       ! error, etc.
       import square_matrix
       import dp
       class(square_matrix), intent(inout) :: this
       real(dp), dimension(:), intent(in) :: rhs
       real(dp), dimension(size(rhs)) :: solution
     end function solve
  end interface
  
contains

  pure function mat_mult(this, rhs) result(product)
    class(square_matrix), intent(in) :: this
    real(dp), dimension(:), intent(in) :: rhs
    real(dp), dimension(size(rhs)) :: product
    real(dp), allocatable, dimension(:,:) :: mat
    if (this%get_size() /= size(rhs)) error stop "Matrix and array of different sizes"
    product = matmul(this%get_matrix(), rhs)
  end function mat_mult
  
end module square_matrix_mod


module general_square_matrix_mod
  use square_matrix_mod
  implicit none
  private

  real(dp), dimension(:), allocatable :: real_work_array
  integer, dimension(:), allocatable :: integer_work_array
  
  type, extends(square_matrix), public :: general_square_matrix
     private
     real(dp), dimension(:,:), allocatable :: matrix
     real(dp), dimension(:,:), allocatable :: factored_matrix
     real(dp), dimension(:), allocatable :: col_scale_factors
     real(dp), dimension(:), allocatable :: row_scale_factors
     integer, dimension(:), allocatable :: pivots
     logical :: factored = .false.
     character :: equilibration = "N"
   contains
     procedure :: get_matrix
     procedure :: get_size
     procedure :: inv_mat_mult
  end type general_square_matrix

  interface general_square_matrix
     module procedure constructor
  end interface general_square_matrix
  
  interface
     pure subroutine dgesvx(fact, trans, n, nrhs, a, lda, af, ldaf,   &
          ipiv, equed, r, c, b, ldb, x, ldx, rcond, ferr, berr, work, &
          iwork, info)
       import dp
       character, intent(in) :: fact
       character, intent(in) :: trans
       integer, intent(in) :: n
       integer, intent(in) :: nrhs
       real(dp), intent(inout), dimension(lda, n) :: a
       integer, intent(in) :: lda
       real(dp), intent(inout), dimension(ldaf, n) :: af
       integer, intent(in) :: ldaf
       integer, intent(inout), dimension(n) :: ipiv
       character, intent(inout) :: equed
       real(dp), intent(inout), dimension(n) :: r
       real(dp), intent(inout), dimension(n) :: c
       real(dp), intent(inout), dimension(ldb, nrhs) :: b
       integer, intent(in) :: ldb
       real(dp), intent(out), dimension(ldx, nrhs) :: x
       integer, intent(in) :: ldx
       real(dp), intent(out) :: rcond
       real(dp), intent(out), dimension(nrhs) :: ferr
       real(dp), intent(out), dimension(nrhs) :: berr
       real(dp), intent(out), dimension(4*n) :: work
       integer, intent(out), dimension(n) :: iwork
       integer, intent(out) :: info
     end subroutine dgesvx
  end interface
  
contains

  function constructor(matrix) result(this)
    real(dp), dimension(:, :), intent(in) :: matrix
    type(general_square_matrix) :: this
    integer :: n
    n = size(matrix, 1)
    if (n /= size(matrix, 2)) then
       error stop "Non-square matrix used to build type(general_square_matrix)"
    end if
    allocate(this%matrix(n, n), source=matrix)
    allocate(this%factored_matrix(n, n))
    allocate(this%col_scale_factors(n))
    allocate(this%row_scale_factors(n))
    allocate(this%pivots(n))
    this%col_scale_factors = 1.0_dp
    this%row_scale_factors = 1.0_dp
  end function constructor
  
  pure function get_matrix(this) result(matrix)
    class(general_square_matrix), intent(in) :: this
    real(dp), dimension(this%get_size(), this%get_size()) :: matrix
    integer :: i, j
    do concurrent (i=1:this%get_size(), j=1:this%get_size())
       matrix(i, j) = this%matrix(i, j)
       if (this%equilibration == 'R' .or. this%equilibration == 'B') &
            matrix(i, j) = matrix(i,j) / this%row_scale_factors(i)
       if (this%equilibration == 'C' .or. this%equilibration == 'B') &
            matrix(i, j) = matrix(i,j) / this%col_scale_factors(j)
    end do
  end function get_matrix

  pure function get_size(this) result(mat_size)
    class(general_square_matrix), intent(in) :: this
    integer :: mat_size
    mat_size = size(this%matrix, 1)
  end function get_size

  function inv_mat_mult(this, rhs) result(solution)
    class(general_square_matrix), intent(inout) :: this
    real(dp), dimension(:), intent(in) :: rhs
    real(dp), dimension(size(rhs)) :: solution

    integer :: n, info
    real(dp), dimension(size(rhs), 1) :: x, b
    real(dp), dimension(1) :: ferr, berr
    real(dp) :: rcond
    character :: fact

    n = size(rhs)
    if (n /= this%get_size()) error stop "Mismatched vector/matrix sizes"

    if (.not. allocated(real_work_array)) then
       allocate(real_work_array(4*n))
       allocate(integer_work_array(n))
    else
       if (size(real_work_array) < 4*n) then
          deallocate(real_work_array)
          allocate(real_work_array(4*n))
       end if
       if (size(integer_work_array) < n) then
          deallocate(integer_work_array)
          allocate(integer_work_array(n))
       end if
    end if
    b(:, 1) = rhs
    
    if (this%factored) then
       fact = 'F'
    else
       fact = 'E'
    end if
    call dgesvx(fact, 'N', n, 1, this%matrix, n, this%factored_matrix, &
         n, this%pivots, this%equilibration, this%row_scale_factors,  &
         this%col_scale_factors, b, n, x, n, rcond, ferr, berr,       &
         real_work_array, integer_work_array, info)

    if (info /= 0) error stop "DGESVX returned with non-zero INFO argument"
    this%factored = .true.
    solution = x(:,1)
  end function inv_mat_mult
  
end module general_square_matrix_mod


program test_solver
  use square_matrix_mod, only: dp
  use general_square_matrix_mod, only: general_square_matrix
  implicit none

  type(general_square_matrix) :: solver
  integer, parameter :: n = 5
  real(dp), dimension(n, n) :: matrix
  real(dp), dimension(n) :: x_actual, x_solved, b
  integer :: i
  
  matrix(1, 1) = 3.5_dp
  matrix(1, 2) = 1._dp 
  matrix(1, 3) = -5._dp
  matrix(1, 4) = 1._dp 
  matrix(1, 5) = 0._dp 

  matrix(2, 1) = -0.5_dp
  matrix(2, 2) = 0.03_dp
  matrix(2, 3) = 8._dp
  matrix(2, 4) = 0._dp
  matrix(2, 5) = -7._dp

  matrix(3, 1) = -2.2_dp
  matrix(3, 2) = 100._dp
  matrix(3, 3) = 0._dp
  matrix(3, 4) = -1._dp
  matrix(3, 5) = -1._dp

  matrix(4, 1) = 5.5_dp
  matrix(4, 2) = 0._dp
  matrix(4, 3) = -11._dp
  matrix(4, 4) = -82._dp
  matrix(4, 5) = 2._dp

  matrix(5, 1) = 0._dp
  matrix(5, 2) = 5._dp
  matrix(5, 3) = 4._dp
  matrix(5, 4) = 3._dp
  matrix(5, 5) = -6._dp

  write(*, "(A)") "Solving linear system"
  do i = 1, n
     write(*, "(5F9.2)") matrix(i, :)
  end do

  solver = general_square_matrix(matrix)

  x_actual(1) = 1._dp
  x_actual(2) = 2._dp
  x_actual(3) = 3._dp
  x_actual(4) = 4._dp
  x_actual(5) = 5._dp

  b(1) = -5.5_dp
  b(2) = -11.44_dp
  b(3) = 188.8_dp
  b(4) = -345.5_dp
  b(5) = 7._dp

  write(*, "(/, A, T25, '[', 5F9.2, ']')") "RHS of linear system is", b
  write(*, "(A, T25, '[', 5F9.2, ']')") "Expected solution is", x_actual

  b = solver * x_actual
  x_solved = solver%inv_mat_mult(b)

  write(*, "(/, A, T25, '[', 5F9.2, ']')") "Actual solution is", x_solved
  write(*, "(A, F8.3)") "Backward Error = ", sqrt(sum((solver * x_solved - b)**2))
  write(*, "(A, F8.3)") "Forward Error  = ", sqrt(sum((x_solved - x_actual)**2))
  
end program test_solver
