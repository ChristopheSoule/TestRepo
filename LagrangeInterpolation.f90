module lagrange_interpolation

use interpolation_point_2d
implicit none

private
    type(point_2d), dimension (:), allocatable :: points
    integer :: nb_point

public :: interpolate, initialize
contains

   subroutine initialize(input_points)
    type(point_2d), dimension (:), intent(in) :: input_points
    integer :: i

    nb_point = size(input_points)
    allocate(points(nb_point))

    do i = 1, nb_point
        points(i) = input_points(i)
    end do
   end subroutine

   function interpolate(x)
    real(8), intent(in) :: x
    real(8) :: interpolate
    real(8) :: polynomial_i
    integer :: i, j
    interpolate = 0

      do i = 1, nb_point
        polynomial_i = 1
        do j = 1, nb_point
            if(j /= i) then
               polynomial_i = polynomial_i * (x - points(j)%x) / (points(i)%x - points(j)%x)
            end if
        end do
        interpolate = interpolate + polynomial_i * points(i)%f
    end do
   end function interpolate

end module lagrange_interpolation



