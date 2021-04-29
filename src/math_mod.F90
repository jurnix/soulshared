!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  SHR_maths_mod
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!> Common math subroutines 
!>
!> 
!> 
!------------------------------------------------------------------------------
module SHR_maths_mod

  use SHR_precision_mod, only: sp, dp

  implicit none

  private

  public :: linearInterpolation


contains

  function linearInterpolation(xvals, yvals, requestedX) result (Ycalc)
    !< linear interpolation
    !< example:
    !<    linearInterpolation([1,5], [10,35], [1,2,3,4,5]) -> [10, 16.25, 22.5, 28.76, 35]
    real(kind=sp), intent(in) :: xvals(2) !< values from x axis
    real(kind=sp), intent(in) :: yvals(2) !< values from Y axis (from - to +)
    real(kind=sp), intent(in) :: requestedX(:) !< request x axis values (from - to +)
    real(kind=sp), allocatable :: Ycalc(:) !< output, calculated Y values

    real(kind=sp) :: slope, minYvalue, maxYvalue
    real(kind=sp) :: minXvalue, maxXvalue
    integer :: i
    real(kind=sp) :: ylen, xlen
    real(kind=sp) :: relativeX
    character(len=20) :: tmp, tmp1

    if (maxval(requestedX) > maxval(xvals)) then
      write(tmp,*) maxval(xvals)
      write(tmp1,*) maxval(requestedX)
      write(*,*) "============================== Error =============================="
      write(*,*) "File:    "//__FILE__ 
      write(*,*) "Subr:    linearInterpolation"
      write(*,*) "Message: Requested value is higher than provided X"
      write(*,*) "         Max value allowed is "//trim(tmp)
      write(*,*) "         Value found is "//trim(tmp1)

      stop
    endif

    if (allocated(Ycalc)) deallocate(Ycalc)
    allocate(Ycalc(size(requestedX)))

    minYvalue = yvals(1)
    maxYvalue = yvals(size(yvals))
    ylen = maxYvalue - minYvalue

    minXvalue = xvals(1)
    maxXvalue = xvals(size(xvals))
    xlen = maxXvalue - minXvalue 

    slope = ylen / xlen
!    write (*,*) "maths_mod :: linearInterpolation:: slope =", slope

    do i = 1, size(requestedX)
      relativeX = requestedX(i) - minXvalue
      Ycalc(i) = (relativeX * slope) + minYvalue
    enddo    

  end function linearInterpolation



end module SHR_maths_mod
