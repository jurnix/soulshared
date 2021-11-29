!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gAxisMapping_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gAxisMapping maps gridAxis into an integer matrix
!> 
!------------------------------------------------------------------------------
module shr_gAxisMapping_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridAxesCell_mod, only: shr_gGridAxesCell
  use shr_gAxisCellIndex_mod, only: shr_gAxisCellIndex
  
  implicit none

  public :: shr_gAxisMapping

  logical, parameter :: ISDEBUG = .false.


  type shr_gAxisMapping
    type(shr_gGridAxes), allocatable :: axis
    type(shr_gAxisCellIndex), allocatable :: indices(:)
  contains
    procedure :: init => gAxisMapping_initialize 

    procedure :: getIndexByCoord
    procedure :: getIndexByGridAxisCell
    generic :: getIndex => getIndexByCoord, getIndexByGridAxisCell
  end type shr_gAxisMapping

contains

  subroutine gAxisMapping_initialize(self, gridAxis)
    !< gAxisMapping initialization
    class(shr_gAxisMapping), intent(inout) :: self
    type(shr_gGridAxes), intent(in) :: gridAxis
    integer :: iIdx, ncells
    
    allocate(self % axis, source = gridAxis)
    ncells = self % axis % getSize()

    ! map indices
    allocate(self % indices(ncells))
    do iIdx = 1, ncells
      call self % indices(iIdx) % init(iIdx, self % axis % cells(iIdx))
    enddo
  end subroutine gAxisMapping_initialize


  function getIndexByCoord(self, axisCoord) result (foundIdxs)
    !< it returns the index(s) which matches with axisCoord
    !< Multiple indices can be returned in case it lies in 
    !< the grid axis cells border.
    !< In case it lies outside the GridAxis it returns an error
    !<
    !< Example:
    !< getIndexByCoord(2) -> 2, 3 indices
    !< gridAxis: (0 1, 1 2, 2 3, 3 4, 4 5)
    !<  maps to:  (1    2    3    4    5)
    !<
    !< or:
    !<
    !< getIndexByCoord(2) -> 3, 4 indices
    !< gridAxis: (5 4, 4 3, 3 2, 2 1, 1 0)
    !<  maps to:  (1    2    3    4    5)
    class(shr_gAxisMapping), intent(in) :: self
    real(kind=sp), intent(in):: axisCoord
    integer, allocatable :: foundIdxs(:)
    integer :: icell
    logical :: hasAxisCoord

    if (.not. self % axis % hasGridCoord(axisCoord)) then
      call raiseError(__FILE__, "getIndexByCoord", &
           "Given axisCoord is outside gridAxis bounds")
    endif

    allocate(foundIdxs(0)) ! init output

    do icell = 1, self % axis % getSize()
      hasAxisCoord = self % axis % cells(icell) % isIn(axisCoord)
      if (hasAxisCoord) then
        foundIdxs = [foundIdxs, [icell]]
      endif
    enddo
  end function getIndexByCoord
  
  
  integer function getIndexByGridAxisCell(self, gAxisCell)
    !< it returns the index(s) which matches with gAxisCell
    !< only 1 index can be returned
    !< -1 in case is not found
    class(shr_gAxisMapping), intent(in) :: self
    type(shr_gGridAxesCell), intent(in) :: gAxisCell
    integer :: icell
    getIndexByGridAxisCell = -1 ! init output
    do icell = 1, self % axis % getSize()
      !< find gridcell(s) and count
      if (gAxisCell == self % axis % cells(icell)) then
        getIndexByGridAxisCell = icell
        exit !< element found, no need to iterate anymore 
      endif
    enddo
  end function getIndexByGridAxisCell


end module shr_gAxisMapping_mod 

