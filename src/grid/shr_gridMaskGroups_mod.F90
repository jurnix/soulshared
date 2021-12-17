!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskGroups_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> Find groups in a shr_gridMask
!>
!> groups are composed of:
!>  - squared subgrid parts
!>  - enabled gridcells only
!>
!> found groups are returned as:
!>  - same grid
!>  - only enabled those gridcells found as group
!>
!------------------------------------------------------------------------------
module shr_gridMaskGroups_mod
  use shr_error_mod, only: raiseError
  !use SHR_precision_mod, only: sp

  use shr_gridMask_mod, only: shr_gridMask

  implicit none

  public :: shr_gridMaskGroups

  logical, parameter :: ISDEBUG = .false.


  type :: shr_gridMaskGroups
    type(shr_gridMask), allocatable :: mask
    type(shr_gridMask), allocatable :: groups(:)
  contains
    procedure :: init
    procedure :: getSize
    procedure :: get

    procedure, private :: calculate
    procedure, private :: calculateTotal

  end type shr_gridMaskGroups

contains

  subroutine init(self, gridMask)
    !< gridMask initialization
    class(shr_gridMaskGroups), intent(inout) :: self
    type(shr_gridMask), intent(in) :: gridMask
    allocate(self % mask, source = gridMask)

    !< build here

  end subroutine init


  integer function getSize(self)
    !< returns how many groups found
    class(shr_gridMaskGroups), intent(in) :: self
    getSize = size(self % groups)
  end function getSize


  integer function calculateTotal(self)
    !< calculate how many groups
    class(shr_gridMaskGroups), intent(in) :: self
    logical, allocatable :: lmask(:,:)
    lmask = self % mask % getRaw()
    !calculateTotal = shr_mask_calc_SquaredGroups(lmask)
  end function calculateTotal


  subroutine calculate(self)
    !< discover each group and create and new gridMask from it
    class(shr_gridMaskGroups), intent(inout) :: self
  end subroutine calculate


  type(shr_gridMask) function get(self, pos)
    !< it returns selected gridMask requested for position 'pos'
    class(shr_gridMaskGroups), intent(in) :: self
    integer, intent(in) :: pos
    if (self % getSize() > pos) then
      call raiseError(__FILE__, "get", &
          "Requested element position not found")
    end if
    get = self % groups(pos)
  end function get


end module shr_gridMaskGroups_mod

