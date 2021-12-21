!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskClusters_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> Find clusters in a shr_gridMask
!>
!> clusters are composed of:
!>  - squared subgrid parts
!>  - enabled gridcells only
!>
!> found clusters are returned as:
!>  - same grid
!>  - only enabled those gridcells included in the cluster
!>
!------------------------------------------------------------------------------
module shr_gridMaskClusters_mod
  use shr_error_mod, only: raiseError
  !use SHR_precision_mod, only: sp

  use shr_gridMask_mod, only: shr_gridMask
  use shr_maskClusters_mod, only: shr_maskClusters_2d

  implicit none

  public :: shr_gridMaskClusters

  logical, parameter :: ISDEBUG = .false.


  type :: shr_gridMaskClusters
    type(shr_gridMask), allocatable :: mask
    type(shr_gridMask), allocatable :: groups(:)

    type(shr_maskClusters_2d), allocatable :: clusters
  contains
    procedure :: init
    procedure :: getSize
    procedure :: get

    procedure, private :: calculate
    procedure, private :: calculateTotal

  end type shr_gridMaskClusters

contains

  subroutine init(self, gridMask)
    !< gridMask initialization
    class(shr_gridMaskClusters), intent(inout) :: self
    type(shr_gridMask), intent(in) :: gridMask
    allocate(self % mask, source = gridMask)
    allocate(self % clusters)

    !< build here
  end subroutine init


  integer function getSize(self)
    !< returns how many groups found
    class(shr_gridMaskClusters), intent(in) :: self
    getSize = size(self % groups)
  end function getSize


  integer function calculateTotal(self)
    !< calculate how many groups
    class(shr_gridMaskClusters), intent(in) :: self
    logical, allocatable :: lmask(:,:)
    lmask = self % mask % getRaw()
    !calculateTotal = shr_mask_calc_SquaredGroups(lmask)
  end function calculateTotal


  subroutine calculate(self)
    !< discover each group and create and new gridMask from it
    class(shr_gridMaskClusters), intent(inout) :: self
  end subroutine calculate


  type(shr_gridMask) function get(self, pos)
    !< it returns selected gridMask requested for position 'pos'
    class(shr_gridMaskClusters), intent(in) :: self
    integer, intent(in) :: pos
    if (self % getSize() > pos) then
      call raiseError(__FILE__, "get", &
          "Requested element position not found")
    end if
    get = self % groups(pos)
  end function get


end module shr_gridMaskClusters_mod

