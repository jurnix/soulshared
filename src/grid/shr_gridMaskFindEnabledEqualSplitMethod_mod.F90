!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskFindEnabledEqualSplitMethod_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> Convert each found shr_Mask2d into shr_gridMask
!> (shr_gridMask wrapper to shr_maskEnabledEqualSplit)
!>
!------------------------------------------------------------------------------
module shr_gridMaskFindEnabledEqualSplitMethod_mod
  use shr_error_mod, only: raiseError

  use shr_gridMask_mod, only: shr_gridMask, shr_IgridMask

  use shr_maskEnabledEqualSplit_mod, only: shr_maskEnabledEqualSplit
  use shr_mask_mod, only: shr_mask2d
  use shr_gGrid_mod, only: shr_igGrid

  implicit none

  public :: shr_gridMaskFindEnabledEqualSplitMethod

  logical, parameter :: ISDEBUG = .false.


  type :: shr_gridMaskFindEnabledEqualSplitMethod
    class(shr_IgridMask), allocatable :: mask
    type(shr_gridMask), allocatable :: groups(:)

    type(shr_maskEnabledEqualSplit), allocatable :: maskEnabledEqualSplit
  contains
    procedure :: init
    procedure :: getSize
    procedure :: get
  end type shr_gridMaskFindEnabledEqualSplitMethod

contains

  subroutine init(self, nparts, gridMask)
    !< gridMask initialization
    class(shr_gridMaskFindEnabledEqualSplitMethod), intent(inout) :: self
    integer, intent(in) :: nparts
    class(shr_IgridMask), intent(in) :: gridMask
    type(shr_mask2d) :: mask
    logical, allocatable :: lmask(:,:)
    integer :: ipart
    type(shr_mask2d), allocatable :: parts(:)
    type(shr_mask2d) :: tmpMask
    class(shr_igGrid), allocatable :: grid

    allocate(self % mask, source = gridMask)
    allocate(self % groups(nparts))

    !< get row mask from
    lmask = self % mask % get()
    call mask % init(lmask)
    allocate(self % maskEnabledEqualSplit)

    call self % maskEnabledEqualSplit % init(nparts, mask)
    call self % maskEnabledEqualSplit % calculate()

    allocate(grid, source = self % mask % getGrid())
    parts = self % maskEnabledEqualSplit % get()
    !< for each part
    do ipart = 1, nparts !< part found
      tmpMask = parts(ipart)
      !< extract raw mask
      lmask = tmpMask % get()
      call self % groups(ipart) % init(grid, lmask)
    end do
  end subroutine init


  integer function getSize(self)
    !< returns how many groups found
    class(shr_gridMaskFindEnabledEqualSplitMethod), intent(in) :: self
    getSize = size(self % groups)
  end function getSize


  function get(self, pos) result (newGM)
    !< it returns selected gridMask requested for position 'pos'
    class(shr_gridMaskFindEnabledEqualSplitMethod), intent(in) :: self
    class(shr_igridMask), allocatable :: newGM
    integer, intent(in) :: pos
    if (self % getSize() < pos) then
      call raiseError(__FILE__, "get", &
          "Requested element position not found")
    end if
    allocate(newGM, source = self % groups(pos))
  end function get


!  subroutine shr_ObjTogridMaskClusters_cast(obj, gmClusters)
!    !< cast obj into shr_gridMaskSimpleSquaresFindClustersMethod
!    class(*), intent(in) :: obj
!    class(shr_IGridMaskFindClustersMethod), allocatable, intent(out) :: gmClusters
!
!    select type(o => obj)
!    class is(shr_IGridMaskFindClustersMethod)
!      gmClusters = o
!    class default
!      call raiseError(__FILE__, &
!          "shr_ObjTogridMaskClusters_cast", &
!          "Unexpected type found instead of 'shr_IGridMaskFindClustersMethod'")
!    end select
!  end subroutine shr_ObjTogridMaskClusters_cast

end module shr_gridMaskFindEnabledEqualSplitMethod_mod

