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
!> shr_gridMask wrapper to shr_maskClusters_2d
!>
!------------------------------------------------------------------------------
module shr_gridMaskClusters_mod
  use shr_error_mod, only: raiseError

  use shr_gridMask_mod, only: shr_gridMask, shr_IgridMask
  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor
  use shr_maskClusters_mod, only: shr_maskClusters_2d
  use shr_mask_mod, only: shr_mask2d

  implicit none

  public :: shr_gridMaskClusters, shr_IGridMaskClusters
  public :: shr_ObjTogridMaskClusters_cast

  logical, parameter :: ISDEBUG = .false.


  type, abstract :: shr_IGridMaskClusters
  contains
    procedure(iface_init), deferred :: init
    procedure(iface_getSize), deferred :: getSize
    procedure(iface_get), deferred :: get
  end type shr_IGridMaskClusters


  abstract interface
    subroutine iface_init(self, gridMask)
      import :: shr_IGridMaskClusters, shr_IgridMask
      !< gridMask initialization
      class(shr_IGridMaskClusters), intent(inout) :: self
      class(shr_IgridMask), intent(in) :: gridMask
    end subroutine iface_init

    integer function iface_getSize(self)
      import :: shr_IGridMaskClusters
      !< returns how many groups found
      class(shr_IGridMaskClusters), intent(in) :: self
    end function iface_getSize

    type(shr_gridMask) function iface_get(self, pos)
      import :: shr_IGridMaskClusters, shr_gridMask
      !< it returns selected gridMask requested for position 'pos'
      class(shr_IGridMaskClusters), intent(in) :: self
      integer, intent(in) :: pos
    end function iface_get
  end interface


  type, extends(shr_IGridMaskClusters) :: shr_gridMaskClusters
    class(shr_IgridMask), allocatable :: mask
    type(shr_gridMask), allocatable :: groups(:)

    type(shr_maskClusters_2d), allocatable :: clusters
  contains
    procedure :: init
    procedure :: getSize
    procedure :: get
  end type shr_gridMaskClusters

contains

  subroutine init(self, gridMask)
    !< gridMask initialization
    class(shr_gridMaskClusters), intent(inout) :: self
    class(shr_IgridMask), intent(in) :: gridMask
    type(shr_mask2d) :: mask
    logical, allocatable :: lmask(:,:)
    integer :: nclusters, icluster
    type(shr_mask2d) :: tmpMaskCluster
    class(shr_iGGridDescriptor), allocatable :: gDescriptor

    allocate(self % mask, source = gridMask)
    allocate(self % clusters)

    !< initialize each cluster to shr_gridMask
    !< from shr_gridMask to shr_maskClusters
    lmask = self % mask % getRaw()
    call mask % init(lmask)
    call self % clusters % init(mask)


    nclusters = self % clusters % getSize()
    allocate(self % groups(nclusters))

    allocate(gDescriptor, source = self % mask % getGridDescriptor())
    !< for each cluster
    do icluster = 1, nclusters !< cluster found
      tmpMaskCluster = self % clusters % get(icluster)
      lmask = tmpMaskCluster % get()
      call self % groups(icluster) % init(gDescriptor, lmask)
    end do
  end subroutine init


  integer function getSize(self)
    !< returns how many groups found
    class(shr_gridMaskClusters), intent(in) :: self
    getSize = size(self % groups)
  end function getSize


  type(shr_gridMask) function get(self, pos)
    !< it returns selected gridMask requested for position 'pos'
    class(shr_gridMaskClusters), intent(in) :: self
    integer, intent(in) :: pos
    if (self % getSize() < pos) then
      call raiseError(__FILE__, "get", &
          "Requested element position not found")
    end if
    get = self % groups(pos)
  end function get


  subroutine shr_ObjTogridMaskClusters_cast(obj, gmClusters)
    !< cast obj into shr_gridMaskClusters
    class(*), intent(in) :: obj
    type(shr_gridMaskClusters), intent(out) :: gmClusters

    select type(o => obj)
    type is(shr_gridMaskClusters)
      gmClusters = o
    class default
      call raiseError(__FILE__, &
          "shr_ObjTogridMaskClusters_cast", &
          "Unexpected type found instead of 'shr_gridMaskClusters'")
    end select

  end subroutine shr_ObjTogridMaskClusters_cast

end module shr_gridMaskClusters_mod

