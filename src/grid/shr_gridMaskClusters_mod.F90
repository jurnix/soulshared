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
  !use SHR_precision_mod, only: sp

  use shr_gridMask_mod, only: shr_gridMask
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
  use shr_maskClusters_mod, only: shr_maskClusters_2d
  use shr_mask_mod, only: shr_mask2d

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
  end type shr_gridMaskClusters

contains

  subroutine init(self, gridMask)
    !< gridMask initialization
    class(shr_gridMaskClusters), intent(inout) :: self
    type(shr_gridMask), intent(in) :: gridMask
    type(shr_mask2d) :: mask
    logical, allocatable :: lmask(:,:)
    integer :: nclusters, icluster
    type(shr_mask2d) :: tmpMaskCluster
    type(shr_gGridDescriptor) :: gDescriptor

    allocate(self % mask, source = gridMask)
    allocate(self % clusters)

    !< initialize each cluster to shr_gridMask
    !< from shr_gridMask to shr_maskClusters
    lmask = self % mask % getRaw()
    call mask % init(lmask)
    call self % clusters % init(mask)


    nclusters = self % clusters % getSize()
    allocate(self % groups(nclusters))

    gDescriptor = self % mask % getGridDescriptor()
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
    if (self % getSize() > pos) then
      call raiseError(__FILE__, "get", &
          "Requested element position not found")
    end if
    get = self % groups(pos)
  end function get

end module shr_gridMaskClusters_mod

