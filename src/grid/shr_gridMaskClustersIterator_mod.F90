!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskClustersIterator_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> Iterates shr_gridMask block by block
!>
!> At each step it returns:
!> - same grid descriptor
!> - subset of enabled cells
!>
!------------------------------------------------------------------------------
module shr_gridMaskClustersIterator_mod
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_Iterator_mod, only: shr_iterator_abs
  use shr_gridMask_mod, only: shr_gridMask
  use shr_gridMaskClusters_mod, only: shr_gridMaskClusters

  implicit none

  public :: shr_gridMaskClustersIterator, shr_gridMaskClustersIterator_cast

  logical, parameter :: ISDEBUG = .false.


  type, extends(shr_iterator_abs) :: shr_gridMaskClustersIterator
    !< mask
    type(shr_gridMask), allocatable :: gMask
    type(shr_gridMaskClusters), allocatable :: cluster
    integer :: counter
    integer :: total
  contains
    procedure :: init => gridMaskClustersIterator_initialize
    procedure :: hasNext
    procedure :: getNext
  end type shr_gridMaskClustersIterator

contains

  subroutine gridMaskClustersIterator_initialize(self, gridMask)
    !< gridMask initialization
    class(shr_gridMaskClustersIterator), intent(inout) :: self
    type(shr_gridMask), intent(in) :: gridMask

    if (allocated(self % gMask)) deallocate(self % gMask) !< enable resuse of the same object
    if (allocated(self % cluster)) deallocate(self % cluster)

    allocate(self % gMask, source = gridMask)
    allocate(self % cluster)

    call self % cluster % init(self % gMask)
    self % total = self % cluster % getSize()
    self % counter = 1
  end subroutine gridMaskClustersIterator_initialize


  logical function hasNext(self)
    !< true if there is another field to iterate
    class(shr_gridMaskClustersIterator), intent(in) :: self
    hasNext = (self % total > self % counter)
  end function hasNext


  function getNext(self) result (obj)
    !< returns current object
    class(shr_gridMaskClustersIterator), intent(inout) :: self
    class(*), allocatable :: obj
    type(shr_gridMask) :: gmask

    if (.not. self % hasNext()) then
      call raiseError(__FILE__, "getNext", &
          "No more elements available")
    end if

    self % counter = self % counter + 1
    gmask = self % cluster % get(self % counter)

    !< copy
    allocate(obj, source = gmask)
  end function getNext


  subroutine shr_gridMaskClustersIterator_cast(obj, gMIterator)
    !< Cast from * to shr_gridMaskClustersIterator
    class(*), intent(in) :: obj
    type(shr_gridMaskClustersIterator), intent(out) :: gMIterator

    select type(o => obj)
    type is(shr_gridMaskClustersIterator)
      gMIterator = o
    class default
      call raiseError(__FILE__, &
          "shr_gridMaskClustersIterator_cast", &
          "Unexpected type found instead of 'shr_gridMaskClustersIterator'")
    end select

  end subroutine shr_gridMaskClustersIterator_cast

end module shr_gridMaskClustersIterator_mod

