!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskFindClustersIterator_mod
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
module shr_gridMaskFindClustersIterator_mod
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_Iterator_mod, only: shr_iterator_abs
  use shr_gridMask_mod, only: shr_gridMask
  use shr_gridMaskFindClustersMethod_mod, only: shr_IGridMaskFindClustersMethod

  implicit none

  public :: shr_gridMaskFindClustersIterator, shr_gridMaskFindClustersIterator_cast

  logical, parameter :: ISDEBUG = .false.


  type, extends(shr_iterator_abs) :: shr_gridMaskFindClustersIterator
    class(shr_IGridMaskFindClustersMethod), allocatable :: cluster
    integer :: counter
    integer :: total
  contains
    procedure :: init => gridMaskClustersIterator_initialize
    procedure :: hasNext
    procedure :: getNext
  end type shr_gridMaskFindClustersIterator

contains

  subroutine gridMaskClustersIterator_initialize(self, gridMaskClusters)
    !< gridMask initialization
    class(shr_gridMaskFindClustersIterator), intent(inout) :: self
    class(shr_IGridMaskFindClustersMethod), intent(in) :: gridMaskClusters

    if (allocated(self % cluster)) deallocate(self % cluster)
    allocate(self % cluster, source = gridMaskClusters)

    self % total = self % cluster % getSize()
    self % counter = 0
  end subroutine gridMaskClustersIterator_initialize


  logical function hasNext(self)
    !< true if there is another field to iterate
    class(shr_gridMaskFindClustersIterator), intent(in) :: self
    hasNext = (self % total > self % counter)
    ! if(IS_DEBUG) write(*,*) "hasNext :: hasnext (total, counter)? ", hasNext, " (", self % total , self % counter, ")"
  end function hasNext


  function getNext(self) result (obj)
    !< returns current object
    class(shr_gridMaskFindClustersIterator), intent(inout) :: self
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


  subroutine shr_gridMaskFindClustersIterator_cast(obj, gMIterator)
    !< Cast from * to shr_gridMaskFindClustersIterator
    class(*), intent(in) :: obj
    type(shr_gridMaskFindClustersIterator), intent(out) :: gMIterator

    select type(o => obj)
    type is(shr_gridMaskFindClustersIterator)
      gMIterator = o
    class default
      call raiseError(__FILE__, &
          "shr_gridMaskFindClustersIterator_cast", &
          "Unexpected type found instead of 'shr_gridMaskFindClustersIterator'")
    end select

  end subroutine shr_gridMaskFindClustersIterator_cast

end module shr_gridMaskFindClustersIterator_mod

