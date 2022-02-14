!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskFindEnabledEqualSplitIterator_mod
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
module shr_gridMaskFindEnabledEqualSplitIterator_mod
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_Iterator_mod, only: shr_iterator_abs
  use shr_gridMask_mod, only: shr_gridMask, shr_IgridMask
  use shr_gridMaskFindEnabledEqualSplitMethod_mod, only: shr_gridMaskFindEnabledEqualSplitMethod

  implicit none

  public :: shr_gridMaskFindEnabledEqualSplitIterator !, shr_gridMaskFindClustersIterator_cast

  logical, parameter :: ISDEBUG = .false.


  type, extends(shr_iterator_abs) :: shr_gridMaskFindEnabledEqualSplitIterator
    class(shr_gridMaskFindEnabledEqualSplitMethod), allocatable :: gmEnabledEqualSplit
    integer :: counter
    integer :: total
  contains
    procedure :: init => gridMaskClustersIterator_initialize
    procedure :: hasNext
    procedure :: getNext
  end type shr_gridMaskFindEnabledEqualSplitIterator

contains

  subroutine gridMaskClustersIterator_initialize(self, gridMaskEnabledEqualSplit)
    !< gridMask initialization
    class(shr_gridMaskFindEnabledEqualSplitIterator), intent(inout) :: self
    class(shr_gridMaskFindEnabledEqualSplitMethod), intent(in) :: gridMaskEnabledEqualSplit

    if (allocated(self % gmEnabledEqualSplit)) deallocate(self % gmEnabledEqualSplit)
    allocate(self % gmEnabledEqualSplit, source = gridMaskEnabledEqualSplit)

    self % total = self % gmEnabledEqualSplit % getSize()
    self % counter = 0
  end subroutine gridMaskClustersIterator_initialize


  logical function hasNext(self)
    !< true if there is another field to iterate
    class(shr_gridMaskFindEnabledEqualSplitIterator), intent(in) :: self
    hasNext = (self % total > self % counter)
    ! if(IS_DEBUG) write(*,*) "hasNext :: hasnext (total, counter)? ", hasNext, " (", self % total , self % counter, ")"
  end function hasNext


  function getNext(self) result (obj)
    !< returns current object
    class(shr_gridMaskFindEnabledEqualSplitIterator), intent(inout) :: self
    class(*), allocatable :: obj
    class(shr_igridMask), allocatable :: gmask

    if (.not. self % hasNext()) then
      call raiseError(__FILE__, "getNext", &
          "No more elements available")
    end if

    self % counter = self % counter + 1
    allocate(gmask, source = self % gmEnabledEqualSplit % get(self % counter))

    !< copy
    allocate(obj, source = gmask)
  end function getNext


!  subroutine shr_gridMaskFindClustersIterator_cast(obj, gMIterator)
!    !< Cast from * to shr_gridMaskFindClustersIterator
!    class(*), intent(in) :: obj
!    type(shr_gridMaskFindEnabledEqualSplitIterator), intent(out) :: gMIterator
!
!    select type(o => obj)
!    type is(shr_gridMaskFindEnabledEqualSplitIterator)
!      gMIterator = o
!    class default
!      call raiseError(__FILE__, &
!          "shr_gridMaskFindClustersIterator_cast", &
!          "Unexpected type found instead of 'shr_gridMaskFindEnabledEqualSplitIterator'")
!    end select
!
!  end subroutine shr_gridMaskFindClustersIterator_cast

end module shr_gridMaskFindEnabledEqualSplitIterator_mod

