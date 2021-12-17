!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskIterator_mod
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
module shr_gridMaskIterator_mod
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_Iterator_mod, only: shr_iterator_abs
  use shr_gridMask_mod, only: shr_gridMask

  implicit none

  public :: shr_gridMaskIterator, shr_gridMaskIterator_cast

  logical, parameter :: ISDEBUG = .false.


  type, extends(shr_iterator_abs) :: shr_gridMaskIterator
    !< mask
    type(shr_gridMask), allocatable :: gMask
    type(shr_gridMask), allocatable :: maskedGroups
    integer :: counter
    integer :: total
  contains
    procedure :: init => gridMaskIterator_initialize
    procedure :: hasNext
    procedure :: getNext
  end type shr_gridMaskIterator

contains

  subroutine gridMaskIterator_initialize(self, gridMask)
    !< gridMask initialization
    class(shr_gridMaskIterator), intent(inout) :: self
    type(shr_gridMask), intent(in) :: gridMask

    if (allocated(self % gMask)) deallocate(self % gMask) !< enable resuse of the same object
    allocate(self % gMask, source = gridMask)
    !call maskedGrouped % init(self % gMask)
    !self % total = maskedGrouped % size()
    !maskedGrouped % get()
    self % counter = 0
  end subroutine gridMaskIterator_initialize


  logical function hasNext(self)
    !< true if there is another field to iterate
    class(shr_gridMaskIterator), intent(in) :: self
    hasNext = (self % total > self % counter)
  end function hasNext


  function getNext(self) result (obj)
    !< returns current object
    class(shr_gridMaskIterator), intent(inout) :: self
    class(*), allocatable :: obj
    if (self % hasNext()) then
      self % counter = self % counter + 1
      !allocate(obj, source = maskedGrouped % get(self % counter))
    end if
  end function getNext


  subroutine shr_gridMaskIterator_cast(obj, gMIterator)
    !< Cast from * to shr_gridMaskIterator
    class(*), intent(in) :: obj
    type(shr_gridMaskIterator), intent(out) :: gMIterator

    select type(o => obj)
    type is(shr_gridMaskIterator)
      gMIterator = o
    class default
      call raiseError(__FILE__, "shr_gridMaskIterator_cast", &
          "Unexpected type found instead of 'shr_gridMaskIterator'")
    end select

  end subroutine shr_gridMaskIterator_cast

end module shr_gridMaskIterator_mod

