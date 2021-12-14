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
!> At each step it returns itself with only enabled cells
!>
!------------------------------------------------------------------------------
module shr_gridMaskIterator_mod
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_Iterator_mod, only: shr_iterator_abs
  use shr_gridMask_mod, only: shr_gridMask

  implicit none

  public :: shr_gridMaskIterator

  logical, parameter :: ISDEBUG = .false.


  type, extends(shr_iterator_abs) :: shr_gridMaskIterator
    !< mask
    type(shr_gridMask), allocatable :: gMask
  contains
    procedure :: init => gridMaskIterator_initialize
    procedure :: hasNext
    procedure :: get
  end type shr_gridMaskIterator

contains

  subroutine gridMaskIterator_initialize(self, gridMask)
    !< gridMask initialization
    class(shr_gridMaskIterator), intent(inout) :: self
    type(shr_gridMask), intent(in) :: gridMask

  end subroutine gridMaskIterator_initialize


  logical function hasNext(self)
    !< true if there is another field to iterate
    class(shr_gridMaskIterator), intent(in) :: self
  end function hasNext


  function get(self) result (obj)
    !< returns current object
    class(shr_gridMaskIterator), intent(in) :: self
    class(*), allocatable :: obj
  end function get

end module shr_gridMaskIterator_mod

