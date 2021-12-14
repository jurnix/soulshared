!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_Iterator_abs
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> Iterator pattern generic implementation
!>
!------------------------------------------------------------------------------
module shr_Iterator_mod

  implicit none

  public :: shr_Iterator_abs

  logical, parameter :: ISDEBUG = .false.

  !< iterator pattern generic
  type, abstract :: shr_iterator_abs
  contains
    procedure(iface_hasNext), deferred :: hasNext
    procedure(iface_get), deferred :: get
  end type shr_iterator_abs


  abstract interface
    logical function iface_hasNext(self)
      !< true if there is another field to iterate
      import shr_iterator_abs
      class(shr_iterator_abs), intent(in) :: self
    end function iface_hasNext

    function iface_get(self) result (obj)
      !< returns current object
      import shr_iterator_abs
      class(shr_iterator_abs), intent(in) :: self
      class(*), allocatable :: obj
    end function iface_get
  end interface

contains

end module shr_Iterator_mod

