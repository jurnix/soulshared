!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  shr_arrayContainer_mod
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!> Common Array subroutines
!>
!> array class (real sp, real dp, int)
!> 
!------------------------------------------------------------------------------


module SHR_arrayContainer_mod

  use SHR_precision_mod, only: sp! dp, eqReal
!  use SHR_error_mod, only: raiseError 
!  use SHR_strings_mod, only: string

  implicit none

  private

  public :: shr_arrayContainer


  type, abstract :: shr_arrayContainer
  contains
    ! copy
    procedure(iface_copy_scalar), deferred :: copy_scalar 
    procedure(iface_copy_arrayContainer), deferred :: copy_arrayContainer

    generic, public :: assignment(=) => copy_scalar, copy_arrayContainer

    ! add
    procedure(iface_add_arrayContainer), deferred :: add_arrayContainer
    generic, public :: operator(+) => add_arrayContainer
!    procedure(iface_sub_arrayContainer), deferred :: sub_arrayContainer
!    procedure(iface_div_arrayContainer), deferred :: div_arrayContainer
!    procedure(iface_mul_arrayContainer), deferred :: mul_arrayContainer
  end type


  abstract interface
    pure function iface_add_arrayContainer(left, right) Result(total)
      import :: shr_arrayContainer
      class(shr_arrayContainer), intent(in) :: left, right
      class(shr_arrayContainer), allocatable :: total
    end function iface_add_arrayContainer

    pure subroutine iface_copy_scalar(self, other)
      import :: shr_arrayContainer, sp
      class(shr_arrayContainer), intent(inout) :: self
      real(kind=sp), intent(in) :: other
    end subroutine iface_copy_scalar

    pure subroutine iface_copy_arrayContainer(self, other)
      import :: shr_arrayContainer
      class(shr_arrayContainer), intent(inout) :: self
      class(shr_arrayContainer), intent(in) :: other
    end subroutine iface_copy_arrayContainer
  end interface

contains

end module SHR_arrayContainer_mod
