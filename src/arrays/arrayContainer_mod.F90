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
  use shr_arrayDim_mod, only: shr_arrayDim, shr_arrayDimContainer

  implicit none

  private

  public :: shr_arrayContainer


  type, abstract :: shr_arrayContainer
    integer :: ndims !< total number of dimensions
    type(shr_arrayDimContainer), allocatable :: dimensions(:)
  contains
    procedure :: getSize
    procedure :: getDims

    procedure :: init 

    ! copy
    procedure(iface_copy_scalar_rsp), deferred :: copy_scalar_rsp
    procedure(iface_copy_array_raw_rsp_1), deferred :: copy_array_raw_rsp_1
    procedure(iface_copy_arrayContainer), deferred :: copy_arrayContainer

    generic, public :: assignment(=) => copy_scalar_rsp, copy_arrayContainer, &
            copy_array_raw_rsp_1

    ! add
    procedure(iface_add_scalar_rsp), deferred :: add_scalar_rsp
    procedure(iface_add_array_raw_rsp_1), deferred :: add_array_raw_rsp_1
    procedure(iface_add_arrayContainer), deferred :: add_arrayContainer

    generic, public :: operator(+) => add_scalar_rsp, add_arrayContainer, &
            add_array_raw_rsp_1
!    procedure(iface_sub_arrayContainer), deferred :: sub_arrayContainer
!    procedure(iface_div_arrayContainer), deferred :: div_arrayContainer
!    procedure(iface_mul_arrayContainer), deferred :: mul_arrayContainer
  end type


  abstract interface
    ! add
    pure function iface_add_arrayContainer(left, right) Result(total)
      import :: shr_arrayContainer
      class(shr_arrayContainer), intent(in) :: left, right
      class(shr_arrayContainer), allocatable :: total
    end function iface_add_arrayContainer

    pure function iface_add_array_raw_rsp_1(left, right) Result(total)
      import :: shr_arrayContainer, sp
      class(shr_arrayContainer), intent(in) :: left
      real(kind=sp), intent(in) :: right(:)
      class(shr_arrayContainer), allocatable :: total
    end function iface_add_array_raw_rsp_1

    pure function iface_add_scalar_rsp(left, right) Result(total)
      import :: shr_arrayContainer, sp
      class(shr_arrayContainer), intent(in) :: left
      real(kind=sp), intent(in) :: right
      class(shr_arrayContainer), allocatable :: total
    end function iface_add_scalar_rsp


    ! copy
    pure subroutine iface_copy_scalar_rsp(self, other)
      import :: shr_arrayContainer, sp
      class(shr_arrayContainer), intent(inout) :: self
      real(kind=sp), intent(in) :: other
    end subroutine iface_copy_scalar_rsp

    pure subroutine iface_copy_array_raw_rsp_1(self, other)
      import :: shr_arrayContainer, sp
      class(shr_arrayContainer), intent(inout) :: self
      real(kind=sp), intent(in) :: other(:)
    end subroutine iface_copy_array_raw_rsp_1

    pure subroutine iface_copy_arrayContainer(self, other)
      import :: shr_arrayContainer
      class(shr_arrayContainer), intent(inout) :: self
      class(shr_arrayContainer), intent(in) :: other
    end subroutine iface_copy_arrayContainer
  end interface

contains

  pure integer function getSize(self)
    !< array dimensions
    class(shr_arrayContainer), intent(in) :: self
    getSize = self % ndims
  end function getSize


  pure integer function getDims(self)
    !< total number of dimensions
    class(shr_arrayContainer), intent(in) :: self
    getDims = self % ndims
  end function getDims


  pure subroutine init(self, dimensions)
    !< initialize shr_arrayContainer
    class(shr_arrayContainer), intent(inout) :: self
    type(shr_arrayDimContainer), intent(in) :: dimensions(:)
    !self % dimensions = dimensions
    allocate(self % dimensions, source = dimensions)
    self % ndims = size(dimensions)
  end subroutine init

end module SHR_arrayContainer_mod
