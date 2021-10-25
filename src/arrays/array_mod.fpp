!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  array_mod
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

#:include "../common.fpp"


module SHR_array_mod

  use SHR_precision_mod, only: sp, dp!, eqReal
!  use SHR_error_mod, only: raiseError 
  use SHR_strings_mod, only: string
  use SHR_arrayDim_mod, only: shr_arrayDim
  use shr_arrayContainer_mod, only: shr_arrayContainer

  use shr_arrayContainerAllocatable_mod, only: shr_arrayContainerAllocatable

  implicit none

  private

  public :: arrayAbs


  type, abstract :: arrayAbs !< interface to array
    type(string), allocatable :: name
    class(shr_arrayDim), allocatable :: dims(:) 
    type(string), allocatable :: units
    type(string), allocatable :: description

    class(shr_arrayContainer), allocatable :: data 
  contains
    !< add (arrayAbs, scalar, raw (matching dimensions) array)
    !< and sub, div, mul
!    procedure(deferred), (iface_add_arrayAbs_scalar) :: add_arrayAbs_scalar

    !< assignament (=)
!    procedure :: copy_arrayAbs_scalar
    !< arrayAbs (same type), scalar or raw array (matching dimensions)
    !<
    !< equal(arrayAbs (matching dimensions), raw array (matching dimesion's),
    !<       
  end type arrayAbs


  ! type specific array, real - kind single precision -
  type, extends(arrayAbs) :: shr_arrayRsp !< apply each type and kind
  contains
    !< operations: add, sub, div, mul
    !< type: real, int
    !< kind: single, double

    procedure :: init_array_rsp
    generic :: init => init_array_rsp

    ! copy
    procedure :: copy_scalar_rsp
    procedure :: copy_array_raw_rsp_1
    procedure :: copy_array

    generic, public :: assignment(=) => copy_scalar_rsp, copy_array, &
                copy_array_raw_rsp_1

    ! add
    procedure :: add_array_scalar_rsp
    procedure :: add_array_raw_rsp_1
    procedure :: add_array_array
    generic, public :: operator(+) => add_array_scalar_rsp, add_array_array, &
            add_array_raw_rsp_1
  end type shr_arrayRsp


contains
  ! init
  pure subroutine init_array_rsp(self, name, dimensions, units, description)
    !< shr_arrayRsp initialization
    class(shr_arrayRsp), intent(inout) :: self
    type(string), intent(in) :: name
    class(shr_arrayDim), intent(in) :: dimensions(:)
    type(string), intent(in) :: units
    type(string), intent(in) :: description

    self % name = name
    self % units = units
    self % description = description
    self % dims = dimensions

    allocate( shr_arrayContainerAllocatable :: self % data )


  end subroutine init_array_rsp


  ! copy
  pure subroutine copy_scalar_rsp(self, other)
      !< Copy to current array container allocatable
      !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
      class(shr_arrayRsp), intent(inout) :: self
      real(kind=sp), intent(in) :: other
      self % data = other
  end subroutine copy_scalar_rsp


  pure subroutine copy_array_raw_rsp_1(self, other)
    !< copy to current array 'self' from 'other' array
    class(shr_arrayRsp), intent(inout) :: self
    real(kind=sp), intent(in) :: other(:)
  end subroutine copy_array_raw_rsp_1


  pure subroutine copy_array(self, other)
      !< Copy to current array container allocatable
      !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
      class(shr_arrayRsp), intent(inout) :: self
      class(shr_arrayRsp), intent(in) :: other
      self % data = other % data
  end subroutine copy_array


  pure function add_array_scalar_rsp(left, right) Result(total)
    !< addition shr_arrayRsp and scalar rsp
    class(shr_arrayRsp), intent(in) :: left
    real(kind=sp), intent(in) :: right
    class(shr_arrayRsp), allocatable :: total !< output
    total = left % data + right
  end function add_array_scalar_rsp


  pure function add_array_raw_rsp_1(left, right) Result(total)
    !< addition shr_arrayRsp and scalar rsp
    class(shr_arrayRsp), intent(in) :: left
    real(kind=sp), intent(in) :: right(:)
    class(shr_arrayRsp), allocatable :: total !< output
  end function add_array_raw_rsp_1


  pure function add_array_array(left, right) Result(total)
    !< addition from shr_arrayRsp and shr_arrayRsp
    class(shr_arrayRsp), intent(in) :: left
    class(shr_arrayRsp), intent(in) :: right
    class(shr_arrayRsp), allocatable :: total !< output
    total = left % data + right % data
  end function add_array_array

end module SHR_array_mod
