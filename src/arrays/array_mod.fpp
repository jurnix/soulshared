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
!> Allowed operations between types(and kind)
!>
!> operations: add, sub, div, mul
!> type: real, int
!> kind: single, double
!>
!> int: int + int
!> rsp: rsp + rsp
!>      rsp + int
!> dsp: dsp + dsp
!>      dsp + rsp
!>      dsp + int
!> 
!------------------------------------------------------------------------------

#:include "../common.fpp"


module SHR_array_mod

  use SHR_precision_mod, only: sp, dp!, eqReal
!  use SHR_error_mod, only: raiseError 
  use SHR_strings_mod, only: string
  use SHR_arrayDim_mod, only: shr_arrayDim, shr_arrayDimContainer
  use shr_arrayContainer_mod, only: shr_arrayContainer

  use shr_arrayContainerAllocatable_mod, only: shr_arrayContainerAllocatable

  implicit none

  private

  public :: arrayAbs, shr_arrayRsp


  type, abstract :: arrayAbs !< interface to array
    !< array descriptor
    type(string), allocatable :: name
!    class(shr_arrayDim), allocatable :: dims(:) 
    type(shr_arrayDimContainer), allocatable :: dims(:) 
    type(string), allocatable :: units
    type(string), allocatable :: description

    !< array data
    class(shr_arrayContainer), allocatable :: data 
  contains
    procedure :: getName => getName_arrayAbs
    procedure :: getSize => getSize_arrayAbs
    procedure :: getDims => getDims_arrayAbs
    procedure :: getUnits => getUnits_arrayAbs
    procedure :: getDescription => getDescription_arrayAbs
  end type arrayAbs


  ! type specific array, real - kind single precision -
  type, extends(arrayAbs) :: shr_arrayRsp !< apply each type and kind
  contains

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

    ! equal
    procedure :: equal_scalar_rsp
    procedure :: equal_array_raw_rsp_1
    procedure :: equal_array
    generic, public :: operator(==) => equal_scalar_rsp, equal_array, &
            equal_array_raw_rsp_1
  end type shr_arrayRsp


contains
  !
  ! arrayAbs
  !
   
  pure type(string) function getName_arrayAbs(self)
    !< returns how many dimensions 
    class(arrayAbs), intent(in) :: self
    getName_arrayAbs = self % name
  end function getName_arrayAbs

   
  pure integer function getSize_arrayAbs(self)
    !< returns how many dimensions 
    class(arrayAbs), intent(in) :: self
    getSize_arrayAbs = size(self % dims)
  end function getSize_arrayAbs


  pure function getDims_arrayAbs(self) result (dims)
    !< returns array dimensions as an allocatable array
    class(arrayAbs), intent(in) :: self
!    class(shr_arrayDim), allocatable :: dims(:) !< output
    type(shr_arrayDimContainer), allocatable :: dims(:) !< output
    if (allocated(dims)) deallocate(dims)
    allocate(dims, source = self % dims)
!    dims = self % dims
  end function getDims_arrayAbs


  pure type(string) function getUnits_arrayAbs(self)
    !< its returns its units
    class(arrayAbs), intent(in) :: self
    getUnits_arrayAbs = self % units
  end function getUnits_arrayAbs


  pure type(string) function getDescription_arrayAbs(self)
    !< it returns its description
    class(arrayAbs), intent(in) :: self
    getDescription_arrayAbs = self % description
  end function getDescription_arrayAbs


  !
  ! arrayRsp
  !
  ! init
  pure subroutine init_array_rsp(self, name, dimensions, units, description)
    !< shr_arrayRsp initialization
    class(shr_arrayRsp), intent(inout) :: self
    character(*), intent(in) :: name
!    class(shr_arrayDim), intent(in) :: dimensions(:)
    type(shr_arrayDimContainer), intent(in) :: dimensions(:)
    character(*), intent(in) :: units
    character(*), intent(in) :: description

    allocate(self % name)
    self % name = string(name)
    allocate(self % units)
    self % units = string(units)
    allocate(self % description)
    self % description = string(description)

!    self % dims = dimensions
    allocate(self % dims, source = dimensions)

    ! todo, creational design pattern?
    allocate( shr_arrayContainerAllocatable :: self % data )
    call self % data % init(dimensions)

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
    self % data = other
  end subroutine copy_array_raw_rsp_1


  pure subroutine copy_array(self, other)
      !< Copy to current array container allocatable
      !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
      class(shr_arrayRsp), intent(inout) :: self
      class(shr_arrayRsp), intent(in) :: other
      self % data = other % data
  end subroutine copy_array

  !
  ! add
  !
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
    total % data = left % data + right
  end function add_array_raw_rsp_1


  pure function add_array_array(left, right) Result(total)
    !< addition from shr_arrayRsp and shr_arrayRsp
    class(shr_arrayRsp), intent(in) :: left
    class(shr_arrayRsp), intent(in) :: right
    class(shr_arrayRsp), allocatable :: total !< output
    total = left % data + right % data
  end function add_array_array

  !
  ! equal
  !
  elemental logical function equal_array(self, other)
    !< true if self and other are the same
    class(shr_arrayRsp), intent(in) :: self
    class(shr_arrayRsp), intent(in) :: other

    logical :: hasSameNAme, hasSameDims, hasSameUnits
    logical :: hasSameDescription, hasSameData

    equal_array = .false.
    ! compare array descriptor
    hasSameName = self % getName() == other % getName()
    if (.not. hasSameName) return

    hasSameDims = all(self % getDims() == other % getDims())
    if (.not. hasSameDims) return

    hasSameUnits = self % getUnits() == other % getUnits()
    if (.not. hasSameUnits) return

    hasSameDescription = self % getDescription() == other % getDescription()
    if (.not. hasSameDescription) return

    ! compare data
    hasSameData = (self % data == other % data)

    equal_array = hasSameData
  end function equal_array


  elemental logical function equal_scalar_rsp(self, other)
    !< true if self and other are the same
    class(shr_arrayRsp), intent(in) :: self
    real(kind=sp), intent(in) :: other
    equal_scalar_rsp = (self % data == other)
  end function equal_scalar_rsp


  pure logical function equal_array_raw_rsp_1(self, other)
    !< true if self and other are the same
    class(shr_arrayRsp), intent(in) :: self
    real(kind=sp), intent(in) :: other(:)
    equal_array_raw_rsp_1 = all(self % data == other)
  end function equal_array_raw_rsp_1

end module SHR_array_mod
