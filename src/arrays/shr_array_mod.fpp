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

  public :: shr_array, shr_arrayRsp


  type, abstract :: shr_array !< interface to array
    !< array descriptor
    type(string), allocatable :: name
    type(shr_arrayDimContainer), allocatable :: dims(:) 
    type(string), allocatable :: units
    type(string), allocatable :: description

    !< array data
    class(shr_arrayContainer), allocatable :: data 
  contains
    procedure :: getName => getName_array
    procedure :: getSize => getSize_array
    procedure :: getDims => getDims_array
    procedure :: getUnits => getUnits_array
    procedure :: getDescription => getDescription_array
  end type shr_array


  ! type specific array, real - kind single precision -
  type, extends(shr_array) :: shr_arrayRsp !< apply each type and kind
  contains

    procedure :: init_array_rsp
    generic :: init => init_array_rsp

    ! copy
    procedure :: copy_scalar_rsp
#:for RANK in RANKS
    procedure :: copy_array_to_raw_rsp_${RANK}$
#:endfor
#:for RANK in RANKS
    procedure, pass(self) :: copy_raw_rsp_${RANK}$_to_array
#:endfor
    procedure :: copy_array

    generic, public :: assignment(=) => copy_scalar_rsp, copy_array!, &
#:for RANK in RANKS
    generic, public :: assignment(=) => copy_array_to_raw_rsp_${RANK}$
#:endfor
#:for RANK in RANKS
    generic, public :: assignment(=) => copy_raw_rsp_${RANK}$_to_array
#:endfor

    ! add
    procedure :: add_array_scalar_rsp
#:for RANK in RANKS
    procedure :: add_array_raw_rsp_${RANK}$
#:endfor
    procedure :: add_array_array
    generic, public :: operator(+) => add_array_scalar_rsp, add_array_array!, &
#:for RANK in RANKS
    generic, public :: operator(+) =>  add_array_raw_rsp_${RANK}$
#:endfor

    ! equal
    procedure :: equal_scalar_rsp
#:for RANK in RANKS
    procedure :: equal_array_raw_rsp_${RANK}$
#:endfor
    procedure :: equal_array
    generic, public :: operator(==) => equal_scalar_rsp, equal_array!, &
#:for RANK in RANKS
    generic, public :: operator(==) => equal_array_raw_rsp_${RANK}$
#:endfor
  end type shr_arrayRsp


contains
  !
  ! array
  !
   
  pure type(string) function getName_array(self)
    !< returns how many dimensions 
    class(shr_array), intent(in) :: self
    getName_array = self % name
  end function getName_array

   
  pure integer function getSize_array(self)
    !< returns how many dimensions 
    class(shr_array), intent(in) :: self
    getSize_array = size(self % dims)
  end function getSize_array


  pure function getDims_array(self) result (dims)
    !< returns array dimensions as an allocatable array
    class(shr_array), intent(in) :: self
!    class(shr_arrayDim), allocatable :: dims(:) !< output
    type(shr_arrayDimContainer), allocatable :: dims(:) !< output
    if (allocated(dims)) deallocate(dims)
    allocate(dims, source = self % dims)
!    dims = self % dims
  end function getDims_array


  pure type(string) function getUnits_array(self)
    !< its returns its units
    class(shr_array), intent(in) :: self
    getUnits_array = self % units
  end function getUnits_array


  pure type(string) function getDescription_array(self)
    !< it returns its description
    class(shr_array), intent(in) :: self
    getDescription_array = self % description
  end function getDescription_array


  !
  ! arrayRsp
  !
  ! init
  pure subroutine init_array_rsp(self, name, dimensions, units, description)
    !< shr_arrayRsp initialization
    class(shr_arrayRsp), intent(inout) :: self
    character(*), intent(in) :: name
    type(shr_arrayDimContainer), intent(in) :: dimensions(:)
    character(*), intent(in) :: units
    character(*), intent(in) :: description

    allocate(self % name)
    self % name = string(name)
    allocate(self % units)
    self % units = string(units)
    allocate(self % description)
    self % description = string(description)

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


#:for RANK in RANKS
  pure subroutine copy_raw_rsp_${RANK}$_to_array(other, self)
    !< copy from arrayRsp 'self' to 'other' array
    !< raw array = arrayRsp
    real(kind=sp), allocatable, intent(inout) :: other${ranksuffix(RANK)}$
    class(shr_arrayRsp), intent(in) :: self
!    other = self % data
  end subroutine copy_raw_rsp_${RANK}$_to_array
#:endfor


#:for RANK in RANKS
  pure subroutine copy_array_to_raw_rsp_${RANK}$(self, other)
    !< copy to current array 'self' from 'other' array
    !< arrayRsp = raw array
    class(shr_arrayRsp), intent(inout) :: self
    real(kind=sp), intent(in) :: other${ranksuffix(RANK)}$
!    self % data = other
  end subroutine copy_array_to_raw_rsp_${RANK}$
#:endfor


  pure subroutine copy_array(self, other)
      !< Copy to current array container allocatable
      !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
      class(shr_arrayRsp), intent(inout) :: self
      class(shr_arrayRsp), intent(in) :: other
      self % name = other % getName()
      self % dims = other % getDims()
      self % units = other % getUnits()
      self % description = other % getDescription()
      allocate(self % data, source = other % data)
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


#:for RANK in RANKS
  pure function add_array_raw_rsp_${RANK}$(left, right) Result(total)
    !< addition shr_arrayRsp and scalar rsp
    class(shr_arrayRsp), intent(in) :: left
    real(kind=sp), intent(in) :: right${ranksuffix(RANK)}$
    class(shr_arrayRsp), allocatable :: total !< output
!    total % data = left % data + right
  end function add_array_raw_rsp_${RANK}$
#:endfor


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


#:for RANK in RANKS
  pure logical function equal_array_raw_rsp_${RANK}$(self, other)
    !< true if self and other are the same
    class(shr_arrayRsp), intent(in) :: self
    real(kind=sp), intent(in) :: other${ranksuffix(RANK)}$
!    equal_array_raw_rsp_${RANK}$ = all(self % data == other)
  end function equal_array_raw_rsp_${RANK}$
#:endfor

end module SHR_array_mod
