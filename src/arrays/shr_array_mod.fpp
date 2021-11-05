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
#:for _, _, IHEADER  in ALL_KINDS_TYPES
  use shr_arrayContainer_mod, only: shr_arrayContainer${IHEADER}$
#:endfor

#:for _, _, IHEADER  in ALL_KINDS_TYPES
  use shr_arrayContainerAllocatable_mod, only: shr_arrayContainer${IHEADER}$Allocatable
#:endfor

  implicit none

  private

  public :: shr_array
#:for _, _, IHEADER  in ALL_KINDS_TYPES
  public :: shr_array${IHEADER}$
#:endfor


  type, abstract :: shr_array !< interface to array
    !< array descriptor
    type(string), allocatable :: name
    type(shr_arrayDimContainer), allocatable :: dims(:) 
    type(string), allocatable :: units
    type(string), allocatable :: description
  contains
    procedure :: getName => getName_array
    procedure :: getSize => getSize_array
    procedure :: getDims => getDims_array
    procedure :: getUnits => getUnits_array
    procedure :: getDescription => getDescription_array
  end type shr_array


#:for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES
  !
  ! ${IHEADER}$, ${ITYPE}$, ${IKIND}$
  !
  type, extends(shr_array) :: shr_array${IHEADER}$ !< apply each type and kind

    !< array data
    class(shr_arrayContainer${IHEADER}$), allocatable :: data 
  contains

    procedure :: init_array_${IHEADER}$
    generic :: init => init_array_${IHEADER}$

    ! copy
    procedure :: copy_array${IHEADER}$_scalar_${IHEADER}$
  #:for RANK in RANKS
    procedure :: copy_array${IHEADER}$_to_raw_${IHEADER}$_${RANK}$
  #:endfor
  #:for RANK in RANKS
    procedure, pass(self) :: copy_raw_${IHEADER}$_${RANK}$_to_array${IHEADER}$
  #:endfor
    procedure :: copy_array${IHEADER}$_array${IHEADER}$

    generic, public :: assignment(=) => copy_array${IHEADER}$_scalar_${IHEADER}$, copy_array${IHEADER}$_array${IHEADER}$!, &
  #:for RANK in RANKS
    generic, public :: assignment(=) => copy_array${IHEADER}$_to_raw_${IHEADER}$_${RANK}$
  #:endfor
  #:for RANK in RANKS
    generic, public :: assignment(=) => copy_raw_${IHEADER}$_${RANK}$_to_array${IHEADER}$
  #:endfor

  #:for OP_NAME, OP_SYMB in OPERATOR_TYPES
    ! ${OP_NAME}$ (${OP_SYMB}$)
    procedure :: ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADER}$
    #:for RANK in RANKS
    procedure :: ${OP_NAME}$_array${IHEADER}$_raw_${IHEADER}$_${RANK}$
    #:endfor
    procedure :: ${OP_NAME}$_array${IHEADER}$_array_${IHEADER}$
    generic, public :: operator(${OP_SYMB}$) => ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADER}$, ${OP_NAME}$_array${IHEADER}$_array_${IHEADER}$!, &
    #:for RANK in RANKS
    generic, public :: operator(${OP_SYMB}$) =>  ${OP_NAME}$_array${IHEADER}$_raw_${IHEADER}$_${RANK}$
    #:endfor
  #:endfor

    ! equal
    procedure :: equal_array${IHEADER}$_scalar_${IHEADER}$
  #:for RANK in RANKS
    procedure :: equal_array${IHEADER}$_raw_${IHEADER}$_${RANK}$
  #:endfor
    procedure :: equal_array${IHEADER}$_array${IHEADER}$
    generic, public :: operator(==) => equal_array${IHEADER}$_scalar_${IHEADER}$, equal_array${IHEADER}$_array${IHEADER}$
  #:for RANK in RANKS
    generic, public :: operator(==) => equal_array${IHEADER}$_raw_${IHEADER}$_${RANK}$
  #:endfor
  end type shr_array${IHEADER}$
#:endfor
! for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES


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


#:for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES
  !
  ! ${IHEADER}$, ${ITYPE}$, ${IKIND}$
  !
  pure subroutine init_array_${IHEADER}$(self, name, dimensions, units, description)
    !< shr_arrayRsp initialization
    class(shr_array${IHEADER}$), intent(inout) :: self
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
    allocate( shr_arrayContainer${IHEADER}$Allocatable :: self % data )
    call self % data % init(dimensions)

  end subroutine init_array_${IHEADER}$


  ! copy
  pure subroutine copy_array${IHEADER}$_scalar_${IHEADER}$(self, other)
      !< Copy to current array container allocatable
      !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
      class(shr_array${IHEADER}$), intent(inout) :: self
      ${ITYPE}$, intent(in) :: other
      self % data = other
  end subroutine copy_array${IHEADER}$_scalar_${IHEADER}$


  #:for RANK in RANKS
  pure subroutine copy_raw_${IHEADER}$_${RANK}$_to_array${IHEADER}$(other, self)
    !< copy from array${IHEADER}$ 'self' to 'other' array
    !< raw array = array${IHEADER}$
    ${ITYPE}$, allocatable, intent(inout) :: other${ranksuffix(RANK)}$
    class(shr_array${IHEADER}$), intent(in) :: self
    other = self % data
  end subroutine copy_raw_${IHEADER}$_${RANK}$_to_array${IHEADER}$
  #:endfor


  #:for RANK in RANKS
  pure subroutine copy_array${IHEADER}$_to_raw_${IHEADER}$_${RANK}$(self, other)
    !< copy to current array 'self' from 'other' array
    !< array${IHEADER}$ = raw array
    class(shr_array${IHEADER}$), intent(inout) :: self
    ${ITYPE}$, intent(in) :: other${ranksuffix(RANK)}$
    self % data = other
  end subroutine copy_array${IHEADER}$_to_raw_${IHEADER}$_${RANK}$
  #:endfor


  pure subroutine copy_array${IHEADER}$_array${IHEADER}$(self, other)
      !< Copy to current array container allocatable
      !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
      class(shr_array${IHEADER}$), intent(inout) :: self
      class(shr_array${IHEADER}$), intent(in) :: other
      if (.not. allocated(self % name)) allocate(self % name)
      self % name = other % getName()
      allocate(self % dims, source = other % getDims())
      if (.not. allocated(self % units)) allocate(self % units)
      self % units = other % getUnits()
      if (.not. allocated(self % description)) allocate(self % description)
      self % description = other % getDescription()
      allocate(self % data, source = other % data)
  end subroutine copy_array${IHEADER}$_array${IHEADER}$

  #:for OP_NAME, OP_SYMB in OPERATOR_TYPES
  !
  ! ${OP_NAME}$, ${OP_SYMB}$
  !

  pure function ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADER}$(left, right) Result(total)
    !< addition shr_arrayRsp and scalar rsp
    class(shr_array${IHEADER}$), intent(in) :: left
    ${ITYPE}$, intent(in) :: right
    class(shr_array${IHEADER}$), allocatable :: total !< output
    total = left % data ${OP_SYMB}$ right
  end function ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADER}$


  #:for RANK in RANKS
  pure function ${OP_NAME}$_array${IHEADER}$_raw_${IHEADER}$_${RANK}$(left, right) Result(total)
    !< addition shr_array${IHEADER}$ and scalar ${IHEADER}$
    class(shr_array${IHEADER}$), intent(in) :: left
    ${ITYPE}$, intent(in) :: right${ranksuffix(RANK)}$
    class(shr_array${IHEADER}$), allocatable :: total !< output
    total % data = left % data ${OP_SYMB}$ right
  end function ${OP_NAME}$_array${IHEADER}$_raw_${IHEADER}$_${RANK}$
  #:endfor


  pure function ${OP_NAME}$_array${IHEADER}$_array_${IHEADER}$(left, right) Result(total)
    !< addition from shr_arrayRsp and shr_arrayRsp
    class(shr_array${IHEADER}$), intent(in) :: left
    class(shr_array${IHEADER}$), intent(in) :: right
    class(shr_array${IHEADER}$), allocatable :: total !< output
    total = left % data ${OP_SYMB}$ right % data
  end function ${OP_NAME}$_array${IHEADER}$_array_${IHEADER}$

  ! OP_NAME, OP_SYMB in OPERATOR_TYPES  
  #:endfor

  !
  ! equal
  !
  elemental logical function equal_array${IHEADER}$_array${IHEADER}$(self, other)
    !< true if self and other are the same
    class(shr_array${IHEADER}$), intent(in) :: self
    class(shr_array${IHEADER}$), intent(in) :: other

    logical :: hasSameNAme, hasSameDims, hasSameUnits
    logical :: hasSameDescription, hasSameData

    equal_array${IHEADER}$_array${IHEADER}$ = .false.
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

    equal_array${IHEADER}$_array${IHEADER}$ = hasSameData
  end function equal_array${IHEADER}$_array${IHEADER}$


  elemental logical function equal_array${IHEADER}$_scalar_${IHEADER}$(self, other)
    !< true if self and other are the same
    class(shr_array${IHEADER}$), intent(in) :: self
    ${ITYPE}$, intent(in) :: other
    equal_array${IHEADER}$_scalar_${IHEADER}$ = (self % data == other)
  end function equal_array${IHEADER}$_scalar_${IHEADER}$


  #:for RANK in RANKS
  pure logical function equal_array${IHEADER}$_raw_${IHEADER}$_${RANK}$(self, other)
    !< true if self and other are the same
    class(shr_array${IHEADER}$), intent(in) :: self
    ${ITYPE}$, intent(in) :: other${ranksuffix(RANK)}$
    equal_array${IHEADER}$_raw_${IHEADER}$_${RANK}$ = all(self % data == other)
  end function equal_array${IHEADER}$_raw_${IHEADER}$_${RANK}$
  #:endfor
#:endfor

end module SHR_array_mod
