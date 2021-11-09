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

    !< array data
    class(shr_arrayContainer), allocatable :: data 
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
  contains

    procedure :: init_array_${IHEADER}$
    generic :: init => init_array_${IHEADER}$

    ! copy (shr_array = <type, kind> scalar)
#:for _, _, IHEADERSRC in ALL_KINDS_TYPES
    procedure :: copy_array${IHEADER}$_scalar_${IHEADERSRC}$
#:endfor

    ! copy (shr_array = <type, kind> array)
#:for _, _, IHEADERSRC in ALL_KINDS_TYPES
  #:for RANK in RANKS
    procedure :: copy_array${IHEADER}$_to_raw_${IHEADERSRC}$_${RANK}$
  #:endfor
#:endfor

    ! reverse copy (<type, kind> array = shr_array)
#:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
  #:for RANK in RANKS
    procedure, pass(self) :: copy_raw_${IHEADERSRC}$_${RANK}$_to_array${IHEADER}$
  #:endfor
#:endfor

    ! copy (shr_array = shr_array)
    procedure :: copy_array${IHEADER}$_array${IHEADER}$

#:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
    generic, public :: assignment(=) => copy_array${IHEADER}$_scalar_${IHEADERSRC}$
#:endfor
    generic, public :: assignment(=) => copy_array${IHEADER}$_array${IHEADER}$

#:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
  #:for RANK in RANKS
    generic, public :: assignment(=) => copy_array${IHEADER}$_to_raw_${IHEADERSRC}$_${RANK}$
  #:endfor
#:endfor

#:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
  #:for RANK in RANKS
    generic, public :: assignment(=) => copy_raw_${IHEADERSRC}$_${RANK}$_to_array${IHEADER}$
  #:endfor
#:endfor

  #:for OP_NAME, OP_SYMB in OPERATOR_TYPES
    ! ${OP_NAME}$ (${OP_SYMB}$)
    ! ${OP_NAME}$ shr_array op <type, kind> scalar
    #:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
    procedure :: ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADERSRC}$
    #:endfor

    ! ${OP_NAME}$ shr_array op <type, kind> array
    #:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
      #:for RANK in RANKS
      procedure :: ${OP_NAME}$_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$
      #:endfor
    #:endfor

    ! ${OP_NAME}$ shr_array op shr_array
    procedure :: ${OP_NAME}$_array${IHEADER}$_array${IHEADER}$

    generic, public :: operator(${OP_SYMB}$) => ${OP_NAME}$_array${IHEADER}$_array${IHEADER}$
    #:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
    generic, public :: operator(${OP_SYMB}$) => ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADERSRC}$
    #:endfor

    #:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
      #:for RANK in RANKS
    generic, public :: operator(${OP_SYMB}$) =>  ${OP_NAME}$_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$
      #:endfor
    #:endfor

    ! OP_NAME, OP_SYMB
  #:endfor

    ! equal
    ! equal (shr_array = <type, kind> scalar)
  #:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
    procedure :: equal_array${IHEADER}$_scalar_${IHEADERSRC}$
  #:endfor

    ! equal (shr_array = <type, kind> array)
  #:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
    #:for RANK in RANKS
    procedure :: equal_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$
    #:endfor
  #:endfor

    ! equal (shr_array = shr_arry)
    procedure :: equal_array${IHEADER}$_array${IHEADER}$

  #:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
    generic, public :: operator(==) => equal_array${IHEADER}$_scalar_${IHEADERSRC}$
  #:endfor
    generic, public :: operator(==) => equal_array${IHEADER}$_array${IHEADER}$
  #:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
    #:for RANK in RANKS
    generic, public :: operator(==) => equal_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$
    #:endfor
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


#:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
  ! copy (shr_array = <type, kind> scalar)
  pure subroutine copy_array${IHEADER}$_scalar_${IHEADERSRC}$(self, other)
      !< Copy to current array container allocatable
      !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
      class(shr_array${IHEADER}$), intent(inout) :: self
      ${ITYPESRC}$, intent(in) :: other

      select type(data => self % data)
      type is (shr_arrayContainer${IHEADER}$Allocatable)
        data = other
      class default
        !< unexpected class found
      end select
  end subroutine copy_array${IHEADER}$_scalar_${IHEADERSRC}$
#:endfor


#:for IKINDTGT, ITYPETGT, IHEADERTGT in ALL_KINDS_TYPES
  ! reverse copy ( <type, kind> array = shr_array)
  #:for RANK in RANKS
  pure subroutine copy_raw_${IHEADERTGT}$_${RANK}$_to_array${IHEADER}$(other, self)
    !< copy from array${IHEADER}$ 'self' to 'other' array
    !< raw array = array${IHEADER}$
    ${ITYPETGT}$, allocatable, intent(inout) :: other${ranksuffix(RANK)}$
    class(shr_array${IHEADER}$), intent(in) :: self

    select type(data => self % data)
    type is (shr_arrayContainer${IHEADER}$Allocatable)
      other = data
    class default
      !< unexpected class found
    end select
  end subroutine copy_raw_${IHEADERTGT}$_${RANK}$_to_array${IHEADER}$
  #:endfor
#:endfor


#:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
  #:for RANK in RANKS
  ! copy (shr_array = <type, kind> array)
  pure subroutine copy_array${IHEADER}$_to_raw_${IHEADERSRC}$_${RANK}$(self, other)
    !< copy to current array 'self' from 'other' array
    !< array${IHEADER}$ = raw array
    class(shr_array${IHEADER}$), intent(inout) :: self
    ${ITYPESRC}$, intent(in) :: other${ranksuffix(RANK)}$

    select type(data => self % data)
    type is (shr_arrayContainer${IHEADER}$Allocatable)
      data = other
    class default
      !< unexpected class found
    end select
  end subroutine copy_array${IHEADER}$_to_raw_${IHEADERSRC}$_${RANK}$
  #:endfor
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

  #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
  pure function ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADERSRC}$(left, right) Result(total)
    !< addition shr_arrayRsp and scalar rsp
    class(shr_array${IHEADER}$), intent(in) :: left
    ${ITYPESRC}$, intent(in) :: right
    class(shr_array${IHEADER}$), allocatable :: total !< output

    select type(data => left % data)
    type is (shr_arrayContainer${IHEADER}$Allocatable)
      total = data ${OP_SYMB}$ right
    class default
      !< unexpected class found
    end select
  end function ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADERSRC}$
  #:endfor


  #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
    #:for RANK in RANKS

  pure function ${OP_NAME}$_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$(left, right) Result(total)
    !< addition shr_array${IHEADER}$ and scalar ${IHEADER}$
    class(shr_array${IHEADER}$), intent(in) :: left
    ${ITYPESRC}$, intent(in) :: right${ranksuffix(RANK)}$
    class(shr_array${IHEADER}$), allocatable :: total !< output

    select type(data => left % data)
    type is (shr_arrayContainer${IHEADER}$Allocatable)
      total % data = data ${OP_SYMB}$ right
    class default
      !< unexpected class found
    end select
  end function ${OP_NAME}$_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$

    #:endfor
  #:endfor


  pure function ${OP_NAME}$_array${IHEADER}$_array${IHEADER}$(left, right) Result(total)
    !< addition from shr_arrayRsp and shr_arrayRsp
    class(shr_array${IHEADER}$), intent(in) :: left
    class(shr_array${IHEADER}$), intent(in) :: right
    class(shr_array${IHEADER}$), allocatable :: total !< output

    select type(data => left % data)
    type is (shr_arrayContainer${IHEADER}$Allocatable)
      total = data ${OP_SYMB}$ right % data
    class default
      !< unexpected class found
    end select
  end function ${OP_NAME}$_array${IHEADER}$_array${IHEADER}$

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
    select type(data => self % data)
    type is (shr_arrayContainer${IHEADER}$Allocatable)
      hasSameData = (data == other % data)
    class default
      !< unexpected class found
    end select

    equal_array${IHEADER}$_array${IHEADER}$ = hasSameData
  end function equal_array${IHEADER}$_array${IHEADER}$


  #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
  elemental logical function equal_array${IHEADER}$_scalar_${IHEADERSRC}$(self, other)
    !< true if self and other are the same
    class(shr_array${IHEADER}$), intent(in) :: self
    ${ITYPESRC}$, intent(in) :: other
    select type(data => self % data)
    type is (shr_arrayContainer${IHEADER}$Allocatable)
      equal_array${IHEADER}$_scalar_${IHEADERSRC}$ = (data == other)
    class default
      !< unexpected class found
    end select
  end function equal_array${IHEADER}$_scalar_${IHEADERSRC}$
  #:endfor


  #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
    #:for RANK in RANKS
    pure logical function equal_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$(self, other)
      !< true if self and other are the same
      class(shr_array${IHEADER}$), intent(in) :: self
      ${ITYPESRC}$, intent(in) :: other${ranksuffix(RANK)}$
      select type(data => self % data)
      type is (shr_arrayContainer${IHEADER}$Allocatable)
        equal_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$ = all(data == other)
      class default
        !< unexpected class found
      end select !
    end function equal_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$
    #:endfor
  #:endfor
#:endfor

end module SHR_array_mod
