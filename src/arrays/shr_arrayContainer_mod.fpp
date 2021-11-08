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

#:include "../common.fpp"


module SHR_arrayContainer_mod

  use SHR_precision_mod, only: sp, dp!, eqReal
!  use SHR_error_mod, only: raiseError 
!  use SHR_strings_mod, only: string
  use shr_arrayDim_mod, only: shr_arrayDim, shr_arrayDimContainer

  implicit none

  private

  public :: shr_arrayContainer

#:for _, _, IHEADER  in ALL_KINDS_TYPES
  public :: shr_arrayContainer${IHEADER}$
#:endfor


  type, abstract :: shr_arrayContainer
    integer :: ndims !< total number of dimensions
    type(shr_arrayDimContainer), allocatable :: dimensions(:)
  contains
    procedure :: getSize
    procedure :: getDims

    procedure :: init 
  end type shr_arrayContainer


#:for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES
  !
  ! ${IHEADER}$, ${IKIND}$, ${ITYPE}$
  !
  type, extends(shr_arrayContainer), abstract :: shr_arrayContainer${IHEADER}$
  contains

    ! copy (arrayContainer = scalar value)
#:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
    procedure(iface_arrayContainer${IHEADER}$_copy_scalar_${IHEADERSRC}$), deferred :: &
    copy_arrayContainer${IHEADER}$_copy_scalar_${IHEADERSRC}$
#:endfor

    ! copy (arrayContainer = <type, kind> array)
#:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
  #:for RANK in RANKS
    procedure(iface_arrayContainer${IHEADER}$_copy_array_raw_${IHEADERSRC}$_${RANK}$), deferred :: &
    copy_arrayContainer${IHEADER}$_copy_array_raw_${IHEADERSRC}$_${RANK}$
  #:endfor
#:endfor

    ! copy (arrayContainer = arrayContainer)
    procedure(iface_arrayContainer${IHEADER}$_copy_arrayContainer), deferred :: copy_arrayContainer${IHEADER}$_copy_arrayContainer
    
    ! reverse copy (<type, kind> array = arrayContainer)
  #:for RANK in RANKS
    #:for _, _, IHEADERTGT in ALL_KINDS_TYPES
    procedure(iface_copy_raw_${IHEADERTGT}$_${RANK}$_to_arrayContainer${IHEADER}$), deferred, pass(self) :: copy_raw_${IHEADERTGT}$_${RANK}$_to_arrayContainer${IHEADER}$
    #:endfor
  #:endfor

  #:for _, _, IHEADERSRC in ALL_KINDS_TYPES
    generic, public :: assignment(=) => copy_arrayContainer${IHEADER}$_copy_scalar_${IHEADERSRC}$
  #:endfor
    generic, public :: assignment(=) => copy_arrayContainer${IHEADER}$_copy_arrayContainer
  #:for RANK in RANKS
    #:for _, _, IHEADERSRC in ALL_KINDS_TYPES
      generic, public :: assignment(=) => copy_arrayContainer${IHEADER}$_copy_array_raw_${IHEADERSRC}$_${RANK}$
    #:endfor
    #:for _, _, IHEADERTGT in ALL_KINDS_TYPES
      generic, public :: assignment(=) => copy_raw_${IHEADERTGT}$_${RANK}$_to_arrayContainer${IHEADER}$
    #:endfor
  #:endfor

    ! equal (arrayContainer == arrayContainer)
    procedure(iface_equal_arrayContainer${IHEADER}$_equal_arrayContainer${IHEADER}$), deferred :: equal_arrayContainer${IHEADER}$_equal_arrayContainer${IHEADER}$

    ! equal (arrayContainer == <type, kind> scalar)
#:for _, _, IHEADERSRC in ALL_KINDS_TYPES
    procedure(iface_equal_arrayContainer${IHEADER}$_equal_scalar_${IHEADERSRC}$), deferred :: &
    equal_arrayContainer${IHEADER}$_equal_scalar_${IHEADERSRC}$
#:endfor

  #:for RANK in RANKS
    ! equal (arrayContainer == <type, kind> array)
    #:for _, _, IHEADERSRC in ALL_KINDS_TYPES
    procedure(iface_equal_arrayContainer${IHEADER}$_equal_array_raw_${IHEADERSRC}$_${RANK}$), deferred :: &
    equal_arrayContainer${IHEADER}$_equal_array_raw_${IHEADERSRC}$_${RANK}$
    #:endfor
  #:endfor

  #:for _, _, IHEADERSRC in ALL_KINDS_TYPES
    generic, public :: operator(==) => equal_arrayContainer${IHEADER}$_equal_scalar_${IHEADERSRC}$
  #:endfor 
    generic, public :: operator(==) => equal_arrayContainer${IHEADER}$_equal_arrayContainer${IHEADER}$
  #:for RANK in RANKS
    #:for _, _, IHEADERSRC in ALL_KINDS_TYPES
    generic, public :: operator(==) => equal_arrayContainer${IHEADER}$_equal_array_raw_${IHEADERSRC}$_${RANK}$
    #:endfor
  #:endfor

  #:for OP_NAME, OP_SYMB in OPERATOR_TYPES
    ! ${OP_NAME}$ (${OP_SYMB}$)
    ! ${OP_NAME}$ (${OP_SYMB}$) (arrayContainer op <type, kind> scalar)
    #:for _, _, IHEADERSRC in ALL_KINDS_TYPES
    procedure(iface_${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_scalar_${IHEADERSRC}$), deferred :: &
    ${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_scalar_${IHEADERSRC}$
    #:endfor

    #:for _, _, IHEADERSRC in ALL_KINDS_TYPES
      #:for RANK in RANKS
    ! ${OP_NAME}$ (${OP_SYMB}$) (arrayContainer op <type, kind> array)
    procedure(iface_${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_array_raw_${IHEADERSRC}$_${RANK}$), deferred :: &
    ${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_array_raw_${IHEADERSRC}$_${RANK}$
      #:endfor
    #:endfor

    ! ${OP_NAME}$ (${OP_SYMB}$) (arrayContainer op arrayContainer)
    procedure(iface_${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_arrayContainer), deferred :: ${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_arrayContainer

    #:for _, _, IHEADERSRC in ALL_KINDS_TYPES
    generic, public :: operator(${OP_SYMB}$) => ${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_scalar_${IHEADERSRC}$
    #:endfor
    generic, public :: operator(${OP_SYMB}$) => ${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_arrayContainer 
    #:for _, _, IHEADERSRC in ALL_KINDS_TYPES
      #:for RANK in RANKS
    generic, public :: operator(${OP_SYMB}$) =>  ${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_array_raw_${IHEADERSRC}$_${RANK}$
      #:endfor
    #:endfor
  #:endfor

  end type
#:endfor
! for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES


  abstract interface
#:for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES
  #:for IKINDSRC, ITYPESRC, IHEADERSRC  in ALL_KINDS_TYPES
    ! equal
    elemental logical function iface_equal_arrayContainer${IHEADER}$_equal_scalar_${IHEADERSRC}$(self, other)
      import :: shr_arrayContainer${IHEADER}$, ${IKINDSRC}$
      !< true if self and other are the same
      class(shr_arraycontainer${IHEADER}$), intent(in) :: self
      ${ITYPESRC}$, intent(in) :: other
    end function iface_equal_arrayContainer${IHEADER}$_equal_scalar_${IHEADERSRC}$
  #:endfor

#:for RANK in RANKS
  #:for IKINDSRC, ITYPESRC, IHEADERSRC  in ALL_KINDS_TYPES
    pure logical function iface_equal_arrayContainer${IHEADER}$_equal_array_raw_${IHEADERSRC}$_${RANK}$(self, other)
      import :: shr_arrayContainer${IHEADER}$, ${IKINDSRC}$
      !< true if self and other are the same
      class(shr_arraycontainer${IHEADER}$), intent(in) :: self
      ${ITYPESRC}$, intent(in) :: other${ranksuffix(RANK)}$
    end function iface_equal_arrayContainer${IHEADER}$_equal_array_raw_${IHEADERSRC}$_${RANK}$
  #:endfor
#:endfor

    elemental logical function iface_equal_arrayContainer${IHEADER}$_equal_arrayContainer${IHEADER}$(self, other)
      import :: shr_arrayContainer${IHEADER}$, shr_arraycontainer
      !< true if self and other are the same
      class(shr_arraycontainer${IHEADER}$), intent(in) :: self
      class(shr_arraycontainer), intent(in) :: other
    end function iface_equal_arrayContainer${IHEADER}$_equal_arrayContainer${IHEADER}$

#:for OP_NAME, OP_SYMB in OPERATOR_TYPES

    ! ${OP_NAME}$ (${OP_SYMB}$)
    ! arrayContainer op arrayContainer
    pure function iface_${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_arrayContainer(left, right) Result(total)
      import :: shr_arrayContainer${IHEADER}$, shr_arrayContainer
      class(shr_arrayContainer${IHEADER}$), intent(in) :: left
      class(shr_arrayContainer), intent(in) :: right
      class(shr_arrayContainer${IHEADER}$), allocatable :: total
    end function iface_${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_arrayContainer

    ! arrayContainer op <type, kind> array
  #:for IKINDSRC, ITYPESRC, IHEADERSRC  in ALL_KINDS_TYPES
    #:for RANK in RANKS
    pure function iface_${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_array_raw_${IHEADERSRC}$_${RANK}$(left, right) Result(total)
      import :: shr_arrayContainer${IHEADER}$, ${IKINDSRC}$
      class(shr_arrayContainer${IHEADER}$), intent(in) :: left
      ${ITYPESRC}$, intent(in) :: right${ranksuffix(RANK)}$
      class(shr_arrayContainer${IHEADER}$), allocatable :: total
    end function iface_${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_array_raw_${IHEADERSRC}$_${RANK}$
    #:endfor
  #:endfor

    ! arrayContainer op <type, kind> scalar
  #:for IKINDSRC, ITYPESRC, IHEADERSRC  in ALL_KINDS_TYPES
    pure function iface_${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_scalar_${IHEADERSRC}$(left, right) Result(total)
      import :: shr_arrayContainer${IHEADER}$, ${IKINDSRC}$
      class(shr_arrayContainer${IHEADER}$), intent(in) :: left
      ${ITYPESRC}$, intent(in) :: right
      class(shr_arrayContainer${IHEADER}$), allocatable :: total
    end function iface_${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_scalar_${IHEADERSRC}$
  #:endfor
#:endfor


    ! copy
#:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
    pure subroutine iface_arrayContainer${IHEADER}$_copy_scalar_${IHEADERSRC}$(self, other)
      import :: shr_arrayContainer${IHEADER}$, ${IKINDSRC}$
      class(shr_arrayContainer${IHEADER}$), intent(inout) :: self
      ${ITYPESRC}$, intent(in) :: other
    end subroutine iface_arrayContainer${IHEADER}$_copy_scalar_${IHEADERSRC}$
#:endfor

#:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
  #:for RANK in RANKS
    pure subroutine iface_arrayContainer${IHEADER}$_copy_array_raw_${IHEADERSRC}$_${RANK}$(self, other)
      import :: shr_arrayContainer${IHEADER}$, ${IKINDSRC}$
      class(shr_arrayContainer${IHEADER}$), intent(inout) :: self
      ${ITYPESRC}$, intent(in) :: other${ranksuffix(RANK)}$
    end subroutine iface_arrayContainer${IHEADER}$_copy_array_raw_${IHEADERSRC}$_${RANK}$
  #:endfor
#:endfor

#:for RANK in RANKS
  #:for IKINDTGT, ITYPETGT, IHEADERTGT in ALL_KINDS_TYPES
    pure subroutine iface_copy_raw_${IHEADERTGT}$_${RANK}$_to_arrayContainer${IHEADER}$(other, self)
      import :: shr_arrayContainer${IHEADER}$, ${IKIND}$, ${IKINDTGT}$
      ${ITYPETGT}$, allocatable, intent(inout) :: other${ranksuffix(RANK)}$
      class(shr_arrayContainer${IHEADER}$), intent(in) :: self
    end subroutine iface_copy_raw_${IHEADERTGT}$_${RANK}$_to_arrayContainer${IHEADER}$
  #:endfor
#:endfor

    pure subroutine iface_arrayContainer${IHEADER}$_copy_arrayContainer(self, other)
      import :: shr_arrayContainer${IHEADER}$, shr_arrayContainer
      class(shr_arrayContainer${IHEADER}$), intent(inout) :: self
      class(shr_arrayContainer), intent(in) :: other
    end subroutine iface_arrayContainer${IHEADER}$_copy_arrayContainer
#:endfor
! for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES
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
