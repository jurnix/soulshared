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

    ! copy
    procedure(iface_copy_scalar_${IHEADER}$), deferred :: copy_scalar_${IHEADER}$
  #:for RANK in RANKS
    procedure(iface_copy_array_raw_${IHEADER}$_${RANK}$), deferred :: copy_array_raw_${IHEADER}$_${RANK}$
  #:endfor
    procedure(iface_copy_arrayContainer${IHEADER}$), deferred :: copy_arrayContainer${IHEADER}$
    
    ! reverse copy
  #:for RANK in RANKS
    procedure(iface_copy_raw_${IHEADER}$_${RANK}$_to_array), deferred, pass(self) :: copy_raw_${IHEADER}$_${RANK}$_to_array
  #:endfor

    generic, public :: assignment(=) => copy_scalar_${IHEADER}$, copy_arrayContainer${IHEADER}$!, &
  #:for RANK in RANKS
    generic, public :: assignment(=) => copy_array_raw_${IHEADER}$_${RANK}$, copy_raw_${IHEADER}$_${RANK}$_to_array
  #:endfor

    ! equal
    procedure(iface_equal_arrayContainer${IHEADER}$), deferred :: equal_arrayContainer${IHEADER}$
    procedure(iface_equal_scalar_${IHEADER}$), deferred :: equal_scalar_${IHEADER}$
  #:for RANK in RANKS
    procedure(iface_equal_array_raw_${IHEADER}$_${RANK}$), deferred :: equal_array_raw_${IHEADER}$_${RANK}$
  #:endfor
    generic, public :: operator(==) => equal_scalar_${IHEADER}$, equal_arrayContainer${IHEADER}$!, &
  #:for RANK in RANKS
    generic, public :: operator(==) => equal_array_raw_${IHEADER}$_${RANK}$
  #:endfor

  #:for OP_NAME, OP_SYMB in OPERATOR_TYPES
    ! ${OP_NAME}$ (${OP_SYMB}$)
    procedure(iface_${OP_NAME}$_scalar_${IHEADER}$), deferred :: ${OP_NAME}$_scalar_${IHEADER}$
    #:for RANK in RANKS
    procedure(iface_${OP_NAME}$_array_raw_${IHEADER}$_${RANK}$), deferred :: ${OP_NAME}$_array_raw_${IHEADER}$_${RANK}$
    #:endfor
    procedure(iface_${OP_NAME}$_arrayContainer${IHEADER}$), deferred :: ${OP_NAME}$_arrayContainer${IHEADER}$

    generic, public :: operator(${OP_SYMB}$) => ${OP_NAME}$_scalar_${IHEADER}$, ${OP_NAME}$_arrayContainer${IHEADER}$ !, &
    #:for RANK in RANKS
    generic, public :: operator(${OP_SYMB}$) =>  ${OP_NAME}$_array_raw_${IHEADER}$_${RANK}$
    #:endfor
  #:endfor

  end type
#:endfor
! for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES


  abstract interface
#:for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES
    ! equal
    elemental logical function iface_equal_scalar_${IHEADER}$(self, other)
      import :: shr_arrayContainer${IHEADER}$, ${IKIND}$
      !< true if self and other are the same
      class(shr_arraycontainer${IHEADER}$), intent(in) :: self
      ${ITYPE}$, intent(in) :: other
    end function iface_equal_scalar_${IHEADER}$

#:for RANK in RANKS
    pure logical function iface_equal_array_raw_${IHEADER}$_${RANK}$(self, other)
      import :: shr_arrayContainer${IHEADER}$, ${IKIND}$
      !< true if self and other are the same
      class(shr_arraycontainer${IHEADER}$), intent(in) :: self
      ${ITYPE}$, intent(in) :: other${ranksuffix(RANK)}$
    end function iface_equal_array_raw_${IHEADER}$_${RANK}$
#:endfor

    elemental logical function iface_equal_arrayContainer${IHEADER}$(self, other)
      import :: shr_arrayContainer${IHEADER}$
      !< true if self and other are the same
      class(shr_arraycontainer${IHEADER}$), intent(in) :: self
      class(shr_arraycontainer${IHEADER}$), intent(in) :: other
    end function iface_equal_arrayContainer${IHEADER}$

#:for OP_NAME, OP_SYMB in OPERATOR_TYPES

    ! ${OP_NAME}$ (${OP_SYMB}$)
    pure function iface_${OP_NAME}$_arrayContainer${IHEADER}$(left, right) Result(total)
      import :: shr_arrayContainer${IHEADER}$, shr_arrayContainer
      class(shr_arrayContainer${IHEADER}$), intent(in) :: left
      class(shr_arrayContainer), intent(in) :: right
      class(shr_arrayContainer${IHEADER}$), allocatable :: total
    end function iface_${OP_NAME}$_arrayContainer${IHEADER}$

  #:for RANK in RANKS
    pure function iface_${OP_NAME}$_array_raw_${IHEADER}$_${RANK}$(left, right) Result(total)
      import :: shr_arrayContainer${IHEADER}$, ${IKIND}$
      class(shr_arrayContainer${IHEADER}$), intent(in) :: left
      ${ITYPE}$, intent(in) :: right${ranksuffix(RANK)}$
      class(shr_arrayContainer${IHEADER}$), allocatable :: total
    end function iface_${OP_NAME}$_array_raw_${IHEADER}$_${RANK}$
  #:endfor

    pure function iface_${OP_NAME}$_scalar_${IHEADER}$(left, right) Result(total)
      import :: shr_arrayContainer${IHEADER}$, ${IKIND}$
      class(shr_arrayContainer${IHEADER}$), intent(in) :: left
      ${ITYPE}$, intent(in) :: right
      class(shr_arrayContainer${IHEADER}$), allocatable :: total
    end function iface_${OP_NAME}$_scalar_${IHEADER}$
#:endfor


    ! copy
    pure subroutine iface_copy_scalar_${IHEADER}$(self, other)
      import :: shr_arrayContainer${IHEADER}$, ${IKIND}$
      class(shr_arrayContainer${IHEADER}$), intent(inout) :: self
      ${ITYPE}$, intent(in) :: other
    end subroutine iface_copy_scalar_${IHEADER}$

#:for RANK in RANKS
    pure subroutine iface_copy_array_raw_${IHEADER}$_${RANK}$(self, other)
      import :: shr_arrayContainer${IHEADER}$, ${IKIND}$
      class(shr_arrayContainer${IHEADER}$), intent(inout) :: self
      ${ITYPE}$, intent(in) :: other${ranksuffix(RANK)}$
    end subroutine iface_copy_array_raw_${IHEADER}$_${RANK}$
#:endfor

#:for RANK in RANKS
    pure subroutine iface_copy_raw_${IHEADER}$_${RANK}$_to_array(other, self)
      import :: shr_arrayContainer${IHEADER}$, ${IKIND}$
      ${ITYPE}$, allocatable, intent(inout) :: other${ranksuffix(RANK)}$
      class(shr_arrayContainer${IHEADER}$), intent(in) :: self
    end subroutine iface_copy_raw_${IHEADER}$_${RANK}$_to_array
#:endfor

    pure subroutine iface_copy_arrayContainer${IHEADER}$(self, other)
      import :: shr_arrayContainer${IHEADER}$
      class(shr_arrayContainer${IHEADER}$), intent(inout) :: self
      class(shr_arrayContainer${IHEADER}$), intent(in) :: other
    end subroutine iface_copy_arrayContainer${IHEADER}$
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
