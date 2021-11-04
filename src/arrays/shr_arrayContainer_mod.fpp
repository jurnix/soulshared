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

  public :: shr_arrayContainerRsp


  type, abstract :: shr_arrayContainer
    integer :: ndims !< total number of dimensions
    type(shr_arrayDimContainer), allocatable :: dimensions(:)
  contains
    procedure :: getSize
    procedure :: getDims

    procedure :: init 
  end type shr_arrayContainer


  type, extends(shr_arrayContainer), abstract :: shr_arrayContainerRsp
  contains

    ! copy
    procedure(iface_copy_scalar_rsp), deferred :: copy_scalar_rsp
#:for RANK in RANKS
    procedure(iface_copy_array_raw_rsp_${RANK}$), deferred :: copy_array_raw_rsp_${RANK}$
#:endfor
    procedure(iface_copy_arrayContainerRsp), deferred :: copy_arrayContainerRsp
    
    ! reverse copy
#:for RANK in RANKS
    procedure(iface_copy_raw_rsp_${RANK}$_to_array), deferred, pass(self) :: copy_raw_rsp_${RANK}$_to_array
#:endfor

    generic, public :: assignment(=) => copy_scalar_rsp, copy_arrayContainerRsp!, &
#:for RANK in RANKS
    generic, public :: assignment(=) => copy_array_raw_rsp_${RANK}$, copy_raw_rsp_${RANK}$_to_array
#:endfor

    ! equal
    procedure(iface_equal_arrayContainerRsp), deferred :: equal_arrayContainerRsp
    procedure(iface_equal_scalar_rsp), deferred :: equal_scalar_rsp
#:for RANK in RANKS
    procedure(iface_equal_array_raw_rsp_${RANK}$), deferred :: equal_array_raw_rsp_${RANK}$
#:endfor
    generic, public :: operator(==) => equal_scalar_rsp, equal_arrayContainerRsp!, &
#:for RANK in RANKS
    generic, public :: operator(==) => equal_array_raw_rsp_${RANK}$
#:endfor

#:for OP_NAME, OP_SYMB in OPERATOR_TYPES
    ! ${OP_NAME}$ (${OP_SYMB}$)
    procedure(iface_${OP_NAME}$_scalar_rsp), deferred :: ${OP_NAME}$_scalar_rsp
  #:for RANK in RANKS
    procedure(iface_${OP_NAME}$_array_raw_rsp_${RANK}$), deferred :: ${OP_NAME}$_array_raw_rsp_${RANK}$
  #:endfor
    procedure(iface_${OP_NAME}$_arrayContainerRsp), deferred :: ${OP_NAME}$_arrayContainerRsp

    generic, public :: operator(${OP_SYMB}$) => ${OP_NAME}$_scalar_rsp, ${OP_NAME}$_arrayContainerRsp !, &
  #:for RANK in RANKS
    generic, public :: operator(${OP_SYMB}$) =>  ${OP_NAME}$_array_raw_rsp_${RANK}$
  #:endfor
#:endfor

  end type


  abstract interface
    ! equal
    elemental logical function iface_equal_scalar_rsp(self, other)
      import :: shr_arrayContainerRsp, sp
      !< true if self and other are the same
      class(shr_arraycontainerRsp), intent(in) :: self
      real(kind=sp), intent(in) :: other
    end function iface_equal_scalar_rsp

#:for RANK in RANKS
    pure logical function iface_equal_array_raw_rsp_${RANK}$(self, other)
      import :: shr_arrayContainerRsp, sp
      !< true if self and other are the same
      class(shr_arraycontainerRsp), intent(in) :: self
      real(kind=sp), intent(in) :: other${ranksuffix(RANK)}$
    end function iface_equal_array_raw_rsp_${RANK}$
#:endfor

    elemental logical function iface_equal_arrayContainerRsp(self, other)
      import :: shr_arrayContainerRsp
      !< true if self and other are the same
      class(shr_arraycontainerRsp), intent(in) :: self
      class(shr_arraycontainerRsp), intent(in) :: other
    end function iface_equal_arrayContainerRsp

#:for OP_NAME, OP_SYMB in OPERATOR_TYPES
    ! ${OP_NAME}$ (${OP_SYMB}$)
    pure function iface_${OP_NAME}$_arrayContainerRsp(left, right) Result(total)
      import :: shr_arrayContainerRsp
      class(shr_arrayContainerRsp), intent(in) :: left, right
      class(shr_arrayContainerRsp), allocatable :: total
    end function iface_${OP_NAME}$_arrayContainerRsp

  #:for RANK in RANKS
    pure function iface_${OP_NAME}$_array_raw_rsp_${RANK}$(left, right) Result(total)
      import :: shr_arrayContainerRsp, sp
      class(shr_arrayContainerRsp), intent(in) :: left
      real(kind=sp), intent(in) :: right${ranksuffix(RANK)}$
      class(shr_arrayContainerRsp), allocatable :: total
    end function iface_${OP_NAME}$_array_raw_rsp_${RANK}$
  #:endfor

    pure function iface_${OP_NAME}$_scalar_rsp(left, right) Result(total)
      import :: shr_arrayContainerRsp, sp
      class(shr_arrayContainerRsp), intent(in) :: left
      real(kind=sp), intent(in) :: right
      class(shr_arrayContainerRsp), allocatable :: total
    end function iface_${OP_NAME}$_scalar_rsp
#:endfor


    ! copy
    pure subroutine iface_copy_scalar_rsp(self, other)
      import :: shr_arrayContainerRsp, sp
      class(shr_arrayContainerRsp), intent(inout) :: self
      real(kind=sp), intent(in) :: other
    end subroutine iface_copy_scalar_rsp

#:for RANK in RANKS
    pure subroutine iface_copy_array_raw_rsp_${RANK}$(self, other)
      import :: shr_arrayContainerRsp, sp
      class(shr_arrayContainerRsp), intent(inout) :: self
      real(kind=sp), intent(in) :: other${ranksuffix(RANK)}$
    end subroutine iface_copy_array_raw_rsp_${RANK}$
#:endfor

#:for RANK in RANKS
    pure subroutine iface_copy_raw_rsp_${RANK}$_to_array(other, self)
      import :: shr_arrayContainerRsp, sp
      real(kind=sp), allocatable, intent(inout) :: other${ranksuffix(RANK)}$
      class(shr_arrayContainerRsp), intent(in) :: self
    end subroutine iface_copy_raw_rsp_${RANK}$_to_array
#:endfor

    pure subroutine iface_copy_arrayContainerRsp(self, other)
      import :: shr_arrayContainerRsp
      class(shr_arrayContainerRsp), intent(inout) :: self
      class(shr_arrayContainerRsp), intent(in) :: other
    end subroutine iface_copy_arrayContainerRsp
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
