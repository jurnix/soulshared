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
#:for RANK in RANKS
    procedure(iface_copy_array_raw_rsp_${RANK}$), deferred :: copy_array_raw_rsp_${RANK}$
#:endfor
    procedure(iface_copy_arrayContainer), deferred :: copy_arrayContainer
    
    ! reverse copy
#:for RANK in RANKS
    procedure(iface_copy_raw_rsp_${RANK}$_to_array), deferred, pass(self) :: copy_raw_rsp_${RANK}$_to_array
#:endfor

    generic, public :: assignment(=) => copy_scalar_rsp, copy_arrayContainer!, &
#:for RANK in RANKS
    generic, public :: assignment(=) => copy_array_raw_rsp_${RANK}$, copy_raw_rsp_${RANK}$_to_array
#:endfor

    ! equal
    procedure(iface_equal_arrayContainer), deferred :: equal_arrayContainer
    procedure(iface_equal_scalar_rsp), deferred :: equal_scalar_rsp
#:for RANK in RANKS
    procedure(iface_equal_array_raw_rsp_${RANK}$), deferred :: equal_array_raw_rsp_${RANK}$
#:endfor
    generic, public :: operator(==) => equal_scalar_rsp, equal_arrayContainer!, &
#:for RANK in RANKS
    generic, public :: operator(==) => equal_array_raw_rsp_${RANK}$
#:endfor

    ! add
    procedure(iface_add_scalar_rsp), deferred :: add_scalar_rsp
#:for RANK in RANKS
    procedure(iface_add_array_raw_rsp_${RANK}$), deferred :: add_array_raw_rsp_${RANK}$
#:endfor
    procedure(iface_add_arrayContainer), deferred :: add_arrayContainer

    generic, public :: operator(+) => add_scalar_rsp, add_arrayContainer!, &
#:for RANK in RANKS
    generic, public :: operator(+) =>  add_array_raw_rsp_${RANK}$
#:endfor

!    procedure(iface_sub_arrayContainer), deferred :: sub_arrayContainer
!    procedure(iface_div_arrayContainer), deferred :: div_arrayContainer
!    procedure(iface_mul_arrayContainer), deferred :: mul_arrayContainer
  end type


  abstract interface
    ! equal
    elemental logical function iface_equal_scalar_rsp(self, other)
      import :: shr_arrayContainer, sp
      !< true if self and other are the same
      class(shr_arraycontainer), intent(in) :: self
      real(kind=sp), intent(in) :: other
    end function iface_equal_scalar_rsp

#:for RANK in RANKS
    pure logical function iface_equal_array_raw_rsp_${RANK}$(self, other)
      import :: shr_arrayContainer, sp
      !< true if self and other are the same
      class(shr_arraycontainer), intent(in) :: self
      real(kind=sp), intent(in) :: other${ranksuffix(RANK)}$
    end function iface_equal_array_raw_rsp_${RANK}$
#:endfor

    elemental logical function iface_equal_arrayContainer(self, other)
      import :: shr_arrayContainer
      !< true if self and other are the same
      class(shr_arraycontainer), intent(in) :: self
      class(shr_arraycontainer), intent(in) :: other
    end function iface_equal_arrayContainer

    ! add
    pure function iface_add_arrayContainer(left, right) Result(total)
      import :: shr_arrayContainer
      class(shr_arrayContainer), intent(in) :: left, right
      class(shr_arrayContainer), allocatable :: total
    end function iface_add_arrayContainer

#:for RANK in RANKS
    pure function iface_add_array_raw_rsp_${RANK}$(left, right) Result(total)
      import :: shr_arrayContainer, sp
      class(shr_arrayContainer), intent(in) :: left
      real(kind=sp), intent(in) :: right${ranksuffix(RANK)}$
      class(shr_arrayContainer), allocatable :: total
    end function iface_add_array_raw_rsp_${RANK}$
#:endfor

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

#:for RANK in RANKS
    pure subroutine iface_copy_array_raw_rsp_${RANK}$(self, other)
      import :: shr_arrayContainer, sp
      class(shr_arrayContainer), intent(inout) :: self
      real(kind=sp), intent(in) :: other${ranksuffix(RANK)}$
    end subroutine iface_copy_array_raw_rsp_${RANK}$
#:endfor

#:for RANK in RANKS
    pure subroutine iface_copy_raw_rsp_${RANK}$_to_array(other, self)
      import :: shr_arrayContainer, sp
      real(kind=sp), allocatable, intent(inout) :: other${ranksuffix(RANK)}$
      class(shr_arrayContainer), intent(in) :: self
    end subroutine iface_copy_raw_rsp_${RANK}$_to_array
#:endfor

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
