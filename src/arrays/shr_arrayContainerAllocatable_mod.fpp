!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  shr_arrayContainerAllocatable_mod
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!> Array Container Allocatable 
!>
!> Data is kept as allocatable 
!> 
!------------------------------------------------------------------------------

#:include "../common.fpp"


module SHR_arrayContainerAllocatable_mod

  use SHR_precision_mod, only: sp! dp, eqReal
  use shr_arrayDim_mod, only: shr_arrayDim, shr_arrayDimContainer
  use shr_arrayContainer_mod, only: shr_arrayContainer, shr_arrayContainerRsp
!  use SHR_error_mod, only: raiseError 
!  use SHR_strings_mod, only: string

  implicit none

  private

  public :: shr_arrayContainerRspAllocatable

  !< allocatable single precision array
  type, extends(shr_arrayContainerRsp) :: shr_arrayContainerRspAllocatable 
#:for RANK in RANKS          
    real(kind=sp), allocatable :: r${RANK}$${ranksuffix(RANK)}$
#:endfor
  contains
    procedure :: init

    !< available: add, sub, div, mull
    !< kind: sp, dp
    !< rank: 1 to MAXRANK
#:for OP_NAME, OP_SYMB in OPERATOR_TYPES
    ! ${OP_NAME}$ (${OP_SYMB}$)
    procedure :: ${OP_NAME}$_scalar_rsp
  #:for RANK in RANKS          
    procedure :: ${OP_NAME}$_array_raw_rsp_${RANK}$
  #:endfor    
    procedure :: ${OP_NAME}$_arrayContainerRsp => ${OP_NAME}$_arrayContainerRspAllocatable
#:endfor

    procedure :: copy_scalar_rsp
#:for RANK in RANKS          
    procedure :: copy_array_raw_rsp_${RANK}$
#:endfor    
#:for RANK in RANKS          
    procedure, pass(self) :: copy_raw_rsp_${RANK}$_to_array !< reverse
#:endfor    
    procedure :: copy_arrayContainerRsp

    procedure :: equal_arrayContainerRsp => equal_arrayContainerRspAllocatable
    procedure :: equal_scalar_rsp
#:for RANK in RANKS          
    procedure :: equal_array_raw_rsp_${RANK}$
#:endfor    

    final :: destroy_class
  end type shr_arrayContainerRspAllocatable


contains


  pure subroutine init(self, dimensions)
    !< initialize shr_arrayContainer, overload parent subroutine
    !< to customize child initialiation
    class(shr_arrayContainerRspAllocatable), intent(inout) :: self
    type(shr_arrayDimContainer), intent(in) :: dimensions(:)

    integer, allocatable :: alldims(:)

    !< common to all arrayContainer
    !self % dimensions = dimensions
    allocate(self % dimensions, source = dimensions)
    self % ndims = size(dimensions)
    alldims = self % dimensions % getSize()

    !< customized init
#:for RANK in RANKS          
    if (self % ndims == ${RANK}$) then
      allocate(self % r${RANK}$${rankConstructor(RANK,"alldims")}$)
    endif
#:endfor
!    else
      !< unexpected
!    endif
  end subroutine init


#:for OP_NAME, OP_SYMB in OPERATOR_TYPES
  !
  ! ${OP_NAME}$ (${OP_SYMB}$)
  !
  pure function ${OP_NAME}$_arrayContainerRspAllocatable(left, right) Result(total)
    !< addition of arrayCA + arrayC 
    class(shr_arrayContainerRspAllocatable), intent(in) :: left
    class(shr_arrayContainerRsp), intent(in) :: right
    class(shr_arrayContainerRsp), allocatable :: total !< output

    if (.not. allocated(total)) deallocate(total)
    allocate(shr_arrayContainerRspAllocatable :: total)

    !
    select type(rightArray => right)
    type is (shr_arrayContainerRspAllocatable)
#:for RANK in RANKS          
      if (left % getSize() == ${RANK}$ .and. right % getSize() == ${RANK}$) then
        total = left % r${RANK}$ ${OP_SYMB}$ rightArray % r${RANK}$
      endif
#:endfor
!      else
!        !< unexpected, inconsistency found
!      endif
    class default
      !< unexpected, type not found
    end select

  end function ${OP_NAME}$_arrayContainerRspAllocatable


#:for RANK in RANKS          
  pure function ${OP_NAME}$_array_raw_rsp_${RANK}$(left, right) Result(total)
    !<
    class(shr_arrayContainerRspAllocatable), intent(in) :: left
    real(kind=sp), intent(in) :: right${ranksuffix(RANK)}$
    class(shr_arrayContainerRsp), allocatable :: total
    total = left % r${RANK}$ ${OP_SYMB}$ right
  end function ${OP_NAME}$_array_raw_rsp_${RANK}$
#:endfor


  pure function ${OP_NAME}$_scalar_rsp(left, right) Result(total)
    !< add scalar value into array
    !< arrayCA = 24.1
    class(shr_arrayContainerRspAllocatable), intent(in) :: left 
    real(kind=sp), intent(in) :: right 
    class(shr_arrayContainerRsp), allocatable :: total !< output
#:for RANK in RANKS          
    if (left % getSize() == ${RANK}$) then
      total = left % r${RANK}$ ${OP_SYMB}$ right
    endif
#:endfor
!    MAXRANK
!    else
      !< unexpected, inconsistency found
!    endif
  end function ${OP_NAME}$_scalar_rsp

#:endfor

  !
  ! copy
  !
  pure subroutine copy_scalar_rsp(self, other)
    !< Copy to current array container allocatable
    class(shr_arrayContainerRspAllocatable), intent(inout) :: self
    real(kind=sp), intent(in) :: other
#:for RANK in RANKS          
    if (self % getSize() == ${RANK}$) then
      self % r${RANK}$ = other 
    endif
#:endfor
!    MAXRANK
!    else
!      !< unexpected, inconsistency found
!    endif
  end subroutine copy_scalar_rsp


#:for RANK in RANKS          
  pure subroutine copy_array_raw_rsp_${RANK}$(self, other)
    !< Copy to current array 'self' into 'other' rsp array
    class(shr_arrayContainerRspAllocatable), intent(inout) :: self
    real(kind=sp), intent(in) :: other${ranksuffix(RANK)}$
    if (self % getSize() == ${RANK}$) then
      self % r${RANK}$ = other
    else
      !< unexpected, inconsistency found
    endif
  end subroutine copy_array_raw_rsp_${RANK}$
#:endfor

#:for RANK in RANKS
  pure subroutine copy_raw_rsp_${RANK}$_to_array(other, self)
    !< copy self to other
    !< reverse of 'copy_array_raw_rsp_1'
    real(kind=sp), allocatable, intent(inout) :: other${ranksuffix(RANK)}$
    class(shr_arrayContainerRspAllocatable), intent(in) :: self
    if (self % getSize() == ${RANK}$) then
      other = self % r${RANK}$
    else
      !< unexpected, inconsistency found
    endif
  end subroutine copy_raw_rsp_${RANK}$_to_array
#:endfor


  pure subroutine copy_arrayContainerRsp(self, other)
    !< Copy to current array container allocatable
    !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
    class(shr_arrayContainerRspAllocatable), intent(inout) :: self
    class(shr_arrayContainerRsp), intent(in) :: other

    select type(otherArray => other)
    type is (shr_arrayContainerRspAllocatable)
#:for RANK in RANKS
      if (self % getSize() == ${RANK}$ .and. other % getSize() == ${RANK}$) then
        self % r${RANK}$ = otherArray % r${RANK}$
      endif
#:endfor
!      else if (self % getSize() == 2 .and. other % getSize() == 2) then
!        self % r2 = otherArray % r2
      !< MAXRANK
!      endif
    class default
      !< unexpected type found
    end select
  end subroutine copy_arrayContainerRsp

  !
  ! equal
  !
  elemental logical function equal_arrayContainerRspAllocatable(self, other)
    !< true if self and other are the same
    class(shr_arrayContainerRspAllocatable), intent(in) :: self
    class(shr_arraycontainerRsp), intent(in) :: other

    equal_arrayContainerRspAllocatable = .false. 

    select type(otherArray => other)
    type is (shr_arrayContainerRspAllocatable)
#:for RANK in RANKS
      if (self % getSize() == ${RANK}$ .and. other % getSize() == ${RANK}$) then
        equal_arrayContainerRspAllocatable = all(self % r${RANK}$ == otherArray % r${RANK}$)
      endif
#:endfor
!      else
        !< unexpected
!      endif
    class default
      !< unexpected
      equal_arrayContainerRspAllocatable = .false. 
    end select
  end function equal_arrayContainerRspAllocatable


  elemental logical function equal_scalar_rsp(self, other)
    !< true if self and other are the same
    class(shr_arrayContainerRspAllocatable), intent(in) :: self
    real(kind=sp), intent(in) :: other
    equal_scalar_rsp = .false.
#:for RANK in RANKS
    if (self % getSize() == ${RANK}$) then
      equal_scalar_rsp = all(self % r${RANK}$ == other)
    endif
#:endfor
!    MAXRANK
!    else
      !< unexpected, inconsistency found
!      equal_scalar_rsp = .false.
!    endif
  end function equal_scalar_rsp


#:for RANK in RANKS
  pure logical function equal_array_raw_rsp_${RANK}$(self, other)
    !< true if self and other are the same
    class(shr_arrayContainerRspAllocatable), intent(in) :: self
    real(kind=sp), intent(in) :: other${ranksuffix(RANK)}$
    equal_array_raw_rsp_${RANK}$ = .false.
    if (self % getSize() == ${RANK}$) then
      equal_array_raw_rsp_${RANK}$ = all(self % r${RANK}$ == other)
    else
      !< unexpected, inconsistency found
      equal_array_raw_rsp_${RANK}$ = .false.
    endif
  end function equal_array_raw_rsp_${RANK}$
#:endfor

  !
  ! final
  !
  subroutine destroy_class(self)
    !< destroy class
    type(shr_arrayContainerRspAllocatable), intent(inout) :: self

    deallocate(self % dimensions)

#:for RANK in RANKS
    if (allocated(self % r1)) deallocate(self % r1)
#:endfor
  end subroutine destroy_class

end module SHR_arrayContainerAllocatable_mod
