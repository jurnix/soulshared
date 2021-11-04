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
  use shr_arrayContainer_mod, only: shr_arrayContainer
!  use SHR_error_mod, only: raiseError 
!  use SHR_strings_mod, only: string

  implicit none

  private

  public :: shr_arrayContainerAllocatable

  !< allocatable single precision array
  type, extends(shr_arrayContainer) :: shr_arrayContainerAllocatable 
#:for RANK in RANKS          
    real(kind=sp), allocatable :: r${RANK}$${ranksuffix(RANK)}$
#:endfor
  contains
    procedure :: init

    !< available: add, sub, div, mull
    !< kind: sp, dp
    !< rank: 1 to MAXRANK
    procedure :: add_scalar_rsp
#:for RANK in RANKS          
    procedure :: add_array_raw_rsp_${RANK}$
#:endfor    
    procedure :: add_arrayContainer => add_arrayContainerAllocatable

    procedure :: copy_scalar_rsp
#:for RANK in RANKS          
    procedure :: copy_array_raw_rsp_${RANK}$
#:endfor    
#:for RANK in RANKS          
    procedure, pass(self) :: copy_raw_rsp_${RANK}$_to_array !< reverse
#:endfor    
    procedure :: copy_arrayContainer

    procedure :: equal_arrayContainer => equal_arrayContainerAllocatable
    procedure :: equal_scalar_rsp
#:for RANK in RANKS          
    procedure :: equal_array_raw_rsp_${RANK}$
#:endfor    

    final :: destroy_class
  end type shr_arrayContainerAllocatable


contains


  pure subroutine init(self, dimensions)
    !< initialize shr_arrayContainer, overload parent subroutine
    !< to customize child initialiation
    class(shr_arrayContainerAllocatable), intent(inout) :: self
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

  !
  ! add
  !
  pure function add_arrayContainerAllocatable(left, right) Result(total)
    !< addition of arrayCA + arrayC 
    class(shr_arrayContainerAllocatable), intent(in) :: left
    class(shr_arrayContainer), intent(in) :: right
    class(shr_arrayContainer), allocatable :: total !< output

    if (.not. allocated(total)) deallocate(total)
    allocate(shr_arrayContainerAllocatable :: total)

    !
    select type(rightArray => right)
    type is (shr_arrayContainerAllocatable)
#:for RANK in RANKS          
      if (left % getSize() == ${RANK}$ .and. right % getSize() == ${RANK}$) then
        total = left % r${RANK}$ + rightArray % r${RANK}$
      endif
#:endfor
!      else
!        !< unexpected, inconsistency found
!      endif
    class default
      !< unexpected, type not found
    end select

  end function add_arrayContainerAllocatable


#:for RANK in RANKS          
  pure function add_array_raw_rsp_${RANK}$(left, right) Result(total)
    !<
    class(shr_arrayContainerAllocatable), intent(in) :: left
    real(kind=sp), intent(in) :: right${ranksuffix(RANK)}$
    class(shr_arrayContainer), allocatable :: total
    total = left % r${RANK}$ + right
  end function add_array_raw_rsp_${RANK}$
#:endfor


  pure function add_scalar_rsp(left, right) Result(total)
    !< add scalar value into array
    !< arrayCA = 24.1
    class(shr_arrayContainerAllocatable), intent(in) :: left 
    real(kind=sp), intent(in) :: right 
    class(shr_arrayContainer), allocatable :: total !< output
#:for RANK in RANKS          
    if (left % getSize() == ${RANK}$) then
      total = left % r${RANK}$ + right
    endif
#:endfor
!    MAXRANK
!    else
      !< unexpected, inconsistency found
!    endif
  end function add_scalar_rsp

  !
  ! copy
  !
  pure subroutine copy_scalar_rsp(self, other)
    !< Copy to current array container allocatable
    class(shr_arrayContainerAllocatable), intent(inout) :: self
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
    class(shr_arrayContainerAllocatable), intent(inout) :: self
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
    class(shr_arrayContainerAllocatable), intent(in) :: self
    if (self % getSize() == ${RANK}$) then
      other = self % r${RANK}$
    else
      !< unexpected, inconsistency found
    endif
  end subroutine copy_raw_rsp_${RANK}$_to_array
#:endfor


  pure subroutine copy_arrayContainer(self, other)
    !< Copy to current array container allocatable
    !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
    class(shr_arrayContainerAllocatable), intent(inout) :: self
    class(shr_arrayContainer), intent(in) :: other

    select type(otherArray => other)
    type is (shr_arrayContainerAllocatable)
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
  end subroutine copy_arrayContainer

  !
  ! equal
  !
  elemental logical function equal_arrayContainerAllocatable(self, other)
    !< true if self and other are the same
    class(shr_arraycontainerAllocatable), intent(in) :: self
    class(shr_arraycontainer), intent(in) :: other

    equal_arrayContainerAllocatable = .false. 

    select type(otherArray => other)
    type is (shr_arrayContainerAllocatable)
#:for RANK in RANKS
      if (self % getSize() == ${RANK}$ .and. other % getSize() == ${RANK}$) then
        equal_arrayContainerAllocatable = all(self % r${RANK}$ == otherArray % r${RANK}$)
      endif
#:endfor
!      else
        !< unexpected
!      endif
    class default
      !< unexpected
      equal_arrayContainerAllocatable = .false. 
    end select
  end function equal_arrayContainerAllocatable


  elemental logical function equal_scalar_rsp(self, other)
    !< true if self and other are the same
    class(shr_arraycontainerAllocatable), intent(in) :: self
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
    class(shr_arraycontainerAllocatable), intent(in) :: self
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
    type(shr_arrayContainerAllocatable), intent(inout) :: self

    deallocate(self % dimensions)

#:for RANK in RANKS
    if (allocated(self % r1)) deallocate(self % r1)
#:endfor
  end subroutine destroy_class

end module SHR_arrayContainerAllocatable_mod
