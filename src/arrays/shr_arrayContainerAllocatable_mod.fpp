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

  use SHR_precision_mod, only: sp, dp!, eqReal
  use shr_arrayDim_mod, only: shr_arrayDim, shr_arrayDimContainer
  use shr_arrayContainer_mod, only: shr_arrayContainer
#:for _, _, IHEADER  in ALL_KINDS_TYPES
  use shr_arrayContainer_mod, only: shr_arrayContainer${IHEADER}$
#:endfor
!  use SHR_error_mod, only: raiseError 
!  use SHR_strings_mod, only: string

  implicit none

  private

#:for _, _, IHEADER  in ALL_KINDS_TYPES
  public :: shr_arrayContainer${IHEADER}$Allocatable
#:endfor

#:for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES
  !
  ! ${IHEADER}$, ${IKIND}$, ${ITYPE}$
  !
  !< allocatable single precision array
  type, extends(shr_arrayContainer${IHEADER}$) :: shr_arrayContainer${IHEADER}$Allocatable 
  #:for RANK in RANKS          
    ${ITYPE}$, allocatable :: r${RANK}$${ranksuffix(RANK)}$
  #:endfor
  contains
    procedure :: init => init_${IHEADER}$

    !< available: add, sub, div, mull
    !< kind: sp, dp
    !< rank: 1 to MAXRANK
  #:for OP_NAME, OP_SYMB in OPERATOR_TYPES
    ! ${OP_NAME}$ (${OP_SYMB}$)
    procedure :: ${OP_NAME}$_scalar_${IHEADER}$
    #:for RANK in RANKS          
    procedure :: ${OP_NAME}$_array_raw_${IHEADER}$_${RANK}$
    #:endfor    
    procedure :: ${OP_NAME}$_arrayContainer${IHEADER}$ => ${OP_NAME}$_arrayContainer${IHEADER}$Allocatable
  #:endfor

    procedure :: copy_scalar_${IHEADER}$
  #:for RANK in RANKS          
    procedure :: copy_array_raw_${IHEADER}$_${RANK}$
  #:endfor    
  #:for RANK in RANKS          
    procedure, pass(self) :: copy_raw_${IHEADER}$_${RANK}$_to_array !< reverse
  #:endfor    
    procedure :: copy_arrayContainer${IHEADER}$

    procedure :: equal_arrayContainer${IHEADER}$ => equal_arrayContainer${IHEADER}$Allocatable
    procedure :: equal_scalar_${IHEADER}$
  #:for RANK in RANKS          
    procedure :: equal_array_raw_${IHEADER}$_${RANK}$
  #:endfor    

    final :: destroy_class_${IHEADER}$
  end type shr_arrayContainer${IHEADER}$Allocatable
#:endfor

contains


#:for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES
  !
  ! ${IHEADER}$, ${IKIND}$, ${ITYPE}$
  !
  !< allocatable single precision array
  pure subroutine init_${IHEADER}$(self, dimensions)
    !< initialize shr_arrayContainer, overload parent subroutine
    !< to customize child initialiation
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(inout) :: self
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
  end subroutine init_${IHEADER}$


  #:for OP_NAME, OP_SYMB in OPERATOR_TYPES
  !
  ! ${OP_NAME}$ (${OP_SYMB}$)
  !
  pure function ${OP_NAME}$_arrayContainer${IHEADER}$Allocatable(left, right) Result(total)
    !< addition of arrayCA + arrayC 
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: left
    class(shr_arrayContainer${IHEADER}$), intent(in) :: right
    class(shr_arrayContainer${IHEADER}$), allocatable :: total !< output

    if (.not. allocated(total)) deallocate(total)
    allocate(shr_arrayContainer${IHEADER}$Allocatable :: total)

    !
    select type(rightArray => right)
    type is (shr_arrayContainer${IHEADER}$Allocatable)
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

  end function ${OP_NAME}$_arrayContainer${IHEADER}$Allocatable


    #:for RANK in RANKS          
    pure function ${OP_NAME}$_array_raw_${IHEADER}$_${RANK}$(left, right) Result(total)
      !<
      class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: left
      ${ITYPE}$, intent(in) :: right${ranksuffix(RANK)}$
      class(shr_arrayContainer${IHEADER}$), allocatable :: total
      total = left % r${RANK}$ ${OP_SYMB}$ right
    end function ${OP_NAME}$_array_raw_${IHEADER}$_${RANK}$
    #:endfor


  pure function ${OP_NAME}$_scalar_${IHEADER}$(left, right) Result(total)
    !< add scalar value into array
    !< arrayCA = 24.1
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: left 
    ${ITYPE}$, intent(in) :: right 
    class(shr_arrayContainer${IHEADER}$), allocatable :: total !< output
    #:for RANK in RANKS          
    if (left % getSize() == ${RANK}$) then
      total = left % r${RANK}$ ${OP_SYMB}$ right
    endif
    #:endfor
!    MAXRANK
!    else
      !< unexpected, inconsistency found
!    endif
  end function ${OP_NAME}$_scalar_${IHEADER}$

  #:endfor
  ! OP_NAME, OP_SYMB

  !
  ! copy
  !
  pure subroutine copy_scalar_${IHEADER}$(self, other)
    !< Copy to current array container allocatable
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(inout) :: self
    ${ITYPE}$, intent(in) :: other
  #:for RANK in RANKS          
    if (self % getSize() == ${RANK}$) then
      self % r${RANK}$ = other 
    endif
  #:endfor
!    MAXRANK
!    else
!      !< unexpected, inconsistency found
!    endif
  end subroutine copy_scalar_${IHEADER}$


  #:for RANK in RANKS          
  pure subroutine copy_array_raw_${IHEADER}$_${RANK}$(self, other)
    !< Copy to current array 'self' into 'other' rsp array
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(inout) :: self
    ${ITYPE}$, intent(in) :: other${ranksuffix(RANK)}$
    if (self % getSize() == ${RANK}$) then
      self % r${RANK}$ = other
    else
      !< unexpected, inconsistency found
    endif
  end subroutine copy_array_raw_${IHEADER}$_${RANK}$
  #:endfor

  #:for RANK in RANKS
  pure subroutine copy_raw_${IHEADER}$_${RANK}$_to_array(other, self)
    !< copy self to other
    !< reverse of 'copy_array_raw_rsp_1'
    ${ITYPE}$, allocatable, intent(inout) :: other${ranksuffix(RANK)}$
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: self
    if (self % getSize() == ${RANK}$) then
      other = self % r${RANK}$
    else
      !< unexpected, inconsistency found
    endif
  end subroutine copy_raw_${IHEADER}$_${RANK}$_to_array
  #:endfor


  pure subroutine copy_arrayContainer${IHEADER}$(self, other)
    !< Copy to current array container allocatable
    !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(inout) :: self
    class(shr_arrayContainer${IHEADER}$), intent(in) :: other

    select type(otherArray => other)
    type is (shr_arrayContainer${IHEADER}$Allocatable)
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
  end subroutine copy_arrayContainer${IHEADER}$

  !
  ! equal
  !
  elemental logical function equal_arrayContainer${IHEADER}$Allocatable(self, other)
    !< true if self and other are the same
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: self
    class(shr_arraycontainer${IHEADER}$), intent(in) :: other

    equal_arrayContainer${IHEADER}$Allocatable = .false. 

    select type(otherArray => other)
    type is (shr_arrayContainer${IHEADER}$Allocatable)
  #:for RANK in RANKS
      if (self % getSize() == ${RANK}$ .and. other % getSize() == ${RANK}$) then
        equal_arrayContainer${IHEADER}$Allocatable = all(self % r${RANK}$ == otherArray % r${RANK}$)
      endif
  #:endfor
!      else
        !< unexpected
!      endif
    class default
      !< unexpected
      equal_arrayContainer${IHEADER}$Allocatable = .false. 
    end select
  end function equal_arrayContainer${IHEADER}$Allocatable


  elemental logical function equal_scalar_${IHEADER}$(self, other)
    !< true if self and other are the same
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: self
    ${ITYPE}$, intent(in) :: other
    equal_scalar_${IHEADER}$ = .false.
  #:for RANK in RANKS
    if (self % getSize() == ${RANK}$) then
      equal_scalar_${IHEADER}$ = all(self % r${RANK}$ == other)
    endif
  #:endfor
!    else
      !< unexpected, inconsistency found
!      equal_scalar_${IHEADER}$ = .false.
!    endif
  end function equal_scalar_${IHEADER}$


  #:for RANK in RANKS
  pure logical function equal_array_raw_${IHEADER}$_${RANK}$(self, other)
    !< true if self and other are the same
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: self
    ${ITYPE}$, intent(in) :: other${ranksuffix(RANK)}$
    equal_array_raw_${IHEADER}$_${RANK}$ = .false.
    if (self % getSize() == ${RANK}$) then
      equal_array_raw_${IHEADER}$_${RANK}$ = all(self % r${RANK}$ == other)
    else
      !< unexpected, inconsistency found
      equal_array_raw_${IHEADER}$_${RANK}$ = .false.
    endif
  end function equal_array_raw_${IHEADER}$_${RANK}$
  #:endfor

  !
  ! final
  !
  subroutine destroy_class_${IHEADER}$(self)
    !< destroy class
    type(shr_arrayContainer${IHEADER}$Allocatable), intent(inout) :: self

    deallocate(self % dimensions)

  #:for RANK in RANKS
    if (allocated(self % r${RANK}$)) deallocate(self % r${RANK}$)
  #:endfor
  end subroutine destroy_class_${IHEADER}$
#:endfor
! for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES

end module SHR_arrayContainerAllocatable_mod
