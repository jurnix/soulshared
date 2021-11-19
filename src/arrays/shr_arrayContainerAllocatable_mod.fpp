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
  use SHR_error_mod, only: raiseError 
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
    ! ${OP_NAME}$ (${OP_SYMB}$ arrayContainer op scalar)
    #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
    procedure :: ${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_scalar_${IHEADERSRC}$ => &
                 ${OP_NAME}$_arrayContainer${IHEADER}$Alloc_${OP_NAME}$_scalar_${IHEADERSRC}$
    #:endfor

    ! ${OP_NAME}$ (${OP_SYMB}$ arrayContainer op <type, kind> array)
    #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
      #:for RANK in RANKS          
    procedure :: ${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_array_raw_${IHEADERSRC}$_${RANK}$ => &
    ${OP_NAME}$_arrayContainer${IHEADER}$Alloc_${OP_NAME}$_array_raw_${IHEADERSRC}$_${RANK}$
      #:endfor    
    #:endfor

    ! ${OP_NAME}$ (${OP_SYMB}$ arrayContainer op arrayContainer)
    procedure :: ${OP_NAME}$_arrayContainer${IHEADER}$_${OP_NAME}$_arrayContainer => ${OP_NAME}$_arrayContainer${IHEADER}$Alloc_${OP_NAME}$_arrayContainer
  #:endfor

  ! copy (arrayContainer = scalar <type, kind>)
#:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
    procedure :: copy_arrayContainer${IHEADER}$_copy_scalar_${IHEADERSRC}$ => copy_arrayContainer${IHEADER}$Allocatable_copy_scalar_${IHEADERSRC}$
#:endfor

  ! copy (arrayContainer = <type, kind> array)
#:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
  #:for RANK in RANKS          
    procedure :: copy_arrayContainer${IHEADER}$_copy_array_raw_${IHEADERSRC}$_${RANK}$ => &
    copy_arrayContainer${IHEADER}$Allocatable_copy_array_raw_${IHEADERSRC}$_${RANK}$
  #:endfor    
#:endfor

  ! copy (<type, kind> array = arrayContainer)
#:for _, _, IHEADERTGT in ALL_KINDS_TYPES
  #:for RANK in RANKS          
    procedure, pass(self) :: copy_raw_${IHEADERTGT}$_${RANK}$_to_arrayContainer${IHEADER}$ => copy_raw_${IHEADERTGT}$_${RANK}$_to_arrayContainer${IHEADER}$Allocatable !< reverse
  #:endfor
#:endfor

  ! copy (arrayContainer = arrayContainer)
    procedure :: copy_arrayContainer${IHEADER}$_copy_arrayContainer => copy_arrayContainer${IHEADER}$Allocatable_copy_arrayContainer

    ! equal (arrayContainer == arrayContainer)
    procedure :: equal_arrayContainer${IHEADER}$_equal_arrayContainer${IHEADER}$ => &
            equal_arrayContainer${IHEADER}$Allocatable_equal_arrayContainer${IHEADER}$

    ! equal (arrayContainer == <type, kind> scalar)
  #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
    procedure :: equal_arrayContainer${IHEADER}$_equal_scalar_${IHEADERSRC}$ => &
    equal_arrayContainer${IHEADER}$Allocatable_equal_scalar_${IHEADERSRC}$
  #:endfor

  #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
    #:for RANK in RANKS          
    ! equal (arrayContainer == <type, kind> array)
    procedure :: equal_arrayContainer${IHEADER}$_equal_array_raw_${IHEADERSRC}$_${RANK}$ => &
    equal_arrayContainer${IHEADER}$Allocatable_equal_array_raw_${IHEADERSRC}$_${RANK}$
    #:endfor    
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
   subroutine init_${IHEADER}$(self, dimensions)
    !< initialize shr_arrayContainer, overload parent subroutine
    !< to customize child initialiation
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(inout) :: self
    type(shr_arrayDimContainer), intent(in) :: dimensions(:)

    integer, allocatable :: alldims(:)

!    write(*,*) "shr_arrayContainerAllocatable_mod:: init_${IHEADER}$:: initializing..."
!    write(*,*) "shr_arrayContainerAllocatable_mod:: init_${IHEADER}$:: dimensions size =", size(dimensions)

    if (size(dimensions) <= 0) then 
      call raiseError(__FILE__, "init_${IHEADER}$", &
              "At least 1 dimension must be given but none found")
    endif

    !< common to all arrayContainer
    !self % dimensions = dimensions
    allocate(self % dimensions, source = dimensions)
    self % ndims = size(dimensions)
    alldims = self % dimensions % getSize()

    !< customized init
  #:for RANK in RANKS          
    if (self % ndims == ${RANK}$) then
!      write(*,*) "shr_arrayContainerAllocatable_mod:: init_${IHEADER}$:: allocating with ${RANK}$ dims... "
      allocate(self % r${RANK}$${rankConstructor(RANK,"alldims")}$)
!      write(*,*) "shr_arrayContainerAllocatable_mod:: init_${IHEADER}$:: allocating... DONE"
    endif
  #:endfor
!    else
      !< unexpected
!    endif
!write(*,*) "shr_arrayContainerAllocatable_mod:: init_${IHEADER}$:: initializing... DONE"
  end subroutine init_${IHEADER}$


  #:for OP_NAME, OP_SYMB in OPERATOR_TYPES
  !
  ! ${OP_NAME}$ (${OP_SYMB}$)
  !
   function ${OP_NAME}$_arrayContainer${IHEADER}$Alloc_${OP_NAME}$_arrayContainer(left, right) Result(total)
    !< addition of arrayContainerRspAllocatable + arrayContainer
    !< 
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: left
    class(shr_arrayContainer), intent(in) :: right
    class(shr_arrayContainer${IHEADER}$), allocatable :: total !< output

!    write(*,*) "shr_arrayContainerAllocatable_mod:: "//&
!            "${OP_NAME}$_arrayContainer${IHEADER}$Alloc_${OP_NAME}$_arrayContainer:: "// &
!            "starting..."
    allocate(total, source = left)
    !
    select type(rightArray => right)
#:for _, _, IHEADERSRC in ALL_KINDS_TYPES
    type is (shr_arrayContainer${IHEADERSRC}$Allocatable)
    #:for RANK in RANKS          
      if (left % getSize() == ${RANK}$ .and. right % getSize() == ${RANK}$) then
!        write(*,*) "shr_arrayContainerAllocatable_mod:: "//&
!            "${OP_NAME}$_arrayContainer${IHEADER}$Alloc_${OP_NAME}$_arrayContainer:: "// &
!            "${OP_SYMB}$ with rank ${RANK}$..."
        total = left % r${RANK}$ ${OP_SYMB}$ rightArray % r${RANK}$
!        write(*,*) "shr_arrayContainerAllocatable_mod:: "//&
!            "${OP_NAME}$_arrayContainer${IHEADER}$Alloc_${OP_NAME}$_arrayContainer:: "// &
!            "${OP_SYMB}$ with rank ${RANK}$... DONE"
        return
      endif
    #:endfor
#:endfor
!      else
!        !< unexpected, inconsistency found
!      endif
    class default
      !< unexpected, type not found
    end select
!    write(*,*) "shr_arrayContainerAllocatable_mod:: "//&
!            "${OP_NAME}$_arrayContainer${IHEADER}$Alloc_${OP_NAME}$_arrayContainer:: "// &
!            "starting... DONE"

  end function ${OP_NAME}$_arrayContainer${IHEADER}$Alloc_${OP_NAME}$_arrayContainer


  #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
    #:for RANK in RANKS          
     function ${OP_NAME}$_arrayContainer${IHEADER}$Alloc_${OP_NAME}$_array_raw_${IHEADERSRC}$_${RANK}$(left, right) Result(total)
      !< Operate according to 'op'. In case 'total' is not allocated, it is copied from 'left'
      class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: left
      ${ITYPESRC}$, intent(in) :: right${ranksuffix(RANK)}$
      class(shr_arrayContainer${IHEADER}$), allocatable :: total
      if (.not. allocated(total)) allocate(total, source=left)
      total = left % r${RANK}$ ${OP_SYMB}$ right
    end function ${OP_NAME}$_arrayContainer${IHEADER}$Alloc_${OP_NAME}$_array_raw_${IHEADERSRC}$_${RANK}$
    #:endfor
  #:endfor


  #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
   function ${OP_NAME}$_arrayContainer${IHEADER}$Alloc_${OP_NAME}$_scalar_${IHEADERSRC}$(left, right) Result(total)
    !< add scalar value into array
    !< arrayCA = 24.1
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: left 
    ${ITYPESRC}$, intent(in) :: right 
    class(shr_arrayContainer${IHEADER}$), allocatable :: total !< output

    if (.not. allocated(total)) then
      allocate(total, source = left)
    endif

    #:for RANK in RANKS          
    if (left % getSize() == ${RANK}$) then
      total = left % r${RANK}$ ${OP_SYMB}$ right
    endif
    #:endfor
!    else
      !< unexpected, inconsistency found
!    endif
  end function ${OP_NAME}$_arrayContainer${IHEADER}$Alloc_${OP_NAME}$_scalar_${IHEADERSRC}$
  #:endfor

  #:endfor
  ! OP_NAME, OP_SYMB

  !
  ! copy
  !
#:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
   subroutine copy_arrayContainer${IHEADER}$Allocatable_copy_scalar_${IHEADERSRC}$(self, other)
    !< Copy to current array container allocatable
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(inout) :: self
    ${ITYPESRC}$, intent(in) :: other
  #:for RANK in RANKS          
    if (self % getSize() == ${RANK}$) then
      self % r${RANK}$ = other 
    endif
  #:endfor
!    MAXRANK
!    else
!      !< unexpected, inconsistency found
!    endif
  end subroutine copy_arrayContainer${IHEADER}$Allocatable_copy_scalar_${IHEADERSRC}$
#:endfor


#:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
  #:for RANK in RANKS          
   subroutine copy_arrayContainer${IHEADER}$Allocatable_copy_array_raw_${IHEADERSRC}$_${RANK}$(self, other)
    !< Copy to current array 'self' into 'other' rsp array
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(inout) :: self
    ${ITYPESRC}$, intent(in) :: other${ranksuffix(RANK)}$
    if (self % getSize() == ${RANK}$) then
      self % r${RANK}$ = other
    else
      !< unexpected, inconsistency found
    endif
  end subroutine copy_arrayContainer${IHEADER}$Allocatable_copy_array_raw_${IHEADERSRC}$_${RANK}$
  #:endfor
#:endfor
! for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES


#:for IKINDTGT, ITYPETGT, IHEADERTGT in ALL_KINDS_TYPES
  #:for RANK in RANKS
   subroutine copy_raw_${IHEADERTGT}$_${RANK}$_to_arrayContainer${IHEADER}$Allocatable(other, self)
    !< copy self to other
    !< reverse of 'copy_array_raw_rsp_1'
    ${ITYPETGT}$, allocatable, intent(inout) :: other${ranksuffix(RANK)}$
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: self
    if (self % getSize() == ${RANK}$) then
      other = self % r${RANK}$
    else
      !< unexpected, inconsistency found
    endif
  end subroutine copy_raw_${IHEADERTGT}$_${RANK}$_to_arrayContainer${IHEADER}$Allocatable
  #:endfor
#:endfor

   subroutine copy_arrayContainer${IHEADER}$Allocatable_copy_arrayContainer(self, other)
    !< Copy to current array container allocatable
    !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(inout) :: self
    class(shr_arrayContainer), intent(in) :: other

    type(shr_arrayDimContainer), allocatable :: tmpDims(:)

!    write(*,*) "shr_arrayContainerAllocatable_mod:: "// &
!            "copy_arrayContainer${IHEADER}$Allocatable_copy_arrayContainer:: "// &
!            "starting..."

    self % ndims = other % ndims
    ! create temporary array to avoid memory issues when
    ! arrayContainerA = arrayContainerA (copy to itself)
    tmpDims = other % dimensions
    self % dimensions = tmpDims

    select type(otherArray => other)
#:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
    type is (shr_arrayContainer${IHEADERSRC}$Allocatable)
  #:for RANK in RANKS
      if (self % getSize() == ${RANK}$ .and. other % getSize() == ${RANK}$) then
!        write(*,*) "shr_arrayContainerAllocatable_mod:: "// &
!            "copy_arrayContainer${IHEADER}$Allocatable_copy_arrayContainer:: "// &
!            "copying with rank r${RANK}$..."
        self % r${RANK}$ = otherArray % r${RANK}$
!        write(*,*) "shr_arrayContainerAllocatable_mod:: "// &
!            "copy_arrayContainer${IHEADER}$Allocatable_copy_arrayContainer:: "// &
!            "copying with rank r${RANK}$... DONE"
      endif
  #:endfor
#:endfor 

      !< MAXRANK
!      endif
    class default
      !< unexpected type found
      call raiseError(__FILE__, &
              "copy_arrayContainer${IHEADER}$Allocatable_copy_arrayContainer", &
              "Unexpected type found")
    end select

!    write(*,*) "shr_arrayContainerAllocatable_mod:: "// &
!            "copy_arrayContainer${IHEADER}$Allocatable_copy_arrayContainer:: "// &
!            "starting... DONE"
  end subroutine copy_arrayContainer${IHEADER}$Allocatable_copy_arrayContainer

  !
  ! equal
  !
   logical function equal_arrayContainer${IHEADER}$Allocatable_equal_arrayContainer${IHEADER}$(self, other)
    !< true if self and other are the same
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: self
    class(shr_arraycontainer), intent(in) :: other

    equal_arrayContainer${IHEADER}$Allocatable_equal_arrayContainer${IHEADER}$ = .false. 

    select type(otherArray => other)
#:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
    type is (shr_arrayContainer${IHEADERSRC}$Allocatable)
  #:for RANK in RANKS
      if (self % getSize() == ${RANK}$ .and. other % getSize() == ${RANK}$) then
        equal_ArrayContainer${IHEADER}$Allocatable_equal_arrayContainer${IHEADER}$ = all(self % r${RANK}$ == otherArray % r${RANK}$)
      endif
  #:endfor
#:endfor

!      else
        !< unexpected
!      endif
    class default
      !< unexpected
      equal_arrayContainer${IHEADER}$Allocatable_equal_arrayContainer${IHEADER}$ = .false. 
    end select
  end function equal_arrayContainer${IHEADER}$Allocatable_equal_arrayContainer${IHEADER}$


  #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
   logical function equal_arrayContainer${IHEADER}$Allocatable_equal_scalar_${IHEADERSRC}$(self, other)
    !< true if self and other are the same
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: self
    ${ITYPESRC}$, intent(in) :: other
    equal_arrayContainer${IHEADER}$Allocatable_equal_scalar_${IHEADERSRC}$ = .false.
  #:for RANK in RANKS
    if (self % getSize() == ${RANK}$) then
      equal_arrayContainer${IHEADER}$Allocatable_equal_scalar_${IHEADERSRC}$ = all(self % r${RANK}$ == other)
    endif
  #:endfor
!    else
      !< unexpected, inconsistency found
!      equal_arrayContainer${IHEADER}$Allocatable_equal_scalar_${IHEADERSRC}$ = .false.
!    endif
  end function equal_arrayContainer${IHEADER}$Allocatable_equal_scalar_${IHEADERSRC}$
  #:endfor


  #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
    #:for RANK in RANKS
   logical function equal_arrayContainer${IHEADER}$Allocatable_equal_array_raw_${IHEADERSRC}$_${RANK}$(self, other)
    !< true if self and other are the same
    class(shr_arrayContainer${IHEADER}$Allocatable), intent(in) :: self
    ${ITYPESRC}$, intent(in) :: other${ranksuffix(RANK)}$
    equal_arrayContainer${IHEADER}$Allocatable_equal_array_raw_${IHEADERSRC}$_${RANK}$ = .false.
    if (self % getSize() == ${RANK}$) then
      write(*,*) "shr_arrayGridFull_mod:: "//&
              "equal_arrayContainer${IHEADER}$Allocatable_equal_array_raw_${IHEADERSRC}$_${RANK}$::", &
               size(other), size(self % r${RANK}$)
      equal_arrayContainer${IHEADER}$Allocatable_equal_array_raw_${IHEADERSRC}$_${RANK}$ = all(self % r${RANK}$ == other)
    else
      !< unexpected, inconsistency found
      equal_arrayContainer${IHEADER}$Allocatable_equal_array_raw_${IHEADERSRC}$_${RANK}$ = .false.
    endif
  end function equal_arrayContainer${IHEADER}$Allocatable_equal_array_raw_${IHEADERSRC}$_${RANK}$
    #:endfor
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
