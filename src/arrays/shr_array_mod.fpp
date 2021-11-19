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
  use SHR_error_mod, only: raiseError 
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

    !<
    logical :: isInit = .false.
  contains
    procedure :: init_array_as_chars
    procedure(iface_init_array), deferred :: init_array
    generic :: init => init_array, init_array_as_chars

    procedure :: getName => getName_array
    procedure :: getSize => getSize_array
    procedure :: getDims => getDims_array
    procedure :: getUnits => getUnits_array
    procedure :: getDescription => getDescription_array

    procedure(iface_copy_array_copy_array), deferred :: copy_array_copy_array 
    generic, public :: assignment(=) => copy_array_copy_array

    procedure(iface_equal_array_equal_array), deferred :: equal_array_equal_array 
    generic, public :: operator(==) => equal_array_equal_array

  #:for OP_NAME, OP_SYMB in OPERATOR_TYPES

    procedure(iface_op_array_op_array), deferred :: op_array_${OP_NAME}$_array
    generic, public :: operator(${OP_SYMB}$) => op_array_${OP_NAME}$_array
  #:endfor
  end type shr_array


  abstract interface
    subroutine iface_init_array(self, name, dimensions, units, description)
      !< initialize shr_array
      import :: shr_array, shr_arrayDimContainer, string
      class(shr_array), intent(inout) :: self
      type(string), intent(in) :: name
      type(shr_arrayDimContainer), intent(in) :: dimensions(:)
      type(string), intent(in) :: units
      type(string), intent(in) :: description
    end subroutine iface_init_array


    subroutine iface_copy_array_copy_array(self, another)
      !< copy shr_array = shr_array
      import :: shr_array
      class(shr_array), intent(inout) :: self
      class(shr_array), intent(in) :: another
    end subroutine iface_copy_array_copy_array

     logical function iface_equal_array_equal_array(self, other)
      !< true if self and other are the same
      import :: shr_array
      class(shr_array), intent(in) :: self
      class(shr_array), intent(in) :: other
    end function  iface_equal_array_equal_array

     function iface_op_array_op_array(left, right) Result(total)
      !< addition from shr_arrayXXX and shr_array
      import :: shr_array
      class(shr_array), intent(in) :: left
      class(shr_array), intent(in) :: right
      class(shr_array), allocatable :: total !< output
    end function iface_op_array_op_array
  end interface


#:for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES
  !
  ! ${IHEADER}$, ${ITYPE}$, ${IKIND}$
  !
  type, extends(shr_array) :: shr_array${IHEADER}$ !< apply each type and kind
  contains

    procedure :: init_array => init_array_${IHEADER}$ !_as_strings

    procedure :: copy_array_copy_array => copy_array${IHEADER}$_copy_array

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

#:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
    generic, public :: assignment(=) => copy_array${IHEADER}$_scalar_${IHEADERSRC}$
#:endfor

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

    ! ${OP_NAME}$ shr_arrayXXX op shr_array
    procedure :: op_array_${OP_NAME}$_array => ${OP_NAME}$_array${IHEADER}$_${OP_NAME}$_array

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

    ! equal (shr_arrayXXX = shr_array)
    procedure :: equal_array_equal_array => equal_array${IHEADER}$_equal_array

  #:for _, _, IHEADERSRC  in ALL_KINDS_TYPES
    generic, public :: operator(==) => equal_array${IHEADER}$_scalar_${IHEADERSRC}$
  #:endfor

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
   
   type(string) function getName_array(self)
    !< returns how many dimensions 
    class(shr_array), intent(in) :: self
    getName_array = self % name
  end function getName_array

   
   pure integer function getSize_array(self)
    !< returns how many dimensions 
    class(shr_array), intent(in) :: self
    getSize_array = size(self % dims)
  end function getSize_array


   function getDims_array(self) result (dims)
    !< returns array dimensions as an allocatable array
    class(shr_array), intent(in) :: self
    type(shr_arrayDimContainer), allocatable :: dims(:) !< output
    integer :: idim
    !if (allocated(dims)) deallocate(dims)
    allocate(dims, source = self % dims)
!    allocate(dims(size(self % dims)))
!      write(*,*) "shr_array_mod:: getDims_array:: dims size =", size(self % dims)
!    do idim = 1, size(self % dims)
!      write(*,*) "shr_array_mod:: getDims_array:: idim =", idim
!      allocate(dims(idim) % arrayDim, source = self % dims(idim) % arrayDim)
!    enddo
  end function getDims_array


   type(string) function getUnits_array(self)
    !< its returns its units
    class(shr_array), intent(in) :: self
    getUnits_array = self % units
  end function getUnits_array


   type(string) function getDescription_array(self)
    !< it returns its description
    class(shr_array), intent(in) :: self
    getDescription_array = self % description
  end function getDescription_array


  subroutine init_array_as_chars(self, name, dimensions, units, description)
    !< shr_arrayRsp initialization
    class(shr_array), intent(inout) :: self
    character(*), intent(in) :: name
    type(shr_arrayDimContainer), intent(in) :: dimensions(:)
    character(*), intent(in) :: units
    character(*), intent(in) :: description

    !< local vars
    type(string) :: sname, sunits, sdescription

    sname = string(name)
    sunits = string(units)
    sdescription = string(description)

    call self % init_array(sname, dimensions, sunits, sdescription)
  end subroutine init_array_as_chars


#:for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES
  !
  ! ${IHEADER}$, ${ITYPE}$, ${IKIND}$
  !
  subroutine init_array_${IHEADER}$(self, name, dimensions, units, description)
    !< shr_arrayRsp initialization
    class(shr_array${IHEADER}$), intent(inout) :: self
    type(string), intent(in) :: name
    type(shr_arrayDimContainer), intent(in) :: dimensions(:)
    type(string), intent(in) :: units
    type(string), intent(in) :: description

    write(*,*) "shr_array_mod:: init_array_${IHEADER}$:: initializing..."

    if (self % isInit) then
      call raiseError(__FILE__, "init_array_${IHEADER}$", &
              name % toString()//" is already initialized")
    endif
    self % isInit = .true.

    allocate(self % name)
    self % name = name
    allocate(self % units)
    self % units = units
    allocate(self % description)
    self % description = description

    allocate(self % dims, source = dimensions)

    write(*,*) "shr_array_mod:: init_array_${IHEADER}$:: initializing data..."
    allocate( shr_arrayContainer${IHEADER}$Allocatable :: self % data )
    call self % data % init(dimensions)
    write(*,*) "shr_array_mod:: init_array_${IHEADER}$:: initializing data...DONE"
    write(*,*) "shr_array_mod:: init_array_${IHEADER}$:: initializing... DONE"

  end subroutine init_array_${IHEADER}$


#:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
  ! copy (shr_array = <type, kind> scalar)
   subroutine copy_array${IHEADER}$_scalar_${IHEADERSRC}$(self, other)
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
   subroutine copy_raw_${IHEADERTGT}$_${RANK}$_to_array${IHEADER}$(other, self)
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
   subroutine copy_array${IHEADER}$_to_raw_${IHEADERSRC}$_${RANK}$(self, other)
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


  subroutine copy_array${IHEADER}$_copy_array(self, another)
    !< Copy to current array container allocatable
    !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
    class(shr_array${IHEADER}$), intent(inout) :: self
    class(shr_array), intent(in) :: another

    class(shr_arrayContainer), allocatable :: tmpData
    type(string) :: tmpName
    type(shr_arrayDimContainer), allocatable :: tmpDims(:)

!    write(*,*) "shr_array_mod:: copy_array${IHEADER}$_copy_array:: initializing ..."

!    write(*,*) "shr_array_mod:: copy_array${IHEADER}$_copy_array:: another dimensions size =", &
!            size(another % dims)
    self % isInit = another % isInit

!    write(*,*) "shr_array_mod:: copy_array${IHEADER}$_copy_array:: copying name ..."
    if (.not. allocated(self % name)) allocate(self % name)
    self % name = another % getName()

!    write(*,*) "shr_array_mod:: copy_array${IHEADER}$_copy_array:: copying dims ..."
    tmpDims = another % getDims()
    if (allocated(self % dims)) deallocate(self % dims)
    allocate(self % dims, source = tmpDims)

!    write(*,*) "shr_array_mod:: copy_array${IHEADER}$_copy_array:: copying units ..."
    if (.not. allocated(self % units)) allocate(self % units)
    self % units = another % getUnits()

!    write(*,*) "shr_array_mod:: copy_array${IHEADER}$_copy_array:: copying description ..."
    if (.not. allocated(self % description)) allocate(self % description)
    self % description = another % getDescription()

!    write(*,*) "shr_array_mod:: copy_array${IHEADER}$_copy_array:: copying data ..."
    ! temporary copy dataContainer into a temporary variable
    ! to avoid any issues when copying to itself
    allocate(tmpData, source = another % data)
    if (allocated(self % data)) deallocate(self % data)
    allocate(self % data, source = tmpData)
!    write(*,*) "shr_array_mod:: copy_arrayrsp_copy_array:: copying container data ... DONE"

!    write(*,*) "shr_array_mod:: copy_array${IHEADER}$_copy_array:: initializing ... DONE"
  end subroutine copy_array${IHEADER}$_copy_array


  #:for OP_NAME, OP_SYMB in OPERATOR_TYPES
  !
  ! ${OP_NAME}$, ${OP_SYMB}$
  !

  #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
  function ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADERSRC}$(left, right) Result(total)
    !< addition shr_arrayRsp and scalar rsp
    class(shr_array${IHEADER}$), intent(in) :: left
    ${ITYPESRC}$, intent(in) :: right
    class(shr_array${IHEADER}$), allocatable :: total !< output

!    write(*,*) "shr_array_mod:: ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADERSRC}$:: starting..."

    select type(data => left % data)
    type is (shr_arrayContainer${IHEADER}$Allocatable)
!      write(*,*) "shr_array_mod:: ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADERSRC}$:: starting ${OP_NAME}$..."
      total % data = data ${OP_SYMB}$ right
!      write(*,*) "shr_array_mod:: ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADERSRC}$:: starting ${OP_NAME}$... DONE"
    class default
      !< unexpected class found
    end select
!    write(*,*) "shr_array_mod:: ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADERSRC}$:: starting... DONE"
  end function ${OP_NAME}$_array${IHEADER}$_scalar_${IHEADERSRC}$
  #:endfor


  #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
    #:for RANK in RANKS

  function ${OP_NAME}$_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$(left, right) Result(total)
    !< addition shr_array${IHEADER}$ and scalar ${IHEADER}$
    class(shr_array${IHEADER}$), intent(in) :: left
    ${ITYPESRC}$, intent(in) :: right${ranksuffix(RANK)}$
    class(shr_array${IHEADER}$), allocatable :: total !< output

    write(*,*) "shr_array_mod:: ${OP_NAME}$_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$:: starting..."
    if (.not. allocated(total)) allocate(total, source = left)

    select type(data => left % data)
    type is (shr_arrayContainer${IHEADER}$Allocatable)
      write(*,*) "shr_array_mod:: ${OP_NAME}$_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$:: starting ${OP_NAME}$ ..."
      total % data = data ${OP_SYMB}$ right
      write(*,*) "shr_array_mod:: ${OP_NAME}$_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$:: starting ${OP_NAME}$ DONE"
    class default
      !< unexpected class found
    end select
    write(*,*) "shr_array_mod:: ${OP_NAME}$_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$:: starting... DONE"
  end function ${OP_NAME}$_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$

    #:endfor
  #:endfor


   function ${OP_NAME}$_array${IHEADER}$_${OP_NAME}$_array(left, right) Result(total)
    !< addition from shr_arrayRsp and shr_arrayRsp
    class(shr_array${IHEADER}$), intent(in) :: left
    class(shr_array), intent(in) :: right
    class(shr_array), allocatable :: total !< output

    !if (.not. allocated(total)) allocate(total, source=left)
    allocate(total, source=left)

    if (left % getSize() /= right % getSize()) then
      write(*,*) "${OP_NAME}$_array${IHEADER}$_${OP_NAME}$_array:: left size =", left % getSize()
      write(*,*) "${OP_NAME}$_array${IHEADER}$_${OP_NAME}$_array:: right size =", right % getSize()
      call raiseError(__FILE__, "${OP_NAME}$_array${IHEADER}$_${OP_NAME}$_array", &
              "Both shr_array must have the same dimensions")
    endif

    select type(data => left % data)
    type is (shr_arrayContainer${IHEADER}$Allocatable)
      total % data = data ${OP_SYMB}$ right % data
    class default
      !< unexpected class found
    end select
  end function ${OP_NAME}$_array${IHEADER}$_${OP_NAME}$_array

  ! OP_NAME, OP_SYMB in OPERATOR_TYPES  
  #:endfor

  !
  ! equal
  !
   logical function equal_array${IHEADER}$_equal_array(self, other)
    !< true if self and other are the same
    !< - in case 'other' is not initialized, nothing happens or error is raised
    !<   but its values are not copied
    class(shr_array${IHEADER}$), intent(in) :: self
    class(shr_array), intent(in) :: other

    logical :: hasSameName, hasSameDims, hasSameUnits
    logical :: hasSameDescription, hasSameData

    equal_array${IHEADER}$_equal_array = .false.
    ! compare status
    if (.not. other % isInit) then
      call raiseError(__FILE__, "equal_array${IHEADER}$_equal_array", &
              "source array must be initialized")
    endif

    ! self is not initialized
    if (.not. self % isInit) return

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
      hasSameData = .false.
    end select

    equal_array${IHEADER}$_equal_array = hasSameData
  end function equal_array${IHEADER}$_equal_array


  #:for IKINDSRC, ITYPESRC, IHEADERSRC in ALL_KINDS_TYPES
   logical function equal_array${IHEADER}$_scalar_${IHEADERSRC}$(self, other)
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
     logical function equal_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$(self, other)
      !< true if self and other are the same
      class(shr_array${IHEADER}$), intent(in) :: self
      ${ITYPESRC}$, intent(in) :: other${ranksuffix(RANK)}$
      select type(data => self % data)
      type is (shr_arrayContainer${IHEADER}$Allocatable)
        equal_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$ = data == other
      class default
        !< unexpected class found
      end select !
    end function equal_array${IHEADER}$_raw_${IHEADERSRC}$_${RANK}$
    #:endfor
  #:endfor
#:endfor

end module SHR_array_mod
