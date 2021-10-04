!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : set_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!
!>  Set
!>
!> Based on https://docs.python.org/3.9/library/stdtypes.html#set
!>
!> Unordered, unique elements (not repeated)
!>
!> Support basic types and eqObject_abs for derived types:
!>
!> - Supported basic types: integer, real(sp), real(dp) and character
!> - Derived types must be a subclass of eqObject_abs. It implements
!> eq_Object procedure so any given class can be compared.
!> Required by exists procedure.
!>
!------------------------------------------------------------------------------
!
module SHR_set_mod

  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp, dp
  use SHR_linkedlist_mod, only: linkedlist, linkedListNode

  implicit none 

  public :: set, eqObject_abs


  type, abstract :: eqObject_abs !< equal unlimited polymorphic type
                                 !< for non basic types
  contains
    procedure(iface_eq_object), deferred :: eq_object
    generic :: operator(==) => eq_object
  end type eqObject_abs


  abstract interface
    logical function iface_eq_object(self, other)
      import eqObject_abs
      !< true if self and other are not the same
      class(eqObject_abs), intent(in) :: self
      class(eqObject_abs), intent(in) :: other
    end function iface_eq_object
  end interface 


  type, extends(linkedlist) :: set 
    
  contains
     procedure :: append 
     procedure :: exists
     procedure :: eq_set
     procedure :: in_set

     generic :: operator(==) => eq_set     
     generic :: operator(.eq.) => eq_set     
     generic :: operator(.in.) => in_set
  end type set 

  interface set 
    module procedure set_constructor_from_linkedlist, set_constructor_empty
  end interface set


  !< current wrapObject type
  integer, parameter :: WRAP_OBJ_TYPE_NONE=-1
  integer, parameter :: WRAP_OBJ_TYPE_INT=0
  integer, parameter :: WRAP_OBJ_TYPE_RSP=1
  integer, parameter :: WRAP_OBJ_TYPE_RDP=2
  integer, parameter :: WRAP_OBJ_TYPE_CHAR=3
  integer, parameter :: WRAP_OBJ_TYPE_EQO=4 !< eqObject


  ! setObject generic container for 'set' data structure
  type, extends(eqObject_abs) :: wrapObject
    class(*), pointer :: obj => null()

    !< cast value
    class(eqObject_abs), pointer :: eqObj => null()
    integer, pointer :: intObj => null()
    character(:), pointer :: chrObj => null()
    real(kind=sp), pointer :: rspObj => null()
    real(kind=dp), pointer :: rdpObj => null()

    integer :: type !< NONE=-1, 0=int, 1=rsp, 2=rdp, 3=char, 4=eqObject_abs
  contains
    procedure :: init => init_wrapObject !< constructor_wrapObject
    procedure :: eq_object => eq_wrapObject
  end type wrapObject


  interface wrapObject
    module procedure constructor_wrapObject
  end interface wrapObject

contains


  subroutine append(this, value)
    !> Add a value to the list at the tail
    class(set), intent(inout) :: this
    class(*), intent(in), pointer :: value

    class(wrapObject), allocatable :: wrapObj
    write(*,*) "set_mod:: append:: starting..."

    allocate(wrapObj)
    call wrapObj % init(value)

    if (.not. this % exists(value)) then
      write(*,*) "set_mod:: append:: object not found in 'set'. Appending..."
      call this % linkedlist % append(value)
    endif
    write(*,*) "set_mod:: append:: done"
  end subroutine append


  logical function in_set(self, other)
    !< true if all 'other' elements are found in 'self'
    class(set), intent(in) :: self
    class(set), intent(in) :: other
    integer :: nelems, ielem
    logical, allocatable :: hasSameValues(:)

    ielem = 0 
    nelems = other % length()
    allocate(hasSameValues(nelems))
    hasSameValues = .false.

    ! each value found in 'other' set?
    call other % traverse_safe(searchElem)
    in_set = all(hasSameValues)
  contains

    subroutine searchElem(node)
       !< search node value into 'other' set
       type(LinkedListNode), pointer, intent(in)  :: node
       ielem = ielem + 1
       hasSameValues(ielem) = self % exists(node % value)
    end subroutine 
  end function in_set


  logical function eq_set(self, other)
    !< true if other is equal to self
    !< equal is defined as the same object
    !< with no specific order
    class(set), intent(in) :: self
    class(set), intent(in) :: other
    integer :: nelems, ielem
    logical, allocatable :: hasSameValues(:)
    logical :: hasSameLen

    write(*,*) "set_mod:: eq_set:: starting..."

    ielem = 0 
    nelems = self % length()
    write(*,*) "set_mod:: eq_set:: elements found =", nelems
    allocate(hasSameValues(nelems))
    hasSameValues = .false.

    ! both lists have same length?
    hasSameLen = (self % length() == other % length())

    eq_set = .false.
    if (hasSameLen) then
      ! each value found in 'other' set?
      call self % traverse_safe(searchElem)
      eq_set = all(hasSameValues)
    endif
    write(*,*) "set_mod:: eq_set:: done"
  contains

    subroutine searchElem(node)
       !< search node value into 'other' set
       type(LinkedListNode), pointer, intent(in)  :: node
       ielem = ielem + 1
       hasSameValues(ielem) = other % exists(node % value)
    end subroutine 
  end function eq_set


  logical function exists(this, value)
    !< true if key is found in set
    class(set), intent(in) :: this
    class(*), intent(in), pointer :: value

    integer :: nelems, ielem
    logical, allocatable :: hasSameValues(:)
    class(wrapObject), allocatable :: valueWrapObj

!    write (*,*) "set_mod:: exists:: starting..."
    ielem = 0 
    nelems = this % length()
!    write (*,*) "set_mod:: exists:: nelems =", nelems
    allocate(hasSameValues(nelems))
    hasSameValues = .false.

    allocate(valueWrapObj)
!    valueWrapObj => wrapObject(value)
    call valueWrapObj % init(value)

    call this % traverse_safe(existsObject)

    exists = any(hasSameValues)
!    write (*,*) "set_mod:: exists:: done"
  contains

    subroutine existsObject(node)
       !> sets exists(ielem) as true if current object exists
       !> discover type for 'node % value' and 'values'
       !> if same type then it compares their values
       type(LinkedListNode), pointer, intent(in)  :: node

       allocate(nodeObj)
       call nodeObj % init(node % value)

       ielem = ielem + 1
       hasSameValues(ielem) = (nodeObj == valueWrapObj)
    end subroutine existsObject

  end function exists


  type(set) function set_constructor_empty()
    !< set constructor from linked list
    !< all repeated elements from llist are removed
    !set_constructor_empty = 
  end function set_constructor_empty


  type(set) function set_constructor_from_linkedlist(llist)
    !< set constructor from linked list
    !< all repeated elements from llist are removed
    type(linkedlist), intent(inout) :: llist

    call llist % traverse(toSet)
  contains

    subroutine toSet(node)
       !> toSet add the current node into 'set' structure
       !> in case it does not exist yet.
       type(LinkedListNode), pointer, intent(inout)  :: node
       class(*), pointer :: value
       value => node % value
       call set_constructor_from_linkedlist % append(value) 
    end subroutine toSet

  end function set_constructor_from_linkedlist


  logical function eq_wrapObject(self, other)
    !< true if self and other are the same type and have the same values
    !< false if same type and different values
    !< false if different types
    !< error if other is not an otherWrapObj
    class(wrapObject), intent(in) :: self
    class(eqObject_abs), intent(in) :: other

    class(wrapObject), pointer :: otherWrapObj

    ! cast 'other' to wrapObject
    select type(wrap => other)
    class is (wrapObject)
      otherWrapObj => wrap
    class default
      !< error
      call raiseError(__FILE__, "eq_wrapObject", &
              "Unexpected 'other' class type found", &
              "It is only allowed wrapObject type")
    end select

    if (self % type /= otherWrapObj % type) then
      eq_wrapObject = .false.
      return
    endif

    if (self % type == WRAP_OBJ_TYPE_INT) then
      eq_wrapObject = (self % intObj == otherWrapObj % intObj)
    else if (self % type == WRAP_OBJ_TYPE_CHAR) then
      eq_wrapObject = (self % chrObj == otherWrapObj % chrObj)
    else if (self % type == WRAP_OBJ_TYPE_RSP) then
      eq_wrapObject = (self % rspObj == otherWrapObj % rspObj)
    else if (self % type == WRAP_OBJ_TYPE_RDP) then
      eq_wrapObject = (self % rdpObj == otherWrapObj % rdpObj)
    else if (self % type == WRAP_OBJ_TYPE_EQO) then
      eq_wrapObject = (self % eqObj == otherWrapObj % eqObj)
    else
      !< assert
      call raiseError(__FILE__, "eq_wrapObject", &
              "Unexpected error found", &
              "Non supported and misterious type found")
    endif

  end function eq_wrapObject


  type(wrapObject) function constructor_wrapObject(obj)
    !< wrap unlimited polymorhpic type
    class(*), intent(in), pointer :: obj
    call constructor_wrapObject % init(obj)
  end function constructor_wrapObject


  subroutine init_wrapObject(self, obj)
    !< initialize wrapObject
    !< Wrap unlimited polymorhpic type
    class(wrapObject), intent(inout) :: self
    class(*), intent(in), pointer :: obj

    self % obj => obj
    self % type = -1

    ! discover and cast type
    select type(wrap => obj) 
    type is (integer)
      write(*,*) "set_mod:: self:: integer type found"
      self % type = WRAP_OBJ_TYPE_INT
      self % intObj => wrap
    type is (character(*))
      write(*,*) "set_mod:: self:: char type found"
      self % type = WRAP_OBJ_TYPE_CHAR
      self % chrObj => wrap
    type is (real(kind=sp))
      write(*,*) "set_mod:: self:: real sp type found"
      self % type = WRAP_OBJ_TYPE_RSP
      self % rspObj => wrap
    type is (real(kind=dp))
      write(*,*) "set_mod:: self:: real dp type found"
      self % type = WRAP_OBJ_TYPE_RDP
      self % rdpObj => wrap
    class is (eqObject_abs)
      write(*,*) "set_mod:: self:: eqObject_abs type found"
      self % type = WRAP_OBJ_TYPE_EQO
      self % eqObj => wrap
    class default
      !< assert
      call raiseError(__FILE__, "init_wrapObject", &
              "Unsupported type found", &
              "Supported types: int, char, rsp, dsp and eqObject_abs")
    end select
  end subroutine init_wrapObject

end module SHR_set_mod
