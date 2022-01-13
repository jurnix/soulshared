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
!> 'Set' uses wrapObject to ensure given values are all supported
!>
!------------------------------------------------------------------------------
!
module SHR_set_mod

  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp, dp
  use SHR_objects_mod, only: shr_genericObject
  use SHR_linkedlist_mod, only: linkedlist, linkedListNode

  implicit none 

  public :: set


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


contains


  subroutine append(this, value)
    !> Add a value to the list at the tail
    class(set), intent(inout) :: this
    class(*), intent(in), pointer :: value

    class(shr_genericObject), allocatable :: wrapObj
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
    class(shr_genericObject), allocatable :: valueWrapObj

!    write (*,*) "set_mod:: exists:: starting..."
    ielem = 0 
    nelems = this % length()
!    write (*,*) "set_mod:: exists:: nelems =", nelems
    allocate(hasSameValues(nelems))
    hasSameValues = .false.

    allocate(valueWrapObj)
!    valueWrapObj => genericObject(value)
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
       class(shr_genericObject), allocatable :: nodeObj

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

end module SHR_set_mod
