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
!> Unordered
!>
!> Support basic types and eqObject_abs for derived types
!> Derived types must be comparable
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
!     final :: setfinalize
  end type set 

  interface set 
    module procedure set_constructor_from_linkedlist, set_constructor_empty
  end interface set


contains


  subroutine append(this, value)
    !> Add a value to the list at the tail
    class(set), intent(inout) :: this
    class(*), intent(in), pointer :: value

    write(*,*) "set_mod:: append:: starting..."

    ! discover dest type
    select type(wrap => value) 
    type is (integer)
            write(*,*) "set_mod:: append:: integer type found"
    type is (character(*))
            write(*,*) "set_mod:: append:: char type found"
    type is (real(kind=sp))
            write(*,*) "set_mod:: append:: real sp type found"
    type is (real(kind=dp))
            write(*,*) "set_mod:: append:: real dp type found"
    class is (eqObject_abs)
            write(*,*) "set_mod:: append:: eqObject_abs type found"
    class default
      !< assert
      call raiseError(__FILE__, "append", &
              "Unsupported type found")
    end select

    if (.not. this % exists(value)) then
      write(*,*) "set_mod:: append:: object not found in 'set'"
      call this % linkedlist % append(value)
    endif
    write(*,*) "set_mod:: append:: done"
  end subroutine append


  logical function exists(this, value)
    !< true if key is found in set
    class(set), intent(inout) :: this
    class(*), intent(in), pointer :: value
    integer :: nelems, ielem
    logical, allocatable :: hasSameValues(:)

!    write (*,*) "set_mod:: exists:: starting..."
    ielem = 0 
    nelems = this % length()
!    write (*,*) "set_mod:: exists:: nelems =", nelems
    allocate(hasSameValues(nelems))
    hasSameValues = .false.

    call this % traverse(existsObject)

    exists = any(hasSameValues)
!    write (*,*) "set_mod:: exists:: done"
  contains

    subroutine existsObject(node)
       !> sets exists(ielem) as true if current object exists
       !> discover type for 'node % value' and 'values'
       !> if same type then it compares their values
       type(LinkedListNode), pointer, intent(inout)  :: node
       integer, pointer :: intsrc, intdst
       real(kind=sp), pointer :: rspsrc, rspdst
       real(kind=dp), pointer :: rdpsrc, rdpdst
       character(:), pointer :: chrsrc, chrdst
       class(eqObject_abs), pointer :: objsrc, objdst

       rspsrc => null()
       rspdst => null()
       rdpsrc => null()
       rdpdst => null()
       chrsrc => null()
       chrdst => null()
       objsrc => null()
       objdst => null()

       ielem = ielem + 1

       ! discover source type
       select type(wrap => node % value)
       type is (integer)
         write(*,*) "set_mod:: exists:: existsObject:: src is int"
         intsrc => wrap
       type is (character(*))
         write(*,*) "set_mod:: exists:: existsObject:: src is chars"
         chrsrc => wrap
       type is (real(kind=sp))
         write(*,*) "set_mod:: exists:: existsObject:: src is rsp"
         rspsrc => wrap
       type is (real(kind=dp))
         write(*,*) "set_mod:: exists:: existsObject:: src is rdp"
         rdpsrc => wrap
       class is (eqObject_abs)
         write(*,*) "set_mod:: exists:: existsObject:: src is eqObject_abs"
         objsrc => wrap
       class default
         !< assert
         call raiseError(__FILE__, "existsObject", &
                 "Unexpected source type found")
       end select

       ! discover dest type
       select type(wrap => value) 
       type is (integer)
         write(*,*) "set_mod:: exists:: existsObject:: dst is int"
         intdst => wrap
       type is (character(*))
         write(*,*) "set_mod:: exists:: existsObject:: dst is chars"
         chrdst => wrap
       type is (real(kind=sp))
         write(*,*) "set_mod:: exists:: existsObject:: dst is rsp"
         rspdst => wrap
       type is (real(kind=dp))
         write(*,*) "set_mod:: exists:: existsObject:: dst is rdp"
         rdpdst => wrap
       class is (eqObject_abs)
         write(*,*) "set_mod:: exists:: existsObject:: dst is eqObject_abs"
         objdst => wrap
       class default
         !< assert
         call raiseError(__FILE__, "existsObject", &
                 "Unexpected destination type found")
       end select

       ! same value?
       if (associated(intdst) .and. associated(intsrc)) then
         hasSameValues(ielem) = (intsrc == intdst)
       else if (associated(chrdst) .and. associated(chrsrc) ) then
         write(*,*) "set_mod:: exists:: existsObject:: both chars type"
         hasSameValues(ielem) = (chrsrc == chrdst)
       else if (associated(rspdst) .and. associated(rspsrc) ) then
         hasSameValues(ielem) = (rspsrc == rspdst)
       else if (associated(rdpdst) .and. associated(rdpsrc) ) then
         hasSameValues(ielem) = (rdpsrc == rdpdst)
       else if (associated(objdst) .and. associated(objsrc)) then
         hasSameValues(ielem) = (objsrc == objdst)
       else 
         ! different types
         hasSameValues(ielem) = .false.
       endif

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
