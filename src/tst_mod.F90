!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : tst_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!  Ternary search tree
!   https://github.com/sisingh/tst/blob/master/TernaraySearchTree/main.cpp
!   https://www.geeksforgeeks.org/ternary-search-tree/
!   https://www.cs.upc.edu/~ps/downloads/tst/tst.html
!> 
!------------------------------------------------------------------------------
!
module tst_mod

  use error_mod, only: raiseError
  use strings_mod, only: string, printStrings, mergeStringsArray, isEmptyStringsArray 

  implicit none 

  public :: tst

  !> ternary search tree node type - takes ownership of deallocation of the value pointer in finalize
  type :: tstNode
    character(len=:), allocatable   :: charkey
    integer                         :: charcode = -1
    class(*), pointer               :: value    => null()
    type(tstNode), pointer          :: left     => null()
    type(tstNode), pointer          :: eq       => null()
    type(tstNode), pointer          :: right    => null()
    logical                         :: isWord   = .false.
  contains
    procedure :: init
    procedure :: insert => insertnode
    procedure :: traverse => traversenode
    procedure :: getAllKeys => getAllKeysNode
    final :: nodefinalize
  end type tstNode


  !> ternary search tree - takes ownership of deallocation of the value pointer in finalize
  !! Note - do not copy lists, i.e. list1 = list2, this causes memory issues, always pass by reference
  !! Do not return a list from a function
  type :: tst 
  private
    integer                :: size = 0
    type(tstNode), pointer :: head => null()
  contains
    ! generic
    procedure :: insert => insertTst
    procedure :: remove
    procedure :: get 
    procedure :: getSize !< number of keys in the tst
    procedure :: getAllKeys
    procedure :: search => searchTst !< a key exists in the tree -> true/false
    procedure :: traverse

    ! private
    final :: tstfinalize
    procedure, private :: cleanup
    procedure, private :: getNode !< it return the pointer of a node given a key
  end type tst


contains


  recursive function getAllKeysNode(this, word, foundKeys, totalKeys) result (keys)
    class(tstNode), intent(in out) :: this
    character(len=*), intent(in) :: word !< collected chars
    integer, intent(in) :: foundKeys
    integer, intent(in) :: totalKeys
    type(string), dimension(:), allocatable :: keys ! result

    type(string), dimension(:), allocatable :: nkleft, nkeq, nkright
!    type(string), allocatable :: emptyArrString(:)

!    write(*,*) "getAllKeysNode:: ----------------->>>>"
!    write(*,*) "getAllKeysNode:: entering word, foundKeys= '", (word), "', ", foundKeys, ", '", this % charkey, "'"
!    allocate(emptyArrString(1))
!    emptyArrString(1) % val = ""

    if (associated(this % left)) then
       !write(*,*) "getAllKeysNode:: to the left: ", adjustl(word), foundkeys, totalkeys
       nkleft = this % left % getAllKeys(word, foundKeys, totalKeys)
       !write(*,*) "getAllKeysNode:: to the left after, is empty?", (trim(nkleft(1) % val) .eq. "")
       if (isEmptyStringsArray(nkleft)) deallocate(nkleft)
    endif

    if (associated(this % eq)) then
       !write(*,*) "getAllKeysNode:: to the center"
       nkeq = this % eq % getAllKeys(word//this % charkey, foundKeys + 1, totalKeys)
       !write(*,*) "getAllKeysNode:: to the center after" 
       if (isEmptyStringsArray(nkeq)) deallocate(nkeq)
    endif

    if (associated(this % right)) then
       !write(*,*) "getAllKeysNode:: to the right"
       nkright = this % right % getAllKeys(word, foundKeys, totalKeys)
       !write(*,*) "getAllKeysNode:: to the right after"
       if (isEmptyStringsArray(nkeq)) deallocate(nkright)
    endif

    if (this % isWord) then
!       write(*,*) "getAllKeysNode:: word found = '", word//this % charkey, "'"
       allocate(keys(1))
       keys(1) % val =  word//this % charkey
!       write(*,*) "getAllKeysNode:: word found, keys(1) = '", keys(1) % val, "'"
    endif

    call mergeStringsArray(nkleft, keys)
    call mergeStringsArray(nkeq, keys)
    call mergeStringsArray(nkright, keys)

    !write(*,*) "getAllKeysNode:: <<<<----- return keys= '", keys(1) % val, "'"

  end function getAllKeysNode


  subroutine init(this, key)
    class(tstNode), intent(inout) :: this
    character(len=1), intent(in) :: key

    this % charkey = key
    this % charcode = iachar(key)
    this % value    => null()
    this % left     => null()
    this % eq       => null()
    this % right    => null()
    this % isWord = .false.
    
  end subroutine init


  subroutine associnitNode(parent, key)
    !< create a new tstNode, initialize and associated with its parent
    type(tstNode), pointer, intent(in out) :: parent
    character(len=1), intent(in) :: key

    class(tstNode), pointer :: newnode

    allocate(newnode)
    call newnode % init(key)
    parent => newnode
    !write(*,*) "associnitNod:: new node=", key
  end subroutine associnitNode


  recursive subroutine insertnode(this, key, val)
    class(tstNode), intent(in out) :: this
    character(len=*), intent(in) :: key
    class(*), pointer, intent(in) :: val

    integer :: keycode
    logical :: isLastChar

    keycode = iachar(key(1:1))
    !write(*,*) ""
    !write(*,*) "tst_mod::insertnode:: key=", key
    !write(*,*) "tst_mod::current char= ", key(1:1) ," splitchar=", this % charkey

    if (key == "") return

    if (keycode < this % charcode) then
       !write(*,*) "tst_mod::insertnode:: left"
       if (.not. associated(this % left)) call associnitNode(this % left, key(1:1))
       call this % left % insert (key, val)
    else if (keycode > this % charcode) then
       !write(*,*) "tst_mod::insertnode:: right"
       if (.not. associated(this % right)) call associnitNode(this % right, key(1:1))
       call this % right % insert (key, val)
    else 
       isLastChar = (len(key) == 1)
       if (isLastChar) then 
         !write(*,*) "tst_mod::insertnode:: marked as word"
         this % isWord = .true.
         this % value => val
       else ! No 
         !write(*,*) "tst_mod::insertnode:: center"
         if (.not. associated(this % eq)) call associnitNode(this % eq, key(2:len(key)))
         call this % eq % insert (key(2:len(key)), val)
       endif
    endif


  end subroutine insertnode


  subroutine insertTst(this, key, val)
    !< To a a new object into the tst.
    !< if the key already exists then, it overrides the existing object
    !< An empty key does nothing
    class(tst), intent(in out) :: this
    character(len=*), intent(in) :: key
    class(*), pointer, intent(in) :: val

    !write(*,*) "insertTst:: key='", key, "'"

    if (key .eq. "") return

    !write(*,*) "insertTst:: already exists ", key, "? ", this % search(key)
    
    if (.not. this % search(key)) this % size = this % size + 1

    if (.not. associated(this % head)) then
      call associnitNode(this % head, key(1:1))
    end if

    call this % head % insert(key, val) 

  end subroutine insertTst 


  function searchTst(this, key) result (valfound)
    !< iterative algorithm to search in tst
    class(tst), intent(inout) :: this
    character(len=*), intent(in) :: key
    logical :: valfound

    class(tstNode), pointer :: node

    valfound = .false.

    node => this % getNode (key)
    if (associated(node)) valfound = .true.

  end function searchTst


  !> LinkedListNode destructor
  ! Clean up node - The value is deallocated here
  recursive subroutine nodefinalize(this)
    type(tstNode), intent(inout) :: this

    !write(*,*) "nodefinalize:: char=", this % charkey
    if (allocated(this % charkey)) deallocate(this % charkey)
    if (associated(this % value)) nullify(this % value)
    nullify(this%value)
    nullify(this%left)
    nullify(this%eq)
    nullify(this%right)
        
  end subroutine nodefinalize


  recursive subroutine traversenode(this, iterator_func)
    !> Go from bottom left to up
    class(tstNode), target, intent(inout) :: this
    class(tstNode), pointer :: tstnode_ptr
    interface
        subroutine iterator_func(node)
            import tstNode
            type(tstNode), pointer, intent(inout)  :: node
        end subroutine iterator_func
    end interface

    if (associated(this % left)) then
        call this % left % traverse(iterator_func)
    endif

    if (associated(this % eq)) then
        call this % eq % traverse(iterator_func)
    endif

    if (associated(this % right)) then
        call this % right % traverse(iterator_func)
    endif

    !write(*,*) "tst_mod::traverseNode::associated?", associated(this % value )
    if (associated(this % value)) then
      tstnode_ptr => this
      call iterator_func(tstnode_ptr)
      nullify(tstnode_ptr)
    endif

  end subroutine traversenode


  !> Traverse the tst
  subroutine traverse(this, iterator_func)
    class(tst), intent(inout) :: this
    interface
        subroutine iterator_func(node)
            import tstNode
            type(tstNode), pointer, intent(inout)  :: node
        end subroutine iterator_func
    end interface

    type(tstNode), pointer :: current_ptr

    current_ptr => this%head
    if (associated(current_ptr)) then
        call current_ptr % traverse(iterator_func)
    endif

  end subroutine traverse
  

  !> Clean up - deallocation of the nodes in the list
  subroutine cleanup(this)
    class(tst), intent(inout) :: this

    call this%traverse(destroyall)
    nullify(this%head)

    contains
        subroutine destroyall(node)
            type(tstNode), pointer, intent(inout)  :: node

            deallocate(node)
            nullify(node)

            this % size = this % size - 1

        end subroutine destroyall

  end subroutine cleanup


  !> Clean up - deallocation of the nodes in the list
  subroutine tstfinalize(this)
    type(tst), intent(inout) :: this

    call this%cleanup()

  end subroutine tstfinalize


  integer function getSize(this)
    class(tst), intent(in) :: this

    getSize = this % size
  end function


  function getNode(this, key) result (outnode)
    !< Given a key it returns the word node
    !< If there is no key it returns null
    class(tst), intent(in out) :: this
    character(len=*), intent(in) :: key
    class(tstNode), pointer :: outnode

    integer :: i, ascii
    class(tstNode), pointer :: node

    if (len(key) .eq. 0 ) then
      outnode => null()
      return
    endif

    i=1
    node => this % head
    ascii = iachar(key(i:i))
    outnode => null()

    do while (associated(node))
      if (ascii < node % charcode) then
        node => node % left
      else if (ascii > node % charcode) then
        node => node % right
      else ! the same

        ! last character? (it must be a word)
        if (i .eq. len(key) .and. node % isWord ) then 
          ! yes and word found!
          outnode => node
          node => null()
        else if (i .eq. len(key)) then 
          ! yes, last character but not found
          outnode => null()
          node => null()
        else ! nope
          node => node % eq
          i = i + 1 ! move to the next key char
          ascii = iachar(key(i:i))
        endif

      endif

    enddo

  end function getNode


  subroutine remove (this, key)
    !< remove key and value from tst
    !< the reference is nullified and the node is marked as isWord = false
    !< if it does not exists, nothing happens
    class(tst), intent(in out) :: this
    character(len=*), intent(in) :: key

    class(tstNode), pointer :: node

    node => this % getNode(key)
    if (.not. associated(node)) return

    node % value => null()
    node % isWord = .false.
    this % size = this % size - 1
    
  end subroutine remove


  function getAllKeys(this) result (keysFound)
    !< main call to traverse all nodes to discover all keys
    class(tst), intent(in out) :: this
    type(string), dimension(:), allocatable :: keysFound
    character(len=:), allocatable :: word

    word = ""
    if (allocated(keysFound)) deallocate(keysFound)

    if (associated(this % head)) then
      keysFound = this % head % getAllKeys(word, 0, this % getSize())
    else
      allocate(keysFound(0))
    endif

  end function getAllKeys


  function get(this, key) result (val)
    !< it returns the val assigned to the key
    !< otherwise it raises an error as not found
    class(tst), intent(in out) :: this
    character(len=*), intent(in) :: key

    class(tstNode), pointer :: node
    class(*), pointer :: val

    node => this % getNode(key)
    if (.not. associated(node)) then
       call raiseError(__FILE__, "get", "key '"//key//"' not found")
    endif

    val => node % value 

  end function get

end module tst_mod
