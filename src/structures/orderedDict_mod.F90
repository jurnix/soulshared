!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : orderedDict_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!
!>  Ordered Dictionary
!>
!> Is a subtype of dictionary that remembers in which order
!> keys are inserted.
!------------------------------------------------------------------------------
!
module SHR_orderedDict_mod

  use SHR_error_mod, only: raiseError
  use SHR_tst_mod, only: tst
  use SHR_strings_mod, only: string
  use SHR_linkedlist_mod, only: linkedlist, linkedListNode
  use SHR_dict_mod, only: dict

  implicit none 

  public :: orderedDict


  type, extends(dict) :: OrderedDict 
    ! keep track of added keys
    type(linkedlist), allocatable :: keys !<string>
  contains

    procedure :: insertObject

    ! generic
    procedure :: remove
    procedure :: set
    procedure :: getAllKeys
    final :: dictfinalize

  end type orderedDict

  interface orderedDict
    module procedure orderedDict_constructor
  end interface orderedDict


contains

  subroutine insertObject(this, key, value)
    !< insert a new element into the dict with key = value
    !< In case it already exists the key, it overrides its value
    class(Ordereddict), intent(inout) :: this
    character(len=*), intent(in), target :: key
    class(*), pointer, intent(in) :: value
    class(*), pointer :: wrap
    type(string), pointer :: str
!    write(*,*) "orderedDict_mod:: insertObject:: inserting '", key, "' ..."
    allocate(str)
    str = string(key)
    wrap => str
    call this % keys % append(wrap)
    call this % dict % insert(key, value) ! parent
!    write(*,*) "orderedDict_mod:: insertObject:: done"
  end subroutine insertObject


  subroutine remove(this, key)
    !< remove key and its value from the dictionary
    class(orderedDict), intent(inout) :: this
    character(len=*), intent(in) :: key
    logical :: isKeyFound
    integer :: ielem
    type(LinkedListNode), pointer :: node

!    write(*,*) "orderedDict_mod:: remove:: key = '", key, "' ..."

    isKeyFound = .false.
    call this % dict % remove(key)

    ! search key
    do ielem = 1, this % keys % length()
      node => this % keys % atindex(ielem)
      select type(wrap => node % value)
      type is (string)
        if (wrap == key) then
          isKeyFound = .true.
          exit ! found!
        endif
      class default
        !assert
        call raiseError(__FILE__, "remove", &
                "Unexpected type found")
      end select
    enddo
    if (isKeyFound) call this % keys % remove(ielem)
!    write(*,*) "orderedDict_mod:: remove:: done"
  end subroutine remove


  subroutine set(this, key, value)
    !< redefines key's value
    !< if it does not exists it is added as well
    class(orderedDict), intent(inout) :: this
    character(len=*), intent(in), target :: key
    class(*), pointer, intent(in) :: value
    class(*), pointer :: wrap
    class(string), pointer :: strKey
!    write(*,*) "orderedDict_mod:: set:: key = '", key, "' ..."
    if (.not. this % hasKey(key)) then
!      write(*,*) "orderedDict_mod:: set:: key not found, so inserting ..."
      allocate(strKey)
      strKey = string(key)
      wrap => strKey
      call this % keys % append(wrap)
    endif
!    write(*,*) "orderedDict_mod:: set:: done"
    call this % d % insert(key, value)
  end subroutine set


  type(orderedDict) function orderedDict_constructor()
    !< dict constructor
    orderedDict_constructor % dict = dict() ! parent constructor
    allocate(orderedDict_constructor % keys)
  end function orderedDict_constructor


  function getAllKeys(this) result (keys)
    !< returns an array with all keys in Order of append/set
    class(orderedDict), intent(inout) :: this
    type(string), allocatable :: keys(:) !< output
    integer :: ielem, nelems

!    write(*,*) "linkedlist_mod:: getAllKeys:: start..."

    ielem = 1
    nelems = this % getSize()
!    write(*,*) "linkedlist_mod:: getAllKeys:: elems = ", nelems
    if (allocated(keys)) deallocate(keys)
    allocate(keys(nelems)) 
    call this % keys % traverse(extractStrings)
!    write(*,*) "linkedlist_mod:: getAllKeys:: done"

    contains

      subroutine extractStrings(node)
        !< cast linkedListNode value to string
        type(LinkedListNode), pointer, intent(inout)  :: node
        select type(wrap => node % value)
        type is (string)
!           write(*,*) "linkedlist_mod:: extractStrings:: ielem, key, size=", ielem, wrap % toString(), size(keys)
           keys(ielem) = wrap !string(wrap)
        class default
           call raiseError(__FILE__, "extractStrings", &
                   "Unexpected type found")
        end select
        ielem = ielem + 1
      end subroutine extractStrings
  end function getAllKeys


  subroutine dictfinalize(this)
    !< destroy the class
    type(orderedDict), intent(inout) :: this
    if (allocated(this % keys)) deallocate(this % keys)
  end subroutine dictfinalize


end module SHR_orderedDict_mod
