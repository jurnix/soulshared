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
    wrap => key    
    call this % keys % append(wrap)
    call this % dict % insert(key, value) ! parent
  end subroutine insertObject


  subroutine remove(this, key)
    !< remove key and its value from the dictionary
    class(orderedDict), intent(inout) :: this
    character(len=*), intent(in) :: key
    class(*), pointer :: wrap
    logical :: isKeyFound
    integer :: ielem
    type(string), pointer :: str

    isKeyFound = .false.
    call this % dict % remove(key)

    ! search key
    do ielem = 1, this % keys % length()
      wrap => this % keys % atindex(ielem)
      select type(wrap)
      type is (string)
        str => wrap
        if (str == key) then
          isKeyFound = .true.
          exit ! found!
        endif
      class default
        !assert
      end select
    enddo
    if (isKeyFound) call this % keys % remove(ielem)
  end subroutine remove


  subroutine set(this, key, value)
    !< redefines key's value
    !< if it does not exists it is added as well
    class(orderedDict), intent(inout) :: this
    character(len=*), intent(in), target :: key
    class(*), pointer, intent(in) :: value
    class(*), pointer :: wrap
    call this % d % insert(key, value)
    if (.not. this % hasKey(key)) then
      wrap => key
      call this % keys % append(wrap)
    endif
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

    ielem = 1
    nelems = this % getSize()
    if (allocated(keys)) deallocate(keys)
    allocate(keys(nelems)) 
    call this % keys % traverse(extractStrings)

    contains

      subroutine extractStrings(node)
        !< cast linkedListNode value to string
        type(LinkedListNode), pointer, intent(inout)  :: node
        select type(wrap => node % value)
        type is (string)
           keys(ielem) = wrap
        class default
           ! assert
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
