!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : dict_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!  Dictionary (based in Ternary search tree)
!> 
!------------------------------------------------------------------------------
!
module SHR_dict_mod

  use SHR_error_mod, only: raiseError
  use SHR_tst_mod, only: tst
  use SHR_strings_mod, only: string

  implicit none 

  public :: dict, dictWrapperArray_type

  type dictWrapperArray_type !< for real, integer, ... arrays
    class(*), pointer :: array(:)
  end type dictWrapperArray_type

  type :: dict 
  private
    type(tst), allocatable :: d 
  contains

    procedure :: insertObject
    procedure :: insertSomeObjects
    procedure :: getObject
    procedure :: getSomeObjects

    ! generic
    generic :: insert => insertObject, insertSomeObjects
    procedure :: remove
    procedure :: set
    generic :: get => getObject
    generic :: getObjects => getSomeObjects
    procedure :: getSize !< number of keys in the tst
    procedure :: getAllKeys
    procedure :: hasKey

    ! private
    final :: dictfinalize
  end type dict

  interface dict
    module procedure dict_constructor
  end interface dict


contains

  subroutine insertObject(this, key, value)
    !< insert a new element into the dict with key = value
    !< In case it already exists the key, it overrides its value
    class(dict), intent(inout) :: this
    character(len=*), intent(in) :: key
    class(*), pointer, intent(in) :: value
    
    call this % d % insert(key, value)
  end subroutine insertObject


  subroutine insertSomeObjects(this, key, values)
    !< insert a new element into the dict with key = value
    !< In case it already exists the key, it overrides its value
    class(dict), intent(inout) :: this
    character(len=*), intent(in) :: key
    class(*), pointer, intent(in) :: values(:)

    class(*), pointer :: genericWrapper
    type(dictWrapperArray_type), pointer :: wrapperArray

    allocate(wrapperArray)
    wrapperArray % array => values
    
    genericWrapper => wrapperArray
    call this % d % insert(key, genericWrapper)
  end subroutine insertSomeObjects


  subroutine remove(this, key)
    !< remove key and its value form the dictionary
    class(dict), intent(inout) :: this
    character(len=*), intent(in) :: key
 
    call this % d % remove(key)
    ! remove wrapperArray if exists
  end subroutine remove


  subroutine set(this, key, value)
    !< redefines key's value
    !< if it does not exists it is added as well
    class(dict), intent(inout) :: this
    character(len=*), intent(in) :: key
    class(*), pointer, intent(in) :: value

    call this % d % insert(key, value)
  end subroutine set


  function getObject(this, key) result (r)
    !< Read the corresponding value from the key 
    !< an error is raised if the key does not exists
    class(dict), intent(inout) :: this
    character(len=*), intent(in) :: key
    class(*), pointer :: r

    r => this % d % get(key)
  end function getObject


  function getSomeObjects(this, key) result (r)
    !< Read the corresponding value from the key 
    !< an error is raised if the key does not exists
    class(dict), intent(inout) :: this
    character(len=*), intent(in) :: key
    class(*), pointer :: r(:)

    class(*), pointer :: wrapper
    type(dictWrapperArray_type), pointer :: wrapperArray

    wrapper => this % d % get(key)
    select type(wrapper)
    type is (dictWrapperArray_type)
      wrapperArray => wrapper
      r => wrapperArray % array
    class default
      call raiseError(__FILE__, "getSomeObjects", &
              "dictWrapperArray_type not found", &
              "for Key '"//key//"'")
    end select

  end function getSomeObjects


  type(dict) function dict_constructor()
    !< dict constructor
    allocate(dict_constructor % d)
  end function dict_constructor


  integer function getSize(this)
    !< number of elements in the dictionary
    class(dict), intent(in) :: this

    getSize = this % d % getSize()
  end function getSize


  function getAllKeys(this) result (keys)
    !< returns an array with all keys 
    class(dict), intent(inout) :: this
    type(string), allocatable :: keys(:) !< output

    if (allocated(keys)) deallocate(keys)
    keys = this % d % getAllKeys()
  end function getAllKeys


  logical function hasKey(this, key)
    !< true if the 'key' is already found
    class(dict), intent(inout) :: this
    character(len=*), intent(in) :: key
    hasKey = this % d % search(key)
  end function hasKey


  subroutine dictfinalize(this)
    !< destroy the class
    type(dict), intent(inout) :: this
    if (allocated(this % d)) deallocate(this % d)
  end subroutine dictfinalize


end module SHR_dict_mod
