!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : dict_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> Dictionary unit tests
!------------------------------------------------------------------------------
module dict_test
  use SHR_dict_mod, only: dict 
  use SHR_strings_mod, only: string 
  use SHR_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuiteDict

  type, extends(testSuite) :: testSuiteDict

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteDict), intent(inout) :: self

    type(dict) :: d
    class(*), pointer :: wrapper
    integer, target :: newVal
    type(string) :: expAllKeys(2)
    class(*), pointer :: genericValue


    ! create dict with 2 elements
    d = dict()
    newVal = 25
    wrapper => newVal
    call d % insert("first", wrapper)
    
    newVal = 30
    wrapper => newVal
    call d % insert("second", wrapper)

    ! getSize
    call self % assert(d % getSize() == 2, "dict % getSize() .eq. 2 == T")

    ! getAllKeys
    expAllKeys(2) = string("first")
    expAllKeys(1) = string("second")
    call self % assert(all(d % getAllKeys() == expAllKeys), "dict % getAllKeys() .eq. [second, first] == T")

    ! hasKey
    call self % assert(d % hasKey("first"), "dict % getKey(first) == T")

    ! remove
    call d % remove("first")

    ! hasKey
    call self % assert(.not. d % hasKey("first"), "dict % getKey(first) == F (after remove)")

    ! set
    newVal = 35
    wrapper => newVal
    call d % set("second", wrapper)

    ! get
    genericValue => d % get("second")
    select type (genericValue)
    type is (integer)
      call self % assert (genericValue == 35, "dict % get(second) .eq. 35 == T")
    class default
      call self % assert (.false., "dict % get(second) .eq. 35 == T")
    end select


  end subroutine defineTestCases

end module dict_test

