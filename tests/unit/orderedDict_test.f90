!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : orderedDict_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> Ordered Dictionary unit test
!------------------------------------------------------------------------------
module orderedDict_test
  use SHR_orderedDict_mod, only: orderedDict 
  use SHR_strings_mod, only: string, stringCollection
  use SHR_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuiteOrderedDict

  type, extends(testSuite) :: testSuiteOrderedDict

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteOrderedDict), intent(inout) :: self

    type(ordereDdict) :: o
    class(*), pointer :: wrapper
    type(stringCollection) :: sc ! debug only
    type(string), allocatable :: expAllKeys(:)


    ! init ordeerdDict
    o = orderedDict()

    ! populate
    allocate(wrapper, source=20)
    call o % insert("first", wrapper)
    allocate(wrapper, source=30)
    call o % insert("second", wrapper)
    allocate(wrapper, source=40)
    call o % insert("third", wrapper)
    allocate(wrapper, source=50)
    call o % insert("fourth", wrapper)

    ! getSize
    call self % assert(o % getSize() == 4, "o % getSize() .eq. 4 == T")

    ! getAllKeys
    allocate(expAllKeys(4))
    expAllKeys(1) = string("first")
    expAllKeys(2) = string("second")
    expAllKeys(3) = string("third")
    expAllKeys(4) = string("fourth")

    sc = stringCollection(expAllKeys)
!    write(*,*) "testSuiteOrderedDict_test:: expected =", sc % toString()

    sc = stringCollection(o % getAllKeys())
!    write(*,*) "testSuiteOrderedDict_test:: found =", sc % toString()

    call self % assert(all(o % getAllKeys() == expAllKeys), &
            "o % getAllKeys() .eq. [first, second, third, fourth] == T")

    ! hasKey
    call self % assert(o % hasKey("first"), "o % getKey(first) == T")


    ! remove
    call o % remove("fourth")

    expAllKeys = [string("first"), string("second"), string("third")]

    sc = stringCollection(expAllKeys)
!    write(*,*) "testSuiteOrderedDict_test:: expected =", sc % toString()

    sc = stringCollection(o % getAllKeys())
!    write(*,*) "testSuiteOrderedDict_test:: found =", sc % toString()

    call self % assert(all(o % getAllKeys() == expAllKeys), &
            "o % getAllKeys() .eq. [first, second, third] == T")


    allocate(wrapper, source=60)
    call o % insert("fifth", wrapper)

    allocate(wrapper, source=70)
    call o % set("sixth", wrapper)

    expAllKeys = [string("first"), string("second"), string("third"), string("fifth"), string("sixth") ]
    call self % assert(all(o % getAllKeys() == expAllKeys), &
            "o % getAllKeys() .eq. [first, second, third, fifth, sithx] == T")



    ! override value from existing key
    allocate(wrapper, source=10)
    call o % set("fifth", wrapper)

    expAllKeys = [string("first"), string("second"), string("third"), string("fifth"), string("sixth") ]
    call self % assert(all(o % getAllKeys() == expAllKeys), &
            "o % getAllKeys() .eq. [first, second, third, fifth, sithx] == T")

    ! remove
    call o % remove("third")
    call o % remove("fifth")
    call o % remove("sixth")
    call o % remove("first")
    call o % remove("second")
    call self % assert(size(o % getAllKeys()) == 0, &
            "o % getAllKeys() .eq. [] == T")

  end subroutine defineTestCases

end module orderedDict_test

