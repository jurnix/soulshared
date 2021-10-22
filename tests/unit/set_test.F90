!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : set_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> Set unit test
!------------------------------------------------------------------------------
module set_test
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp, dp
  use SHR_set_mod, only: set
  use SHR_objects_mod, only: SHR_eqObject_abs
  use SHR_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuiteSet

  type, extends(testSuite) :: testSuiteSet

    contains
      procedure :: define => defineTestCases
  end type 


  type, extends(SHR_eqObject_abs) :: stringEQ
    character(:), allocatable :: chars
  contains
    procedure :: eq_object => eq_object_StringEq 
  end type

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteSet), intent(inout) :: self
    class(*), pointer :: wrap
    class(stringEQ), pointer :: temperature
    type(set) :: s, c

    s = set()

    allocate(wrap, source=25)
    call s % append(wrap)
    nullify(wrap)

    allocate(wrap, source="home")
    call s % append(wrap)
    nullify(wrap)

    allocate(wrap, source=1.0_sp)
    call s % append(wrap)
    nullify(wrap)

    allocate(wrap, source=3.141516_dp)
    call s % append(wrap)
    nullify(wrap)

    allocate(temperature) ! eqObject_abs
    temperature % chars = "temp"
    wrap => temperature
    call s % append(wrap)
    nullify(wrap)

    call self % assert(s % length() == 5, "s % length() .eq. 5 = T")

    ! same list
    c = set()

    allocate(wrap, source=3.141516_dp)
    call c % append(wrap)
    nullify(wrap)

    allocate(wrap, source="home")
    call c % append(wrap)
    nullify(wrap)

    allocate(wrap, source=25)
    call c % append(wrap)
    nullify(wrap)

    allocate(wrap, source=1.0_sp)
    call c % append(wrap)
    nullify(wrap)

    allocate(temperature) ! eqObject_abs
    temperature % chars = "temp"
    wrap => temperature
    call c % append(wrap)
    nullify(wrap)
    call self % assert(s == c, "s .eq. c = T")

    allocate(wrap, source=25.0_sp)
    call s % append(wrap)
    nullify(wrap)

    call self % assert(.not. s == c, "s .eq. c = F")
    call self % assert(s .in. c, "s .in. c = T")
    call self % assert(.not. (c .in. s), "c .in. s = F")

  end subroutine defineTestCases


  elemental logical function eq_object_stringEq(self, other)
    !< true if self and other are not the same
    !< keep it simple for unit tests purpose only
    class(stringEq), intent(in) :: self
    class(SHR_eqObject_abs), intent(in) :: other
    class(stringEq), pointer :: pstringEq

    select type(obj => other)
    type is (stringEq)
      pStringEq => obj
    class default
      ! error, different types
      eq_object_stringEq = .false.
    end select

    if (.not. allocated(self % chars)) then
      eq_object_stringEq = .false.
    endif
    if (.not. allocated(pStringEq % chars)) then
      eq_object_stringEq = .false.
    endif
    eq_object_stringEq = (self % chars == pStringEq % chars)
  end function eq_object_stringEq

end module set_test

