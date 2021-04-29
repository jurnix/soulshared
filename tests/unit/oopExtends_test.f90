!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : oopExtends_test 
!
!> @author
!
! DESCRIPTION:
!>
!> 
!------------------------------------------------------------------------------
module oopExtends_test
  use SHR_testSuite_mod, only: testSuite
  use SHR_oopExtends_mod, only: all_same_type_as

  implicit none

  private
  public :: testSuiteOopExtends


  ! change here test suite name
  type, extends(testSuite) :: testSuiteOopExtends

  contains
    procedure :: define => defineTestCases
  end type 

  type parent
    integer :: num
  end type parent


  type, extends(parent) :: child1
    integer :: some
  end type child1


  type, extends(parent) :: child2
    integer :: someone
  end type child2


contains


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteOopExtends), intent(inout) :: self

    class(parent), allocatable :: emptyParent(:)
    class(parent), allocatable :: objParent
    class(child1), allocatable :: sameChilds(:)
    class(parent), allocatable :: mixChilds(:)

    class(child1), allocatable :: objChild1

    allocate(emptyParent(0), objParent)
    allocate(sameChilds(3), mixChilds(3))

    select type (wrap => mixChilds(1))
    type is (child1)
       wrap = child1(num=1, some=2)
    end select

    select type (wrap => mixChilds(2))
    type is (child2)
      wrap = child2(num=2, someone=3)
    end select

    select type (wrap => mixChilds(3))
    type is (child1)
      wrap = child1(num=3, some=4)
    end select

    ! all_same_type_as 
    call self % assert (.not. all_same_type_as(emptyParent, objParent), "all_same_type_as(emptyParent, objParent) == F")

    call self % assert ( all_same_type_as(sameChilds, objChild1), "all_same_type_as(sameChilds1, objChild1) == T")

    call self % assert (.not. all_same_type_as(mixChilds, objChild1), "all_same_type_as(mixChilds, objChild1) == F")
    

  end subroutine defineTestCases


end module oopExtends_test

