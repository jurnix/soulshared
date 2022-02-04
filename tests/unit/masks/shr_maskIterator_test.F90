!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_maskIterator_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_maskIterator unit tests
!------------------------------------------------------------------------------
module shr_maskIterator_test

  use shr_testSuite_mod, only: testSuite

  use shr_maskIterator_mod, only: shr_maskIterator
  use shr_mask_stub, only: shr_mask2dEmptyStub
  use shr_arrayIndices_mod, only: shr_arrayGridcellIndex

  implicit none

  private
  public :: testSuiteMaskIterator


  type, extends(shr_mask2dEmptyStub) :: shr_mask2dFake
    !< T T F
    !< F T T
    logical :: mask(6) = [.true.,  .true., .false., &
                           .false., .true., .true.]
    integer :: counter = 1
  contains
    procedure :: getSize
    procedure :: getShape
    procedure :: getByGridcellIndex
  end type shr_mask2dFake


  type, extends(testSuite) :: testSuiteMaskIterator
  contains
    procedure :: define => defineTestCases
  end type testSuiteMaskIterator
contains


  function getShape(self) result (s)
    !< get 'mask' shape
    class(shr_mask2dFake), intent(in) :: self
    integer :: s(2) !< row=1, col=2
    s=[2,3]
  end function getShape


  integer function getSize(self) result (s)
    !< gets lmask size
    class(shr_mask2dFake), intent(in) :: self
    s = 6
  end function getSize


  function getByGridcellIndex(self, arrayGridcellIndex) result (l)
    !< gets unique value requested by
    class(shr_mask2dFake), intent(in) :: self
    type(shr_arrayGridcellIndex) :: arrayGridcellIndex
    integer, save ::  counter = 1
    logical :: l !< output
    l = self % mask(counter)
    counter = counter + 1
  end function getByGridcellIndex


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteMaskIterator), intent(inout) :: self

    type(shr_mask2dFake) :: mask
    type(shr_maskIterator) :: miterator

    call miterator % init(mask)
    call self % assert(miterator % hasNext(), "miterator(TTF,FTT) % hasNext() = T")
    call self % assert(miterator % getLogicalNext(), "miterator(TTF,FTT) % getNext() = T")
    call self % assert(miterator % getLogicalNext(), "miterator(TTF,FTT) % getNext() = T")
    call self % assert(.not. miterator % getLogicalNext(), "miterator(TTF,FTT) % getNext() = F")
    call self % assert(.not. miterator % getLogicalNext(), "miterator(TTF,FTT) % getNext() = F")
    call self % assert(miterator % getLogicalNext(), "miterator(TTF,FTT) % getNext() = T")
    call self % assert(miterator % getLogicalNext(), "miterator(TTF,FTT) % getNext() = T")
    call self % assert(.not. miterator % hasNext(), "miterator(TTF,FTT) % hasNext() = F")

  end subroutine defineTestCases

end module shr_maskIterator_test