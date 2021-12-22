!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskClustersIterator_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gridMaskClustersIterator unit tests
!------------------------------------------------------------------------------
module shr_gridMaskClustersIterator_test
  use SHR_testSuite_mod, only: testSuite

  use shr_gridMask_stub, only: shr_gridMaskStub
  use shr_gridMask_mod, only: shr_gridMask, shr_gridMask_cast
  use shr_gridMaskClusters_mod, only: shr_gridMaskClusters
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
  use shr_gridMaskClustersIterator_mod, only: shr_gridMaskClustersIterator

  implicit none

  private
  public :: testSuitegridMaskClustersIterator


  type, extends(testSuite) :: testSuitegridMaskClustersIterator
  contains
    procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridMaskClustersIterator), intent(inout) :: self
    type(shr_gridMaskStub) :: gmStub
    type(shr_gridMaskClusters) :: clusters
    type(shr_gridMaskClustersIterator) :: i
    type(shr_gridMask) :: expFirst, expSecond, expThird
    type(shr_gridMask) :: foundFirst, foundSecond, foundThird
    type(shr_gGridDescriptor) :: gDescriptor
    logical :: lmask(4,3)
    class(*), allocatable :: pObject

    call clusters % init(gmStub)
    call i % init(clusters)

    call self % assert(.true., "i % init(clusters(TTT,TTT,TFF,FTT)) = T")

    !
    !procedure :: hasNext
    call self % assert(i % hasNext() , "i(TTT,TTT,TFF,FTT) % hasNext() = T")

    !procedure :: getNext
    !< first
    gDescriptor = gmStub % getGridDescriptor()
    lmask = .false.
    lmask(1:2,:) = .true.
    call expFirst % init(gDescriptor, lmask)
    pObject = i % getNext()
    call shr_gridMask_cast(pObject, foundFirst)
    call self % assert(expFirst == foundFirst, &
        "i(TTT,TTT,TFF,FTT) % get(1) .eq. shr_gridMask(TTT,TTT,FFF,FFF) = T")

    !< second
    gDescriptor = gmStub % getGridDescriptor()
    lmask = .false.
    lmask(3,1) = .true.
    call expSecond % init(gDescriptor, lmask)
    pObject = i % getNext()
    call shr_gridMask_cast(pObject, foundSecond)
    call self % assert(expSecond == foundSecond, &
        "c(TTT,TTT,TFF,FTT) % get(2) .eq. shr_gridMask(FFF,FFF,TFF,FFF) = T")

    !< 3rd
    gDescriptor = gmStub % getGridDescriptor()
    lmask = .false.
    lmask(4,2:3) = .true.
    call expThird % init(gDescriptor, lmask)
    pObject = i % getNext()
    call shr_gridMask_cast(pObject, foundThird)
    call self % assert(expThird == foundThird, &
        "c(TTT,TTT,TFF,FTT) % get(3) .eq. shr_gridMask(FFF,FFF,FFF,FTT) = T")
  end subroutine defineTestCases

end module shr_gridMaskClustersIterator_test

