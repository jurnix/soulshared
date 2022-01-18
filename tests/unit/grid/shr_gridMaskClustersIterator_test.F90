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
  use shr_gridMask_mod, only: shr_gridMask, shr_gridMask_cast, shr_IgridMask
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
  use shr_gridMaskFindClustersIterator_mod, only: shr_gridMaskFindClustersIterator

  use shr_gridMaskClusters_stub, only: shr_gridMaskClustersStub

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

    type(shr_gridMaskClustersStub) :: clustersStub
    type(shr_gridMaskFindClustersIterator) :: i
    type(shr_gridMask) :: expFirst, expSecond, expThird
    class(shr_igridMask), allocatable :: foundFirst, foundSecond, foundThird
    class(*), allocatable :: pObject

    call clustersStub % init(gmStub)

    call i % init(clustersStub)
    call self % assert(.true., "i % init(clusters(TTT,TTT,TFF,FTT)) = T")

    !
    !procedure :: hasNext
    call self % assert(i % hasNext() , "i(TTT,TTT,TFF,FTT) % hasNext() = T")

    expFirst = clustersStub % getFirst()
    pObject = i % getNext()
    call shr_gridMask_cast(pObject, foundFirst)
    call self % assert(expFirst == foundFirst, &
        "i(TTT,TTT,TFF,FTT) % get(1) .eq. shr_gridMask(TTT,TTT,FFF,FFF) = T")

    !< second
    expSecond = clustersStub % getSecond()
    pObject = i % getNext()
    call shr_gridMask_cast(pObject, foundSecond)
    call self % assert(expSecond == foundSecond, &
        "c(TTT,TTT,TFF,FTT) % get(2) .eq. shr_gridMask(FFF,FFF,TFF,FFF) = T")

    !< 3rd
    expThird = clustersStub % getThird()
    pObject = i % getNext()
    call shr_gridMask_cast(pObject, foundThird)
    call self % assert(expThird == foundThird, &
        "c(TTT,TTT,TFF,FTT) % get(3) .eq. shr_gridMask(FFF,FFF,FFF,FTT) = T")
  end subroutine defineTestCases

end module shr_gridMaskClustersIterator_test

