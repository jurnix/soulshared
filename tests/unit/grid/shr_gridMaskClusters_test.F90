!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskClusters_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gridMaskClusters unit tests
!------------------------------------------------------------------------------
module shr_gridMaskClusters_test
  use SHR_testSuite_mod, only: testSuite

  use shr_gridMaskClusters_mod, only: shr_gridMaskClusters
  use shr_gridMask_stub, only: shr_gridMaskStub
  use shr_gridMask_mod, only: shr_gridMask
  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor

  implicit none

  private
  public :: testSuitegridMaskClusters

  type, extends(testSuite) :: testSuitegridMaskClusters
  contains
    procedure :: define => defineTestCases
  end type 

contains


  !<
  !< unit test
  !<
  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridMaskClusters), intent(inout) :: self

    class(shr_gridMaskStub), allocatable :: gmStub
    type(shr_gridMaskClusters) :: c
    type(shr_gridMask) :: expFirst, expSecond, expThird
    type(shr_gridMask) :: foundFirst, foundSecond, foundThird
    class(shr_iGgridDescriptor), allocatable :: gDescriptor
    logical :: lmask(4,3)

    !procedure :: init
    allocate(gmStub)
    call c % init(gmStub)
    call self % assert(.true., "c % init(1, [4,0,3,0]) = T")
    deallocate(gmStub)

    !procedure :: getSize
    call self % assert(c % getSize() == 3, &
        "c % getSize() .eq. 3 = T")

    !procedure :: get
    !< first
    allocate(gmStub)
    write(*,*) "running 1 ..."
    gDescriptor = gmStub % getGridDescriptor()
    write(*,*) "running 2 ..."
    lmask = .false.
    lmask(1:2,:) = .true.
    write(*,*) "running 3 ..."
    call expFirst % init(gDescriptor, lmask)
    write(*,*) "running 4 ..."
    foundFirst = c % get(1)
    write(*,*) "running 5 ..."
    call self % assert(expFirst == foundFirst, &
          "c(TTT,TTT,TFF,FTT) % get(1) .eq. shr_gridMask(TTT,TTT,FFF,FFF) = T")
    write(*,*) "running 6 ..."
    deallocate(gmStub)

    !< second
    allocate(gmStub)
    gDescriptor = gmStub % getGridDescriptor()
    lmask = .false.
    lmask(3,1) = .true.
    call expSecond % init(gDescriptor, lmask)
    foundSecond = c % get(2)
    call self % assert(expSecond == foundSecond, &
        "c(TTT,TTT,TFF,FTT) % get(2) .eq. shr_gridMask(FFF,FFF,TFF,FFF) = T")
    deallocate(gmStub)

    !< 3rd
    allocate(gmStub)
    gDescriptor = gmStub % getGridDescriptor()
    lmask = .false.
    lmask(4,2:3) = .true.
    call expThird % init(gDescriptor, lmask)
    foundThird = c % get(3)
    call self % assert(expThird == foundThird, &
        "c(TTT,TTT,TFF,FTT) % get(3) .eq. shr_gridMask(FFF,FFF,FFF,FTT) = T")
    deallocate(gmStub)
  end subroutine defineTestCases

end module shr_gridMaskClusters_test

