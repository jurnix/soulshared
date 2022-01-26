!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMask_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gridMask unit tests
!------------------------------------------------------------------------------
module shr_gridMaskBorder_test

  use shr_precision_mod, only: sp
  use SHR_testSuite_mod, only: testSuite

  use shr_strings_mod, only: string
  use shr_gridMask_mod, only: shr_gridMask
  use shr_gridMaskBorder_mod, only: shr_gridMaskBorder
  use shr_gridMaskEnabled_mod, only: shr_gridMaskEnabled
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
  use shr_gridcellIndex_mod, only: shr_gridcellIndex

  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gAxisMapping_mod, only: shr_gAxisMapping
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_gGridArrayMap_mod, only: shr_gGridArrayMap

  use shr_gGrid_mod, only: shr_gGrid

  implicit none

  private
  public :: testSuiteGridMaskBorder

  real(kind=sp), parameter :: RES = 1.0

  type, extends(testSuite) :: testSuitegridMaskBorder

  contains
    procedure :: define => defineTestCases
    procedure, private :: testCaseIsValidGridMask
    procedure, private :: testCaseIsNotValidGridMask

    procedure, private :: testCaseIsValidGridMaskEnabled
    procedure, private :: testCaseIsNotValidGridMaskEnabled
  end type 

contains

  type(shr_gridMask) function getNewGridMask(newBounds)
    !< create a new shr_gridMask
    real(kind=sp) :: newBounds(4) !< N, S, E, W
    type(shr_gridBounds) :: bounds
    type(shr_gGridDescriptor) :: gDescriptor
    type(shr_gGridArrayMap) :: gridmap
    type(shr_gGrid) :: grid

    call bounds % init(newBounds)
    call gDescriptor % init(RES, bounds)
    gridmap = getNewGridmap(gDescriptor)
    call grid % init(gDescriptor, gridmap)
    call getNewGridMask % init(grid)
  end function getNewGridMask


  type(shr_gridMaskBorder) function getNewGridMaskBorder(newBounds)
    !< create a new shr_gridMask
    real(kind=sp) :: newBounds(4) !< N, S, E, W
    type(shr_gridBounds) :: bounds
    type(shr_gGridDescriptor) :: gDescriptor
    type(shr_gGridArrayMap) :: gridmap
    type(shr_gGrid) :: grid

    call bounds % init(newBounds)
    call gDescriptor % init(RES, bounds)

    gridmap = getNewGridmap(gDescriptor)
    call grid % init(gDescriptor, gridmap)

    call getNewGridMaskBorder % init(grid)
  end function getNewGridMaskBorder


  type(shr_gGridArrayMap) function getNewGridmap(gdescriptor)
    !< create a new gridmap
    type(shr_gGridDescriptor), intent(in) :: gdescriptor
    type(shr_gridBounds) :: bounds
    type(shr_gGridAxesBounds) :: laxisBounds, lonxisBounds
    type(shr_gAxisMapping) :: laxisMapping, lonxisMapping
    real(kind=sp) :: res
    type(shr_gGridAxes) :: laxis, lonxis

    res = gdescriptor % getResolution()
    bounds = gdescriptor % getBounds()

    call laxisBounds % init(bounds % getNorth(), bounds % getSouth())
    call lonxisBounds % init(bounds % getEast(), bounds % getWest())

    call laxis % init(string("lats"), res, laxisBounds)
    call lonxis % init(string("lons"), res, lonxisBounds)

    call laxisMapping % init(laxis)
    call lonxisMapping % init(lonxis)

    call getNewGridmap % init(gdescriptor, laxisMapping, lonxisMapping)
  end function getNEwGridmap


  type(shr_gGridDescriptor) function getNewGridDesriptor(north, south, east, west)
    !< creates a new shr_gGridDescriptor
    real(kind=sp), intent(in) :: north, south
    real(kind=sp), intent(in) :: east, west
    type(shr_gridBounds) :: bounds
    call bounds % init(north, south, east, west)
    call getNewGridDesriptor % init(RES, bounds)
  end function getNewGridDesriptor


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridMaskBorder), intent(inout) :: self
    call self % testCaseIsValidGridMask()
    call self % testCaseIsNotValidGridMask()

    call self % testCaseIsValidGridMaskEnabled()
    call self % testCaseIsNotValidGridMaskEnabled()
  end subroutine defineTestCases


  subroutine testCaseIsNotValidGridMask(self)
    !<
    class(testSuitegridMaskBorder), intent(inout) :: self

    type(shr_gridMaskBorder) :: b
    type(shr_gridMask) :: bOther
    type(shr_gridcellIndex) :: gcIndex

    !< isIncluded
    b = getNewGridMaskBorder([1., -1., 2., 0.]) !< true default
    call gcIndex % init(1, 2)
    call b % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 1)
    call b % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 2)
    call b % setStatus(gcIndex, .false.)

    bOther = getNewGridMask([1., -1., 2., 0.])
    call self % assert( .not. b % isValid(bOther), &
        "testCaseIsNotValidGridMask(TF,FF) % isIncluded(TT,TT) = F")
  end subroutine testCaseIsNotValidGridMask


  subroutine testCaseIsValidGridMask(self)
    !<
    class(testSuitegridMaskBorder), intent(inout) :: self

    type(shr_gridMaskBorder) :: b
    type(shr_gridMask) :: bOther
    type(shr_gridcellIndex) :: gcIndex

    !< isIncluded
    b = getNewGridMaskBorder([1., -1., 2., 0.]) !< true default
    call gcIndex % init(1, 1)
    call b % setStatus(gcIndex, .false.)
    call gcIndex % init(1, 2)
    call b % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 1)
    call b % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 2)
    call b % setStatus(gcIndex, .false.)

    bOther = getNewGridMask([1., -1., 2., 0.])
    call gcIndex % init(1, 1)
    call bOther % setStatus(gcIndex, .false.)

    call self % assert( b % isValid(bOther), &
        "testCaseIsValidGridMask((FF,FF)) % isValid(FT,TT) = T")
  end subroutine testCaseIsValidGridMask


  subroutine testCaseIsNotValidGridMaskEnabled(self)
    !<
    class(testSuitegridMaskBorder), intent(inout) :: self

    class(shr_gridMaskBorder), allocatable :: b
    class(shr_gridMask), allocatable :: bOther
    type(shr_gridcellIndex) :: gcIndex

    !< isIncluded
    !< border vs active -> false
    !< F F T  vs F F T
    !< T T T     T T T
    allocate(b, bOther)
    b = getNewGridMaskBorder([1., -1., 3., 0.]) !< true default
    call gcIndex % init(1, 1)
    call b % setStatus(gcIndex, .false.)
    call gcIndex % init(1, 2)
    call b % setStatus(gcIndex, .false.)

    bOther = getNewGridMaskBorder([1., -1., 3., 0.])
    call gcIndex % init(1, 1)
    call bOther % setStatus(gcIndex, .false.)
    call gcIndex % init(1, 2)
    call bOther % setStatus(gcIndex, .false.)

    call self % assert(.not. b % isValid(bOther), &
        "testCaseIsNotValidGridMaskEnabled, b(FFT,TTT) % isIncluded(FFT,TTT) = F")
  end subroutine testCaseIsNotValidGridMaskEnabled


  subroutine testCaseIsValidGridMaskEnabled(self)
    !<
    class(testSuitegridMaskBorder), intent(inout) :: self

    class(shr_gridMaskBorder), allocatable :: b
    class(shr_gridMask), allocatable :: bOther
    type(shr_gridcellIndex) :: gcIndex

    !< isIncluded
    !< T T F  vs F F T
    !< F F F     T T T
    allocate(b, bOther)
    bOther = getNewGridMaskBorder([1., -1., 3., 0.]) !< true default
    call gcIndex % init(1, 3)
    call bOther % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 1)
    call bOther % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 2)
    call bOther % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 3)
    call bOther % setStatus(gcIndex, .false.)

    b = getNewGridMaskBorder([1., -1., 3., 0.])
    call gcIndex % init(1, 1)
    call b % setStatus(gcIndex, .false.)
    call gcIndex % init(1, 2)
    call b % setStatus(gcIndex, .false.)

    call self % assert( b % isValid(bOther), &
        "testCaseIsValidGridMaskEnabled((FFT,TTT)) % isIncluded(TTF,FFF) = T")
  end subroutine testCaseIsValidGridMaskEnabled


end module shr_gridMaskBorder_test

