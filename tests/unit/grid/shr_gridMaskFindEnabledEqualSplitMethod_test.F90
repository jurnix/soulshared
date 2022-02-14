!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskClusters_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_gridMaskFindEnabledEqualSplitMethod unit tests
!------------------------------------------------------------------------------
module shr_gridMaskFindEnabledEqualSplitMethod_test
  use SHR_testSuite_mod, only: testSuite

  use shr_strings_mod, only: string
  use shr_precision_mod, only: sp
  use shr_gridMaskFindEnabledEqualSplitMethod_mod, only: shr_gridMaskFindEnabledEqualSplitMethod
  use shr_gridMask_mod, only: shr_IgridMask, shr_gridMask
  use shr_gridBoundIndices_mod, only: shr_gridBoundIndices
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gGrid_mod, only: shr_gGrid
  use shr_gGridArrayMap_mod, only: shr_gGridArrayMap, LATITUDE_NAME, LONGITUDE_NAME
  use shr_gAxis_mod, only: shr_gAxis
  use shr_gAxisBounds_mod, only: shr_gAxisBounds
  use shr_gAxisMapping_mod, only: shr_gAxisMapping
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor

  implicit none

  private
  public :: testSuiteGridMaskFindEnabledEqualMethod


  type, extends(testSuite) :: testSuiteGridMaskFindEnabledEqualMethod
  contains
    procedure :: define => defineTestCases
    procedure, private :: testCaseSplitInFive
    procedure, private :: testCaseSplitInTwo
  end type 

contains



  type(shr_gGridArrayMap) function createNewGridmap(gdescriptor)
    !< create a new gridmap
    type(shr_gGridDescriptor), intent(in) :: gdescriptor
    type(shr_gridBounds) :: bounds
    real(kind=sp) :: res
    type(shr_gAxis) :: laxis, lonxis
    type(shr_gAxisBounds) :: laxisBounds, lonxisBounds
    type(shr_gAxisMapping) :: laxisMapping, lonxisMapping

    res = gdescriptor % getResolution()
    bounds = gdescriptor % getBounds()

    call laxisBounds % init(bounds % getNorth(), bounds % getSouth())
    call lonxisBounds % init(bounds % getEast(), bounds % getWest())

    call laxis % init(string(LATITUDE_NAME), res, laxisBounds)
    call lonxis % init(string(LONGITUDE_NAME), res, lonxisBounds)

    call laxisMapping % init(laxis)
    call lonxisMapping % init(lonxis)

    call createNewGridmap % init(gdescriptor, laxisMapping, lonxisMapping)
  end function createNewGridmap


  type(shr_gGridDescriptor) function createNewGridDescriptor(resolution, bounds)
    !<
    !< create a new shr_gGridDescriptor instance
    real(kind=sp), intent(in) :: resolution
    real(kind=sp), intent(in) :: bounds(4) !< n, s, e, w
    type(shr_gridBounds) :: sbounds
    call sbounds % init(bounds)
    call createNewGridDescriptor % init(resolution, sbounds)
  end function createNewGridDescriptor


  type(shr_gGrid) function createNewGrid(gDescriptor)
    !< creates a new grid
    type(shr_gGridDescriptor), intent(in) :: gDescriptor
    type(shr_gGridArrayMap) :: gridmap
    gridmap = createNewGridmap(gDescriptor)
    call createNewGrid % init(gDescriptor, gridmap)
  end function createNewGrid


  function createNewGridMask(resolution, bounds, lmask) result (newGridMask)
    !<
    real(kind=sp), intent(in) :: resolution
    real(kind=sp), intent(in) :: bounds(4)
    logical, intent(in) :: lmask(:,:)
    type(shr_gridMask) :: newGridMask !< output
    type(shr_gGrid) :: grid

    type(shr_gGridDescriptor) :: gDescriptor
    gDescriptor = createNewGridDescriptor(resolution, bounds)
    grid = createNewGrid(gDescriptor)
    call newGridMask % init(grid, lmask)
  end function createNewGridMask

  !<
  !< unit test
  !<
  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteGridMaskFindEnabledEqualMethod), intent(inout) :: self
    call self % testCaseSplitInFive()
    call self % testCaseSplitInTwo()
  end subroutine defineTestCases


  subroutine testCaseSplitInFive(self)
    !<
    !< (5) (3 2 2 2 2) only used for proper alignment
    !<    (4     0)
    !< (3) x x x x    x x x -      - - x
    !<     x x - x ->         -> x       -> - x x x  ->          ->
    !< (0) x x x x                                        x x - -     - - x x
    !<
    class(testSuiteGridMaskFindEnabledEqualMethod), intent(inout) :: self
    type(shr_gridMaskFindEnabledEqualSplitMethod) :: gmSplit
    type(shr_gridMask) :: gmask
    type(shr_gridMask) :: expectedMasks(5)
    logical :: lmask(3,4)
    real(kind=sp), parameter :: BOUNDS(4) = [3., 0., 4., 0.]
    real(kind=sp), parameter :: RES = 1.0
    !type(string) :: tmpstr
    !type(shr_gridMask) :: tmpMask

    !< setup
    lmask = .true.
    lmask(2,3) = .false.
    gmask = createNewGridMask(RES, BOUNDS, lmask)

    !<
    lmask = .false.
    lmask(1,1:3) = .true.
    expectedMasks(1) = createNewGridMask(RES, BOUNDS, lmask)
    lmask = .false.
    lmask(1,4) = .true.
    lmask(2,1) = .true.
    expectedMasks(2) = createNewGridMask(RES, BOUNDS, lmask)
    lmask = .false.
    lmask(2,2:4) = .true.
    expectedMasks(3) = createNewGridMask(RES, BOUNDS, lmask)
    lmask = .false.
    lmask(3,1:2) = .true.
    expectedMasks(4) = createNewGridMask(RES, BOUNDS, lmask)
    lmask = .false.
    lmask(3,3:4) = .true.
    expectedMasks(5) = createNewGridMask(RES, BOUNDS, lmask)

    call gmSplit % init(5, gmask)

    !> tests
    call self % assert(gmSplit % getSize() == 5, &
        "gmSplit(...) % getSize() .eq. 5 = T")

    call self % assert(gmSplit % get(1) == expectedMasks(1), &
        "gmSplit(...) % get(1) .eq. mask(...) = T")

    call self % assert(gmSplit % get(2) == expectedMasks(2), &
        "gmSplit(...) % get(2) .eq. mask(...) = T")

    !tmpMask = gmSplit % get(3)
    !tmpStr = tmpMask % toString()
    !write(*,*) "gmSplit % get(3) =", tmpStr % toString()
    call self % assert(gmSplit % get(3) == expectedMasks(3), &
        "gmSplit(...) % get(3) .eq. mask(...) = T")

    call self % assert(gmSplit % get(4) == expectedMasks(4), &
        "gmSplit(...) % get(4) .eq. mask(...) = T")

    call self % assert(gmSplit % get(5) == expectedMasks(5), &
        "gmSplit(...) % get(5) .eq. mask(...) = T")

  end subroutine testCaseSplitInFive


  subroutine testCaseSplitInTwo(self)
    !<
    !< (2) (3 3) only used for proper alignment
    !<
    !< (3) x x - x    x x - x
    !<     - - - x ->         -> - - - x
    !< (0) x - - x               x - - x
    !<
    class(testSuiteGridMaskFindEnabledEqualMethod), intent(inout) :: self
    type(shr_gridMaskFindEnabledEqualSplitMethod) :: gmSplit
    type(shr_gridMask) :: gmask
    type(shr_gridMask) :: expectedMasks(2)
    logical :: lmask(3,4)
    real(kind=sp), parameter :: BOUNDS(4) = [3., 0., 4., 0.]
    real(kind=sp), parameter :: RES = 1.0

    !< setup
    lmask = .false.
    lmask(1,:) = [.true., .true., .false., .true.]
    lmask(2,:) = [.false., .false., .false., .true.]
    lmask(3,:) = [.true., .false., .false., .true.]
    gmask = createNewGridMask(RES, BOUNDS, lmask)

    !<
    lmask = .false.
    lmask(1,:) = .true.
    expectedMasks(1) = createNewGridMask(RES, BOUNDS, lmask)
    lmask = .false.
    lmask(2:3,:) = .true.
    expectedMasks(2) = createNewGridMask(RES, BOUNDS, lmask)

    call gmSplit % init(2, gmask)

    !> tests
    call self % assert(gmSplit % getSize() == 2, &
        "gmSplit(...) % getSize() .eq. 2 = T")

    call self % assert(gmSplit % get(1) == expectedMasks(1), &
        "gmSplit(...) % get(1) .eq. mask(...) = T")

    call self % assert(gmSplit % get(2) == expectedMasks(2), &
        "gmSplit(...) % get(2) .eq. mask(...) = T")

  end subroutine testCaseSplitInTwo

end module shr_gridMaskFindEnabledEqualSplitMethod_test

