!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        :   shr_gridDomainPartitioningMethodBySameNumberOfGridcells_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> grid domain partitioning method by same number of gridcells unit test
!------------------------------------------------------------------------------
module shr_gridDomainPartitioningMethodBySameNumberOfGridcells_test

  use shr_testSuite_mod, only: testSuite
  use shr_gridDomainPartitioningMethodBySameNumberOfGridcells_mod, only: &
            shr_gridDomainPartitioningMethodBySameNumberOfGridcells

  !< factory methods
  use shr_precision_mod, only: sp
  use shr_gridDomain_mod, only: shr_gridDomain, shr_iGridDomain
  use shr_strings_mod, only: string
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
  use shr_gridMask_mod, only: shr_gridMask
  use shr_gGridArrayMap_mod, only: shr_gGridArrayMap, LATITUDE_NAME, LONGITUDE_NAME
  use shr_gGrid_mod, only: shr_gGrid
  use shr_gAxis_mod, only: shr_gAxis
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gAxisBounds_mod, only: shr_gAxisBounds
  use shr_gAxisMapping_mod, only: shr_gAxisMapping

  implicit none

  private
  public :: testSuitegridDomainPartitioningMethodBySameNumberOfGridcells

  type, extends(testSuite) :: testSuitegridDomainPartitioningMethodBySameNumberOfGridcells

  contains
    procedure :: define => defineTestCases
    procedure, private :: testCaseWithThree
  end type
contains

  type(shr_gGrid) function createNewGrid(gDescriptor)
    !< creates a new grid
    type(shr_gGridDescriptor), intent(in) :: gDescriptor
    type(shr_gGridArrayMap) :: gridmap
    gridmap = createNewGridmap(gDescriptor)
    call createNewGrid % init(gDescriptor, gridmap)
  end function createNewGrid


  type(shr_gridDomain) function createNewGridDomain(resolution, bounds, emask, bmask)
    !< create a new shr_gridDomain instance
    real(kind=sp), intent(in) :: resolution
    real(kind=sp), intent(in) :: bounds(4) !< n, s, e, w
    logical, intent(in) :: emask(:,:) !< raw mask
    logical, intent(in) :: bmask(:,:) !< raw mask

    class(shr_gGridDescriptor), allocatable :: gDesc
    !type(shr_gridBounds) :: sbounds
    class(shr_gridMask), allocatable :: gmEnabled, gmBorder
    type(shr_gGridArrayMap) :: gridmap
    type(shr_gGrid) :: grid

    allocate(gDesc)
    gDesc = createNewGridDescriptor(resolution, bounds)
    gridmap = createNewGridmap(gDesc)
    call grid % init(gDesc, gridmap)

    allocate(gmEnabled, gmBorder)
    call gmEnabled % init(grid, emask)
    call gmBorder % init(grid, bmask)

    call createNewGridDomain % init(grid, gmEnabled, gmBorder)
  end function createNewGridDomain


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


  type(shr_gridMask) function createNewGridMask(resolution, bounds, mask)
    !< create a new shr_gridMask instance
    real(kind=sp), intent(in) :: resolution
    real(kind=sp), intent(in) :: bounds(4) !< n, s, e, w
    logical, intent(in) :: mask(:,:) !< raw mask
    class(shr_gGridDescriptor), allocatable :: gDesc
    type(shr_gGrid) :: grid

    allocate(gDesc)
    gDesc = createNewGridDescriptor(resolution, bounds)
    grid = createNewGrid(gDesc)
    call createNewGridMask % init(grid, mask)
  end function createNewGridMask


  subroutine defineTestCases(self)
    !< test
    use iso_c_binding
    class(testSuitegridDomainPartitioningMethodBySameNumberOfGridcells), intent(inout) :: self

    call self % testCaseWithThree()
  end subroutine defineTestCases


  subroutine testCaseWithThree(self)
    !<
    !> 8 enabled gridcells into 3 parts:
    !>            (domain 1)  (domain 2)    (domain 3)
    !>    (5)   (1)
    !> (4) x x - x     x x - x
    !>     x - - x  ->          -> x - - x  ->
    !>     x x - -                 x b b b      b x - -
    !> (0) - - x -                              - - x -
    class(testSuitegridDomainPartitioningMethodBySameNumberOfGridcells), intent(inout) :: self

    type(shr_gridDomainPartitioningMethodBySameNumberOfGridcells) :: gdpartition
    logical, allocatable :: emask(:,:), bmask(:,:)
    type(shr_gridDomain) :: gdomain
    class(shr_igridDomain), allocatable :: results(:)
    real(kind=sp), parameter :: BOUNDS(4) = [4., 0., 5., 1.]
    real(kind=sp), parameter :: RES = 1.0
    type(shr_gridDomain) :: expected(3)

    !< setup
    allocate(emask(4,4), bmask(4,4))
    emask(1,:) = [.true.,  .true.,  .false., .true.]
    emask(2,:) = [.true.,  .false., .false., .true.]
    emask(3,:) = [.true.,  .true.,  .false., .false.]
    emask(4,:) = [.false., .false., .true., .false.]

    bmask = .false.
    gdomain = createNewGridDomain(RES, BOUNDS, emask, bmask)
    deallocate(emask, bmask)

    !< setup expected output
    !< first
    allocate(emask(1,4), bmask(1,4))
    emask(1,:) = [.true., .true., .false., .true.]
    bmask(1,:) = [.false., .false., .false., .false.]
    expected(1) = createNewGridDomain(RES, [4.,3.,5.,1.], emask, bmask)
    deallocate(emask, bmask)

    !< second
    allocate(emask(2,4), bmask(2,4))
    emask(1,:) = [.true., .false., .false., .true.]
    emask(2,:) = [.true., .false., .false., .false.]
    bmask(1,:) = [.false., .false., .false., .false.]
    bmask(2,:) = [.false., .true., .true., .true.]
    expected(2) = createNewGridDomain(RES, [3.,1.,5.,1.], emask, bmask)
    deallocate(emask, bmask)

    !< third
    allocate(emask(2,4), bmask(2,4))
    emask(1,:) = [.false., .true., .false., .false.]
    emask(2,:) = [.false., .false., .true., .false.]
    bmask(1,:) = [.true., .false., .false., .false.]
    bmask(2,:) = [.false., .false., .false., .false.]
    expected(3) = createNewGridDomain(RES, [2.,0.,5.,1.], emask, bmask)
    deallocate(emask, bmask)

    !< run
    call gdpartition % init(3)
    call gdpartition % calculate(gdomain)
    results = gdpartition % get()

    call self % assert(size(results) == 3, &
        "gdpartition % init(3) .eq. 3 = T ")
    call self % assert(results(1) == expected(1), &
        "gdpartition % get(1) .eq. gridDomain(...) = T")
    call self % assert(results(2) == expected(2), &
        "gdpartition % get(2) .eq. gridDomain(...) = T")
    call self % assert(results(3) == expected(3), &
        "gdpartition % get(3) .eq. gridDomain(...) = T")

  end subroutine testCaseWithThree


end module shr_gridDomainPartitioningMethodBySameNumberOfGridcells_test