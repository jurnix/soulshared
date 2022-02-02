!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        :   shr_gridDomainToSquaredConverter_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!------------------------------------------------------------------------------
module shr_gridDomainToSquaredConverter_test

  use shr_precision_mod, only: sp
  use shr_testSuite_mod, only: testSuite
  use shr_gridDomainToSquaredConverter_mod, only: shr_gridDomainToSquaredConverter
  use shr_gridDomain_mod, only: shr_gridDomain

  !< factory methods
  use shr_strings_mod, only: string
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
  use shr_gridMask_mod, only: shr_gridMask
  use shr_gGridArrayMap_mod, only: shr_gGridArrayMap, LATITUDE_NAME, LONGITUDE_NAME
  use shr_gGrid_mod, only: shr_gGrid
  use shr_gAxis_mod, only: shr_gAxis
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gAxisBounds_mod, only: shr_gAxisBounds
  use shr_gAxisMapping_mod, only: shr_gAxisMapping
  use shr_gridDomainSquared_mod, only: shr_gridDomainSquared

  implicit none

  private
  public :: testSuitegridDomainToSquaredConverter

  type, extends(testSuite) :: testSuitegridDomainToSquaredConverter

  contains
    procedure :: define => defineTestCases

    procedure, private :: testCaseSimpleConversion
  end type
contains

  type(shr_gridDomainSquared) function createNewGridDomainSquared(resolution, bounds, emask)
    !< create a new shr_gridDomainSquared instance
    real(kind=sp), intent(in) :: resolution
    real(kind=sp), intent(in) :: bounds(4) !< n, s, e, w
    logical, intent(in) :: emask(:,:) !< raw mask

    class(shr_gGridDescriptor), allocatable :: gDesc
    class(shr_gridMask), allocatable :: gmEnabled
    type(shr_gGridArrayMap) :: gridmap
    type(shr_gGrid) :: grid

    allocate(gDesc)
    gDesc = createNewGridDescriptor(resolution, bounds)
    gridmap = createNewGridmap(gDesc)
    call grid % init(gDesc, gridmap)

    allocate(gmEnabled)
    call gmEnabled % init(grid, emask)

    call createNewGridDomainSquared % init(grid, gmEnabled)
  end function createNewGridDomainSquared


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

    !call sbounds % init(bounds)

    !allocate(gDesc)
    !call gDesc % init(resolution, sbounds)
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
    class(testSuitegridDomainToSquaredConverter), intent(inout) :: self
    call self % testCaseSimpleConversion()
  end subroutine defineTestCases


  subroutine testCaseSimpleConversion(self)
    !< how to partition(algorithm):
    !< 7      1
    !<  ---xxx -> 1st 	(xxx)     5
    !<  xxxxxx					(xxxxxx)  |
    !<  xxxxxx -> 2nd	  (xxxxxx)  |
    !<  xxxxxx					(xxxxxx)  |
    !<  xxxx-- -> 3rd	  (xxxx)    1
    !<
    !<
    class(testSuitegridDomainToSquaredConverter), intent(inout) :: self

    type(shr_gridDomainToSquaredConverter) :: converter
    type(shr_gridDomain) :: domains(3)
    class(shr_gridDomainSquared), allocatable :: foundDomainsSquared(:)
    class(shr_gridDomainSquared), allocatable :: expDomainsSquared(:)
    real(kind=sp), parameter :: RES = 1.0_sp
    logical, allocatable :: emask(:,:), bmask(:,:)

    !< setup
    !< create input grid domains
    allocate(emask(1,3), bmask(1,3))
    emask(:,:) = .true.
    bmask(:,:) = .false.
    domains(1) = createNewGridDomain(RES, [5., 4., 4., 1.], emask,bmask)
    deallocate(emask, bmask)

    allocate(emask(3,6), bmask(3,6))
    emask(:,:) = .true.
    bmask(:,:) = .false.
    domains(2) = createNewGridDomain(RES, [4., 1., 7., 1.], emask, bmask)
    deallocate(emask, bmask)

    allocate(emask(1,4), bmask(1,4))
    emask(1,:) = .true.
    bmask(1,:) = .false.
    domains(3) = createNewGridDomain(RES, [2., 1., 7., 3.], emask, bmask)
    deallocate(emask, bmask)

    !< create expected squared grid domains
    allocate(expDomainsSquared(3))
    allocate(emask(1,3))
    emask(:,:) = .true.
    expDomainsSquared(1) = createNewGridDomainSquared(RES, [5., 4., 4., 1.], emask)
    deallocate(emask)

    allocate(emask(3,6))
    emask(:,:) = .true.
    expDomainsSquared(2) = createNewGridDomainSquared(RES, [4., 1., 7., 1.], emask)
    deallocate(emask)

    allocate(emask(1,4))
    emask(:,:) = .true.
    expDomainsSquared(3) = createNewGridDomainSquared(RES, [2., 1., 7., 3.], emask)
    deallocate(emask)

    !< run
    call converter % init(domains)
    foundDomainsSquared = converter % get()

    call self % assert(all(foundDomainsSquared == expDomainsSquared), &
            "gd2squareConverter % get() .eq. [sgd1, sqd2, sqd3] = T")
  end subroutine testCaseSimpleConversion


end module shr_gridDomainToSquaredConverter_test