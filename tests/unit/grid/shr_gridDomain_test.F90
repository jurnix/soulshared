!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridDomain_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!------------------------------------------------------------------------------
module shr_gridDomain_test

  use shr_precision_mod, only: sp
  use shr_testSuite_mod, only: testSuite

  use shr_gridDomain_mod, only: shr_gridDomain
  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor, shr_gGridDescriptor
  use shr_gGridDescriptor_stub, only: shr_gGridDescriptorEmptyStub
  use shr_gridMask_stub, only: shr_gridMaskStub
  use shr_gridMask_mod, only: shr_IgridMask, shr_gridMask
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gGrid_mod, only: shr_gGrid, shr_igGrid
  use shr_gGridArrayMap_mod, only: shr_gGridArrayMap, LATITUDE_NAME, LONGITUDE_NAME
  use shr_gAxis_mod, only: shr_gAxis
  use shr_gAxisBounds_mod, only: shr_gAxisBounds
  use shr_gAxisMapping_mod, only: shr_gAxisMapping
  use shr_strings_mod, only: string

  implicit none

  private
  public :: testSuitegridDomain
  public :: createNewGrid, createNewGridDomain, createNewGridDescriptor, createNewGridMap, createNewGridMask

!  type, extends(shr_gridMaskStub) :: shr_gridMaskEnabledStub
!  contains
    !< overload here
!  end type shr_gridMaskEnabledStub

  type, extends(shr_gridMaskStub) :: shr_gridMaskSelectStub
  contains
    !< overload  here
    procedure :: select => gridMaskSelectStub_select
    procedure :: isIncluded => gridMaskSelectStub_isIncluded
  end type shr_gridMaskSelectStub

  type, extends(shr_gGridDescriptorEmptyStub) :: shr_gGridDescriptorSelectStub
  contains
    procedure :: getResolution => gdSelectStub_getResolution
    procedure :: getBounds => gdSelectStub_getBounds
  end type shr_gGridDescriptorSelectStub


  type, extends(testSuite) :: testSuitegridDomain

  contains
    procedure :: define => defineTestCases
    procedure, private :: testCaseCombine
    procedure, private :: testCaseFilter
    procedure, private :: testCaseSelect
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



  function gridMaskSelectStub_select(self, grid) result (newGMask)
    !< select a new shr_gridMask according to gDescriptor
    !< new gDscriptor must fit self % gridDescriptor
    class(shr_gridMaskSelectStub), intent(in) :: self
    class(shr_igGrid), intent(in) :: grid
    class(shr_igridMask), allocatable :: newGMask !< output
    ! (3.)  (-1.)
    !   x x -  (2.0)
    !   - - -  (0. )
    allocate(shr_gridMaskSelectStub :: newGMask)
    !allocate(newGMask % gridDescriptor, source = gDescriptor)
    !allocate(newGMask % mask(2,3))
    !newGMask % mask(2,:) = [.true., .true., .false.]
    !newGMask % mask(2,:) = [.false., .false., .false.]
  end function gridMaskSelectStub_select


  logical function gridMaskSelectStub_isIncluded(self, other)
    !< true if other gridMask true gridcells also match self mask array
    class(shr_gridMaskSelectStub), intent(in) :: self
    class(shr_igridMask), intent(in) :: other
    gridMaskSelectStub_isIncluded = .true.
  end function gridMaskSelectStub_isIncluded


  elemental real(kind=sp) function gdSelectStub_getResolution(self)
    !< resolution
    class(shr_gGridDescriptorSelectStub), intent(in) :: self
    gdSelectStub_getResolution = 1.0_sp
  end function gdSelectStub_getResolution


  elemental impure type(shr_gridBounds) function gdSelectStub_getBounds(self)
    !< bounds
    class(shr_gGridDescriptorSelectStub), intent(in) :: self
    call gdSelectStub_getBounds % init(north=3., south=-1., east=0., west=3.)
  end function gdSelectStub_getBounds


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridDomain), intent(inout) :: self
    call self % testCaseCombine()
    call self % testCaseFilter()
    call self % testCaseSelect()
  end subroutine defineTestCases


  subroutine testCaseCombine(self)
    !< combine unit test
    class(testSuitegridDomain), intent(inout) :: self
    type(shr_gridDomain) :: d, dother, combined, expected

    logical :: emask(2,3), bmask(2,3)
    logical :: doEmask(2,2), doBmask(2,2)
    logical :: expEmask(3,4), expBmask(3,4)
    !procedure :: gridDomain_combine (+)
    ! total           d         +  dother
    !             (border all T) (border all T)
    !(3)  (-1)       (2) (-1)      (3)(1)
    !   x x b (3)     x x b (3)
    ! x x x -     <=  x x - (1) +   x b (2)
    ! x -      (0)                  x - (0)
    !
    ! to (border undefined mask cells)
    !
    !(3) (-1)
    ! b x x b (3)
    ! x x x -
    ! x - b b (0)
    !
    ! Final:          (border)
    !(3)   (-1)
    ! - x x - (3)   x - - x
    ! x x x -       - - - -
    ! x - - - (0)   - - x x
    !
    !< setup (d)
    !< enabled (d)
    emask(1,:) = [.true., .true., .false.]
    emask(2,:) = [.true.,  .true., .false.]
    !< border (d)
    bmask(1,:) = [.false., .false., .true.]
    bmask(2,:) = [.false., .false., .false.]
    d = createNewGridDomain(1.0, [3.,1.,2.,-1.], emask, bmask)

    !< setup (dother)
    !< enabled (dother)
    doEmask(1,:) = [.true., .false.]
    doEmask(2,:) = [.true.,  .false.]
    !< border (dother)
    doBmask(1,:) = [.false., .true.]
    doBmask(2,:) = [.false., .false.]
    dother = createNewGridDomain(1.0, [2.,0.,3.,1.], doEmask, doBmask)

    !< setup (expected)
    !< enabled (expected)
    expEmask(1,:) = [.false., .true., .true., .false.]
    expEmask(2,:) = [.true., .true., .true., .false.]
    expEmask(3,:) = [.true., .false., .false., .false.]

    !< border (expected)
    expBmask(1,:) = [.true., .false., .false., .true.]
    expBmask(2,:) = [.false., .false., .false., .false.]
    expBmask(3,:) = [.false., .false., .true., .true.]

    expected = createNewGridDomain(1.0, [3.,0.,3.,-1.], expEmask, expBmask)

    combined = d + dother !< gridDomaine_combine
    call self % assert(combined == expected, &
        "d % combine(dother) .eq. expected = T")
  end subroutine testCaseCombine


  subroutine testCaseFilter(self)
    !< combine unit test
    class(testSuitegridDomain), intent(inout) :: self
    class(shr_gridDomain), allocatable :: d, filteredGM
    !< expected output
    class(shr_gridDomain), allocatable :: expected
    type(shr_gridMask) :: gmask

    logical :: emask(3,4), bmask(3,4)
    logical :: lemask(3,4), lbmask(3,4)
    logical :: lmask(3,4)
    real(kind=sp), parameter :: BOUNDS(4) = [3., 0., 4., 0.]
    real(kind=sp), parameter :: RES = 1.0

    !procedure :: filter
    !
    ! domain        mask        final
    !
    !(4.)  (0.)
    ! - x x -  (3.)    - - - -       - - - -
    ! x x x -       +  - x x -   =>  - x x -
    ! x - b b  (0.)    - x x -       - - b -

    !< setup
    !< expected enabled
    emask(1,:) = [.false., .false., .false., .false.]
    emask(2,:) = [.false.,  .true., .true., .false.]
    emask(3,:) = [.false., .false., .false., .false.]
    !< expected border
    bmask(1,:) = [.false., .false., .false., .false.]
    bmask(2,:) = [.false., .false., .false., .false.]
    bmask(3,:) = [.false., .false., .true., .false.]
    allocate(expected)
    expected = createNewGridDomain(RES, BOUNDS, emask, bmask)

    !< original enabled
    lemask(1,:) = [.false., .true., .true., .false.]
    lemask(2,:) = [.true., .true., .true., .false.]
    lemask(3,:) = [.true., .false., .false., .false.]
    !< original border
    lbmask(1,:) = [.false., .false., .false., .false.]
    lbmask(2,:) = [.false., .false., .false., .false.]
    lbmask(3,:) = [.false., .false., .true., .true.]
    allocate(d)
    d = createNewGridDomain(RES, BOUNDS, lemask, lbmask)

    !< request
    lmask(1,:) = [.false., .false., .false., .false.]
    lmask(2,:) = [.false., .true., .true., .false.]
    lmask(3,:) = [.false., .true., .true., .false.]
    gmask = createNewGridMask(RES, BOUNDS, lmask)
    allocate(filteredGM)
    filteredGM = d % filter(gmask)
    call self % assert(filteredGM == expected, &
        "d % filter(mask) .eq. expected = T")
  end subroutine testCaseFilter


  subroutine testCaseSelect(self)
    !< 'select' unit test
    !< same enabled and border masks
    class(testSuitegridDomain), intent(inout) :: self
    class(shr_gridDomain), allocatable :: d, selectedGM
    !< expected output
    class(shr_gridDomain), allocatable :: expected

    !< to init gridDomain
    type(shr_gGridDescriptor) :: gDescrip

    type(shr_gGrid) :: gridToSelect

    logical :: emask(2,3), bmask(2,3)
    logical :: lemask(3,4), lbmask(3,4)

    !< setup
    !< enabled
    emask(1,:) = [.true., .true., .false.]
    emask(2,:) = [.false., .false., .false.]
    !< border
    bmask(1,:) = [.false., .false., .false.]
    bmask(2,:) = [.true., .true., .true.]
    allocate(expected)
    expected = createNewGridDomain(1.0, [2., 0., 2., -1.], emask, bmask)

    lemask(1,:) = [.false., .true., .true., .false.]
    lemask(2,:) = [.true., .true., .true., .false.]
    lemask(3,:) = [.true., .false., .false., .false.]
    !< border
    lbmask(1,:) = [.false., .false., .false., .false.]
    lbmask(2,:) = [.false., .false., .false., .false.]
    lbmask(3,:) = [.false., .true., .true., .true.]
    allocate(d)
    d = createNewGridDomain(1.0, [3.,0.,3.,-1.], lemask, lbmask)
    !procedure :: select
    ! (3.)  (-1.)    (2.) (-1.)
    !< - x x - (3.)
    !< x x x -      -> x x - (2.0)
    !< x b b b (0.)    b b b (0.)
    !
    !< b -> border (true)
    !< x -> enabled (b=false)
    !< - -> disabled (b=false)
    gDescrip = createNewGridDescriptor(1., [2.,0.,2.,-1.])
    gridToSelect = createNewGrid(gDescrip)
    allocate(selectedGM)
    selectedGM = d % select(gridToSelect)
    call self % assert(selectedGM == expected, &
        "selectedGM % select(gm) .eq. expected  = T")
  end subroutine testCaseSelect


  subroutine testCaseSelectMidSection(self)
    !< 'select' unit test
    !< same enabled and border masks
    !>
    !>    (5)   (1)
    !> (4) x x - x      select
    !>     x - - x  ->  x - - x
    !>     x x - -      x x - -
    !> (0) - - x -                              - - x -
    class(testSuitegridDomain), intent(inout) :: self
    class(shr_gridDomain), allocatable :: d, selectedGM
    !< expected output
    class(shr_gridDomain), allocatable :: expected

    !< to init gridDomain
    type(shr_gGridDescriptor) :: gDescrip

    type(shr_gGrid) :: gridToSelect

    logical :: emask(2,3), bmask(2,3)
    logical :: lemask(3,4), lbmask(3,4)

    !< setup
    !< enabled
    emask(1,:) = [.true., .true., .false.]
    emask(2,:) = [.false., .false., .false.]
    !< border
    bmask(1,:) = [.false., .false., .false.]
    bmask(2,:) = [.true., .true., .true.]
    allocate(expected)
    expected = createNewGridDomain(1.0, [2., 0., 2., -1.], emask, bmask)

    lemask(1,:) = [.false., .true., .true., .false.]
    lemask(2,:) = [.true., .true., .true., .false.]
    lemask(3,:) = [.true., .false., .false., .false.]
    !< border
    lbmask(1,:) = [.false., .false., .false., .false.]
    lbmask(2,:) = [.false., .false., .false., .false.]
    lbmask(3,:) = [.false., .true., .true., .true.]
    allocate(d)
    d = createNewGridDomain(1.0, [3.,0.,3.,-1.], lemask, lbmask)
    !procedure :: select
    ! (3.)  (-1.)    (2.) (-1.)
    !< - x x - (3.)
    !< x x x -      -> x x - (2.0)
    !< x b b b (0.)    b b b (0.)
    !
    !< b -> border (true)
    !< x -> enabled (b=false)
    !< - -> disabled (b=false)
    gDescrip = createNewGridDescriptor(1., [2.,0.,2.,-1.])
    gridToSelect = createNewGrid(gDescrip)
    allocate(selectedGM)
    selectedGM = d % select(gridToSelect)
    call self % assert(selectedGM == expected, &
        "selectedGM % select(gm) .eq. expected  = T")
  end subroutine testCaseSelectMidSection

end module shr_gridDomain_test