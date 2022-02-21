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
module shr_gridMask_test

  use shr_precision_mod, only: sp
  use SHR_testSuite_mod, only: testSuite

  use shr_strings_mod, only: string
  use shr_gAxis_mod, only: shr_gAxis
  use shr_gAxisBounds_mod, only: shr_gAxisBounds
  use shr_gAxisMapping_mod, only: shr_gAxisMapping
  use shr_gridMask_mod, only: shr_gridMask
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_gridBoundIndices_mod, only: shr_gridBoundIndices
  use shr_gGridArrayMap_mod, only: shr_gGridArrayMap, LATITUDE_NAME, LONGITUDE_NAME
  use shr_gGrid_mod, only: shr_gGrid


  implicit none

  private
  public :: testSuitegridMask

  real(kind=sp), parameter :: RES = 1.0

  type, extends(testSuite) :: testSuitegridMask

  contains
    procedure :: define => defineTestCases
    procedure, private :: testCaseExpandWithDefault
    procedure, private :: testCaseShrink
  end type 

contains

  type(shr_gGrid) function getNewGrid(gDescriptor)
    !< creates a new grid
    type(shr_gGridDescriptor), intent(in) :: gDescriptor
    type(shr_gGridArrayMap) :: gridmap
    gridmap = getNewGridMap(gDescriptor)
    call getNewGrid % init(gDescriptor, gridmap)
  end function getNewGrid


  type(shr_gGridArrayMap) function getNewGridMap(gDescriptor)
    !< creates a new gridmap
    type(shr_gGridDescriptor), intent(in) :: gDescriptor

    type(shr_gAxis) :: laxis, lonxis
    real(kind=sp) :: res
    type(shr_gridBounds) :: bounds
    type(shr_gAxisBounds) :: laxisBounds, lonxisBounds
    type(shr_gAxisMapping) :: laxisMapping, lonxisMapping
    res = gDescriptor % getResolution()
    bounds = gDescriptor % getBounds()

    call laxisBounds % init(bounds % getNorth(), bounds % getSouth())
    call lonxisBounds % init(bounds % getEast(), bounds % getWest())

    call laxis % init(string(LATITUDE_NAME), res, laxisBounds)
    call lonxis % init(string(LONGITUDE_NAME), res, lonxisBounds)

    call laxisMapping % init(laxis)
    call lonxisMapping % init(lonxis)

    call getNewGridMap % init(gDescriptor, laxisMapping, lonxisMapping)
  end function getNewGridMap


  type(shr_gridMask) function getNewGridMask(resolution, newBounds, mask)
    !< create a new shr_gridMask
    real(kind=sp), intent(in) :: resolution
    real(kind=sp), intent(in) :: newBounds(4) !< N, S, E, W
    logical, intent(in), optional :: mask(:,:)
    type(shr_gridBounds) :: bounds
    type(shr_gGridDescriptor) :: gDescriptor
    type(shr_gGrid) :: grid

    call bounds % init(newBounds)
    call gDescriptor % init(resolution, bounds)
    grid = getNewGrid(gDescriptor)

    if (present(mask)) then
      call getNewGridMask % init(grid, mask)
    else
      call getNewGridMask % init(grid)
    end if
  end function getNewGridMask


  type(shr_gGridDescriptor) function getNewGridDesriptor(resolution, north, south, east, west)
    !< creates a new shr_gGridDescriptor
    real(kind=sp), intent(in) :: resolution
    real(kind=sp), intent(in) :: north, south
    real(kind=sp), intent(in) :: east, west
    type(shr_gridBounds) :: bounds
    call bounds % init(north, south, east, west)
    call getNewGridDesriptor % init(resolution, bounds)
  end function getNewGridDesriptor


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridMask), intent(inout) :: self

    class(shr_gridMask), allocatable :: m
    type(shr_gridMask) :: other, m1, m2

    logical :: expMask(2,2), rawMask(2,2), rawSmallMask(1,2)
    logical :: expBigMask(4,4)
    logical, allocatable :: foundMask(:,:)

    type(shr_gridcellIndex) :: gcIndex
    type(string) :: tmpStr, expStr

    type(shr_gGridDescriptor) :: gmDescSmall
    class(shr_gridMask), allocatable :: smallGMask
    type(shr_gridBoundIndices) :: gBoundIndices

    type(shr_gridMask) :: gmSmall
    class(shr_gridMask), allocatable :: gmBig
    type(shr_gGridDescriptor) :: gmDescBig
    type(shr_gGrid) :: gridSmall, gridBig

    !
    ! grid
    !
    ! X -> requested coordinates
    !
    ! (2) (1) (0)
    !  |   |   |
    !  +---X---X-  (1.)
    !  |   |   |
    !  +---+---+-  (0.)
    !  |   | X |
    !  +---+---+- (-1.)
    !
    !  mapped into:
    !
    ! +-----+-----+
    ! | 1,1 | 1,2 |
    ! +-----+-----+
    ! | 2,1 | 2,2 |
    ! *-----+-----+
    !
    allocate(m)
    m = getNewGridMask(RES, [1.,-1.,2.,0.])
    call self % assert(m % countEnabled() == 4, "m % count() .eq. 4 = T")

    !< setStatus
    m = getNewGridMask(RES, [1.,-1.,2.,0.])
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .false.)
    call gcIndex % init(1, 2)
    call m % setStatus(gcIndex, .false.)

    call self % assert(m % countEnabled() == 2, "m % count() .eq. 2 = T")

    !< get
    m = getNewGridMask(RES, [1.,-1.,2.,0.])
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .false.)
    call gcIndex % init(1, 2)
    call m % setStatus(gcIndex, .false.)
    expMask = .true.
    expMask(1,2) = .false.
    expMask(2,2) = .false.
    foundMask = m % get()
    call self % assert(all(foundMask .eqv. expMask), &
            "m % get() .eq. ((T,F), (T,F)) = T")

    !< get status
    m = getNewGridMask(RES, [1.,-1.,2.,0.])
    call gcIndex % init(1, 1)
    call self % assert(m % getStatus(gcIndex), &
            "m % getStatus(1,1) = T")

    m = getNewGridMask(RES, [1.,-1.,2.,0.])
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .false.)
    call self % assert(.not. m % getStatus(gcIndex), &
            "m % getStatus(2,2) = F")

    m = getNewGridMask(RES, [1.,-1.,2.,0.])
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .false.)
    call gcIndex % init(1, 2)
    call m % setStatus(gcIndex, .false.)
    !< == (2d raw array)
    rawMask(1,:) = [.true., .false.]
    rawMask(2,:) = [.true., .false.]
    call self % assert(( m == rawMask ), &
            "m == rawMask(FT, TF) = T")

    !< == (scalar)
    m = getNewGridMask(RES, [1.,-1.,2.,0.])
    call gcIndex % init(1, 1)
    call m % setStatus(gcIndex, .false.)
    call gcIndex % init(1, 2)
    call m % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .false.)
    call self % assert(.not. ( m == .true. ), &
            "m == .true. = F")

    !< revert changes
    m = getNewGridMask(RES, [1.,-1.,2.,0.])
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .true.)
    call gcIndex % init(1, 2)
    call m % setStatus(gcIndex, .true.)
    call self % assert( ( m == .true. ), "m(true) == .true. = T")

    !< revert
    m = getNewGridMask(RES, [1.,-1.,2.,0.])
    call m % reverse()
    call self % assert( ( m == .false. ), "m(true) % revert() .eq. .false. = T")

    m = getNewGridMask(RES, [1.,-1.,2.,0.])
    m = .not. m
    call self % assert( ( m == .false. ), ".not. m(false) .eq. false. = T")

    !< copy (=)
    m = getNewGridMask(RES, [1.,-1.,2.,0.])
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .true.)
    other = m
    !foundMask = other
    !rawMask(1,:) = [.false., .false.]
    !rawMask(2,:) = [.false., .true.]
    call self % assert( other == m , &
            "other .eq. m = T")

    ! or
    m1 = getNewGridMask(RES, [1.,-1.,2.,0.])
    ! [.false., .true.]
    ! [.false., .false.]
    call gcIndex % init(1, 1)
    call m1 % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 2)
    call m1 % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 1)
    call m1 % setStatus(gcIndex, .false.)

    m2 = getNewGridMask(RES, [1.,-1.,2.,0.])
    ! [.false., .false.]
    ! [.true.,  .false.]
    call gcIndex % init(1, 1)
    call m2 % setStatus(gcIndex, .false.)
    call gcIndex % init(1, 2)
    call m2 % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 2)
    call m2 % setStatus(gcIndex, .false.)


    m1 = (m1 .or. m2)
    foundMask = m1
    rawMask(1,:) = [.false., .true.]
    rawMask(2,:) = [.true.,  .false.]
    call self % assert( ( all(foundMask .eqv.  rawMask) ), &
        "m1(FTFF) .or. m2(FFTF) .eq. m1(FTTF) = T")

    ! toChars
    tmpStr = m2 % toString()
    write(*,*) "gridMask_test:: tmpStr =", tmpStr % toString()
    !expStr = string("resolution= 1.0000, bounds=(1.0000, -1.0000, 2.0000, 0.0000)" &
    expStr = string("resolution= 1.0000, bounds=(1.0000, -1.0000, 2.0000, 0.0000), " // &
        "lat=(size= 2, axis="//LATITUDE_NAME//", resolution=1.0000, bounds=(1.0000, -1.0000)) - " // &
        "lon=(size= 2, axis="//LONGITUDE_NAME//", resolution=1.0000, bounds=(2.0000, 0.0000))" &
          // new_line('A') // "'F F'"// new_line('A') // "'T F'")
    call self % assert( tmpStr == expStr, &
        "m2(F F, T F) % toString() .eq. expStr(F F T F) = T")

    ! select
    ! (argument)
    ! (2) (1) (0)
    !  |   |   |
    !  +---X---X-  (1.)
    !  |   |   |
    !  +---+---+-  (0.)
    !
    !  (parent)
    ! (2) (1) (0)
    !  |   |   |
    !  +---X---X-  (1.)
    !  | X | X |
    !  +---+---+-  (0.)
    !  |   |   |
    !  +---+---+- (-1.)
    !
    m = getNewGridMask(RES, [1.,-1.,2.,0.])
    call gcIndex % init(1, 1)
    call m % setStatus(gcIndex, .false.)

    gmDescSmall = getNewGridDesriptor(RES, 1.,0.,2.,0.)
    gridSmall = getNewGrid(gmDescSmall)

    smallGMask = m % select(gridSmall)

    rawSmallMask(1,:) = [.false., .true.]
    foundMask = smallGMask % get()
    call self % assert( all(foundMask .eqv. rawSmallMask ), &
        "m(FT,FF) % select(XX,--) .eq. mask(FT) = T")


    ! set
    m = getNewGridMask(RES, [4.,0.,4.,0.]) !< default = true, coordinates
    call gBoundIndices % init(2, 3, 2, 3) !< indices
    rawMask = .false.

    call m % set(rawMask, gBoundIndices)

    expBigMask(1,:) = .true.
    expBigMask(2,:) = [.true., .false., .false., .true.]
    expBigMask(3,:) = [.true., .false., .false., .true.]
    expBigMask(4,:) = .true.
    foundMask = m % get()
    call self % assert( all(foundMask .eqv. expBigMask ), &
        "m(T...T) % set(F, 3,2,1,2) .eq. mask() = T")

    ! expand

    ! from
    ! (2) (1) (0)
    !  |   |   |
    !  +---X---X-  (1.)
    !  | F | T |
    !  +---+---+-  (0.)

    ! to
    ! (2) (1) (0)
    !  |   |   |
    !  +---X---X-  (1.)
    !  | F | T |
    !  +---+---+-  (0.)
    !  | T | T |
    !  +---+---+- (-1.)
    !

    gmSmall = getNewGridMask(RES, [1., 0., 2., 0.])
    call gcIndex % init(1, 1)
    call gmSmall % setStatus(gcIndex, .false.)
    gmDescBig = getNewGridDesriptor(RES, 1.,-1.,2.,0.)
    gridBig = getNewGrid(gmDescBig)

    gmBig = gmSmall % expand(gridBig)

    rawMask(1,:) = [.false., .true.]
    rawMask(2,:) = [.true., .true.] ! -> extended row
    foundMask = gmBig % get()

    call self % assert( all(foundMask .eqv. rawMask ), &
        "m(1,0,2,0, (FT,FF)) % expand(1,-1,2,0) .eq. mask(FT,TT) = T")

    call self % testCaseExpandWithDefault()
    call self % testCaseShrink()

  end subroutine defineTestCases


  subroutine testCaseExpandWithDefault(self)
    !< test case for expand method with default argument enabled
    class(testSuitegridMask), intent(inout) :: self
    logical :: lmask(2,3), expectedlMask(3,4)
    type(shr_gGridDescriptor) :: gmDescBig
    type(shr_gridMask) :: gmSmall, expectedGMask
    class(shr_gridMask), allocatable :: gmBig
    type(string) :: tmp
    type(shr_gGrid) :: gridBigDesc
    ! expand with default

    ! from [2,-1,0,-1]
    !
    ! (2) (1) (0) (-1)
    !  |   |   |   |
    !  +---+---+---+  (3.)
    !  | F | T | F |
    !  +---+---+---+  (2.)
    !  | T | T | F |
    !  +---+---+---+  (1.)
    !
    ! to [3,0,3,-1]
    !
    ! (3) (2) (1) (0) (-1)
    !  |   |   |   |   |
    !  +---+---+---+---+  (3.)
    !  | F | F | T | F |
    !  +---+---+---+---+  (2.)
    !  | F | T | T | F |
    !  +---+---+---+---+  (1.)
    !  | F | F | F | F |
    !  +---+---+---+---+  (0.)
    !

    !< setup
    lmask(1,:) = [.false., .true., .false.]
    lmask(2,:) = [.true., .true., .false.]
    gmSmall = getNewGridMask(1.0, [3., 1., 2., -1.], lmask)
    tmp = gmSmall % toString()
    write(*,*) "gridMask_test:: testCaseExpandWithDefault:: original =", tmp % toString()
    gmDescBig = getNewGridDesriptor(RES, 3.,0.,3.,-1.)
    gridBigDesc = getNewGrid(gmDescBig)

    gmBig = gmSmall % expand(gridBigDesc, default = .false.)

    tmp = gmBig % toString()
    write(*,*) "gridMask_test:: testCaseExpandWithDefault:: expanded =", tmp % toString()

    expectedlMask(1,:) = [.false., .false., .true., .false.]
    expectedlMask(2,:) = [.false., .true., .true., .false.]
    expectedlMask(3,:) = [.false., .false., .false., .false.]
    expectedGMask = getNewGridMask(1., [3.,0.,3.,-1.], expectedlMask)

    tmp = expectedGMask % toString()
    write(*,*) "gridMask_test:: testCaseExpandWithDefault:: expected =", tmp % toString()

    call self % assert( gmBig == expectedGMask, &
        "gm(3,1,2,-1, (FTF,TTF)) % expand(3,0,3,-1) .eq. mask(FTFF,TTFF,FFFF) = T")
  end subroutine testCaseExpandWithDefault


  subroutine testCaseShrink(self)
    !< test case for Shrink method
    !
    ! from [5,0,3,-1]
    !
    ! (3) (2) (1) (0) (-1)
    !  |   |   |   |   |
    !  +---+---+---+---+  (5.)
    !  | F | F | F | F |
    !  +---+---+---+---+  (4.)
    !  | F | F | T | F |
    !  +---+---+---+---+  (3.)
    !  | F | F | T | F |
    !  +---+---+---+---+  (2.)
    !  | F | T | T | F |
    !  +---+---+---+---+  (1.)
    !  | F | F | F | F |
    !  +---+---+---+---+  (0.)
    !
    ! To: [4,0,2,0]
    !
    ! (2) (1) (0)
    !  |   |   |
    !  +---+---+  (4.)
    !  | F | T |
    !  +---+---+  (3.)
    !  | F | T |
    !  +---+---+  (2.)
    !  | T | T |
    !  +---+---+  (1.)
    !
    class(testSuitegridMask), intent(inout) :: self
    logical :: lmask(5,4), shmask(3,2)
    type(shr_gridMask) :: gm
    class(shr_gridMask), allocatable :: foundGM
    class(shr_gridMask), allocatable :: expectedGM
    type(string) :: tmp

    !< setup
    lmask = .false.
    lmask(2,3) = .true.
    lmask(3,3) = .true.
    lmask(4,2:3) = .true.
    gm = getNewGridMask(1.0, [5., 0., 3., -1.], lmask)

    allocate(expectedGM)
    shmask = .true.
    shmask(1,1) = .false.
    shmask(2,1) = .false.
    expectedGM = getNewGridMask(1.0, [4.,1.,2.,0.], shmask)

    !< run
    foundGM = gm % shrink()
    tmp = foundGM % toString()
    write(*,*) "found =", tmp % toString()
    tmp = expectedGM % toString()
    write(*,*) "expected =", tmp % toString()
    call self % assert(foundGM == expectedGM, "gm(...) % shrink() .eq. gm(FT,FT,TT) = T")
  end subroutine testCaseShrink

end module shr_gridMask_test

