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
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_gridMask_mod, only: shr_gridMask
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_gridBoundIndices_mod, only: shr_gridBoundIndices

  implicit none

  private
  public :: testSuitegridMask

  real(kind=sp), parameter :: RES = 1.0

  type, extends(testSuite) :: testSuitegridMask

  contains
    procedure :: define => defineTestCases
  end type 

contains

  type(shr_gridMask) function getNewGridMask(newBounds)
    !< create a new shr_gridMask
    real(kind=sp) :: newBounds(4) !< N, S, E, W
    type(shr_gridBounds) :: bounds
    type(shr_gGridDescriptor) :: gDescriptor

    call bounds % init(newBounds)
    call gDescriptor % init(RES, bounds)
    call getNewGridMask % init(gDescriptor)
  end function getNewGridMask


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
    class(testSuitegridMask), intent(inout) :: self

    type(shr_gridMask) :: m, other, m1, m2, mOther

    logical :: expMask(2,2), rawMask(2,2), rawSmallMask(1,2)
    logical :: expBigMask(4,4)
    logical, allocatable :: foundMask(:,:)

    type(shr_gridcellIndex) :: gcIndex
    type(string) :: tmpStr, expStr

    type(shr_gGridDescriptor) :: gmDescSmall
    type(shr_gridMask) :: smallGMask
    type(shr_gridBoundIndices) :: gBoundIndices

    type(shr_gridMask) :: gmSmall, gmBig
    type(shr_gGridDescriptor) :: gmDescBig

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
    m = getNewGridMask([1.,-1.,2.,0.])
    call self % assert(m % countEnabled() == 4, "m % count() .eq. 4 = T")

    !< setStatus
    m = getNewGridMask([1.,-1.,2.,0.])
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .false.)
    call gcIndex % init(1, 2)
    call m % setStatus(gcIndex, .false.)

    call self % assert(m % countEnabled() == 2, "m % count() .eq. 2 = T")

    !< getRaw
    m = getNewGridMask([1.,-1.,2.,0.])
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .false.)
    call gcIndex % init(1, 2)
    call m % setStatus(gcIndex, .false.)
    expMask = .true.
    expMask(1,2) = .false.
    expMask(2,2) = .false.
    foundMask = m % getRaw()
    call self % assert(all(foundMask .eqv. expMask), &
            "m % getRaw() .eq. ((T,F), (T,F)) = T")

    !< get status
    m = getNewGridMask([1.,-1.,2.,0.])
    call gcIndex % init(1, 1)
    call self % assert(m % getStatus(gcIndex), &
            "m % getStatus(1,1) = T")

    m = getNewGridMask([1.,-1.,2.,0.])
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .false.)
    call self % assert(.not. m % getStatus(gcIndex), &
            "m % getStatus(2,2) = F")

    m = getNewGridMask([1.,-1.,2.,0.])
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
    m = getNewGridMask([1.,-1.,2.,0.])
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
    m = getNewGridMask([1.,-1.,2.,0.])
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .true.)
    call gcIndex % init(1, 2)
    call m % setStatus(gcIndex, .true.)
    call self % assert( ( m == .true. ), "m(true) == .true. = T")

    !< revert
    m = getNewGridMask([1.,-1.,2.,0.])
    call m % reverse()
    call self % assert( ( m == .false. ), "m(true) % revert() .eq. .false. = T")

    m = getNewGridMask([1.,-1.,2.,0.])
    m = .not. m
    call self % assert( ( m == .false. ), ".not. m(false) .eq. false. = T")

    !< copy (=)
    m = getNewGridMask([1.,-1.,2.,0.])
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .true.)
    other = m
    !foundMask = other
    !rawMask(1,:) = [.false., .false.]
    !rawMask(2,:) = [.false., .true.]
    call self % assert( other == m , &
            "other .eq. m = T")

    ! or
    m1 = getNewGridMask([1.,-1.,2.,0.])
    ! [.false., .true.]
    ! [.false., .false.]
    call gcIndex % init(1, 1)
    call m1 % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 2)
    call m1 % setStatus(gcIndex, .false.)
    call gcIndex % init(2, 1)
    call m1 % setStatus(gcIndex, .false.)

    m2 = getNewGridMask([1.,-1.,2.,0.])
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
    expStr = string("'F F'"// new_line('A') // "'T F'")
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
    m = getNewGridMask([1.,-1.,2.,0.])
    call gcIndex % init(1, 1)
    call m % setStatus(gcIndex, .false.)

    gmDescSmall = getNewGridDesriptor(1.,0.,2.,0.)

    smallGMask = m % select(gmDescSmall)

    rawSmallMask(1,:) = [.false., .true.]
    foundMask = smallGMask % getRaw()
    call self % assert( all(foundMask .eqv. rawSmallMask ), &
        "m(FT,FF) % select(XX,--) .eq. mask(FT) = T")


    ! set
    m = getNewGridMask([4.,0.,4.,0.]) !< default = true, coordinates
    call gBoundIndices % init(2, 3, 2, 3) !< indices
    rawMask = .false.

    call m % set(rawMask, gBoundIndices)

    expBigMask(1,:) = .true.
    expBigMask(2,:) = [.true., .false., .false., .true.]
    expBigMask(3,:) = [.true., .false., .false., .true.]
    expBigMask(4,:) = .true.
    foundMask = m % getRaw()
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

    gmSmall = getNewGridMask([1., 0., 2., 0.])
    call gcIndex % init(1, 1)
    call gmSmall % setStatus(gcIndex, .false.)
    gmDescBig = getNewGridDesriptor(1.,-1.,2.,0.)

    gmBig = gmSmall % expand(gmDescBig)

    rawMask(1,:) = [.false., .true.]
    rawMask(2,:) = [.true., .true.] ! -> extended row
    foundMask = gmBig % getRaw()

    call self % assert( all(foundMask .eqv. rawMask ), &
        "m(1,0,2,0, (FT,FF)) % expand(1,-1,2,0) .eq. mask(FT,TT) = T")


    !< isIncluded
    m = getNewGridMask([1., -1., 2., 0.]) !< true default
    call gcIndex % init(1, 1)
    call gmSmall % setStatus(gcIndex, .false.)

    mOther = getNewGridMask([1., -1., 2., 0.])
    call self % assert( all(foundMask .eqv. rawMask ), &
        "m(FT,TT) % isIncluded(TT,TT) = F")
    call self % assert( all(foundMask .eqv. rawMask ), &
        "m((TT,TT)) % isIncluded(FT,TT) = T")


  end subroutine defineTestCases


end module shr_gridMask_test

