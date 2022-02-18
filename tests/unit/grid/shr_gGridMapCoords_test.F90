!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridMapCoords_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gGridMapCoords unit tests
!>
!------------------------------------------------------------------------------
module shr_gGridMapCoords_test

  use shr_precision_mod, only: sp
  use shr_testSuite_mod, only: testSuite
  use shr_gGridMapCoords_mod, only: shr_gGridMapCoords, shr_gGridMapCoordsBuilder

  use shr_gGridDescriptor_stub, only: shr_gGridDescriptorEmptyStub
  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor
  use shr_gridBoundIndices_mod, only: shr_gridBoundIndices
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_coord_mod, only: shr_coord

  ! for debug
  use shr_strings_mod, only: string

  implicit none

  private
  public :: testSuitegGridMapCoords


  type, extends(shr_gGridDescriptorEmptyStub) :: gGridDescriptorFake
  contains
    !< overload
    procedure :: getResolution => gGridDescriptorFake_getResolution
    procedure :: getBounds => gGridDescriptorFake_getBounds
  end type gGridDescriptorFake


  type, extends(testSuite) :: testSuitegGridMapCoords
  contains
    procedure :: define => defineTestCases
    procedure, private :: testCaseGetGetCoordByGridIndex
    procedure, private :: testCaseGetByGridBoundIndicesWithSingleColumn
    procedure, private :: testCaseGetByGridBoundIndicesWithMultipleColumns
  end type

contains

  elemental real(kind=sp) function gGridDescriptorFake_getResolution(self)
    !< it returns class resolution
    class(gGridDescriptorFake), intent(in) :: self
    gGridDescriptorFake_getResolution = 1.0_sp
  end function gGridDescriptorFake_getResolution


  elemental impure type(shr_gridBounds) function gGridDescriptorFake_getBounds(self)
    !< return class bounds
    class(gGridDescriptorFake), intent(in) :: self
    call gGridDescriptorFake_getBounds % init(6.,1.,4.,0.)
  end function gGridDescriptorFake_getBounds


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegGridMapCoords), intent(inout) :: self

    call self % testCaseGetGetCoordByGridIndex()
    call self % testCaseGetByGridBoundIndicesWithSingleColumn()
    call self % testCaseGetByGridBoundIndicesWithMultipleColumns()
  end subroutine defineTestCases


  subroutine testCaseGetGetCoordByGridIndex(self)
    !< getCoord_byGridindex unit test
    !<
    !< resolution = 1.0           indices
    !< (4.)      (0.)
    !<  |         |
    !<  +---------+- 6.0
    !<  | x x x x |           1,1  1,2  ...  1,4
    !<  | x x x x |           2,1  2,2  ...
    !<  | x x x x |           3,1  3,2  3,3  3,4
    !<  | x x x x |           4,1  4,2  ...
    !<  | x x x x |           5,1  5,2  5,3  5,4
    !<  +---------+- 1.0
    !<
    !< getCoord(2,3) -> coord(4.5, 1.5)
    !<
    !< (4.)      (0.)
    !<  |         |
    !<  +---------+- 6.0
    !<  | x x x x |           1,1  1,2  ...  1,4
    !<  | x x x x |           2,1  2,2  ...
    !<  | x x x x |           3,1  3,2  3,3  3,4
    !<  | x x x x |           4,1  4,2  ...
    !<  | x x x x |           5,1  5,2  5,3  5,4
    !<  +---------+- 1.0
    !<
    class(testSuitegGridMapCoords), intent(inout) :: self
    type(shr_gridcellIndex) :: gcIndex
    type(shr_gGridMapCoords) :: gridmapCoords
    clasS(gGridDescriptorFake), allocatable :: newGDescriptor
    type(shr_coord) :: expectedCoord, foundCoord

    !< setup
    allocate(newGDescriptor)
    call gcIndex % init(2,3) !> startRow, endRow, startCol, endCol

    !< test
    gridMapCoords = shr_gGridMapCoordsBuilder(newGDescriptor)
    foundCoord = gridMapCoords % getCoord(gcIndex)
    call self % assert(shr_coord(4.5,1.5) == foundCoord, &
          "gridmapArray % getCoord() .eq. (4.5, 1.5) = T")

  end subroutine testCaseGetGetCoordByGridIndex


  subroutine testCaseGetByGridBoundIndicesWithSingleColumn(self)
    !< toString unit test
    !<
    !< resolution = 1.0           indices
    !< (4.)      (0.)
    !<  |         |
    !<  +---------+- 6.0
    !<  | x x x x |           1,1  1,2  ...  1,4
    !<  | x x x x |           2,1  2,2  ...
    !<  | x x x x |           3,1  3,2  3,3  3,4
    !<  | x x x x |           4,1  4,2  ...
    !<  | x x x x |           5,1  5,2  5,3  5,4
    !<  +---------+- 1.0
    !<
    !< getCoord(2,3,1,1) -> gridBounds(north=5., south=3., east=3, west=4)
    !<
    !< (4.)      (0.)
    !<  |         |
    !<  +---------+- 6.0
    !<  | x x x x |           1,1  1,2  ...  1,4
    !<  | x x x x |           2,1  2,2  ...
    !<  | x x x x |           3,1  3,2  3,3  3,4
    !<  | x x x x |           4,1  4,2  ...
    !<  | x x x x |           5,1  5,2  5,3  5,4
    !<  +---------+- 1.0
    !<
    class(testSuitegGridMapCoords), intent(inout) :: self
    type(shr_gGridMapCoords) :: gridMapCoords
    type(shr_gridBoundIndices) :: gridBoundIndices
    type(shr_gridBounds) :: expectedGridBounds, foundGridBounds
    class(gGridDescriptorFake), allocatable :: newGDescriptor

    !< setup
    allocate(newGDescriptor)
    call gridBoundIndices % init(2,3,1,1) !> startRow, endRow, startCol, endCol
    call expectedGridBounds % init(5.,3.,4.,3.)

    !< test
    gridMapCoords = shr_gGridMapCoordsBuilder(newGDescriptor)
    foundgridBounds = gridMapCoords % getCoord(gridBoundIndices)
    call self % assert(foundGridBounds == expectedGridBounds, &
        "gridMapCoords() % getCoord(2,3,1,1) .eq. (5.,3.,3.,4.) = T")
  end subroutine testCaseGetByGridBoundIndicesWithSingleColumn


  subroutine testCaseGetByGridBoundIndicesWithMultipleColumns(self)
    !< toString unit test
    !<
    !< resolution = 1.0           indices
    !< (4.)      (0.)
    !<  |         |
    !<  +---------+- 6.0
    !<  | x x x x |           1,1  1,2  ...  1,4
    !<  | x x x x |           2,1  2,2  ...
    !<  | x x x x |           3,1  3,2  3,3  3,4
    !<  | x x x x |           4,1  4,2  ...
    !<  | x x x x |           5,1  5,2  5,3  5,4
    !<  +---------+- 1.0
    !<
    !< getCoord(2,3,3,4) -> gridBounds(north=5., south=3., east=0, west=2)
    !<
    !< (4.)      (0.)
    !<  |         |
    !<  +---------+- 6.0
    !<  | x x x x |           1,1  1,2  ...  1,4
    !<  | x x x x |           2,1  2,2  ...
    !<  | x x x x |           3,1  3,2  3,3  3,4
    !<  | x x x x |           4,1  4,2  ...
    !<  | x x x x |           5,1  5,2  5,3  5,4
    !<  +---------+- 1.0
    !<
    class(testSuitegGridMapCoords), intent(inout) :: self
    type(shr_gGridMapCoords) :: gridMapCoords
    type(shr_gridBoundIndices) :: gridBoundIndices
    type(shr_gridBounds) :: expectedGridBounds, foundGridBounds
    class(gGridDescriptorFake), allocatable :: newGDescriptor
    type(string) :: tmp

    !< setup
    allocate(newGDescriptor)
    call gridBoundIndices % init(2,3,3,4) !> startRow, endRow, startCol, endCol
    call expectedGridBounds % init(5.,3.,2.,0.)

    !< test
    gridMapCoords = shr_gGridMapCoordsBuilder(newGDescriptor)
    foundGridBounds = gridMapCoords % getCoord(gridBoundIndices)
    tmp = foundGridBounds % toString()
    write(*,*) "found =", tmp % toString()
    tmp = expectedGridBounds % toString()
    write(*,*) "expected =", tmp % toString()
    call self % assert(expectedGridBounds == foundGridBounds, &
        "gridMapCoords() % getCoord(2,3,3,4) .eq. (5.,3.,0.,2.) = T")
  end subroutine testCaseGetByGridBoundIndicesWithMultipleColumns

end module shr_gGridMapCoords_test

