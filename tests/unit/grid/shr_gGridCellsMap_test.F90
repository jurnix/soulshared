!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridCellsMap_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gGridCellsMap unit tests
!------------------------------------------------------------------------------
module shr_gGridCellsMap_test

  use SHR_testSuite_mod, only: testSuite
  use shr_precision_mod, only: sp
  use shr_gGridCellsMap_mod, only: shr_gGridCellsMap
  use shr_gGridAxes_mod, only: shr_igGridAxes
  use shr_gGridAxes_stub, only: shr_gGridAxesEmptyStub

  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor
  use shr_gGridDescriptor_stub, only: shr_gGridDescriptorEmptyStub
  use shr_strings_mod, only: string
  use shr_gridShape_mod, only: shr_gridShape
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_gGridAxesCell_mod, only: shr_gGridAxesCell

  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_coord_mod, only: shr_coord

  use shr_gridcell_mod, only: shr_gridcell

  implicit none

  private
  public :: testSuitegGridCellsMap

  type, extends(testSuite) :: testSuitegGridCellsMap

    contains
      procedure :: define => defineTestCases
      procedure, private :: testToString
      procedure, private :: testGetGridcells
      procedure ,private :: testEqual
      procedure, private :: testGetShape
  end type


  type, extends(shr_gGridAxesEmptyStub) :: shr_gAxisFakeLat
  contains
    procedure :: toString => gAxisFakeLat_toString
    procedure :: getCells => gAxisFakeLat_getCells
    procedure :: equal => gAxisFakeLat_equal
    procedure :: getSize => gAxisFakeLat_getSize
  end type shr_gAxisFakeLat


  type, extends(shr_gGridAxesEmptyStub) :: shr_gAxisFakeLon
  contains
    procedure :: toString => gAxisFakeLon_toString
    procedure :: getCells => gAxisFakeLon_getCells
    procedure :: equal => gAxisFakeLon_equal
    procedure :: getSize => gAxisFakeLon_getSize
  end type shr_gAxisFakeLon


  type, extends(shr_gGridDescriptorEmptyStub) :: shr_gGridDescriptorFake
  contains
    procedure :: toString => gGridDescritorFake_toString
    procedure :: equal => gGridDescriptorFake_equal
  end type shr_gGridDescriptorFake


contains

  elemental logical function gAxisFakeLat_equal(self, other)
    !< true if self and equal have the same attributes
    class(shr_gAxisFakeLat), intent(in) :: self
    class(shr_igGridAxes), intent(in) :: other
    gAxisFakeLat_equal = .true.
  end function gAxisFakeLat_equal


  elemental logical function gAxisFakeLon_equal(self, other)
    !< true if self and equal have the same attributes
    class(shr_gAxisFakeLon), intent(in) :: self
    class(shr_igGridAxes), intent(in) :: other
    gAxisFakeLon_equal = .true.
  end function gAxisFakeLon_equal


  type(string) function gAxisFakeLat_toString(self)
    !< string representation of shr_gAxisMapping
    class(shr_gAxisFakeLat), intent(in) :: self
    gAxisFakeLat_toString = string("-shr_gAxisFakeLat-")
  end function gAxisFakeLat_toString


  type(string) function gAxisFakeLon_toString(self)
    !< string representation of shr_gAxis
    class(shr_gAxisFakeLon), intent(in) :: self
    gAxisFakeLon_toString = string("-shr_gAxisFakeLon-")
  end function gAxisFakeLon_toString


  function gAxisFakeLat_getCells(self, axisCoord) result (gcells)
    !< given an coordinate from the current axis, it returns itss shr_gGridAxesCell(s)
    !< 1. 2. 3.
    !<   |   |
    !<   x   x  - 4.
    !<     *    <------ request
    !<   x   x  - 2.
    !<
    class(shr_gAxisFakeLat), intent(in) :: self
    real(kind=sp), intent(in) :: axisCoord
    type(shr_gGridAxesCell), allocatable :: gcells(:)
    type(shr_gGridAxesBounds) :: axisBounds

    allocate(gcells(2))
    call axisBounds % init(3., 2.)
    call gcells(1) % init(2.5_sp, axisBounds)

    call axisBounds % init(4., 3.)
    call gcells(2) % init(3.5_sp, axisBounds)
  end function gAxisFakeLat_getCells


  function gAxisFakeLon_getCells(self, axisCoord) result (gcells)
    !< given an coordinate from the current axis, it returns itss shr_gGridAxesCell(s)
    class(shr_gAxisFakeLon), intent(in) :: self
    real(kind=sp), intent(in) :: axisCoord
    type(shr_gGridAxesCell), allocatable :: gcells(:)
    type(shr_gGridAxesBounds) :: axisBounds

    allocate(gcells(2))
    call axisBounds % init(2., 1.)
    call gcells(1) % init(1.5_sp, axisBounds)

    call axisBounds % init(3., 2.)
    call gcells(2) % init(2.5_sp, axisBounds)
  end function gAxisFakeLon_getCells


  type(string) function gGridDescritorFake_toString(self)
    !< string representation of shr_gAxisMapping
    class(shr_gGridDescriptorFake), intent(in) :: self
    gGridDescritorFake_toString = string("-shr_gGridDescriptorFake-")
  end function gGridDescritorFake_toString


  elemental logical function gGridDescriptorFake_equal(self, other)
    !< ==
    class(shr_gGridDescriptorFake), intent(in) :: self
    class(shr_iGGridDescriptor), intent(in) :: other
    gGridDescriptorFake_equal = .true.
  end function gGridDescriptorFake_equal


  integer function gAxisFakeLat_getSize(self)
    !< lat axis size
    class(shr_gAxisFakeLat), intent(in) :: self
    gAxisFakeLat_getSize = 2
  end function gAxisFakeLat_getSize


  integer function gAxisFakeLon_getSize(self)
    !< lon axis size
    class(shr_gAxisFakeLon), intent(in) :: self
    gAxisFakeLon_getSize = 4
  end function gAxisFakeLon_getSize


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegGridCellsMap), intent(inout) :: self

    call self % testToString()
    call self % testGetGridcells()
    call self % testEqual()
    call self % testGetShape()
  end subroutine defineTestCases


  subroutine testToString(self)
    !< toString unit test
    class(testSuitegGridCellsMap), intent(inout) :: self
    type(shr_gGridCellsMap) :: g
    type(shr_gGridDescriptorFake) :: fakeGridDesc
    type(shr_gAxisFakeLat) :: fakeLaxisLat
    type(shr_gAxisFakeLon) :: fakeLonxisLon
    type(string) :: gstr

    !< toString
    call g % init(fakeGridDesc, fakeLaxisLat, fakeLonxisLon)
    gstr = g % toString()
    call self % assert(gstr == string("-shr_gGridDescriptorFake-, "// &
          "lat=(-shr_gAxisFakeLat-) - lon=(-shr_gAxisFakeLon-)"), &
          "g % toString() .eq. = T")
  end subroutine testToString


  subroutine testGetGridcells(self)
    !< getIndex unit test
    !<
    !< 1. 2. 3.
    !<   |   |
    !<   x   x  - 4.
    !<     *    <------ request
    !<   x   x  - 2.
    !<
    class(testSuitegGridCellsMap), intent(inout) :: self
    type(shr_gGridCellsMap) :: g
    type(shr_gGridDescriptorFake) :: fakeGridDesc
    type(shr_gAxisFakeLat) :: fakeLaxisLat
    type(shr_gAxisFakeLon) :: fakeLonxisLon
    !type(string) :: gstr
    type(shr_coord) :: center
    type(shr_gridcell) :: expgcs(4)
    type(shr_gridcell), allocatable :: foundgcs(:)
    real(kind=sp), parameter :: RES = 1.0_sp
    integer, parameter :: IDGC = -1
    logical, parameter :: ENABLED = .true.

    !< setup
    center = shr_coord(3.5, 1.5)
    expgcs(1) = shr_gridcell(IDGC, RES, center, ENABLED)

    center = shr_coord(3.5, 2.5)
    expgcs(2) = shr_gridcell(IDGC, RES, center, ENABLED)

    center = shr_coord(2.5, 1.5)
    expgcs(3) = shr_gridcell(IDGC, RES, center, ENABLED)

    center = shr_coord(2.5, 2.5)
    expgcs(4) = shr_gridcell(IDGC, RES, center, ENABLED)

    !< main call
    call g % init(fakeGridDesc, fakeLaxisLat, fakeLonxisLon)
    center = shr_coord(3., 2.)
    foundgcs = g % getgridcells(center)

    !< test
    !call self % assertTrueAllocated(foundgcs, "allocated? g % getGridcells(...) = T")
    !if (status /= 0) return

    !call self % assertTrueSameSize(foundgcs, expgcs, "size? (g % getGridcells(...) vs expgcs ) = T", status)
    !if (status /= 0) return
    if (size(foundgcs) /= size(expgcs)) then
      call self % assert(.false., &
          "g % getGridcells(3,2) .eq. ([3.5,1.5][3.5,2.5][2.5,1.5][2.5,2.5]) = T")
    else
      call self % assert(all(foundgcs == expgcs), &
          "g % getGridcells(3,2) .eq. ([3.5,1.5][3.5,2.5][2.5,1.5][2.5,2.5]) = T")
    end if


  end subroutine testGetGridcells


  subroutine testEqual(self)
    !< == unit test
    class(testSuitegGridCellsMap), intent(inout) :: self
    type(shr_gGridCellsMap) :: g, gother
    type(shr_gGridDescriptorFake) :: fakeGridDesc
    type(shr_gAxisFakeLat) :: fakeLaxisLat
    type(shr_gAxisFakeLon) :: fakeLonxisLon

    !< main call
    call g % init(fakeGridDesc, fakeLaxisLat, fakeLonxisLon)
    call gother % init(fakeGridDesc, fakeLaxisLat, fakeLonxisLon)

    !< test
    call self % assert((g == gother), &
        "g(1., lats=[2,4], lons=[1,3]) .eq. gother(1., lats=[2,4], lons=[1,3]) = T")
  end subroutine testEqual


  subroutine testGetShape(self)
    !< == unit test
    class(testSuitegGridCellsMap), intent(inout) :: self
    type(shr_gGridCellsMap) :: g, gother
    type(shr_gGridDescriptorFake) :: fakeGridDesc
    type(shr_gAxisFakeLat) :: fakeLaxisLat
    type(shr_gAxisFakeLon) :: fakeLonxisLon
    type(shr_gridShape) :: gshape
    type(shr_gridShape) :: gexpected

    !< setup
    gexpected % nlats = 2
    gexpected % nlons = 4

    !< main call
    call g % init(fakeGridDesc, fakeLaxisLat, fakeLonxisLon)

    !< test
    gshape = g % getShape()
    call self % assert(gshape == gexpected, &
        "g(...) % getShape() .eq. [2,4] = T")

  end subroutine testGetShape

end module shr_gGridCellsMap_test

