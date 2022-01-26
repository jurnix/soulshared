!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridArrayMap_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gGridArrayMap unit tests
!------------------------------------------------------------------------------
module shr_gGridArrayMap_test

  use SHR_testSuite_mod, only: testSuite
  use shr_precision_mod, only: sp
  use shr_gGridArrayMap_mod, only: shr_gGridArrayMap
  use shr_gAxisMapping_stub, only: shr_gAxisMappingEmptyStub
  use shr_gAxisMapping_mod, only: shr_igAxisMapping
  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor
  use shr_gGridDescriptor_stub, only: shr_gGridDescriptorEmptyStub
  use shr_strings_mod, only: string
  use shr_gridShape_mod, only: shr_gridShape

  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_coord_mod, only: shr_coord

  implicit none

  private
  public :: testSuitegGridArrayMap

  type, extends(testSuite) :: testSuitegGridArrayMap

    contains
      procedure :: define => defineTestCases
      procedure, private :: testToString
      procedure, private :: testGetIndexMultipleGridcells
      procedure ,private :: testEqual
      procedure, private :: testGetShape
  end type


  type, extends(shr_gAxisMappingEmptyStub) :: shr_gAxisMappingFakeLat
  contains
    procedure :: toString => gAxisMappingFakeLat_toString
    procedure :: getIndexByCoord => gAxisMappingFakeLat_getIndexByCoord
    procedure :: equal => gAxisMappingFakeLat_equal
    procedure :: getSize => gAxisMappingFakeLat_getSize
  end type shr_gAxisMappingFakeLat


  type, extends(shr_gAxisMappingEmptyStub) :: shr_gAxisMappingFakeLon
  contains
    procedure :: toString => gAxisMappingFakeLon_toString
    procedure :: getIndexByCoord => gAxisMappingFakeLon_getIndexByCoord
    procedure :: equal => gAxisMappingFakeLon_equal
    procedure :: getSize => gAxisMappingFakeLon_getSize
  end type shr_gAxisMappingFakeLon


  type, extends(shr_gGridDescriptorEmptyStub) :: shr_gGridDescriptorFake
  contains
    procedure :: toString => gGridDescritorFake_toString
    procedure :: equal => gGridDescriptorFake_equal
  end type shr_gGridDescriptorFake


contains

  logical function gAxisMappingFakeLat_equal(self, other)
    !< true if self and equal have the same attributes
    class(shr_gAxisMappingFakeLat), intent(in) :: self
    class(shr_igAxisMapping), intent(in) :: other
    gAxisMappingFakeLat_equal = .true.
  end function gAxisMappingFakeLat_equal


  logical function gAxisMappingFakeLon_equal(self, other)
    !< true if self and equal have the same attributes
    class(shr_gAxisMappingFakeLon), intent(in) :: self
    class(shr_igAxisMapping), intent(in) :: other
    gAxisMappingFakeLon_equal = .true.
  end function gAxisMappingFakeLon_equal


  type(string) function gAxisMappingFakeLat_toString(self)
    !< string representation of shr_gAxisMapping
    class(shr_gAxisMappingFakeLat), intent(in) :: self
    gAxisMappingFakeLat_toString = string("-shr_gAxisMappingFakeLat-")
  end function gAxisMappingFakeLat_toString


  type(string) function gAxisMappingFakeLon_toString(self)
    !< string representation of shr_gAxisMapping
    class(shr_gAxisMappingFakeLon), intent(in) :: self
    gAxisMappingFakeLon_toString = string("-shr_gAxisMappingFakeLon-")
  end function gAxisMappingFakeLon_toString


  function gAxisMappingFakeLat_getIndexByCoord(self, axisCoord) result (foundIdxs)
    !< it returns the index(s) which matches with axisCoord
    class(shr_gAxisMappingFakeLat), intent(in) :: self
    real(kind=sp), intent(in):: axisCoord
    integer, allocatable :: foundIdxs(:)
    allocate(foundIdxs(2))
    foundIdxs = [0, 1]
  end function gAxisMappingFakeLat_getIndexByCoord


  function gAxisMappingFakeLon_getIndexByCoord(self, axisCoord) result (foundIdxs)
    !< it returns the index(s) which matches with axisCoord
    class(shr_gAxisMappingFakeLon), intent(in) :: self
    real(kind=sp), intent(in):: axisCoord
    integer, allocatable :: foundIdxs(:)
    allocate(foundIdxs(2))
    foundIdxs = [2, 3]
  end function gAxisMappingFakeLon_getIndexByCoord


  type(string) function gGridDescritorFake_toString(self)
    !< string representation of shr_gAxisMapping
    class(shr_gGridDescriptorFake), intent(in) :: self
    gGridDescritorFake_toString = string("-shr_gGridDescriptorFake-")
  end function gGridDescritorFake_toString


  elemental logical function gGridDescriptorFake_equal(self, other)
    !< ==
    class(shr_gGridDescriptorFake), intent(in) :: self
    class(shr_iGGridDescriptor), intent(in) :: other
    gGridDescriptorFake_equal = .false.
  end function gGridDescriptorFake_equal


  integer function gAxisMappingFakeLat_getSize(self)
    !< lat axis size
    class(shr_gAxisMappingFakeLat), intent(in) :: self
    gAxisMappingFakeLat_getSize = 2
  end function gAxisMappingFakeLat_getSize


  integer function gAxisMappingFakeLon_getSize(self)
    !< lon axis size
    class(shr_gAxisMappingFakeLon), intent(in) :: self
    gAxisMappingFakeLon_getSize = 4
  end function gAxisMappingFakeLon_getSize


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegGridArrayMap), intent(inout) :: self

    call self % testToString()
    call self % testGetIndexMultipleGridcells()
    call self % testEqual()
    call self % testGetShape()
    !< getShape
    !< getLatAxisMapping
    !< getLonAxisMapping
    !< getGridDescriptor

  end subroutine defineTestCases


  subroutine testToString(self)
    !< toString unit test
    class(testSuitegGridArrayMap), intent(inout) :: self
    type(shr_gGridArrayMap) :: g
    type(shr_gGridDescriptorFake) :: fakeGridDesc
    type(shr_gAxisMappingFakeLat) :: fakeLaxisLat
    type(shr_gAxisMappingFakeLon) :: fakeLonxisLon
    type(string) :: gstr

    !< toString
    call g % init(fakeGridDesc, fakeLaxisLat, fakeLonxisLon)
    gstr = g % toString()
    call self % assert(gstr == string("-shr_gGridDescriptorFake-, "// &
          "lat=(-shr_gAxisMappingFakeLat-) - lon=(-shr_gAxisMappingFakeLon-)"), &
          "g % toString() .eq. = T")
  end subroutine testToString


  subroutine testGetIndexMultipleGridcells(self)
    !< getIndex unit test
    !<
    !<(2.) (4.)
    !< |   |
    !< +-+-+ - (3.)
    !< |x|x|
    !< +-+-+
    !> |x|x|
    !< +-+-+ - (1.)
    class(testSuitegGridArrayMap), intent(inout) :: self
    type(shr_gGridArrayMap) :: g
    type(shr_gGridDescriptorFake) :: fakeGridDesc
    type(shr_gAxisMappingFakeLat) :: fakeLaxisLat
    type(shr_gAxisMappingFakeLon) :: fakeLonxisLon
    type(string) :: gstr
    type(shr_coord) :: c
    type(shr_gridcellIndex) :: expgcs(4)
    type(shr_gridcellIndex), allocatable :: foundgcs(:)

    !< setup
    expgcs(1) = shr_gridcellIndex(0,2)
    expgcs(2) = shr_gridcellIndex(0,3)
    expgcs(3) = shr_gridcellIndex(1,2)
    expgcs(4) = shr_gridcellIndex(1,3)
    c = shr_coord(2., 3.)

    !< main call
    call g % init(fakeGridDesc, fakeLaxisLat, fakeLonxisLon)
    foundgcs = g % getIndex(c)

    !< test
    call self % assert(all(foundgcs == expgcs), &
        "g % getIndex(2,3) .eq. ([0,2][0,3][1,2][1,3]) = T")

  end subroutine testGetIndexMultipleGridcells


  subroutine testEqual(self)
    !< == unit test
    class(testSuitegGridArrayMap), intent(inout) :: self
    type(shr_gGridArrayMap) :: g, gother
    type(shr_gGridDescriptorFake) :: fakeGridDesc
    type(shr_gAxisMappingFakeLat) :: fakeLaxisLat
    type(shr_gAxisMappingFakeLon) :: fakeLonxisLon

    !< main call
    call g % init(fakeGridDesc, fakeLaxisLat, fakeLonxisLon)
    call gother % init(fakeGridDesc, fakeLaxisLat, fakeLonxisLon)

    !< test
    call self % assert(.not. (g == gother), &
        "g(2,3) .eq. gother(2,4) = F")
  end subroutine testEqual


  subroutine testGetShape(self)
    !< == unit test
    class(testSuitegGridArrayMap), intent(inout) :: self
    type(shr_gGridArrayMap) :: g, gother
    type(shr_gGridDescriptorFake) :: fakeGridDesc
    type(shr_gAxisMappingFakeLat) :: fakeLaxisLat
    type(shr_gAxisMappingFakeLon) :: fakeLonxisLon
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

end module shr_gGridArrayMap_test

