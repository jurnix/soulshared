!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridMap_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gGridMap unit tests
!------------------------------------------------------------------------------
module shr_gGridMap_test

  use SHR_testSuite_mod, only: testSuite
  use shr_gGridMap_mod, only: shr_gGridMap
  use shr_gAxisMapping_stub, only: shr_gAxisMappingEmptyStub
  use shr_gGridDescriptor_stub, only: shr_gGridDescriptorEmptyStub
  use shr_strings_mod, only: string

  implicit none

  private
  public :: testSuitegGridMap

  type, extends(testSuite) :: testSuitegGridMap

    contains
      procedure :: define => defineTestCases
      procedure, private :: testToString
  end type


  type, extends(shr_gAxisMappingEmptyStub) :: shr_gAxisMappingFakeLat
  contains
    procedure :: toString => gAxisMappingFakeLat_toString
  end type shr_gAxisMappingFakeLat


  type, extends(shr_gAxisMappingEmptyStub) :: shr_gAxisMappingFakeLon
  contains
    procedure :: toString => gAxisMappingFakeLon_toString
  end type shr_gAxisMappingFakeLon


  type, extends(shr_gGridDescriptorEmptyStub) :: shr_gGridDescriptorFake
  contains
    procedure :: toString => gGridDescritorFake_toString
  end type shr_gGridDescriptorFake


contains

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


  type(string) function gGridDescritorFake_toString(self)
    !< string representation of shr_gAxisMapping
    class(shr_gGridDescriptorFake), intent(in) :: self
    gGridDescritorFake_toString = string("-shr_gGridDescriptorFake-")
  end function gGridDescritorFake_toString


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegGridMap), intent(inout) :: self

    call self % testToString()
    !< getShape
    !< getLatAxisMapping
    !< getLonAxisMapping
    !< getGridDescriptor
    !< getIndex !< main feature
    !< equal

  end subroutine defineTestCases


  subroutine testToString(self)
    !< toString unit test
    class(testSuitegGridMap), intent(inout) :: self
    type(shr_gGridMap) :: g
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

end module shr_gGridMap_test

