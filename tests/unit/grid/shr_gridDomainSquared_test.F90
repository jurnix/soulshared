!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridDomainSquared_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!------------------------------------------------------------------------------
module shr_gridDomainSquared_test

  use shr_precision_mod, only: sp
  use shr_testSuite_mod, only: testSuite
  use shr_gGrid_mod, only: shr_gGrid
  use shr_gridMask_mod, only: shr_gridMask
  use shr_gridDomainSquared_mod, only: shr_gridDomainSquared

  use shr_gridDomain_mod, only: shr_gridDomain
  use shr_gridDomain_test, only: createNewGridDescriptor, createNewGridmap, createNewGridDomain
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
  use shr_gGridArrayMap_mod, only: shr_gGridArrayMap

  implicit none

  private
  public :: testSuitegridDomainSquared

  type, extends(testSuite) :: testSuitegridDomainSquared

  contains
    procedure :: define => defineTestCases
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


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridDomainSquared), intent(inout) :: self
    type(shr_gridDomainSquared) :: sqdomain
    type(shr_gridDomain) :: foundDomain, expectedDomain
    logical :: emask(2,3), bmask(2,3)

    !< setup
    !< enabled
    emask(1,:) = [.true., .true., .false.]
    emask(2,:) = [.true.,  .true., .false.]
    !< border
    bmask(1,:) = [.false., .false., .false.]
    bmask(2,:) = [.false.,  .false., .false.]

    sqDomain = createNewGridDomainSquared(1.0, [3.,1.,2.,-1.], emask)
    expectedDomain = createNewGridDomain(1.0, [3.,1.,2.,-1.], emask, bmask)
    !< test
    foundDomain = sqdomain % toGridDomain()

    !< result
    call self % assert(expectedDomain == foundDomain, &
        "shr_gridDomainSquared(...) .eq. shr_gridDomain(...) = T")

    call self % assert(sqdomain == sqdomain, &
        "gridDomainSquared(...) .eq. gridDomainSquared(...) = T")
  end subroutine defineTestCases

end module shr_gridDomainSquared_test