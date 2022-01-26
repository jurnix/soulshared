!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMask_stub 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_gridMask stub
!------------------------------------------------------------------------------
module shr_gridMask_stub

  use shr_precision_mod, only: sp
  use shr_gridMask_mod, only: shr_IgridMask
  !< dependencies
  use shr_strings_mod, only: string
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor, shr_gGridDescriptor
  use shr_gridBoundIndices_mod, only: shr_gridBoundIndices
  use shr_gGrid_mod, only: shr_gGrid
  use shr_gGridArrayMap_mod, only: shr_gGridArrayMap
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_gAxisMapping_mod, only: shr_gAxisMapping

  implicit none

  private
  public :: shr_gridMaskStub


  !< stub
  type, extends(shr_IgridMask) :: shr_gridMaskStub
  contains
    procedure :: initialize, initialize_by_larray

    procedure :: get
    procedure :: getGrid

    procedure :: isIncluded

    procedure :: equal_scalar_logical
    procedure :: equal_rawMask
    procedure :: equal_gridMask

    procedure :: or_gridMask
    procedure :: and_gridMask
    procedure :: any

    procedure :: expand
    procedure :: select
    procedure :: set_by_gridmask
    procedure :: set_by_lmask
    procedure :: toString
    procedure :: getShape
  end type shr_gridMaskStub


contains

  subroutine initialize_by_larray(self, grid, lmask)
    !< gridMask initialization
    class(shr_gridMaskStub), intent(inout) :: self
    class(shr_gGrid), intent(in) :: grid
    logical, intent(in) :: lmask(:,:)
  end subroutine initialize_by_larray

  subroutine initialize(self, grid, default)
    !< gridMask initialization
    class(shr_gridMaskStub), intent(inout) :: self
    class(shr_gGrid), intent(in) :: grid
    logical, intent(in), optional :: default !< define default value (def: true)
  end subroutine initialize

  !<
  !< shr_gridMaskStub
  !<
  function get(self, gBoundIndices) result (newMask)
    !< returns current mask
    !< in case gBoundIndices are defined it returns the selected indices
    !< those indices must be valid
    class(shr_gridMaskStub), intent(in) :: self
    type(shr_gridBoundIndices), intent(in), optional :: gBoundIndices
    logical, allocatable :: newMask(:,:) !< output
    allocate(newMask(4,3))
    newMask(1,:) = [.true., .true., .true.]
    newMask(2,:) = [.true., .true., .true.]
    newMask(3,:) = [.true., .false., .false.]
    newMask(4,:) = [.false., .true., .true.]
  end function get


  type(shr_gGridAxesBounds) function getNewAxisBounds(start, end)
    !< creates a new gridaxisbounds
    real(kind=sp), intent(in) :: start, end
    call getNewAxisBounds % init(start, end)
  end function getNewAxisBounds


  function getGrid(self) result(newGrid)
    !< returns self gridDescriptor
    class(shr_gridMaskStub), intent(in) :: self
    class(shr_gGrid), allocatable :: newGrid !< output
    class(shr_iGGridDescriptor), allocatable :: newGDescriptor !< output
    type(shr_gGridArrayMap) :: newGMap
    type(shr_gridBounds) :: bounds !< n, s, e, w
    type(shr_gGridAxes) :: laxis, lonxis
    type(shr_gAxisMapping) :: laxisMapping, lonxisMapping
    type(shr_gGridAxesBounds) :: laxisBounds, lonxisBounds
    call bounds % init(4.,0.,3.,0.)
    allocate(shr_gGridDescriptor :: newGDescriptor)
    call newGDescriptor % init(1., bounds)

    laxisBounds = getNewAxisBounds(4., 0.)
    lonxisBounds = getNewAxisBounds(3., 0.)

    call laxis % init(string("lats"), 1., laxisBounds)
    call lonxis % init(string("lons"), 1., lonxisBounds)

    call laxisMapping % init(laxis)
    call lonxisMapping % init(lonxis)

    call newGMap % init(newGDescriptor, laxisMapping, lonxisMapping)

    allocate(newGrid)
    call newGrid % init(newGDescriptor, newGMap)
  end function getGrid

  
  logical function isIncluded(self, other)
    !< true if other gridMask true gridcells also match self mask array
    class(shr_gridMaskStub), intent(in) :: self
    class(shr_igridMask), intent(in) :: other
  end function isIncluded


  logical function equal_scalar_logical(self, value)
    !< true if all values match 'value'
    class(shr_gridMaskStub), intent(in) :: self
    logical, intent(in) :: value
  end function equal_scalar_logical


  logical function equal_rawMask(self, mask)
    !< true if all values match 'value'
    class(shr_gridMaskStub), intent(in) :: self
    logical, intent(in) :: mask(:,:)
  end function equal_rawMask


  logical function equal_gridMask(self, other)
    !< true if all values match 'value'
    class(shr_gridMaskStub), intent(in) :: self
    class(shr_igridMask), intent(in) :: other
  end function equal_gridMask


  function expand(self, grid, default) result (newGMask)
    !< returns a new shr_gridMask with an expanded grid
    !< - 'self' must fit into 'gDescriptor'
    !< - mask remains the same
    class(shr_gridMaskStub), intent(in) :: self
    class(shr_gGrid), intent(in) :: grid
    logical, intent(in), optional :: default
    class(shr_igridMask), allocatable :: newGMask !< output
  end function expand


  function select(self, grid) result (newGMask)
    !< select a new shr_gridMask according to gDescriptor
    !< new gDscriptor must fit self % gridDescriptor
    class(shr_gridMaskStub), intent(in) :: self
    class(shr_gGrid), intent(in) :: grid
    class(shr_igridMask), allocatable :: newGMask !< output
  end function select


  function or_gridMask(self, other) result (newMask)
    !< returns a new gridMask with matching gridcells
    !< both must have the same size
    !<
    !< gridMask(T T F) = gridMask(T T F) .and. gridMask(F T F)
    class(shr_gridMaskStub), intent(in) :: self
    class(shr_igridMask), intent(in) :: other
    class(shr_igridMask), allocatable :: newMask !< output
  end function or_gridMask


  function and_gridMask(self, other) result (newMask)
    !< returns a new gridMask with matching gridcells
    !< both must have the same size
    !<
    !< gridMask(F T F) = gridMask(T T F) .and. gridMask(F T F)
    class(shr_gridMaskStub), intent(in) :: self
    class(shr_igridMask), intent(in) :: other
    class(shr_igridMask), allocatable :: newMask !< output
  end function and_gridMask


  subroutine set_by_gridmask(self, gridMask)
    !< set values of 'gridMask' into 'self'
    !< 'gridMask' shape must be consistent with 'self'
    !< - gridMask must fit in 'self'
    class(shr_gridMaskStub), intent(inout) :: self
    class(shr_igridMask), intent(in) :: gridMask
  end subroutine set_by_gridmask


  subroutine set_by_lmask(self, mask, gBindices)
    !< set values of mask into self
    !< when defined gBindices:
    !< - place 'mask' into gBIndices indices
    !<
    !< 'mask' shape must be consistent with 'self'
    !< 'gBindices' must be consistent with 'mask' and 'self'
    class(shr_gridMaskStub), intent(inout) :: self
    logical, intent(in) :: mask(:,:)
    type(shr_gridBoundIndices), intent(in), optional :: gBindices
  end subroutine set_by_lmask


  logical function any(self)
    !< true if any 'a' and 'b' has true value
    !< wrap to enable 'any' from shr_gridMask
    class(shr_gridMaskStub), intent(in) :: self
  end function any


  type(string) function toString(self)
    !< mask to string type
    class(shr_gridMaskStub), intent(in) :: self
  end function toString


  function getShape(self) result (shape)
    !<
    class(shr_gridMaskStub), intent(in) :: self
    integer :: shape(2)
  end function getShape

end module shr_gridMask_stub

