!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridDomain_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gridDomain is a part of a grid. It is only attached to the current processor.
!> 
!------------------------------------------------------------------------------
module shr_gridDomain_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor
  use shr_gridcellsMapping_mod, only: shr_gridcellsMapping
  use shr_gridIndicesMapping_mod, only: shr_gridIndicesMapping
  use shr_gridMask_mod, only: shr_gridMask
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_coord_mod, only: shr_coord

  implicit none

  public :: shr_gridDomain

  logical, parameter :: ISDEBUG = .false.


  type shr_gridDomain
    class(shr_iGGridDescriptor), allocatable :: descriptor

    type(shr_gridcellsMapping), allocatable :: gcsMapping !< gridcells
    type(shr_gridIndicesMapping), allocatable :: idxMapping !< array indices
    type(shr_gridMask), allocatable :: maskEnabled !< allowed to modify, sea vs land (?)
    type(shr_gridMask), allocatable :: maskBorder !< non available gridcells due to partitioning
  contains
    procedure :: gridDomain_initialize
    generic :: init => gridDomain_initialize

    !< getters
    procedure :: getGridDescriptor
    procedure :: getEnabledGridMask
    procedure :: getBorderGridMask

    procedure :: gridDomain_combine !< +
    generic :: operator(+) => gridDomain_combine
!    procedure :: gridDomain_difference !< -
!    generic, operator(-) :: gridDomain_difference
!    procedure :: gridDomain_copy !< -

!    generic, assignment(=) :: gridDomain_copy
!    procedure :: gridDomain_equal !< -
!    generic, operator(==) :: gridDomain_equal
    procedure :: filter
    procedure :: select
  end type shr_gridDomain

contains


  subroutine gridDomain_initialize(self, descriptor, enabled, border)
    !< grid domain initialization
    class(shr_gridDomain), intent(inout) :: self
    class(shr_iGGridDescriptor), intent(in) :: descriptor
    type(shr_gridMask), intent(in) :: enabled !< def: all enabled
    type(shr_gridMask), intent(in) :: border !< def: all enabled

    logical :: expectedBorder

    allocate(self % descriptor, source = descriptor)

    !< init maskEnabled
    allocate(self % maskEnabled, source = enabled)

    !< init maskBorder
    allocate(self % maskBorder, source = border)

    !< 'border' mask matches with 'enabled' gridcells?
    expectedBorder = self % maskBorder % isIncluded(self % maskEnabled)
    if (.not. expectedBorder) then
      call raiseError(__FILE__, "gridDomain_initialize", &
      "'enabled' overlaps with 'border' mask")
    end if

    allocate(self % gcsMapping)
    call self % gcsMapping % init(descriptor)
    allocate(self % idxMapping)
    call self % idxMapping % init(self % descriptor)
  end subroutine gridDomain_initialize


  type(shr_gridDomain) function gridDomain_combine(self, other) result (combinedDomain)
    !< it combines 'self' and 'other' into 'combinedDomain'
    !< Combines:
    !< - grid descriptor must be compatible (same resolution and gridcells bounds from domain)
    !< - grid borders applies an 'and' operation
    !< - grid enabled gridcells must be consistent with grid borders
    !< - grid descriptor to largest bounds
    !< - overlapping gridcells: 'and' operation for its values
    class(shr_gridDomain), intent(in) :: self
    type(shr_gridDomain), intent(in) :: other

    type(shr_gridMask) :: newMaskGrid, newMaskBounds
    class(shr_iGgridDescriptor), allocatable :: cgDescriptor
    type(shr_gridMask) :: expandedESelfMask, expandedEOtherMask
    type(shr_gridMask) :: expandedSelfMaskBorders, expandedOtherMaskBorders
    type(shr_gridMask) ::otherEnabledGridmask, otherBorderGridMask

    !< combine grid descriptors
    cgDescriptor = (self % getGridDescriptor() + other % getGridDescriptor())

    !< adapt both masks to new grid descriptor dimensions
    expandedESelfMask = self % maskEnabled % expand(cgDescriptor)
    otherEnabledGridMask = other % getEnabledGridMask()
    expandedEOtherMask = otherEnabledGridmask % expand(cgDescriptor)

    !< combine
    newMaskGrid = expandedESelfMask .and. expandedEOtherMask

    !< expand
    expandedSelfMaskBorders = self % maskBorder % expand(cgDescriptor)
    otherBorderGridMask = other % getBorderGridMask()
    expandedOtherMaskBorders = otherBorderGridMask % expand(cgDescriptor)
    !< combine
    newMaskGrid = expandedSelfMaskBorders .and. expandedOtherMaskBorders

    call combinedDomain % init(cgDescriptor, newMaskGrid, newMaskBounds)
  end function gridDomain_combine


  function getGridDescriptor(self) result (gDescriptor)
    !< returns self shr_gridDescriptor
    class(shr_gridDomain), intent(in) :: self
    class(shr_iGgridDescriptor), allocatable :: gDescriptor
    allocate(gDescriptor, source = self % descriptor)
  end function getGridDescriptor


  type(shr_gridMask) function getEnabledGridMask(self)
    !< returns maskEnabled mask
    class(shr_gridDomain), intent(in) :: self
    getEnabledGridMask = self % maskEnabled
  end function getEnabledGridMask


  type(shr_gridMask) function getBorderGridMask(self)
    !< returns maskEnabled mask
    class(shr_gridDomain), intent(in) :: self
    getBorderGridMask = self % maskEnabled
  end function getBorderGridMask


  type(shr_gridDomain) function filter(self, newGMask)
    !< returns the current gridDomain with all values filtered
    !< given the newGMask.
    !< - newGMask and self must have the ssame gridDescriptor
    class(shr_gridDomain), intent(in) :: self
    type(shr_gridMask), intent(in) :: newGMask
    type(shr_gridMask) :: newMaskBorders
    type(shr_gridMask) :: newMaskEnabled
    class(shr_iGGridDescriptor), allocatable :: newGDescriptor

    newMaskBorders = (self % maskBorder .and. newGMask)
    newMaskEnabled = (self % maskEnabled .and. newGMask)

    allocate(newGDescriptor, source = self % getGridDescriptor())
    call filter % init(newGDescriptor, newMaskEnabled, newMaskBorders)
  end function filter


   function select(self, newGDescriptor) result (newGMask)
    !< Selects from 'self' a new shr_gridDomain 'newGMask'
    !< 'newGMask' must fit into the 'self'
    class(shr_gridDomain), intent(in) :: self
    class(shr_iGGridDescriptor), intent(in) :: newGDescriptor
    class(shr_gridDomain), allocatable :: newGMask !< output
    type(shr_gridMask) :: selectedBorder, selectedEnabled

    selectedBorder = self % maskBorder % select(newGDescriptor)
    selectedEnabled = self % maskEnabled % select(newGDescriptor)
    call newGMask % gridDomain_initialize(newGDescriptor, selectedEnabled, selectedBorder)
  end function select

end module shr_gridDomain_mod 

