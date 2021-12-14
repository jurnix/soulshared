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

  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
  use shr_gridcellsMapping_mod, only: shr_gridcellsMapping
  use shr_gridIndicesMapping_mod, only: shr_gridIndicesMapping
  use shr_gridMask_mod, only: shr_gridMask

  implicit none

  public :: shr_gridDomain

  logical, parameter :: ISDEBUG = .false.


  type shr_gridDomain
    type(shr_gGridDescriptor), allocatable :: descriptor

    type(shr_gridcellsMapping), allocatable :: gcsMapping !< gridcells
    type(shr_gridIndicesMapping), allocatable :: idxMapping !< array indices
    type(shr_gridMask), allocatable :: maskEnabled !< allowed to modify, sea vs land (?)
    type(shr_gridMask), allocatable :: maskBorder !< non available gridcells due to partitioning
  contains
    procedure :: gridDomain_initialize
    generic :: init => gridDomain_initialize

    !< getters
    procedure :: getGridDescriptor
    procedure :: getMaskGrid
    procedure :: getMaskBounds
!    procedure :: toGrid !< transfrom shr_gridDomain into shr_grid 

    procedure :: gridDomain_combine !< +
    generic :: operator(+) => gridDomain_combine
!    procedure :: gridDomain_difference !< -
!    generic, operator(-) :: gridDomain_difference
!    procedure :: gridDomain_copy !< -

!    generic, assignment(=) :: gridDomain_copy
!    procedure :: gridDomain_equal !< -
!    generic, operator(==) :: gridDomain_equal
  end type shr_gridDomain

contains


  subroutine gridDomain_initialize(self, descriptor, enabled, border)
    !< grid domain initialization
    class(shr_gridDomain), intent(inout) :: self
    type(shr_gGridDescriptor), intent(in) :: descriptor
    type(shr_gridMask), intent(in) :: enabled !< def: all enabled
    type(shr_gridMask), intent(in) :: border !< def: all enabled

    type(shr_gridMask), allocatable :: disabledMask, disabledBorder
    !type(shr_gridMask), allocatable :: expectedBorder
    logical :: expectedBorder

    allocate(self % descriptor, source = descriptor)

    !< init maskEnabled
    allocate(self % maskEnabled, source = enabled)

    !< init maskBorder
    allocate(self % maskBorder, source = border)


    !< 'border' matches with 'enabled' gridcells?
    !< ('border' gridcells are disabled in 'enabled')
    !< partition border
    !< T T F F F T -> True where border
    !<
    !< land grid cells -> True where land grid cells
    !< F F T F T F   -> rev (select disabled) -> T T F T F T
    !<                                              .and.
    !<                                 (border)  T T - - - T
    !< all true? -> yes
    !< todo: create shr_gGridMaskOverlay? higher level ops
    allocate(disabledMask, disabledBorder)
    disabledMask = enabled
    !< select potential border cells
    call disabledMask % reverse()
    !< potential border cells match with chosen 'border'?
    disabledBorder = (Border .and. disabledMask)
    !< all match?
    expectedBorder = (disabledBorder == border)
    if (.not. expectedBorder) then
      call raiseError(__FILE__, "gridDomain_initialize", &
      "'enabled' overlaps with 'border' mask")
    end if

    allocate(self % gcsMapping)
    !< todo: pass grid descriptor
    call self % gcsMapping % init(self % descriptor % getResolution(), &
            self % descriptor % getLatAxis(), self % descriptor % getLonAxis())
    allocate(self % idxMapping)
    call self % idxMapping % init(self % descriptor % getLatAxis(), &
            self % descriptor % getLonAxis())
  end subroutine gridDomain_initialize


  type(shr_gridDomain) function gridDomain_combine(self, other) result (combinedDomain)
    !< it combines 'self' and 'other' into 'combinedDomain'
    !< Combination merges both grid bounds when:
    !< - overlap: both gridcells are enabled
    !< - no overlap:  if true where enabled
    class(shr_gridDomain), intent(in) :: self
    type(shr_gridDomain), intent(in) :: other

    type(shr_gGridDescriptor) :: newDescriptor
    type(shr_gridMask) :: newMaskGrid, newMaskBounds

    if (.not. self % getGridDescriptor() == other % getGridDescriptor()) then
      call raiseError(__FILE__, "gridDomain_combine", &
            "'self' and other 'must' have the same grid descriptor", &
            "But it is not the case")
    end if

    !< combine
    newDescriptor = self % getGridDescriptor()
    newMaskGrid = self % getMaskGrid() .and. other % getMaskGrid()
    newMaskBounds = self % getMaskBounds() .and. other % getMaskBounds()

    call combinedDomain % init(newDescriptor, newMaskGrid, newMaskBounds)
  end function gridDomain_combine


  type(shr_gGridDescriptor) function getGridDescriptor(self)
    !< returns self shr_gridDescriptor
    class(shr_gridDomain), intent(in) :: self
    getGridDescriptor = self % descriptor
  end function getGridDescriptor


  type(shr_gridMask) function getMaskGrid(self)
    !< returns maskEnabled mask
    class(shr_gridDomain), intent(in) :: self
    getMaskGrid = self % maskEnabled
  end function getMaskGrid


  type(shr_gridMask) function getMaskBounds(self)
    !< returns maskEnabled mask
    class(shr_gridDomain), intent(in) :: self
    getMaskBounds = self % maskEnabled
  end function getMaskBounds

end module shr_gridDomain_mod 

