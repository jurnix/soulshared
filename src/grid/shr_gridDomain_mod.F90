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
  use shr_gridMask_mod, only: shr_IgridMask
  use shr_gridMaskEnabled_mod, only: shr_gridMaskEnabled
  use shr_gridMaskBorder_mod, only: shr_gridMaskBorder
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_coord_mod, only: shr_coord
  use shr_strings_mod, only: string
  use shr_gGrid_mod, only: shr_gGrid

  implicit none

  public :: shr_gridDomain

  logical, parameter :: ISDEBUG = .false.


  type, abstract :: shr_iGridDomain
  contains
    procedure(iface_initialize), deferred :: init_simple
    generic :: init => init_simple
    procedure(iface_getGridMask), deferred :: getBorderGridMask
    procedure(iface_getGridMask), deferred :: getEnabledGridMask
    procedure(iface_filter), deferred :: filter
    procedure(iface_getGrid), deferred :: getGrid

    procedure(iface_copy), deferred :: copy
    generic :: assignment(=) => copy
  end type shr_iGridDomain

  abstract interface
    subroutine iface_initialize(self, grid, enabled, border)
      import :: shr_igridDomain, shr_gGrid, shr_igridMask
      !< grid domain initialization
      !< enabled and border must have the same shape as descriptor
      !< enabled mask must be included in border mask
      !< todo: upgrade enabled and border into class to combine them 'gridDomainEnabledCells'
      class(shr_igridDomain), intent(inout) :: self
      class(shr_gGrid), intent(in) :: grid
      class(shr_igridMask), intent(in) :: enabled !< def: all enabled
      class(shr_igridMask), intent(in) :: border !< def: all enabled
    end subroutine iface_initialize

    function iface_getGridMask(self) result (mask)
      import :: shr_igridDomain, shr_igridMask
      !< returns mask
      class(shr_igridDomain), intent(in) :: self
      class(shr_igridMask), allocatable :: mask
    end function iface_getGridMask

    function iface_filter(self, newGMask) result (newGDomain)
      import :: shr_igridDomain, shr_igridMask, shr_gridDomain
      !< returns the current gridDomain with all values filtered
      !< given the newGMask.
      !< - newGMask and self must have the ssame gridDescriptor
      class(shr_igridDomain), intent(in) :: self
      class(shr_igridMask), intent(in) :: newGMask
      class(shr_igridDomain), allocatable :: newGDomain
    end function iface_filter

    function iface_getGrid(self) result (grid)
      import :: shr_igridDomain, shr_gGrid
      !< returns self shr_gridDescriptor
      class(shr_igridDomain), intent(in) :: self
      class(shr_gGrid), allocatable :: grid
    end function iface_getGrid

    subroutine iface_copy(self, other)
      import :: shr_igridDomain
      !< copy values from other to self
      class(shr_igridDomain), intent(inout) :: self
      class(shr_igridDomain), intent(in) :: other
    end subroutine iface_copy
  end interface


  type, extends(shr_igridDomain) :: shr_gridDomain
    class(shr_gGrid), allocatable :: grid

    !< todo: refactor XXXmapping into GridMapping(unique interface) :: mapping
    !type(shr_gridcellsMapping), allocatable :: gcsMapping !< gridcells indices
    !type(shr_gridIndicesMapping), allocatable :: idxMapping !< array indices
    !< todo: refactor maskXXX into GridMaskAvailable :: available
    class(shr_igridMask), allocatable :: maskEnabled !< allowed to modify, sea vs land (?)
    class(shr_igridMask), allocatable :: maskBorder !< non available gridcells due to partitioning
  contains
    procedure :: init_simple => gridDomain_initialize

    !< getters
    procedure :: getGrid => gridDomain_getGrid
    procedure :: getEnabledGridMask => gridDomain_getEnabledGridMask
    procedure :: getBorderGridMask => gridDomain_getBorderGridMask

    procedure :: gridDomain_combine !< +
    generic :: operator(+) => gridDomain_combine
!    procedure :: gridDomain_difference !< -
!    generic, operator(-) :: gridDomain_difference

    procedure :: copy => gridDomain_copy

    procedure :: gridDomain_and
    generic :: operator(.and.) => gridDomain_and

    procedure :: gridDomain_equal
    generic :: operator(==) => gridDomain_equal

    procedure :: expand
    procedure :: filter => gridDomain_filter
    procedure :: select
  end type shr_gridDomain

contains


  subroutine gridDomain_initialize(self, grid, enabled, border)
    !< grid domain initialization
    !< enabled and border must have the same shape as descriptor
    !< enabled mask must be included in border mask
    !< todo: upgrade enabled and border into class to combine them 'gridDomainEnabledCells'
    class(shr_gridDomain), intent(inout) :: self
    class(shr_gGrid), intent(in) :: grid
    class(shr_igridMask), intent(in) :: enabled !< def: all enabled
    class(shr_igridMask), intent(in) :: border !< def: all enabled

    type(shr_gridMaskEnabled) :: gmEnabled
    type(shr_gridMaskBorder) :: gmBorder
    type(string), allocatable :: tmpBorder, tmpEnabled
    logical :: expectedBorder
    logical, allocatable :: lmask(:,:)

    allocate(self % grid, source = grid)

    !< init maskEnabled
    allocate(self % maskEnabled, source = enabled)

    !< init maskBorder
    allocate(self % maskBorder, source = border)

    !< from gridMAsk to gridMaskEnabled
    call gmEnabled % init(self % maskEnabled)
    !< from gridMask to gridMaskBorder
    lmask = border % get()
    call gmBorder % init(grid, lmask)
    !< 'border' mask matches with 'enabled' gridcells?
    !expectedBorder = self % maskBorder % isIncluded(self % maskEnabled)
    !expectedBorder = gmBorder % isValid(self % maskEnabled)
    expectedBorder = gmBorder % isValid(gmEnabled)
    if (.not. expectedBorder) then
      allocate(tmpBorder, tmpEnabled)
      tmpBorder = self % maskBorder % toString()
      tmpEnabled = self % maskEnabled % toString()
      write(*,*) "maskBorder = ", tmpBorder % toString()
      write(*,*) "maskEnabled= ", tmpEnabled % toString()
      call raiseError(__FILE__, "gridDomain_initialize", &
      "Found active gridcells(enabled) but not included in 'border' mask")
    end if

    !allocate(self % gcsMapping)
    !call self % gcsMapping % init(descriptor)
    !allocate(self % idxMapping)
    !call self % idxMapping % init(self % descriptor)
  end subroutine gridDomain_initialize


  type(shr_gridDomain) function gridDomain_combine(self, other) result (combinedDomain)
    !< it combines 'self' and 'other' into 'combinedDomain'
    !< Combines:
    !< - grid descriptor must be compatible (same resolution and gridcells center)
    !< - grid borders applies an 'and' operation
    !< - grid enabled gridcells must be consistent with grid borders
    !< - grid descriptor to largest bounds
    !< - overlapping gridcells: 'and' operation for its values
    !< in case of conflict:
    !< enabled > disabled > border
    class(shr_gridDomain), intent(in) :: self
    type(shr_gridDomain), intent(in) :: other

    class(shr_gridDomain), allocatable :: expandedSelf, expandedOther
    class(shr_gGrid), allocatable :: combinedGrid

    !< combine grid descriptors
    combinedGrid = (self % getGrid() + other % getGrid())

    allocate(expandedSelf, expandedOther)
    expandedSelf = self % expand(combinedGrid) !< default enabled = false, defalt border = true
    expandedOther = other % expand(combinedGrid) !< default enabled = false, defalt border = true
    !< same grid descriptors
    combinedDomain = expandedSelf .and. expandedOther
  end function gridDomain_combine


  type(shr_gridDomain) function gridDomain_and(self, other) result (newGDomain)
    !< make an 'and' operator of grid domain
    !< 'self' and 'other' grid descriptors must be the same
    !< - enabled mask does an 'and'
    !< - border mask does an 'or'
    class(shr_gridDomain), intent(in) :: self
    type(shr_gridDomain), intent(in) :: other

    class(shr_IgridMask), allocatable :: andEnabledMask, orBorderMask
    class(shr_gGrid), allocatable :: grid

    andEnabledMask = self % getEnabledGridMask() .or. other % getEnabledGridMask()
    orBorderMask = self % getBorderGridMask() .and. other % getBorderGridMask()

    grid = self % getGrid()
    call newGDomain % init(grid, andEnabledMask, orBorderMask)
  end function gridDomain_and


  subroutine gridDomain_copy(self, other)
    !< copy values from other to self
    class(shr_gridDomain), intent(inout) :: self
    class(shr_igridDomain), intent(in) :: other
    class(shr_gGrid), allocatable :: grid
    class(shr_IgridMask), allocatable :: enabled, border

    grid = other % getGrid()
    enabled = other % getEnabledGridMask()
    border = other % getBorderGridMask()

    if (allocated(self % grid)) deallocate(self % grid)
    if (allocated(self % maskEnabled)) deallocate(self % maskEnabled)
    if (allocated(self % maskBorder)) deallocate(self % maskBorder)
    !if (allocated(self % idxMapping)) deallocate(self % idxMapping)
    !if (allocated(self % gcsMapping)) deallocate(self % gcsMapping)

    call self % init(grid, enabled, border)
  end subroutine gridDomain_copy


  function gridDomain_getGrid(self) result (grid)
    !< returns self shr_gridDescriptor
    class(shr_gridDomain), intent(in) :: self
    class(shr_gGrid), allocatable :: grid
    allocate(grid, source = self % grid)
  end function gridDomain_getGrid


  function gridDomain_getEnabledGridMask(self) result (emask)
    !< returns maskEnabled mask
    class(shr_gridDomain), intent(in) :: self
    class(shr_igridMask), allocatable :: emask !< output
    allocate(emask, source = self % maskEnabled)
  end function gridDomain_getEnabledGridMask


  function gridDomain_getBorderGridMask(self) result (bmask)
    !< returns maskEnabled mask
    class(shr_gridDomain), intent(in) :: self
    class(shr_igridMask), allocatable :: bmask
    allocate(bmask, source = self % maskBorder)
  end function gridDomain_getBorderGridMask


  function gridDomain_filter(self, newGMask) result (newGDomain)
    !< returns the current gridDomain with all values filtered
    !< given the newGMask.
    !< - newGMask and self must have the ssame gridDescriptor
    class(shr_gridDomain), intent(in) :: self
    class(shr_igridMask), intent(in) :: newGMask
    class(shr_igridDomain), allocatable :: newGDomain
    class(shr_igridMask), allocatable :: newMaskBorders
    class(shr_igridMask), allocatable :: newMaskEnabled
    class(shr_gGrid), allocatable :: newGrid

    newMaskBorders = (self % maskBorder .and. newGMask)
    newMaskEnabled = (self % maskEnabled .and. newGMask)

    allocate(newGrid, source = self % getGrid())
    allocate(shr_gridDomain :: newGDomain)
    call newGDomain % init(newGrid, newMaskEnabled, newMaskBorders)
  end function gridDomain_filter


   function select(self, newGrid) result (newGDomain)
    !< Selects from 'self' a new shr_gridDomain 'newGMask'
    !< 'newGMask' must fit into the 'self'
    class(shr_gridDomain), intent(in) :: self
    class(shr_gGrid), intent(in) :: newGrid
    class(shr_gridDomain), allocatable :: newGDomain !< output
    class(shr_igridMask), allocatable :: selectedBorder, selectedEnabled
    !type(string) :: tmp

    !tmp = self % maskBorder % toString()
    !write(*,*) "gridDomain_mod:: select:: current border mask =", tmp % toString()
    !tmp = self % maskEnabled % toString()
    !write(*,*) "gridDomain_mod:: select:: current enabled mask =", tmp % toString()

    selectedBorder = self % maskBorder % select(newGrid)
    selectedEnabled = self % maskEnabled % select(newGrid)

    !tmp = selectedBorder % toString()
    !write(*,*) "gridDomain_mod:: select:: selected border mask =", tmp % toString()
    !tmp = selectedEnabled % toString()
    !write(*,*) "gridDomain_mod:: select:: selected enabled mask =", tmp % toString()
    allocate(shr_gridDomain :: newGDomain)

    call newGDomain % init(newgrid, selectedEnabled, selectedBorder)
  end function select


  logical function gridDomain_equal(self, other)
    !< true if self and other have the same attributes
    class(shr_gridDomain), intent(in) :: self
    class(shr_gridDomain), intent(in) :: other

    logical :: hasSameGrid
    logical :: hasSameEnabledMask, hasSameBorderMask
    class(shr_IgridMask), allocatable :: tmpGMask
    type(string) :: tmp1, tmp

    hasSameGrid = (self % grid == other % getGrid())
    !write(*,*) "gridDomain_equal:: same grid? ", hasSameGrid

    !< enabled
    hasSameEnabledMask = (self % maskEnabled == other % getEnabledGridMask())
    !tmp = self % maskEnabled % toString()
    !tmpGMask = other % getEnabledGridMask()
    !tmp1 = tmpGMask % toString()
    !write(*,*) "gridDomain_equal:: same enabled? ", hasSameEnabledMask
    !write(*,*) "gridDomain_equal:: self enabled mask =", tmp % toString()
    !write(*,*) "gridDomain_equal:: other enabled mask =", tmp1 % toString()

    !< border
    hasSameBorderMask = (self % maskBorder == other % getBorderGridMask())
    !write(*,*) "gridDomain_equal:: same border? ", hasSameBorderMask
    !tmp = self % maskBorder % toString()
    !tmpGMask = other % getBorderGridMask()
    !tmp1 = tmpGMask % toString()
    !write(*,*) "gridDomain_equal:: self border mask =", tmp % toString()
    !write(*,*) "gridDomain_equal:: other border mask =", tmp1 % toString()

    gridDomain_equal = (hasSameGrid .and. hasSameEnabledMask .and. &
                        hasSameBorderMask)
  end function gridDomain_equal


  type(shr_gridDomain) function expand(self, grid) result (newGDomain)
    !< resize to bigger domain defined by gDescriptor
    !< 'gDescripter' must be the same or bigger than 'self'
    !< by default:
    !< - enabled mask: new mask cells set to false
    !< - border mask: new mask cells set to true
    class(shr_gridDomain), intent(in) :: self
    class(shr_gGrid), intent(in) :: grid

    class(shr_IgridMask), allocatable :: expandedBorder, expandedEnabled

    expandedBorder = self % maskBorder % expand(grid, default = .true.)
    expandedEnabled = self % maskEnabled % expand(grid, default = .false.)

    call newGDomain % init(grid, expandedEnabled, expandedBorder)
  end function expand

end module shr_gridDomain_mod 

