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
  use shr_gridMask_mod, only: shr_IgridMask
  use shr_gridMaskEnabled_mod, only: shr_gridMaskEnabled
  use shr_gridMaskBorder_mod, only: shr_gridMaskBorder
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_coord_mod, only: shr_coord
  use shr_strings_mod, only: string

  implicit none

  public :: shr_gridDomain

  logical, parameter :: ISDEBUG = .false.


  type shr_gridDomain
    class(shr_iGGridDescriptor), allocatable :: descriptor

    !< todo: refactor XXXmapping into GridMapping(unique interface) :: mapping
    type(shr_gridcellsMapping), allocatable :: gcsMapping !< gridcells indices
    type(shr_gridIndicesMapping), allocatable :: idxMapping !< array indices
    !< todo: refactor maskXXX into GridMaskAvailable :: available
    class(shr_igridMask), allocatable :: maskEnabled !< allowed to modify, sea vs land (?)
    class(shr_igridMask), allocatable :: maskBorder !< non available gridcells due to partitioning
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
    procedure :: gridDomain_equal
    generic :: operator(==) => gridDomain_equal

    procedure :: filter
    procedure :: select
  end type shr_gridDomain

contains


  subroutine gridDomain_initialize(self, descriptor, enabled, border)
    !< grid domain initialization
    !< enabled and border must have the same shape as descriptor
    !< enabled mask must be included in border mask
    !< todo: upgrade enabled and border into class to combine them 'gridDomainEnabledCells'
    class(shr_gridDomain), intent(inout) :: self
    class(shr_iGGridDescriptor), intent(in) :: descriptor
    class(shr_igridMask), intent(in) :: enabled !< def: all enabled
    class(shr_igridMask), intent(in) :: border !< def: all enabled

    type(shr_gridMaskEnabled) :: gmEnabled
    type(shr_gridMaskBorder) :: gmBorder
    type(string), allocatable :: tmpBorder, tmpEnabled
    logical :: expectedBorder
    logical, allocatable :: lmask(:,:)

    allocate(self % descriptor, source = descriptor)

    !< init maskEnabled
    allocate(self % maskEnabled, source = enabled)

    !< init maskBorder
    allocate(self % maskBorder, source = border)

    !< from gridMAsk to gridMaskEnabled
    call gmEnabled % init(self % maskEnabled)
    !< from gridMask to gridMaskBorder
    lmask = border % getRaw()
    call gmBorder % init(descriptor, lmask)
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

    allocate(self % gcsMapping)
    call self % gcsMapping % init(descriptor)
    allocate(self % idxMapping)
    call self % idxMapping % init(self % descriptor)
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
    !< todo: include gridDomain_expand
    class(shr_gridDomain), intent(in) :: self
    type(shr_gridDomain), intent(in) :: other

    class(shr_igridMask), allocatable :: newEnabledMaskGrid, newBorderMaskGrid
    class(shr_iGgridDescriptor), allocatable :: cgDescriptor
    class(shr_igridMask), allocatable :: expandedESelfMask, expandedEOtherMask
    class(shr_igridMask), allocatable :: expandedSelfMaskBorders, expandedOtherMaskBorders
    class(shr_igridMask), allocatable ::otherEnabledGridmask, otherBorderGridMask

    !type(string) :: tmp

    !< combine grid descriptors
    cgDescriptor = (self % getGridDescriptor() + other % getGridDescriptor())
    !tmp = cgDescriptor % toString()
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: combined descriptor=", tmp % toString()

    !< adapt both masks to new grid descriptor dimensions
    !< self enabled
    !tmp = self % maskEnabled % toString()
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: original self enabled mask=", tmp % toString()
    expandedESelfMask = self % maskEnabled % expand(cgDescriptor, default = .false.)
    !tmp = expandedESelfMask % toString()
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: expanded self enabled mask=", tmp % toString()

    !< other enabled mask
    otherEnabledGridMask = other % getEnabledGridMask()
    !tmp = otherEnabledGridMask % toString()
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: original other enabled mask=", tmp % toString()
    expandedEOtherMask = otherEnabledGridmask % expand(cgDescriptor, default = .false.)
    !tmp = expandedEOtherMask % toString()
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: expanded other enabled mask=", tmp % toString()

    !write(*,*) "gridDomain_mod:: gridDomain_combine:: -----------------------------------------------------"
    !write(*,*) "gridDomain_mod:: gridDomain_combine::                   ...combining..."
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: -----------------------------------------------------"
    !< combine enabled
    newEnabledMaskGrid = expandedESelfMask .or. expandedEOtherMask
    !tmp = newEnabledMaskGrid % toString()
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: combined enabled mask=", tmp % toString()
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: ...done"
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: "

    !< expand border
    !< expand 'self'
    !tmp = self % maskBorder % toString()
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: original self border mask=", tmp % toString()
    expandedSelfMaskBorders = self % maskBorder % expand(cgDescriptor)
    !tmp = expandedSelfMaskBorders % toString()
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: expanded self border mask=", tmp % toString()

    !< expand 'other'
    otherBorderGridMask = other % getBorderGridMask()
    !tmp = otherBorderGridMask % toString()
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: original other border mask=", tmp % toString()
    expandedOtherMaskBorders = otherBorderGridMask % expand(cgDescriptor)
    !tmp = expandedOtherMaskBorders % toString()
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: expanded other border mask=", tmp % toString()

    !< combine border
    newBorderMaskGrid = expandedSelfMaskBorders .and. expandedOtherMaskBorders
    !tmp = newBorderMaskGrid % toString()
    !write(*,*) "gridDomain_mod:: gridDomain_combine:: combined border mask=", tmp % toString()

    call combinedDomain % init(cgDescriptor, newEnabledMaskGrid, newBorderMaskGrid)
  end function gridDomain_combine


  function getGridDescriptor(self) result (gDescriptor)
    !< returns self shr_gridDescriptor
    class(shr_gridDomain), intent(in) :: self
    class(shr_iGgridDescriptor), allocatable :: gDescriptor
    allocate(gDescriptor, source = self % descriptor)
  end function getGridDescriptor


  function getEnabledGridMask(self) result (emask)
    !< returns maskEnabled mask
    class(shr_gridDomain), intent(in) :: self
    class(shr_igridMask), allocatable :: emask !< output
    allocate(emask, source = self % maskEnabled)
  end function getEnabledGridMask


  function getBorderGridMask(self) result (bmask)
    !< returns maskEnabled mask
    class(shr_gridDomain), intent(in) :: self
    class(shr_igridMask), allocatable :: bmask
    allocate(bmask, source = self % maskBorder)
  end function getBorderGridMask


  type(shr_gridDomain) function filter(self, newGMask)
    !< returns the current gridDomain with all values filtered
    !< given the newGMask.
    !< - newGMask and self must have the ssame gridDescriptor
    class(shr_gridDomain), intent(in) :: self
    class(shr_igridMask), intent(in) :: newGMask
    class(shr_igridMask), allocatable :: newMaskBorders
    class(shr_igridMask), allocatable :: newMaskEnabled
    class(shr_iGGridDescriptor), allocatable :: newGDescriptor

    newMaskBorders = (self % maskBorder .and. newGMask)
    newMaskEnabled = (self % maskEnabled .and. newGMask)

    allocate(newGDescriptor, source = self % getGridDescriptor())
    call filter % init(newGDescriptor, newMaskEnabled, newMaskBorders)
  end function filter


   function select(self, newGDescriptor) result (newGDomain)
    !< Selects from 'self' a new shr_gridDomain 'newGMask'
    !< 'newGMask' must fit into the 'self'
    class(shr_gridDomain), intent(in) :: self
    class(shr_iGGridDescriptor), intent(in) :: newGDescriptor
    class(shr_gridDomain), allocatable :: newGDomain !< output
    class(shr_igridMask), allocatable :: selectedBorder, selectedEnabled
    type(string) :: tmp

    tmp = self % maskBorder % toString()
    write(*,*) "gridDomain_mod:: select:: current border mask =", tmp % toString()
    tmp = self % maskEnabled % toString()
    write(*,*) "gridDomain_mod:: select:: current enabled mask =", tmp % toString()

    selectedBorder = self % maskBorder % select(newGDescriptor)
    selectedEnabled = self % maskEnabled % select(newGDescriptor)

    tmp = selectedBorder % toString()
    write(*,*) "gridDomain_mod:: select:: selected border mask =", tmp % toString()
    tmp = selectedEnabled % toString()
    write(*,*) "gridDomain_mod:: select:: selected enabled mask =", tmp % toString()
    allocate(shr_gridDomain :: newGDomain)
    call newGDomain % gridDomain_initialize(newGDescriptor, selectedEnabled, selectedBorder)
  end function select


  logical function gridDomain_equal(self, other)
    !< true if self and other have the same attributes
    class(shr_gridDomain), intent(in) :: self
    class(shr_gridDomain), intent(in) :: other

    logical :: hasSameGDescriptor
    logical :: hasSameEnabledMask, hasSameBorderMask
    class(shr_IgridMask), allocatable :: tmpGMask
    type(string) :: tmp1, tmp

    hasSameGDescriptor = (self % descriptor == other % getGridDescriptor())
    write(*,*) "gridDomain_equal:: same descriptor? ", hasSameGDescriptor

    !< enabled
    hasSameEnabledMask = (self % maskEnabled == other % getEnabledGridMask())
    tmp = self % maskEnabled % toString()
    tmpGMask = other % getEnabledGridMask()
    tmp1 = tmpGMask % toString()
    write(*,*) "gridDomain_equal:: same enabled? ", hasSameEnabledMask
    write(*,*) "gridDomain_equal:: self enabled mask =", tmp % toString()
    write(*,*) "gridDomain_equal:: other enabled mask =", tmp1 % toString()

    !< border
    hasSameBorderMask = (self % maskBorder == other % getBorderGridMask())
    write(*,*) "gridDomain_equal:: same border? ", hasSameBorderMask
    tmp = self % maskBorder % toString()
    tmpGMask = other % getBorderGridMask()
    tmp1 = tmpGMask % toString()
    write(*,*) "gridDomain_equal:: self border mask =", tmp % toString()
    write(*,*) "gridDomain_equal:: other border mask =", tmp1 % toString()

    gridDomain_equal = (hasSameGDescriptor .and. hasSameEnabledMask .and. &
                        hasSameBorderMask)
  end function gridDomain_equal

end module shr_gridDomain_mod 

