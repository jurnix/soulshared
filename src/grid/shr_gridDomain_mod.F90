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
!> gridDomain is a part of a grid
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
    procedure :: init => gridDomain_initialize 
    procedure :: isSquared !< yes if the current domain is squared
!    procedure :: toSquaredDomains !< it divides the current domain into smaller with
                                  !< but all squared

!    procedure :: toGrid !< transfrom shr_gridDomain into shr_grid 

!    procedure :: gridDomain_combine !< +
!    generic, operator(+) :: gridDomain_combine
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
    type(shr_gridMask), optional :: enabled !< def: all enabled
    type(shr_gridMask), optional :: border !< def: all enabled

    type(shr_gridMask), allocatable :: disabledMask, disabledBorder
    !type(shr_gridMask), allocatable :: expectedBorder
    logical :: expectedBorder

    allocate(self % descriptor, source = descriptor)

    !< init maskEnabled 
    if (present(enabled)) then
      allocate(self % maskEnabled, source = enabled)
    else
      allocate(self % maskEnabled)
      call self % maskEnabled % init(descriptor, .true.)
    endif

    !< init maskBorder
    if (present(border)) then
      allocate(self % maskBorder, source = border)
    else
     allocate(self % maskBorder)
      call self % maskBorder % init(descriptor, .false.)
    endif

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
    !< todo: create shr_gGridMaskOverlay?
    if (present(border) .and. present(enabled)) then
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
    end if

    allocate(self % gcsMapping)
    !< todo: pass grid descriptor
    call self % gcsMapping % init(self % descriptor % getResolution(), &
            self % descriptor % getLatAxis(), self % descriptor % getLonAxis())
    allocate(self % idxMapping)
    call self % idxMapping % init(self % descriptor % getLatAxis(), &
            self % descriptor % getLonAxis())
  end subroutine gridDomain_initialize


  logical function isSquared(self)
    !< true if all maskBorder gridcells are enabled 
    class(shr_gridDomain), intent(in) :: self
    isSquared = self % maskBorder % any()
  end function isSquared


end module shr_gridDomain_mod 

