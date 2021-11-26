!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridAxesCell_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gGridAxesCell describes a 1 dimension gridcell 
!> 
!------------------------------------------------------------------------------
module shr_gGridAxesCell_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_coord_mod, only: shr_coord
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gridcell_mod, only: shr_gridcell

  implicit none

  public :: shr_gGridAxesCell

  logical, parameter :: ISDEBUG = .false.


  type shr_gGridAxesCell
    type(shr_gGridAxesBounds), allocatable :: bounds
    real(kind=sp) :: center
  contains
    procedure :: init => gGridAxesCell_initialize
    procedure :: getBounds
    procedure :: getCenter
    procedure :: isIn
    procedure :: equal_gGridAxisCell
    generic :: operator(==) => equal_gGridAxisCell

    procedure :: create_gridcell
    generic :: operator(*) => create_gridcell
  end type shr_gGridAxesCell

contains

  subroutine gGridAxesCell_initialize(self, center, bounds)
    !< gGridAxesCell initialization
    !< startCoord > endCoord
    class(shr_gGridAxesCell), intent(inout) :: self
    real(kind=sp), intent(in) :: center
    type(shr_gGridAxesBounds), intent(in) :: bounds
    self % center = center
    allocate(self % bounds, source = bounds)
  end subroutine gGridAxesCell_initialize
 

  elemental logical function isIn(self, axesCoord)
    !< true if axesCoord is located inside the grid axes cell
    class(shr_gGridAxesCell), intent(in) :: self
    real(kind=sp), intent(in) :: axesCoord
    isIn = self % bounds % isIn(axesCoord)
  end function isIn


  elemental logical function equal_gGridAxisCell(self, other) result (areEqual)
    !< true if 'self' and 'other' have the same attributes
    class(shr_gGridAxesCell), intent(in) :: self
    type(shr_gGridAxesCell), intent(in) :: other
    logical :: hasSameBounds, hasSameCenter
    hasSameBounds = (self % bounds == other % bounds)
    hasSameCenter = (self % center == other % center)
    areEqual = (hasSameBounds .and. hasSameCenter)
  end function equal_gGridAxisCell


  elemental function getBounds(self) result (bounds)
    !< returns self % bounds
    class(shr_gGridAxesCell), intent(in) :: self
    type(shr_gGridAxesBounds) :: bounds !< output
    bounds = self % bounds
  end function getBounds


  elemental real(kind=sp) function getCenter(self) result (center)
    !< return self % center
    class(shr_gGridAxesCell), intent(in) :: self
    center =  self % center
  end function getCenter


  elemental impure type(shr_gridcell) function create_gridcell(lat, lon) result (newGc)
    !< create a new shr_gridcell when combining a set of shr_gGridAxesCells
    class(shr_gGridAxesCell), intent(in) :: lat
    type(shr_gGridAxesCell), intent(in) :: lon
    type(shr_coord) :: ccenter
    real(kind=sp) :: resolution
    type(shr_gGridAxesBounds) :: latBounds

    ccenter = shr_coord(lat % getCenter(), lon % getCenter())
    latBounds = lat % getBounds()
    resolution = latBounds % getStart() - latBounds % getEnd()
    newGc = shr_gridcell(-1, resolution, ccenter, .true.)
  end function create_gridcell

end module shr_gGridAxesCell_mod 

