!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridAxes_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gGridAxes defines a grid axes.
!> 
!------------------------------------------------------------------------------
module shr_gGridAxes_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_arrayDim_mod, only: shr_arrayRspDim
  use shr_strings_mod, only: string
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_gGridAxesCell_mod, only: shr_gGridAxesCell
  
  implicit none

  public :: shr_gGridAxes

  logical, parameter :: ISDEBUG = .false.


  type shr_gGridAxes
    type(string), allocatable :: name
    real(kind=sp) :: resolution
    type(shr_gGridAxesBounds), allocatable :: bounds
    type(shr_gGridAxesCell), allocatable :: cells(:)
  contains
    procedure :: init => gGridAxes_initialize 

    procedure :: getName
    procedure :: getBounds
    procedure :: getResolution
    procedure :: getSize

    procedure :: expand !< array2d 

    procedure :: hasGridCoord 
    procedure :: hasGridAxisCell

    procedure :: eq_gGridAxes
    generic :: operator(==) => eq_gGridAxes
  end type shr_gGridAxes

contains

  subroutine gGridAxes_initialize(self, name, resolution, bounds)
    !< gGridAxes initialization
    class(shr_gGridAxes), intent(inout) :: self
    type(string), intent(in) :: name !< grid axes name
    real(kind=sp), intent(in) :: resolution !< axes cell width ( always +)
    type(shr_gGridAxesBounds), intent(in) :: bounds !< axes boundaries

    type(shr_arrayRspDim) :: arrayDim !< used to generate axes cells
    integer :: icell !< iterator
    type(shr_gGridAxesBounds), allocatable :: cellBounds
    real(kind=sp) :: center, cellStart, cellEnd
    integer :: nCells !< total number of axes cells

    if (resolution <= 0) then
      call raiseError(__FILE__, "gGridAxes_initialize", &
              "'resolution cannot be 0 or negative'")
    endif

    allocate(self % name, source = name)
    self % resolution = resolution
    allocate(self % bounds, source = bounds)

    !< generate grid axes boundaries
    call arrayDim % init(name % toString(), bounds % getStart(), &
            bounds % getEnd(), -resolution)

    nCells = arraydim % getSize() - 1
    allocate(self % cells(nCells))

    !< create grid axes cells
    do icell = 1, nCells !- 1
      ! find axes cell boundary
      cellStart = arrayDim % getValue(icell)
      cellEnd = arrayDim % getValue(icell+1)

      allocate(cellBounds)
      call cellBounds % init(cellStart, cellEnd)
      ! calculate its axes cell center
      center = (cellStart + cellEnd) / 2.0

      ! add
      call self % cells(icell) % init(center, cellBounds)
      deallocate(cellBounds) !< reuse
    enddo
  end subroutine gGridAxes_initialize


  elemental type(string) function getName(self)
    !< it returns the name attribute
    class(shr_gGridAxes), intent(in) :: self
    getName = self %  name
  end function getName


  elemental type(shr_gGridAxesBounds) function getBounds(self)
    !< it returns the bounds attribute
    class(shr_gGridAxes), intent(in) :: self
    getBounds = self % bounds
  end function getBounds


  elemental real(kind=sp) function getResolution(self)
    !< it returns the resolution attribute
    class(shr_gGridAxes), intent(in) :: self
    getResolution = self % resolution
  end function getResolution


  integer function getSize(self)
    !< it returns how many grid axes cells has
    class(shr_gGridAxes), intent(in) :: self
    getSize = size(self % cells)
  end function getSize


  function expand(self, otherAxes) result (gridcells)
    !< expands 'self' and 'other' to create a 2d array of shr_gridcells
    !< its expansion provides each possible combination between each
    !< grid axes.
    !<
    !< example: gridcells(:,:) = latsAxes(1d) % expand( lonsAxes(1d) )
    !<
    use shr_gridcell_mod, only: shr_gridcell
    class(shr_gGridAxes), intent(in) :: self
    type(shr_gGridAxes), intent(in) :: otherAxes
    type(shr_gridcell), allocatable :: gridcells(:,:) !< output

    integer :: ilat, ilon
    integer :: nlats, nlons

    nlats = self % getSize()
    nlons = otherAxes % getSize()
    allocate(gridcells(nlats, nlons))

    do ilat = 1, nlats
      do ilon = 1, nlons
        gridcells(ilat, ilon) = self % cells(ilat) * otherAxes % cells(ilon)
      enddo
    enddo 

  end function expand


  logical function hasGridCoord(self, axisCoord)
    !< true if gridCoord is found inside gridAxes bounds
    class(shr_gGridAxes), intent(in) :: self
    real(kind=sp), intent(in):: axisCoord
    hasGridCoord = self % bounds % isIn(axisCoord) 
  end function hasGridCoord


  logical function hasGridAxisCell(self, gridAxisCell)
    !< true if gridAxisCell is found 
    class(shr_gGridAxes), intent(in) :: self
    type(shr_gGridAxesCell), intent(in):: gridAxisCell
    integer :: icell
    hasGridAxisCell = .false.
    do icell = 1, self % getSize()
      if (gridAxisCell == self % cells(icell)) then
        hasGridAxisCell = .true.
        exit
      endif
    enddo
  end function hasGridAxisCell


  elemental logical function eq_gGridAxes(self, other)
    !< true if 'self' and 'other' have the same attributes
    class(shr_gGridAxes), intent(in) :: self
    type(shr_gGridAxes), intent(in) :: other 
    logical :: hasSameName, hasSameRes, hasSameBounds, hasSameCells

    hasSameName = (self % name == other % getName())
    hasSameRes = (self % resolution == other % getResolution())
    hasSameBounds = (self % bounds == other % getBounds())
    hasSameCells = all(self % cells == other % Cells)

    eq_gGridAxes = (hasSameName .and. hasSameRes .and. &
                    hasSameBounds .and. hasSameCells)
  end function eq_gGridAxes


!  type(shr_gridcellsMap) function create_gridcellsMap(self, other) result (newGcMap)
!    !< TODO - circular dependency found, error on cmake
!    use shr_gridcellsMap_mod, only: shr_gridcellsMap
    !< creates a new shr_gridcellsMap 'self' and 'other' to create shr_gridcellsMap
    !< example: gridcellsMap = latsAxes * lonsAxes
!    class(shr_gGridAxes), intent(in) :: self
!    type(shr_gGridAxes), intent(in) :: other

!    real(kind=sp) :: resolution

!    resolution = self % getResolution()
!    call newGcMap % init(resolution, self, other)
!  end function create_gridcellsMap

end module shr_gGridAxes_mod 

