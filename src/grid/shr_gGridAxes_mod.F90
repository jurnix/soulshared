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
!    procedure :: getSize !< total number of cells
!    procedure :: getIndices !< Given an axes coordinates it return its index
!    procedure :: getGridAxesCell !< Given an index it return its AxesCell info 
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


  type(string) function getName(self)
    !< it returns the name attribute
    class(shr_gGridAxes), intent(in) :: self
    getName = self %  name
  end function getName


  type(shr_gGridAxesBounds) function getBounds(self)
    !< it returns the bounds attribute
    class(shr_gGridAxes), intent(in) :: self
    getBounds = self % bounds
  end function getBounds


  real(kind=sp) function getResolution(self)
    !< it returns the resolution attribute
    class(shr_gGridAxes), intent(in) :: self
    getResolution = self % resolution
  end function getResolution


  integer function getSize(self)
    !< it returns how many grid axes cells has
    class(shr_gGridAxes), intent(in) :: self
    getSize = size(self % cells)
  end function getSize

end module shr_gGridAxes_mod 

