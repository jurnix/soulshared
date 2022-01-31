!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridBoundIndices_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> grid bounds indices which defines a grid
!------------------------------------------------------------------------------

module shr_gridBoundIndices_mod

  use SHR_precision_mod, only: sp
  use SHR_error_mod, only: raiseError
  use shr_gAxisBounds_mod, only: shr_gAxisBounds
  use shr_gridcellIndex_mod, only: shr_gridcellIndex

  implicit none

  public :: shr_gridBoundIndices

  private 

  logical, parameter :: ISDEBUG = .false.


  !<
  type shr_gridBoundIndices ! coordinates to select a part of the world
    integer :: startRow, endRow
    integer :: startCol, endCol
  contains
    procedure :: init_by_gridcellIndices
    procedure :: init_by_indices
    generic :: init => init_by_indices, init_by_gridcellIndices

    procedure :: getStartRow
    procedure :: getEndRow
    procedure :: getStartCol
    procedure :: getEndCol

    procedure :: countRows
    procedure :: countCols
  end type shr_gridBoundIndices

contains

  subroutine init_by_gridcellIndices(self, topleft, bottomright)
    !< initialize using 2 gridcell indices from the grid
    !< top-left(north-west) gridcell index
    !< bottom-right(south-east) gridcell index
    !<
    !< x - - <---- 1,1 (top-left)
    !< - - -
    !< - - x <---- 3,3 (bottom-right)
    !<
    class(shr_gridBoundIndices), intent(inout) :: self
    type(shr_gridcellIndex), intent(in) :: topleft, bottomright
    integer :: startlat, endlat
    integer :: startlon, endlon

    startlat = topleft % idxLat
    endlat = bottomright % idxLat
    startlon = topleft % idxLon
    endlon =  bottomright % idxLon
    call self % init(startlat, endlat, startlon, endlon)
  end subroutine init_by_gridcellIndices


  subroutine init_by_indices(self, startRow, endRow, startCol, endCol)
    !< initialize
    class(shr_gridBoundIndices), intent(inout) :: self
    integer, intent(in) :: startRow, endRow, startCol, endCol
    if (startRow > endRow) then
      write(*,*) "shr_gridBoundsIndices_mod:: init:: startRow, endRow =", startRow, endRow
      call raiseError(__FILE__, "init", &
        "'endRow' must be bigger than 'startRow' but its not the case")
    end if
    if (startCol > endCol) then
      write(*,*) "shr_gridBoundsIndices_mod:: init:: startCol, endCol =", startCol, endCol
      call raiseError(__FILE__, "init", &
          "'endCol' must be bigger than 'startCol' but its not the case")
    end if
    self % startRow = startRow
    self % endRow = endRow
    self % startCol = startCol
    self % endCol = endCol
  end subroutine init_by_indices


  integer function getStartRow(self) result (idx)
    !< start row index
    class(shr_gridBoundIndices), intent(in) :: self
    idx = self % startRow
  end function getStartRow


  integer function getEndRow(self) result (idx)
    !< end row index
    class(shr_gridBoundIndices), intent(in) :: self
    idx = self % endRow
  end function getEndRow


  integer function getStartCol(self) result (idx)
    !< start col index
    class(shr_gridBoundIndices), intent(in) :: self
    idx = self % startCol
  end function getStartCol


  integer function getEndCol(self) result (idx)
    !< end col index
    class(shr_gridBoundIndices), intent(in) :: self
    idx = self % endCol
  end function getEndCol


  integer function countRows(self) result (nrows)
    !< total number of rows
    class(shr_gridBoundIndices), intent(in) :: self
    nrows = self % endRow - self % startRow + 1
  end function countRows


  integer function countCols(self) result (ncols)
    !< total number of rows
    class(shr_gridBoundIndices), intent(in) :: self
    ncols = self % endCol - self % startCol + 1
  end function countCols

end module shr_gridBoundIndices_mod

