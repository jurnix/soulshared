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
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

  implicit none

  public :: shr_gridBoundIndices

  private 

  logical, parameter :: ISDEBUG = .false.


  !<
  type shr_gridBoundIndices ! coordinates to select a part of the world
    integer :: startRow, endRow
    integer :: startCol, endCol
  contains
    procedure :: init

    procedure :: getStartRow
    procedure :: getEndRow
    procedure :: getStartCol
    procedure :: getEndCol

    procedure :: countRows
    procedure :: countCols
  end type shr_gridBoundIndices

contains

  subroutine init(self, startRow, endRow, startCol, endCol)
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
  end subroutine init


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

