!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  shr_arrayIndices_mod
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!>
!> Array(2d) indices
!>
!------------------------------------------------------------------------------

module shr_arrayIndices_mod

  use shr_error_mod, only: raiseError
  use SHR_precision_mod, only: sp, dp!, eqReal

  implicit none

  private

  public :: shr_arrayIndices, shr_arrayGridcellIndex
  public :: arrayGridcellIndex_cast

  !< describes the indices of a gridcell
  type shr_arrayGridcellIndex
    integer :: row, col
  contains
    procedure :: arrayGridcellIndex_equal
    procedure :: arrayGridcellIndex_equal_byArray
    generic :: operator(==) => arrayGridcellIndex_equal, arrayGridcellIndex_equal_byArray
    procedure :: getRow => arrayGridcellIndex_getRow
    procedure :: getCol => arrayGridcellIndex_getCol
  end type

  !< describes several indices from the same axis
  type shr_arrayAxisIndices
    integer :: start
    integer :: end
  contains
    !< getter, setter
    !< init
    procedure :: getStart => arrayAxisIndices_getStart
    procedure :: getEnd => arrayAxisIndices_getEnd
    procedure :: init => arrayAxisIndices_init
    procedure :: getSize => arrayAxisIndices_getSize
  end type shr_arrayAxisIndices


  !< describes a region of a grid
  type shr_arrayIndices
    type(shr_arrayAxisIndices), allocatable :: cols, rows
  contains
    !< init(rows, cols)
    procedure :: arrayIndices_init_by_indices
    procedure :: arrayIndices_init
    generic :: init => arrayIndices_init, arrayIndices_init_by_indices

    !< getters
    procedure :: getCols
    procedure :: getRows
  end type shr_arrayIndices

contains

  subroutine arrayAxisIndices_init(self, start, end)
    !< initialize arrayAxisIndices
    class(shr_arrayAxisIndices), intent(inout) :: self
    integer, intent(in) :: start, end
    self % start = start
    self % end = end
  end subroutine arrayAxisIndices_init


  integer function arrayAxisIndices_getStart(self)
    !< returns start array index
    class(shr_arrayAxisIndices), intent(in) :: self
    arrayAxisIndices_getStart = self % start
  end function arrayAxisIndices_getStart


  integer function arrayAxisIndices_getEnd(self)
    !< returns end array index
    class(shr_arrayAxisIndices), intent(in) :: self
    arrayAxisIndices_getEnd = self % end
  end function arrayAxisIndices_getEnd


  integer function arrayAxisIndices_getSize(self)
    !< size between start and end
    class(shr_arrayAxisIndices), intent(in) :: self
    arrayAxisIndices_getSize = (self % end - self % start) + 1
  end function arrayAxisIndices_getSize


  subroutine arrayIndices_init(self, rows, cols)
    !< initialize arrayIndices
    class(shr_arrayIndices), intent(inout) :: self
    type(shr_arrayAxisIndices) :: rows, cols
    allocate(self % rows, source = rows)
    allocate(self % cols, source = cols)
  end subroutine arrayIndices_init


  subroutine arrayIndices_init_by_indices(self, startRow, endRow, startCol, endCol)
    !< initialize arrayIndices
    class(shr_arrayIndices), intent(inout) :: self
    integer, intent(in) :: startRow, endRow
    integer, intent(in) :: startCol, endCol
    allocate(self % rows)
    call self % rows % init(startRow, endRow)
    allocate(self % cols)
    call self % cols % init(startCol, endCol)
  end subroutine arrayIndices_init_by_indices


  type(shr_arrayAxisIndices) function getCols(self)
    !< returns current cols axis
    class(shr_arrayIndices), intent(in) :: self
    getCols = self % cols
  end function getCols


  type(shr_arrayAxisIndices) function getRows(self)
    !< returns current cols axis
    class(shr_arrayIndices), intent(in) :: self
    getRows = self % rows
  end function getRows


  elemental logical function arrayGridcellIndex_equal(self, other)
    !< true if 'self' and 'other' have the same attributes
    class(shr_arrayGridcellIndex), intent(in) :: self
    class(shr_arrayGridcellIndex), intent(in) :: other
    logical :: hasSameRow, hasSameCol
    hasSameRow = (self % row == other % row)
    hasSameCol = (self % col == other % col)
    arrayGridcellIndex_equal = (hasSameRow .and. hasSameCol)
  end function arrayGridcellIndex_equal


  logical function arrayGridcellIndex_equal_byArray(self, other)
    !< true if 'self' and 'other' have the same attributes
    class(shr_arrayGridcellIndex), intent(in) :: self
    integer, intent(in) :: other(2) !< row, col
    integer, parameter :: ROW_IDX = 1
    integer, parameter :: COL_IDX = 2
    logical :: hasSameRow, hasSameCol
    hasSameRow = (self % row == other(ROW_IDX))
    hasSameCol = (self % col == other(COL_IDX))
    arrayGridcellIndex_equal_byArray = (hasSameRow .and. hasSameCol)
  end function arrayGridcellIndex_equal_byArray


  elemental impure function arrayGridcellIndex_cast(obj) result (newArrayGCIndex)
    !< cast from unlimited polymorphic into shr_arrayGridcellIndex
    class(*), intent(in) :: obj
    type(shr_arrayGridcellIndex) :: newArrayGCIndex
    select type(o => obj)
    type is (shr_arrayGridcellIndex)
      newArrayGCIndex = o
    class default
      call raiseError(__FILE__, "arrayGridcellIndex_cast", &
          "Unexpected type found instead of 'shr_arrayGridcellIndex'")
    end select
  end function arrayGridcellIndex_cast


  elemental pure function arrayGridcellIndex_getCol(self) result (col)
    !< returns 'col' attribute
    class(shr_arrayGridcellIndex), intent(in) :: self
    integer :: col !< output
    col = self % col
  end function arrayGridcellIndex_getCol


  elemental pure function arrayGridcellIndex_getRow(self) result (row)
    !< returns 'row' attribute
    class(shr_arrayGridcellIndex), intent(in) :: self
    integer :: row !< output
    row = self % row
  end function arrayGridcellIndex_getRow

end module shr_arrayIndices_mod
