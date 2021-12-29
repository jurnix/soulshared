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

  use SHR_precision_mod, only: sp, dp!, eqReal

  implicit none

  private

  public :: shr_arrayIndices

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

end module shr_arrayIndices_mod
