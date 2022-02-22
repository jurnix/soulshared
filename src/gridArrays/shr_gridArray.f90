!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridArray_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> main gridArray interface
!>
!> gridArray type is an array map into a grid. For instance, each value
!> belongs to a grid cell from the grid.
!>
!------------------------------------------------------------------------------
module shr_gridArray_mod

  use shr_precision_mod, only: sp
  use shr_error_mod, only: raiseError
  use shr_strings_mod, only: string

  use shr_array_mod, only: shr_array
  use shr_arrayDim_mod, only: shr_arrayDimContainer
  use shr_soulsharedGrid_mod, only: shr_soulsharedGrid

  implicit none

  private

  public :: shr_gridArray

  type :: shr_gridArray
    class(shr_soulsharedGrid), allocatable :: shrgrid
    class(shr_array) , allocatable :: darray
  contains
    procedure :: init

  end type shr_gridArray


contains


  subroutine init(self, name, dimensions, units, description, datatype, shr_sgrid)
    !< shr_gridArray initialization
    class(shr_gridArray), intent(inout) :: self
    type(string), intent(in) :: name
    type(shr_arrayDimContainer), intent(in) :: dimensions(:)
    type(string), intent(in) :: units
    type(string), intent(in) :: description
    type(string), intent(in) :: datatype
    clasS(shr_soulsharedGrid), intent(in) :: shr_sgrid
  end subroutine init

end module shr_gridArray_mod
