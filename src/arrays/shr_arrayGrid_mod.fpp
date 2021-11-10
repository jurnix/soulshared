!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  shr_arrayGrid_mod
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!> ArrayGrid class
!>
!> array class (real sp, real dp, int)
!>
!> Allowed operations between types(and kind)
!>
!> operations: add, sub, div, mul
!> type: real, int
!> kind: single, double
!> 
!------------------------------------------------------------------------------

#:include "../common.fpp"


module shr_arrayGrid_mod

  use shr_precision_mod, only: sp, dp!, eqReal
  use shr_grid_mod, only: shr_grid
  use shr_strings_mod, only: string
  use shr_array_mod, only: shr_array
  use shr_arrayDim_mod, only: shr_arrayDim, shr_arrayDimContainer

  implicit none

  private

  public :: shr_arrayGrid


  type, abstract :: shr_arrayGrid
    class(shr_array), allocatable :: array
    type(shr_grid), allocatable :: grid
  contains
    procedure(iface_init), deferred :: init !< initialization

    procedure(iface_copy_arrayGrid_copy_arrayGrid), deferred :: copy_arrayGrid_copy_arrayGrid
    generic, public :: assignment(=) => copy_arrayGrid_copy_arrayGrid

    procedure(iface_equal_arrayGrid_equal_arrayGrid), deferred :: equal_arrayGrid_equal_arrayGrid
    generic, public :: operator(==) => equal_arrayGrid_equal_arrayGrid

    procedure(iface_op_arrayGrid_op_arrayGrid), deferred :: op_arrayGrid_add_arrayGrid
    generic, public :: operator(+) => op_arrayGrid_add_arrayGrid
  end type shr_arrayGrid


  abstract interface
    subroutine iface_init(self, name, grid, dimensions, units, description)
      !<
      import :: shr_arrayGrid, shr_grid, shr_arrayDimContainer, string
      class(shr_arrayGrid), intent(inout) :: self
      type(string), intent(in) :: name
      type(shr_grid), intent(in) :: grid
      type(shr_arrayDimContainer), intent(in) :: dimensions(:)
      type(string), intent(in) :: units 
      type(string), intent(in) :: description
    end subroutine iface_init

    function iface_getArray(self) result (newArray)
      !< return shr_arrayGrid as shr_array
      import :: shr_arrayGrid, shr_array
      class(shr_arrayGrid), intent(in) :: self
      class(shr_array), allocatable :: newArray !< output
    end function iface_getArray


    pure subroutine iface_copy_arrayGrid_copy_arrayGrid(self,other)
      !< copy shr_array = shr_array
      import :: shr_arrayGrid
      class(shr_arrayGrid), intent(inout) :: self
      class(shr_arrayGrid), intent(in) :: other
    end subroutine iface_copy_arrayGrid_copy_arrayGrid

    elemental logical function iface_equal_arrayGrid_equal_arrayGrid(self, other)
      !< true if self and other are the same
      import :: shr_arrayGrid
      class(shr_arrayGrid), intent(in) :: self
      class(shr_arrayGrid), intent(in) :: other
    end function  iface_equal_arrayGrid_equal_arrayGrid

    pure function iface_op_arrayGrid_op_arrayGrid(left, right) Result(total)
      !< addition from shr_arrayXXX and shr_array
      import :: shr_arrayGrid
      class(shr_arrayGrid), intent(in) :: left
      class(shr_arrayGrid), intent(in) :: right
      class(shr_arrayGrid), allocatable :: total !< output
    end function iface_op_arrayGrid_op_arrayGrid
  end interface

contains

end module shr_arrayGrid_mod
