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
!> int: int + int
!> rsp: rsp + rsp
!>      rsp + int
!> dsp: dsp + dsp
!>      dsp + rsp
!>      dsp + int
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


  type, extends(shr_array), abstract :: shr_arrayGrid
    type(shr_grid), allocatable :: grid
  contains
    procedure(iface_init), deferred :: init !< initialization
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
  end interface

contains

end module shr_arrayGrid_mod
