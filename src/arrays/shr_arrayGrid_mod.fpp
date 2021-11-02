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
  use shr_array_mod, only: arrayAbs
  use shr_arrayDim_mod, only: shr_arrayDim, shr_arrayDimContainer

  implicit none

  private

  public :: shr_arrayGrid


  type, extends(arrayAbs), abstract :: shr_arrayGrid
    type(shr_grid), allocatable :: grid
  contains
    procedure :: init !< initialization
    procedure(iface_init_custom), deferred :: init_custom !< custom initialization for any subclass
  end type shr_arrayGrid


  abstract interface
    subroutine iface_init_custom(self)
      !<
      import :: shr_arrayGrid
      class(shr_arrayGrid), intent(inout) :: self
    end subroutine iface_init_custom
  end interface



contains

  subroutine init(self, name, grid, dimensions, units, description)
    !< initialize shr_arrayGrid
    !<
    class(shr_arrayGrid), intent(inout) :: self
    character(*), intent(in) :: name !< array name
    type(shr_grid), intent(in) :: grid !< grid
    type(shr_arrayDimContainer), intent(in) :: dimensions(:) !< other dimensions (but no spatial)
    character(*), intent(in) :: units !< dimensions
    character(*), intent(in) :: description !< array description

  end subroutine init

end module shr_arrayGrid_mod
