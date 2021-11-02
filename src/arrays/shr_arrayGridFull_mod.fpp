!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  shr_arrayGridFull_mod
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


module shr_arrayGridFull_mod

  use shr_precision_mod, only: sp, dp!, eqReal
  use shr_grid_mod, only: shr_grid
  use shr_arrayGrid_mod, only: shr_arrayGrid
  use shr_arrayDim_mod, only: shr_arrayDim, shr_arrayDimContainer

  implicit none

  private

  public :: shr_arrayGridFull, shr_arrayGridFullRsp


  type, extends(shr_arrayGrid), abstract :: shr_arrayGridFull
  contains
!    procedure :: getArray !< get shr_array
!    procedure :: toArrayGridSlim !< transform into arrayGridSlim
!    procedure :: splitToSquaredArrays !< subset of shr_arrayGrid(s) into squared 
  end type shr_arrayGridFull


  ! type bindings
  type, extends(shr_arrayGridFull) :: shr_arrayGridFullRsp
  contains
    procedure :: init_custom
    !< init
    !< copy, equal, add/sub/mul/div
    !< copy vs shr_arrayGridFullRsp, arrayRsp, raw array, scalar
    !< equal vs shr_arrayGridFullRsp, arrayRsp, raw array, scalar
    !< add/... vs shr_arrayGridFullRsp, arrayRsp, raw array, scalar
    !<
    !< kind, types vs :
    !<
    !< int: int vs int
    !<
    !< rsp: rsp vs rsp
    !<      rsp vs int
    !<
    !< rdp: rdp vs rdp
    !<      rdp vs rsp
    !<      rdp vs int
  end type shr_arrayGridFullRsp


contains


  subroutine init_custom(self)
    !< shr_arrayGridFullRsp initialization
    class(shr_arrayGridFullRsp), intent(inout) :: self
  end subroutine init_custom

end module shr_arrayGridFull_mod
