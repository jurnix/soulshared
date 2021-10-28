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

  use SHR_precision_mod, only: sp, dp!, eqReal
!  use SHR_strings_mod, only: string
  use SHR_arrayDim_mod, only: shr_arrayDim, shr_arrayDimContainer

  implicit none

  private

  public :: shr_arrayGrid


  type shr_arrayGrid
  contains
  end type shr_arrayGrid


contains

end module shr_arrayGrid_mod
