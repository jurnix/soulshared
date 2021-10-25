!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  array_mod
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!> Common Array subroutines
!>
!> array class (real sp, real dp, int)
!> 
!------------------------------------------------------------------------------

#:include "../common.fpp"


module SHR_array_mod

  use SHR_precision_mod, only: sp, dp!, eqReal
!  use SHR_error_mod, only: raiseError 
  use SHR_strings_mod, only: string
  use SHR_arrayDim_mod, only: shr_arrayDim

  implicit none

  private

  public :: arrayAbs


  type, abstract :: arrayAbs !< interface to array
    type(string), allocatable :: name
    class(shr_arrayDim), allocatable :: dims(:) 
    type(string), allocatable :: units
    type(string), allocatable :: description
  contains
    !< add (arrayAbs, scalar, raw (matching dimensions) array)
    !< and sub, div, mul

    !< assignament (=)
    !< arrayAbs (same type), scalar or raw array (matching dimensions)
    !<
    !< equal(arrayAbs (matching dimensions), raw array (matching dimesion's),
    !<       
  end type arrayAbs

  ! type specific array
  type, extends(arrayAbs) :: arrayRsp !< apply each type and kind
  end type arrayRsp


contains


end module SHR_array_mod
