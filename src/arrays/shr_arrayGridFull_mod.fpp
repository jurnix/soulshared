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
  use shr_strings_mod, only: string

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
    procedure :: init
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

    !procedure :: init_array_rsp
    !generic :: init => init_array_rsp

    ! copy
    procedure :: copy_scalar_rsp
!    procedure :: copy_array_to_raw_rsp_1
!    procedure, pass(self) :: copy_raw_rsp_1_to_array
!    procedure :: copy_array

    generic, public :: assignment(=) => copy_scalar_rsp!, copy_array, &
!            copy_array_to_raw_rsp_1, copy_raw_rsp_1_to_array

    ! add
!    procedure :: add_array_scalar_rsp
!    procedure :: add_array_raw_rsp_1
!    procedure :: add_array_array
!    generic, public :: operator(+) => add_array_scalar_rsp, add_array_array, &
!            add_array_raw_rsp_1

    ! equal
!    procedure :: equal_scalar_rsp
!    procedure :: equal_array_raw_rsp_1
!    procedure :: equal_array
!    generic, public :: operator(==) => equal_scalar_rsp, equal_array, &
!            equal_array_raw_rsp_1
  end type shr_arrayGridFullRsp


contains


  subroutine init(self, name, grid, dimensions, units, description)
    !<
    class(shr_arrayGridFullRsp), intent(inout) :: self
    type(string), intent(in) :: name
    type(shr_grid), intent(in) :: grid
    type(shr_arrayDimContainer), intent(in) :: dimensions(:)
    type(string), intent(in) :: units 
    type(string), intent(in) :: description
  end subroutine init


  ! copy
  pure subroutine copy_scalar_rsp(self, other)
      !< Copy to current array container allocatable
      !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
      class(shr_arrayGridFullRsp), intent(inout) :: self
      real(kind=sp), intent(in) :: other

      ! 
!      self % data = other
  end subroutine copy_scalar_rsp

end module shr_arrayGridFull_mod
