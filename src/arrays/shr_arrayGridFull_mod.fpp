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
  use shr_arrayDim_mod, only: shr_arrayDim, shr_arrayDimContainer, shr_arrayRspDim
  use shr_gridBounds_mod, only: SHR_GRIDBOUNDS_NCOORDS, SHR_GRIDBOUNDS_NORTH, &
                   SHR_GRIDBOUNDS_SOUTH, SHR_GRIDBOUNDS_WEST, SHR_GRIDBOUNDS_EAST
  use shr_strings_mod, only: string

  use shr_arrayContainer_mod, only: shr_arrayContainerRsp
  use shr_arrayContainerAllocatable_mod, only: shr_arrayContainerRspAllocatable


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
    procedure :: init => init_fullRsp
    !< copy, equal, add/sub/mul/div
    !< copy vs shr_arrayGridFullRsp, arrayRsp, raw array, scalar
    !< equal vs shr_arrayGridFullRsp, arrayRsp, raw array, scalar
    !< add/... vs shr_arrayGridFullRsp, arrayRsp, raw array, scalar
    !<

    ! copy
    procedure :: copy_gridFullRsp_copy_scalar_rsp
    procedure :: copy_gridFullRsp_copy_raw_rsp_1
    procedure, pass(self) :: copy_raw_rsp_1_copy_gridFullRsp
    procedure :: copy_gridFullRsp_copy_gridFullRsp
!    procedure :: copy_gridFullRsp_copy_raw_rsp_1
!    procedure, pass(self) :: copy_gridFullRsp_copy_raw_rsp_1
!    procedure :: copy_gridFullRsp_copy_gridFullRsp

    generic, public :: assignment(=) => copy_gridFullRsp_copy_scalar_rsp, &
            copy_gridFullRsp_copy_raw_rsp_1, copy_raw_rsp_1_copy_gridFullRsp, &
            copy_gridFullRsp_copy_gridFullRsp

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


  subroutine init_fullRsp(self, name, grid, dimensions, units, description)
    !<
    class(shr_arrayGridFullRsp), intent(inout) :: self
    type(string), intent(in) :: name
    type(shr_grid), intent(in) :: grid
    type(shr_arrayDimContainer), intent(in) :: dimensions(:)
    type(string), intent(in) :: units 
    type(string), intent(in) :: description

    ! local vars
    type(shr_arrayRspDim) :: latDim, lonDim
    type(shr_arrayDimContainer), allocatable :: varDims(:)

    real(kind=sp) :: limits(SHR_GRIDBOUNDS_NCOORDS)
    real(kind=sp) :: gridStep, latStart, latEnd
    real(kind=sp) :: lonStart, lonEnd

    gridStep = grid % getResolution()
    limits = grid % getLimits() ! n, s, e, w
    latStart = limits(SHR_GRIDBOUNDS_SOUTH)
    latEnd = limits(SHR_GRIDBOUNDS_NORTH)
    lonStart = limits(SHR_GRIDBOUNDS_WEST)
    lonEnd = limits(SHR_GRIDBOUNDS_EAST)

    call latDim % init("latitude", latstart, latEnd, gridStep)
    call lonDim % init("longitude", lonstart, lonEnd, gridStep)
    allocate(varDims(size(dimensions) + 2)) !< dimensions + grid lat + grid lon
    varDims(1) % arrayDim = latDim
    varDims(2) % arrayDim = lonDim
    varDims(3:) = dimensions(:)

    allocate(self % dims, source=varDims)

    ! taken from shr_array % init_arrayXXX(...)
    allocate(self % name)
    self % name = string(name)
    allocate(self % units)
    self % units = string(units)
    allocate(self % description)
    self % description = string(description)

    allocate( shr_arrayContainerRspAllocatable :: self % data )
    call self % data % init(self % dims)
  end subroutine init_fullRsp


  ! copy (arrayGridFull = <type, kind> scalar)
  pure subroutine copy_gridFullRsp_copy_scalar_rsp(self, other)
      !< Copy to current array container allocatable
      !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
      class(shr_arrayGridFullRsp), intent(inout) :: self
      real(kind=sp), intent(in) :: other

      select type(data => self % data)
      type is (shr_arrayContainerRspAllocatable)
        data = other
      class default
        !< unexpected class found
      end select
  end subroutine copy_gridFullRsp_copy_scalar_rsp


  ! copy (arrayGridFullRsp = <type, kind> array) 
  pure subroutine copy_gridFullRsp_copy_raw_rsp_1(self, other)
      !< Copy to current array container allocatable
      class(shr_arrayGridFullRsp), intent(inout) :: self
      real(kind=sp), intent(in) :: other(:)

      select type(data => self % data)
      type is (shr_arrayContainerRspAllocatable)
        data = other
      class default
        !< unexpected class found
      end select
  end subroutine copy_gridFullRsp_copy_raw_rsp_1


  ! copy ( <type, kind> array = gridFullRsp )
  pure subroutine copy_raw_rsp_1_copy_gridFullRsp(other, self)
      !< Copy to current array container allocatable
      real(kind=sp), allocatable, intent(inout) :: other(:)
      class(shr_arrayGridFullRsp), intent(in) :: self

      select type(data => self % data)
      type is (shr_arrayContainerRspAllocatable)
        other = data
      class default
        !< unexpected class found
      end select
  end subroutine copy_raw_rsp_1_copy_gridFullRsp


  ! copy (arrayGridFullRsp = arrayGridFullRsp) 
  pure subroutine copy_gridFullRsp_copy_gridFullRsp(self, other)
      !< Copy to current array container allocatable
      class(shr_arrayGridFullRsp), intent(inout) :: self
      class(shr_arrayGridFull), intent(in) :: other
      
      if (.not. allocated(self % name)) allocate(self % name)
      self % name = other % getName()
      allocate(self % dims, source = other % getDims())
      if (.not. allocated(self % units)) allocate(self % units)
      self % units = other % getUnits()
      if (.not. allocated(self % description)) allocate(self % description)
      self % description = other % getDescription()
      allocate(self % data, source = other % data)
  end subroutine copy_gridFullRsp_copy_gridFullRsp


end module shr_arrayGridFull_mod
