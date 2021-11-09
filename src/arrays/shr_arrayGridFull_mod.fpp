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

    ! copy
    procedure :: copy_gridFullRsp_copy_scalar_rsp
    procedure :: copy_gridFullRsp_copy_raw_rsp_1
    procedure, pass(self) :: copy_raw_rsp_1_copy_gridFullRsp
    procedure :: copy_gridFullRsp_copy_gridFullRsp

    generic, public :: assignment(=) => copy_gridFullRsp_copy_scalar_rsp, &
            copy_gridFullRsp_copy_raw_rsp_1, copy_raw_rsp_1_copy_gridFullRsp, &
            copy_gridFullRsp_copy_gridFullRsp

    ! add
    procedure :: add_gridFullRsp_add_scalar_rsp
    procedure :: add_gridFullRsp_add_raw_rsp_1
    procedure :: add_gridFullRsp_add_gridFullRsp
    generic, public :: operator(+) => add_gridFullRsp_add_scalar_rsp, &
            add_gridFullRsp_add_raw_rsp_1, add_gridFullRsp_add_gridFullRsp

    ! equal
    procedure :: equal_gridFullRsp_equal_scalar_rsp
    procedure :: equal_gridFullRsp_equal_raw_rsp_1
    procedure :: equal_gridFullRsp_equal_gridFullRsp

    generic, public :: operator(==) => equal_gridFullRsp_equal_scalar_rsp, &
            equal_gridFullRsp_equal_raw_rsp_1, equal_gridFullRsp_equal_gridFullRsp
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


  ! equal ( gridFullRsp == gridFull)
  elemental logical function equal_gridFullRsp_equal_gridFullRsp(self, other)
    !< true if self and other are the same
    class(shr_arrayGridFullRsp), intent(in) :: self
    class(shr_arrayGridFull), intent(in) :: other

    logical :: hasSameName, hasSameDims, hasSameUnits
    logical :: hasSameDescription, hasSameData

    equal_gridFullRsp_equal_gridFullRsp = .false.
    ! compare array descriptor
    hasSameName = self % getName() == other % getName()
    if (.not. hasSameName) return

    hasSameDims = all(self % getDims() == other % getDims())
    if (.not. hasSameDims) return

    hasSameUnits = self % getUnits() == other % getUnits()
    if (.not. hasSameUnits) return

    hasSameDescription = self % getDescription() == other % getDescription()
    if (.not. hasSameDescription) return

    ! compare data
    select type(data => self % data)
    type is (shr_arrayContainerRspAllocatable)
      hasSameData = (data == other % data)
    class default
      !< unexpected class found
    end select

    equal_gridFullRsp_equal_gridFullRsp = hasSameData
  end function equal_gridFullRsp_equal_gridFullRsp


  ! equal ( gridFullRsp == <type, kind> scalar)
  elemental logical function equal_gridFullRsp_equal_scalar_rsp(self, other)
    !< true if self and other are the same
    class(shr_arrayGridFullRsp), intent(in) :: self
    real(kind=sp), intent(in) :: other

    select type(data => self % data)
    type is (shr_arrayContainerRspAllocatable)
      equal_gridFullRsp_equal_scalar_rsp = (data == other)
    class default
      !< unexpected class found
    end select
  end function equal_gridFullRsp_equal_scalar_rsp


  ! equal ( gridFullRsp == <type, kind> array)
  pure logical function equal_gridFullRsp_equal_raw_rsp_1(self, other)
    !< true if self and other are the same
    class(shr_arrayGridFullRsp), intent(in) :: self
    real(kind=sp), intent(in) :: other(:)

    select type(data => self % data)
    type is (shr_arrayContainerRspAllocatable)
      equal_gridFullRsp_equal_raw_rsp_1 = all(data == other)
    class default
      !< unexpected class found
    end select
  end function equal_gridFullRsp_equal_raw_rsp_1


  ! add ( gridFullRsp + gridFullRsp )
  pure function add_gridFullRsp_add_gridFullRsp(left, right) Result(total)
    !< addition from shr_arrayRsp and shr_arrayRsp
    class(shr_arrayGridFullRsp), intent(in) :: left
    class(shr_arrayGridFull), intent(in) :: right
    class(shr_arrayGridFullRsp), allocatable :: total !< output

    select type(data => left % data)
    type is (shr_arrayContainerRspAllocatable)
      total = data + right % data
    class default
      !< unexpected class found
    end select
  end function add_gridFullRsp_add_gridFullRsp 


  ! add ( gridFullRsp + <type, kind> scalar )
  pure function add_gridFullRsp_add_scalar_rsp(left, right) Result(total)
    !< addition from shr_arrayRsp and shr_arrayRsp
    class(shr_arrayGridFullRsp), intent(in) :: left
    real(kind=sp), intent(in) :: right
    class(shr_arrayGridFullRsp), allocatable :: total !< output

    select type(data => left % data)
    type is (shr_arrayContainerRspAllocatable)
      total % data = data + right
    class default
      !< unexpected class found
    end select
  end function add_gridFullRsp_add_scalar_rsp 


  ! add ( gridFullRsp + <type, kind> array )
  pure function add_gridFullRsp_add_raw_rsp_1(left, right) Result(total)
    !< addition from shr_arrayRsp and shr_arrayRsp
    class(shr_arrayGridFullRsp), intent(in) :: left
    real(kind=sp), intent(in) :: right(:)
    class(shr_arrayGridFullRsp), allocatable :: total !< output

    select type(data => left % data)
    type is (shr_arrayContainerRspAllocatable)
      total % data = data + right
    class default
      !< unexpected class found
    end select
  end function add_gridFullRsp_add_raw_rsp_1 

end module shr_arrayGridFull_mod
