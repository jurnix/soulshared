!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  shr_arrayGridFull_mod
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!> ArrayGridFull class
!> 
!> Its interfaces ranged from 2 dimensions (grid) to MAXRANK
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

  use shr_error_mod, only: raiseError
  use shr_precision_mod, only: sp, dp!, eqReal
  use shr_grid_mod, only: shr_grid
  use shr_array_mod, only: shr_array, shr_arrayRsp
  use shr_arrayGrid_mod, only: shr_arrayGrid
  use shr_arrayDim_mod, only: shr_arrayDim, shr_arrayDimContainer, shr_arrayRspDim
  use shr_gridBounds_mod, only: SHR_GRIDBOUNDS_NCOORDS, SHR_GRIDBOUNDS_NORTH, &
                   SHR_GRIDBOUNDS_SOUTH, SHR_GRIDBOUNDS_WEST, SHR_GRIDBOUNDS_EAST
  use shr_strings_mod, only: string

!  use shr_arrayContainer_mod, only: shr_arrayContainerRsp
!  use shr_arrayContainerAllocatable_mod, only: shr_arrayContainerRspAllocatable


  implicit none

  private

  public :: shr_arrayGridFull, shr_arrayGridFullRsp


  type, extends(shr_arrayGrid), abstract :: shr_arrayGridFull
  contains
!    procedure :: toArrayGridSlim !< transform into arrayGridSlim
!    procedure :: splitToSquaredArrays !< subset of shr_arrayGrid(s) into squared 
  end type shr_arrayGridFull


  ! type bindings
  type, extends(shr_arrayGridFull) :: shr_arrayGridFullRsp
  contains
    procedure :: init_arrayGrid => init_fullRsp

    ! other
    procedure :: getArray !< get shr_array

    ! copy
    procedure :: copy_gridFullRsp_copy_scalar_rsp
    procedure :: copy_gridFullRsp_copy_raw_rsp_2
    procedure, pass(self) :: copy_raw_rsp_2_copy_gridFullRsp
    procedure :: copy_arrayGrid_copy_arrayGrid => copy_arrayGridFullRsp_copy_arrayGrid 

    generic, public :: assignment(=) => copy_gridFullRsp_copy_scalar_rsp, &
            copy_gridFullRsp_copy_raw_rsp_2, copy_raw_rsp_2_copy_gridFullRsp!, &

    ! add
    procedure :: add_gridFullRsp_add_scalar_rsp
    procedure :: add_gridFullRsp_add_raw_rsp_2
    procedure :: op_arrayGrid_add_arrayGrid => op_arrayGridFullRsp_add_arrayGrid
    generic, public :: operator(+) => add_gridFullRsp_add_scalar_rsp, &
            add_gridFullRsp_add_raw_rsp_2

    ! equal
    procedure :: equal_gridFullRsp_equal_scalar_rsp
    procedure :: equal_gridFullRsp_equal_raw_rsp_2
    procedure :: equal_arrayGrid_equal_arrayGrid => equal_arrayGridFullRsp_equal_arrayGrid

    generic, public :: operator(==) => equal_gridFullRsp_equal_scalar_rsp, &
            equal_gridFullRsp_equal_raw_rsp_2
  end type shr_arrayGridFullRsp


contains


  subroutine init_fullRsp(self, name, grid, dimensions, units, description)
    !<
    class(shr_arrayGridFullRsp), intent(inout) :: self
    type(string), intent(in) :: name
    type(shr_grid), intent(in) :: grid
    type(shr_arrayDimContainer), intent(in), optional :: dimensions(:)
    type(string), intent(in) :: units 
    type(string), intent(in) :: description

    ! local vars
    type(shr_arrayRspDim) :: latDim, lonDim
    type(shr_arrayDimContainer), allocatable :: varDims(:)

    type(string) :: sname, sunits, sdescription

    real(kind=sp) :: limits(SHR_GRIDBOUNDS_NCOORDS)
    real(kind=sp) :: gridStep, latStart, latEnd
    real(kind=sp) :: lonStart, lonEnd

    integer :: inDimensionsSize

!    write(*,*) "shr_arrayGridFull_mod:: init_fullRsp:: initializing (", sname % toString(), " )"
    inDimensionsSize = 0
    if (present(dimensions)) inDimensionsSize = size(dimensions)
    self % grid = grid

    gridStep = grid % getResolution()
    limits = grid % getLimits() ! n, s, e, w
    latStart = limits(SHR_GRIDBOUNDS_SOUTH)
    latEnd = limits(SHR_GRIDBOUNDS_NORTH)
    lonStart = limits(SHR_GRIDBOUNDS_WEST)
    lonEnd = limits(SHR_GRIDBOUNDS_EAST)

    call latDim % init("latitude", latstart, latEnd, gridStep)
    call lonDim % init("longitude", lonstart, lonEnd, gridStep)
    allocate(varDims(inDimensionsSize + 2)) !< dimensions + grid lat + grid lon
    allocate(varDims(1) % arrayDim, source = latDim)
    allocate(varDims(2) % arrayDim, source = lonDim)
    if (present(dimensions)) varDims(3:) = dimensions(:)

    sname = string(name)
    sunits = string(units)
    sdescription = string(description)

!    write(*,*) "shr_arrayGridFull_mod:: init_fullRsp:: var name =", sname % toString()
!    write(*,*) "shr_arrayGridFull_mod:: init_fullRsp:: varDims =", size(varDims)
!    write(*,*) "shr_arrayGridFull_mod:: init_fullRsp:: initializing array..."

    allocate(shr_arrayRsp :: self % array)
    call self % array % init(sname, varDims, sunits, sdescription)
!    write(*,*) "shr_arrayGridFull_mod:: init_fullRsp:: initializing array... DONE"
  end subroutine init_fullRsp


  function getArray(self) result (newArray)
    !< return shr_arrayGrid as shr_array
    class(shr_arrayGridFullRsp), intent(in) :: self
    class(shr_array), allocatable :: newArray !< output
    if (allocated(newArray)) deallocate(newArray)
    allocate(newArray, source = self % array)
  end function getArray


  ! copy (arrayGridFull = <type, kind> scalar)
  subroutine copy_gridFullRsp_copy_scalar_rsp(self, other)
      !< Copy to current array container allocatable
      !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
      class(shr_arrayGridFullRsp), intent(inout) :: self
      real(kind=sp), intent(in) :: other

      select type(array => self % array)
      type is (shr_arrayRsp)
        array = other
      class default
        !< unexpected class found
      end select
  end subroutine copy_gridFullRsp_copy_scalar_rsp


  ! copy (arrayGridFullRsp = <type, kind> array) 
  subroutine copy_gridFullRsp_copy_raw_rsp_2(self, other)
      !< Copy to current array container allocatable
      class(shr_arrayGridFullRsp), intent(inout) :: self
      real(kind=sp), intent(in) :: other(:,:)

      select type(array => self % array)
      type is (shr_arrayRsp)
        array = other
      class default
        !< unexpected class found
      end select
  end subroutine copy_gridFullRsp_copy_raw_rsp_2


  ! copy ( <type, kind> array = gridFullRsp )
  subroutine copy_raw_rsp_2_copy_gridFullRsp(other, self)
      !< Copy to current array container allocatable
      real(kind=sp), allocatable, intent(inout) :: other(:,:)
      class(shr_arrayGridFullRsp), intent(in) :: self

      select type(array => self % array)
      type is (shr_arrayRsp)
        other = array
      class default
        !< unexpected class found
      end select
  end subroutine copy_raw_rsp_2_copy_gridFullRsp


  ! copy (arrayGridFullRsp = arrayGrid) 
  subroutine copy_arrayGridFullRsp_copy_arrayGrid(self, other)
      !< Copy to current array container allocatable
      class(shr_arrayGridFullRsp), intent(inout) :: self
      class(shr_arrayGrid), intent(in) :: other
      if (allocated(self % array)) deallocate(self % array)
      if (allocated(self % grid)) deallocate(self % grid)
      allocate(self % array, source = other % array)
      allocate(self % grid, source = other % grid)
  end subroutine copy_arrayGridFullRsp_copy_arrayGrid


  ! equal (gridFullRsp == gridFull)
   logical function equal_arrayGridFullRsp_equal_arrayGrid(self, other)
    !< true if self and other are the same
    class(shr_arrayGridFullRsp), intent(in) :: self
    class(shr_arrayGrid), intent(in) :: other

    logical :: hasSameName, hasSameDims, hasSameUnits
    logical :: hasSameDescription, hasSameData

    equal_arrayGridFullRsp_equal_arrayGrid = .false.
    ! compare array descriptor
    hasSameName = self % array % getName() == other % array % getName()
    if (.not. hasSameName) return

    hasSameDims = all(self % array % getDims() == other % array % getDims())
    if (.not. hasSameDims) return

    hasSameUnits = self % array % getUnits() == other % array % getUnits()
    if (.not. hasSameUnits) return

    hasSameDescription = self % array % getDescription() == other % array % getDescription()
    if (.not. hasSameDescription) return

    ! compare data
    hasSameData = (self % array == other % array)

    equal_arrayGridFullRsp_equal_arrayGrid = hasSameData
  end function equal_arrayGridFullRsp_equal_arrayGrid


  ! equal ( gridFullRsp == <type, kind> scalar)
  logical function equal_gridFullRsp_equal_scalar_rsp(self, other)
    !< true if self and other are the same
    class(shr_arrayGridFullRsp), intent(in) :: self
    real(kind=sp), intent(in) :: other

    select type(array => self % array)
    type is (shr_arrayRsp)
      equal_gridFullRsp_equal_scalar_rsp = (array == other)
    class default
      !< unexpected class found
      equal_gridFullRsp_equal_scalar_rsp = .false.
    end select
  end function equal_gridFullRsp_equal_scalar_rsp


  ! equal ( gridFullRsp == <type, kind> array)
  logical function equal_gridFullRsp_equal_raw_rsp_2(self, other)
    !< true if self and other are the same
    class(shr_arrayGridFullRsp), intent(in) :: self
    real(kind=sp), intent(in) :: other(:,:)

    select type(array => self % array)
    type is (shr_arrayRsp)
     equal_gridFullRsp_equal_raw_rsp_2 = array == other
    class default
      !< unexpected class found
      equal_gridFullRsp_equal_raw_rsp_2 = .false.
    end select
  end function equal_gridFullRsp_equal_raw_rsp_2


  ! add ( arrayGridFullRsp + arrayGrid )
  function op_arrayGridFullRsp_add_arrayGrid(left, right) Result(total)
    !< addition from shr_arrayRsp and shr_arrayRsp
    class(shr_arrayGridFullRsp), intent(in) :: left
    class(shr_arrayGrid), intent(in) :: right
    class(shr_arrayGrid), allocatable :: total !< output
    class(shr_arrayRsp), allocatable :: tmpArr
    allocate(total, source = left)
    allocate(tmpArr)

    select type (r => right)
    type is (shr_arrayGridFullRsp)
      tmpArr = left % array + r % array
    class default
      !< unexpected
    end select
!    tmpArr = left % array + right % array

    select type (t => total)
    type is (shr_arrayGridFullRsp)
      t % array = tmpArr
    class default
      !< unexpected
    end select
  end function op_arrayGridFullRsp_add_arrayGrid


  ! add ( gridFullRsp + <type, kind> scalar )
  ! 
  function add_gridFullRsp_add_scalar_rsp(left, right) Result(total)
    !< addition from shr_arrayRsp and shr_arrayRsp
    class(shr_arrayGridFullRsp), intent(in) :: left
    real(kind=sp), intent(in) :: right
    class(shr_arrayGridFullRsp), allocatable :: total !< output
    real(kind=sp), allocatable :: rawdata(:,:)

    allocate(total, source = left)

    select type(array => left % array)
    type is (shr_arrayRsp)
!      write(*,*) "shr_arrayGridFull_mod:: add_gridFullRsp_add_scalar_rsp:: adding (+)... "
      rawdata = array
!      write(*,*) "shr_arrayGridFull_mod:: add_gridFullRsp_add_scalar_rsp:: left % array =", rawdata 
!      total % array = array + right
      total = rawdata + right
!      write(*,*) "shr_arrayGridFull_mod:: add_gridFullRsp_add_scalar_rsp:: adding (+)... DONE"
    class default
      !< unexpected class found
      call raiseError(__FILE__, "add_gridFullRsp_add_scalar_rsp", &
              "unexpected class found")
    end select
!    write(*,*) "shr_arrayGridFull_mod:: add_gridFullRsp_add_scalar_rsp:: starting... DONE"
  end function add_gridFullRsp_add_scalar_rsp 


  ! add ( gridFullRsp + <type, kind> array )
  function add_gridFullRsp_add_raw_rsp_2(left, right) Result(total)
    !< addition from shr_arrayRsp and shr_arrayRsp
    class(shr_arrayGridFullRsp), intent(in) :: left
    real(kind=sp), intent(in) :: right(:,:)
    class(shr_arrayGridFullRsp), allocatable :: total !< output

    if (.not. allocated(total)) allocate(total, source = left)

    select type(array => left % array)
    type is (shr_arrayRsp)
      total % array = array + right
    class default
      !< unexpected class found
      call raiseError(__FILE__, "add_gridFullRsp_add_raw_rsp_2", &
              "unexpected class found")
    end select
  end function add_gridFullRsp_add_raw_rsp_2

end module shr_arrayGridFull_mod
