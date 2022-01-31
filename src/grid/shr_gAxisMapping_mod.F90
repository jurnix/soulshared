!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gAxisMapping_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gAxisMapping maps gridAxis into an integer matrix
!> 
!------------------------------------------------------------------------------
module shr_gAxisMapping_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_strings_mod, only: string, real2string
  use shr_gAxis_mod, only: shr_gAxis
  use shr_gGridAxesCell_mod, only: shr_gGridAxesCell
  use shr_gAxisBounds_mod, only: shr_gAxisBounds
  use shr_strings_mod, only: string, int2string
  
  implicit none

  public :: shr_gAxisMapping, shr_IGAxisMapping

  logical, parameter :: ISDEBUG = .false.


  type, abstract :: shr_IGAxisMapping
  contains
!    procedure(iface_init), deferred :: init
    procedure(iface_getIndexByCoord), deferred :: getIndexByCoord
    procedure(iface_getIndexByGridAxisCell), deferred :: getIndexByGridAxisCell
    generic :: getIndex => getIndexByCoord, getIndexByGridAxisCell
    procedure(iface_getSize), deferred :: getSize
!    procedure(iface_getGridAxis), deferred :: getGridAxis
    procedure(iface_equal), deferred :: equal
    generic :: operator(==) => equal
    procedure(iface_tostring), deferred :: toString
  end type shr_IGAxisMapping

  abstract interface
    type(string) function iface_toString(self)
      import :: shr_igAxisMapping, string
      !< string representation of shr_gAxisMapping
      class(shr_igAxisMapping), intent(in) :: self
    end function iface_toString

    function iface_getIndexByCoord(self, axisCoord) result (foundIdxs)
      import :: shr_igAxisMapping, sp
      !< it returns the index(s) which matches with axisCoord
      class(shr_igAxisMapping), intent(in) :: self
      real(kind=sp), intent(in):: axisCoord
      integer, allocatable :: foundIdxs(:)
    end function iface_getIndexByCoord

    integer function iface_getIndexByGridAxisCell(self, gAxisCell)
      import :: shr_igAxisMapping, shr_gGridAxesCell
      !< it returns the index(s) which matches with gAxisCell
      !< only 1 index can be returned
      !< -1 in case is not found
      class(shr_igAxisMapping), intent(in) :: self
      type(shr_gGridAxesCell), intent(in) :: gAxisCell
    end function iface_getIndexByGridAxisCell

    integer function iface_getSize(self)
      import :: shr_igAxisMapping
      !< axis size
      class(shr_igAxisMapping), intent(in) :: self
    end function iface_getSize

    logical function iface_equal(self, other)
      import :: shr_igAxisMapping
      !< true if self and equal have the same attributes
      class(shr_igAxisMapping), intent(in) :: self
      class(shr_igAxisMapping), intent(in) :: other
    end function iface_equal
  end interface


  type, extends(shr_IGAxisMapping) :: shr_gAxisMapping
    type(shr_gAxis), allocatable :: axis
  contains
    procedure :: init => gAxisMapping_initialize 

    procedure :: getIndexByCoord => gAxisMapping_getIndexByCoord
    procedure :: getIndexByGridAxisCell => gAxisMapping_getIndexByGridAxisCell

    procedure :: getSize => gAxisMapping_getSize
    procedure :: getgridAxis

    procedure :: equal => gAxisMapping_equal

    procedure :: toString => gAxisMapping_toString
  end type shr_gAxisMapping

contains

  subroutine gAxisMapping_initialize(self, gridAxis)
    !< gAxisMapping initialization
    class(shr_gAxisMapping), intent(inout) :: self
    type(shr_gAxis), intent(in) :: gridAxis
    allocate(self % axis, source = gridAxis)
  end subroutine gAxisMapping_initialize


  function gAxisMapping_getIndexByCoord(self, axisCoord) result (foundIdxs)
    !< it returns the index(s) which matches with axisCoord
    !< Multiple indices can be returned in case it lies in 
    !< the grid axis cells border.
    !< In case it lies outside the GridAxis it returns an error
    !<
    !< Example:
    !< getIndexByCoord(2) -> 2, 3 indices
    !< gridAxis: (0 1, 1 2, 2 3, 3 4, 4 5)
    !<  maps to:  (1    2    3    4    5)
    !<
    !< or:
    !<
    !< getIndexByCoord(2) -> 3, 4 indices
    !< gridAxis: (5 4, 4 3, 3 2, 2 1, 1 0)
    !<  maps to:  (1    2    3    4    5)
    class(shr_gAxisMapping), intent(in) :: self
    real(kind=sp), intent(in):: axisCoord
    integer, allocatable :: foundIdxs(:)
    integer :: icell
    logical :: hasAxisCoord
    type(shr_gAxisBounds), allocatable :: axisBounds
    type(string), allocatable :: axisBoundsStr, givenVal

    if (.not. self % axis % hasGridCoord(axisCoord)) then
      allocate(axisBounds)
      allocate(axisBoundsStr, givenVal)
      axisBounds = self % axis % getBounds()
      axisBoundsStr = axisBounds % toString()
      givenVal = real2String(axisCoord)
      call raiseError(__FILE__, "getIndexByCoord", &
           "Given axisCoord ("// givenVal % toString() //")"//&
           " is outside gridAxis bounds", &
           "Found: "//axisBoundsStr % toString())
    endif

    allocate(foundIdxs(0)) ! init output

    do icell = 1, self % axis % getSize()
      hasAxisCoord = self % axis % cells(icell) % isIn(axisCoord)
      if (hasAxisCoord) then
        foundIdxs = [foundIdxs, [icell]]
      endif
    enddo
  end function gAxisMapping_getIndexByCoord
  
  
  integer function gAxisMapping_getIndexByGridAxisCell(self, gAxisCell)
    !< it returns the index(s) which matches with gAxisCell
    !< only 1 index can be returned
    !< -1 in case is not found
    class(shr_gAxisMapping), intent(in) :: self
    type(shr_gGridAxesCell), intent(in) :: gAxisCell
    integer :: icell
    gAxisMapping_getIndexByGridAxisCell = -1 ! init output
    do icell = 1, self % axis % getSize()
      !< find gridcell(s) and count
      if (gAxisCell == self % axis % cells(icell)) then
        gAxisMapping_getIndexByGridAxisCell = icell
        exit !< element found, no need to iterate anymore 
      endif
    enddo
  end function gAxisMapping_getIndexByGridAxisCell


  integer function gAxisMapping_getSize(self)
    !< axis size
    class(shr_gAxisMapping), intent(in) :: self
    gAxisMapping_getSize = self % axis % getSize()
  end function gAxisMapping_getSize


  type(shr_gAxis) function getGridAxis(self)
    !< returns axis internal object
    class(shr_gAxisMapping), intent(in) :: self
    getGridAxis = self % axis
  end function getGridAxis


  logical function gAxisMapping_equal(self, other)
    !< true if self and equal have the same attributes
    class(shr_gAxisMapping), intent(in) :: self
    class(shr_igAxisMapping), intent(in) :: other
    logical :: hasSameGAxis
    select type(o => other)
    type is(shr_gAxisMapping)
      hasSameGAxis = (self % axis == o % getGridAxis())
    class default
      hasSameGAxis = .false.
    end select
    gAxisMapping_equal = (hasSameGAxis)
  end function gAxisMapping_equal


  type(string) function gAxisMapping_toString(self)
    !< string representation of shr_gAxisMapping
    class(shr_gAxisMapping), intent(in) :: self
    type(string) :: strSize
    strSize = int2string(self % getSize())
    gAxisMapping_toString = string("size= ") + strSize + ", axis=" +  self % axis % toString()
  end function gAxisMapping_toString

end module shr_gAxisMapping_mod 

