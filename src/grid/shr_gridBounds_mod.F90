!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridBounds_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> grid bounds defines a squared scope in a grid
!------------------------------------------------------------------------------

module shr_gridBounds_mod

  use SHR_precision_mod, only: sp
  use SHR_error_mod, only: raiseError
  use shr_coord_mod, only: shr_coord
  ! todo: use shr_gGridAxesBounds
  !use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

  implicit none

  public :: shr_gridBounds
  public :: SHR_GRIDBOUNDS_NCOORDS, SHR_GRIDBOUNDS_NORTH, &
             SHR_GRIDBOUNDS_SOUTH, SHR_GRIDBOUNDS_EAST, SHR_GRIDBOUNDS_WEST

  private 

  logical, parameter :: ISDEBUG = .false.


  !< constant grid bounds values
  integer, parameter :: SHR_GRIDBOUNDS_NORTH = 1
  integer, parameter :: SHR_GRIDBOUNDS_SOUTH = 2
  integer, parameter :: SHR_GRIDBOUNDS_EAST = 3
  integer, parameter :: SHR_GRIDBOUNDS_WEST = 4
  integer, parameter :: SHR_GRIDBOUNDS_NCOORDS = 4

  type shr_gridBounds ! coordinates to select a part of the world
    real(kind=sp) :: north, south, east, west
    !type(shr_gGridAxesBounds), allocatable :: latAxis, lonAxis
  contains
    procedure :: initFromArray
    procedure :: initGridBounds
    generic :: init => initGridBounds, initFromArray
    procedure :: fitsGridBounds
    procedure :: fitsCoord
    generic :: fits => fitsGridBounds, fitsCoord !< a given gridBounds fits in the current one
    procedure :: toArray
    procedure :: toString
    procedure :: isOverlapped

    procedure :: getNorth, getSouth
    procedure :: getEast, getWest

    ! operator overloading procedures
    procedure, pass(w0), private :: gridBounds_eq
    procedure, pass(w0), private :: gridBounds_eq_array
    ! 
    generic :: operator(==)  => gridBounds_eq, gridBounds_eq_array

    ! combine
    procedure :: gridBounds_combine
    generic :: operator(+) => gridBounds_combine

    procedure :: getCoordinateNorthEast
    procedure :: getCoordinateNorthWest
    procedure :: getCoordinateSouthEast
    procedure :: getCoordinateSouthWest
  end type shr_gridBounds

contains



  subroutine initFromArray(self, coords)
    !< initialize grid bounds from an array(north, south, east, west)
    class(shr_gridBounds), intent(inout) :: self
    real(kind=sp), intent(in) :: coords(SHR_GRIDBOUNDS_NCOORDS)

    real(kind=sp) :: north, south, east, west

    north = coords(SHR_GRIDBOUNDS_NORTH)
    south = coords(SHR_GRIDBOUNDS_SOUTH)
    east = coords(SHR_GRIDBOUNDS_EAST)
    west = coords(SHR_GRIDBOUNDS_WEST)

    call self % initGridBounds(north, south, east, west)
  end subroutine initFromArray


  subroutine initGridBounds(self, north, south, east, west)
    !< initialize grid bounds for a coordinate
    class(shr_gridBounds), intent(inout) :: self
    real(kind=sp), intent(in) :: north, south, east, west

    character(len=100) :: tmp, tmp1

    if (north < south) then
      write(tmp,*)  north
      write(tmp1,*)  south
      call raiseError(__FILE__, "initGridBounds", &
              "north latitude must be bigger than south", &
              "North: "//tmp, &
              "South: "//tmp1)
    endif

    if (east < west) then
      write(tmp,*) east 
      write(tmp1,*) west
      call raiseError(__FILE__, "initGridBounds", &
              "East(180) longitude must be bigger than the west(-180)", &
              "East: "//tmp, &
              "West: "//tmp1)
    endif

    self % north = north
    self % south = south
    self % east = east
    self % west = west
  end subroutine initGridBounds


  pure elemental logical function fitsGridBounds(self, smallGridBounds)
    !< the given grid bounds fits in the self one
    class(shr_gridBounds), intent(in) :: self, smallGridBounds

    fitsGridBounds = .false.
    if (self % north >= smallGridBounds % north .and. &
        self % south <= smallGridBounds % south .and. &
        self % east >= smallGridBounds % east .and. &
        self % west <= smallGridBounds % west ) then

        fitsGridBounds = .true.
    endif
  end function fitsGridBounds


  pure elemental logical function fitsCoord(self, nCoord)
    !< the given grid bounds fits in the self one
    class(shr_gridBounds), intent(in) :: self
    type(shr_coord), intent(in) :: nCoord

    fitsCoord = .false.
    if (self % north >= nCoord % lat .and. &
        self % south <= nCoord % lat .and. &
        self % east >= nCoord % lon .and. &
        self % west <= nCoord % lon ) then

        fitsCoord = .true.
    endif
  end function fitsCoord


  pure logical function gridBounds_eq(w0,w1) result (res)
    !< compare grid bounds
    class(shr_gridBounds), intent(in) :: w0, w1

    res = .false.
    if (w0 % north == w1 % north .and. &
        w0 % south == w1 % south .and. &
        w0 % east == w1 % east .and. &
        w0 % west == w1 % west) then

      res = .true.
    endif
  end function gridBounds_eq


  logical function gridBounds_eq_array(w0,w1) result (res)
    !< compare grid bounds
    class(shr_gridBounds), intent(in) :: w0
    real(kind=sp), intent(in) :: w1(SHR_GRIDBOUNDS_NCOORDS)

    res = .false.
    if (w0 % north == w1(SHR_GRIDBOUNDS_NORTH) .and. &
        w0 % south == w1(SHR_GRIDBOUNDS_SOUTH) .and. &
        w0 % east == w1(SHR_GRIDBOUNDS_EAST) .and. &
        w0 % west == w1(SHR_GRIDBOUNDS_WEST)) then

      res = .true.
    endif
  end function gridBounds_eq_array


  function toString(self) result (str)
    !< string representation of grid bounds
    class(shr_gridBounds), intent(in) :: self
    character(len=:), allocatable :: str

    character(len=10) :: n, s, e, w
    write(n,'(F10.4)') self % north 
    write(s,'(F10.4)') self % south
    write(e,'(F10.4)') self % east 
    write(w,'(F10.4)') self % west 
    str = trim(adjustl(n))//", "//trim(adjustl(s))//", "//trim(adjustl(e))//", "//trim(adjustl(w))
  end function toString


  function toArray(self) result (vals)
    !< convert grid bounds into an array(4) 
    !< each position corresponds to north, south, east, west
    class(shr_gridBounds), intent(in) :: self
    real(kind=sp) :: vals(SHR_GRIDBOUNDS_NCOORDS)

    vals(SHR_GRIDBOUNDS_NORTH) = self % north
    vals(SHR_GRIDBOUNDS_SOUTH) = self % south
    vals(SHR_GRIDBOUNDS_EAST) = self % east
    vals(SHR_GRIDBOUNDS_WEST) = self % west
  end function toArray


  type(shr_gridBounds) function gridBounds_combine(self, other) result (newBounds)
    !< combines 'self' and 'other' into 'newBounds'
    !< The combination merges the biggest and smallest bounds
    class(shr_gridBounds), intent(in) :: self
    type(shr_gridBounds), intent(in) :: other

    real(kind=sp) :: newNorth, newSouth, newEast, newWest

    newNorth = max(self % north, other % north)
    newSouth = min(self % south, other % south)
    newEast = max(self % east, other % east)
    newWest = min(self % west, other % west)
    call newBounds % init(newNorth, newSouth, newEast, newWest)
  end function gridBounds_combine


  logical function isOverlapped(self, other)
    !< true if self and other bounds (at least) partialy overlap
    !< self (2,-1,3,4)
    !< other(4,1,8,3)
    class(shr_gridBounds), intent(in) :: self
    type(shr_gridBounds), intent(in) :: other
    logical :: isLatOverlapped, isLonOverlapped
    isOverlapped = .false.
    isLatOverlapped = .false.
    !< lats overlapped?
    !< todo: move to shr_gGridAxesBounds % isOverlapped
    if (self % north > other % south .or. &
        self % south > other % north) then
      isLatOverlapped = .true.
    end if
    !< lons overlapped?
    if (self % east > other % west .or. &
        self % west > other % east) then
      isLonOverlapped = .true.
    end if
    isOverlapped = (isLatOverlapped .and. isLonOverlapped)

  end function isOverlapped


  elemental real(kind=sp) function getNorth(self)
    !< returns north
    class(shr_gridBounds), intent(in) :: self
    getNorth = self % north
  end function getNorth


  elemental real(kind=sp) function getSouth(self)
    !< returns south
    class(shr_gridBounds), intent(in) :: self
    getSouth = self % south
  end function getSouth


  elemental real(kind=sp) function getEast(self)
    !< returns east
    class(shr_gridBounds), intent(in) :: self
    getEast = self % east
  end function getEast


  elemental real(kind=sp) function getWest(self)
    !< returns west
    class(shr_gridBounds), intent(in) :: self
    getWest = self % west
  end function getWest


  type(shr_coord) function getCoordinateNorthEast(self) result (c)
    !< returns coordinate from north east boundary
    class(shr_gridBounds), intent(in) :: self
    c = shr_coord(self % getNorth(), self % getEast())
  end function getCoordinateNorthEast


  type(shr_coord) function getCoordinateNorthWest(self) result (c)
    !< returns coordinate from north West boundary
    class(shr_gridBounds), intent(in) :: self
    c = shr_coord(self % getNorth(), self % getWest())
  end function getCoordinateNorthWest


  type(shr_coord) function getCoordinateSouthEast(self) result (c)
    !< returns coordinate from south east boundary
    class(shr_gridBounds), intent(in) :: self
    c = shr_coord(self % getSouth(), self % getEast())
  end function getCoordinateSouthEast


  type(shr_coord) function getCoordinateSouthWest(self) result (c)
    !< returns coordinate from south west boundary
    class(shr_gridBounds), intent(in) :: self
    c = shr_coord(self % getSouth(), self % getWest())
  end function getCoordinateSouthWest

end module shr_gridBounds_mod

