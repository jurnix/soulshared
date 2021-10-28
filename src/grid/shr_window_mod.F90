!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : SIO_window_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> window defines a squared scope in a grid
!------------------------------------------------------------------------------

module shr_window_mod

  use SHR_precision_mod, only: sp
  use SHR_error_mod, only: raiseError
  use shr_coord_mod, only: coord

  implicit none

  public :: window
  public :: WINDOW_NCOORDS, WINDOW_NORTH, &
             WINDOW_SOUTH, WINDOW_EAST, WINDOW_WEST

  private 

  logical, parameter :: ISDEBUG = .false.


  !< constant window values
  integer, parameter :: WINDOW_NORTH = 1
  integer, parameter :: WINDOW_SOUTH = 2
  integer, parameter :: WINDOW_EAST = 3
  integer, parameter :: WINDOW_WEST = 4
  integer, parameter :: WINDOW_NCOORDS = 4

  type window ! coordinates to select a part of the world
    real(kind=sp) :: north, south, east, west
  contains
    procedure :: initFromArray
    procedure :: initWindow
    generic :: init => initWindow, initFromArray
    procedure :: fitsWindow
    procedure :: fitsCoord
    generic :: fits => fitsWindow, fitsCoord !< a given window fits in the current one
    procedure :: toArray
    procedure :: toString

    ! operator overloading procedures
    procedure, pass(w0), private :: window_eq
    procedure, pass(w0), private :: window_eq_array
    ! 
    generic :: operator(==)  => window_eq, window_eq_array
  end type

contains



  subroutine initFromArray(self, coords)
    !< initialize window from an array(north, south, east, west)
    class(window), intent(inout) :: self
    real(kind=sp), intent(in) :: coords(WINDOW_NCOORDS)

    real(kind=sp) :: north, south, east, west

    north = coords(WINDOW_NORTH)
    south = coords(WINDOW_SOUTH)
    east = coords(WINDOW_EAST)
    west = coords(WINDOW_WEST)

    call self % initWindow(north, south, east, west)
  end subroutine initFromArray


  subroutine initWindow(self, north, south, east, west)
    !< initialize window for a coordinate
    class(window), intent(inout) :: self
    real(kind=sp), intent(in) :: north, south, east, west

    character(len=100) :: tmp, tmp1

    if (north < south) then
      write(tmp,*)  north
      write(tmp1,*)  south
      call raiseError(__FILE__, "initWindow", &
              "north latitude must be bigger than south", &
              "North: "//tmp, &
              "South: "//tmp1)
    endif

    if (east < west) then
      write(tmp,*) east 
      write(tmp1,*) west
      call raiseError(__FILE__, "initWindow", &
              "East(180) longitude must be bigger than the west(-180)", &
              "East: "//tmp, &
              "West: "//tmp1)
    endif

    self % north = north
    self % south = south
    self % east = east
    self % west = west
  end subroutine initWindow


  pure elemental logical function fitsWindow(self, smallWindow)
    !< the given window gits in the self one
    class(window), intent(in) :: self, smallWindow

    fitsWindow = .false.
    if (self % north >= smallWindow % north .and. &
        self % south <= smallWindow % south .and. &
        self % east >= smallWindow % east .and. &
        self % west <= smallWindow % west ) then

        fitsWindow = .true.
    endif
  end function fitsWindow


  pure elemental logical function fitsCoord(self, nCoord)
    !< the given window gits in the self one
    class(window), intent(in) :: self
    type(coord), intent(in) :: nCoord

    fitsCoord = .false.
    if (self % north >= nCoord % lat .and. &
        self % south <= nCoord % lat .and. &
        self % east >= nCoord % lon .and. &
        self % west <= nCoord % lon ) then

        fitsCoord = .true.
    endif
  end function fitsCoord


  pure logical function window_eq(w0,w1) result (res)
    !< compare window types
    class(window), intent(in) :: w0, w1

    res = .false.
    if (w0 % north == w1 % north .and. &
        w0 % south == w1 % south .and. &
        w0 % east == w1 % east .and. &
        w0 % west == w1 % west) then

      res = .true.
    endif
  end function window_eq


  logical function window_eq_array(w0,w1) result (res)
    !< compare window types
    class(window), intent(in) :: w0
    real(kind=sp), intent(in) :: w1(WINDOW_NCOORDS)

    res = .false.
    if (w0 % north == w1(WINDOW_NORTH) .and. &
        w0 % south == w1(WINDOW_SOUTH) .and. &
        w0 % east == w1(WINDOW_EAST) .and. &
        w0 % west == w1(WINDOW_WEST)) then

      res = .true.
    endif
  end function window_eq_array


  function toString(self) result (str)
    !< string representation of Window
    class(window), intent(inout) :: self
    character(len=:), allocatable :: str

    character(len=10) :: n, s, e, w
    write(n,'(F10.4)') self % north 
    write(s,'(F10.4)') self % south
    write(e,'(F10.4)') self % east 
    write(w,'(F10.4)') self % west 
    str = n//", "//s//", "//e//", "//w
  end function toString


  function toArray(self) result (vals)
    !< convert window into an array(4) 
    !< each position corresponds to north, south, east, west
    class(window), intent(inout) :: self
    real(kind=sp) :: vals(WINDOW_NCOORDS)

    vals(1) = self % north
    vals(2) = self % south
    vals(3) = self % east
    vals(4) = self % west
  end function toArray

end module shr_window_mod

