!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : coord_mod 
!
!> @author
!> 
!
! DESCRIPTION:
!> 
!------------------------------------------------------------------------------
!
module shr_coord_mod

  use SHR_precision_mod, only: sp
  use SHR_error_mod, only: raiseError

  implicit none 

  public :: coord 

  real(kind=sp), parameter :: MAXLAT = 90.
  real(kind=sp), parameter :: MINLAT = -90.
  real(kind=sp), parameter :: MAXLON = 180.
  real(kind=sp), parameter :: MINLON = -180.

  real(kind=sp), parameter :: SPANLAT = 180.
  real(kind=sp), parameter :: SPANLON = 360.
  

  type coord
    real(kind=sp) :: lat
    real(kind=sp) :: lon
  contains
    procedure :: getLat
    procedure :: getLon
    procedure :: toArray
    procedure :: toString

    procedure, pass(w0), private :: coord_eq
    generic :: operator(==) => coord_eq
  end type coord

  interface coord
    module procedure :: coord_constructor
  end interface

contains

  type(coord) function coord_constructor(initlat, initlon)
    real(kind=sp), intent(in) :: initlat
    real(kind=sp), intent(in) :: initlon
    character(len=:), allocatable :: tmp
    character(len=:), allocatable :: tmp1

    if (initlat .gt. MAXLAT .or. initlat .lt. MINLAT ) then
      write(tmp,*) "(", MINLAT, ", ", MAXLAT, ")"
      write(tmp1,*) initlat
      call raiseError(__FILE__, "coord_constructor", &
              "initlat value is not in between latitude values "//tmp, &
              "Value: "//tmp1 )
    endif

    if (initlon .gt. MAXLON .or. initlon .lt. MINLON ) then
      write(tmp,*) "(", MINLON, ", ", MAXLON, ")"
      write(tmp1,*) initlon
      call raiseError(__FILE__, "coord_constructor", &
              "initlon value is not in between longitudes values "//tmp, &
              "Value: "//tmp1 )
    endif

    coord_constructor % lat = initLat 
    coord_constructor % lon = initLon

  end function coord_constructor


  pure elemental real(kind=sp) function getLat(self)
    class(coord), intent(in) :: self
    getLat = self % lat
  end function getLat


  pure elemental real(kind=sp) function getLon(self)
    class(coord), intent(in) :: self
    getLon = self % lon
  end function getLon


  pure function toArray(self) result (r)
    !< it returns an array with the coord data (lat and lon)
    class(coord), intent(in) :: self
    real(kind=sp), allocatable :: r(:)
    integer, parameter :: COORD_NUM = 2 !< total attributes to define a coordinate
    integer, parameter :: COORD_LAT = 1 !< latitude index in the output array
    integer, parameter :: COORD_LON = 2 !< longitude index in the output array

    if (allocated(r)) deallocate(r)
    allocate(r(COORD_NUM))
    r(COORD_LAT) = self % getLat()
    r(COORD_LON) = self % getLon()
  end function toArray


  pure elemental logical function coord_eq(w0,w1) result (res)
    !< coords are the same if those have the same latitude and longitude  
    class(coord), intent(in) :: w0, w1

    res = (w0 % lat == w1 % lat .and. &
                w0 % lon == w1 % lon )
  end function coord_eq


  function toString(self) result (r)
    class(coord), intent(in) :: self
    character(len=:), allocatable :: r
    character(len=10) :: clat, clon
    write(clat,'(F10.4)') self % lat
    write(clon,'(F10.4)') self % lon
    r = clat//","//clon
  end function toString

end module shr_coord_mod
