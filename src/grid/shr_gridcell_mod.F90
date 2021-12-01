!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridcell_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> 
!> grid cell is a smaller part of a grid
!>
!------------------------------------------------------------------------------


module shr_gridcell_mod

  use SHR_precision_mod, only: sp 
  use shr_coord_mod, only: shr_coord
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_objects_mod, only: shr_eqObject_abs

  implicit none

  private

  public :: shr_gridcell

  public :: GRIDCELL_NNEIGHS, GRIDCELL_N_NEAST, GRIDCELL_N_NORTH, GRIDCELL_N_EAST, &
            GRIDCELL_N_SEAST, GRIDCELL_N_SOUTH, GRIDCELL_N_SWEST, GRIDCELL_N_WEST, &
            GRIDCELL_N_NWEST


  integer, parameter :: GRIDCELL_NNEIGHS = 8 !< total number of neighbours
  integer, parameter :: GRIDCELL_N_NORTH = 1 !< north neighbour id
  integer, parameter :: GRIDCELL_N_NEAST = 2 !< north east neighbour id
  integer, parameter :: GRIDCELL_N_EAST = 3 !< east neighbour id
  integer, parameter :: GRIDCELL_N_SEAST = 4 !< south east neighbour id
  integer, parameter :: GRIDCELL_N_SOUTH = 5 !< south neighbour id
  integer, parameter :: GRIDCELL_N_SWEST = 6 !< south west neighbour id
  integer, parameter :: GRIDCELL_N_WEST = 7 !< west neighbour id
  integer, parameter :: GRIDCELL_N_NWEST = 8 !< north west neighbour id



  type, extends(shr_eqObject_abs) :: shr_gridcell !< gridcell from the map
    integer :: idx !< gridcell index in repect to the overall grid (starting from top-left with 1 to ...)
    type(shr_coord) :: center !< gridcell center
    real(kind=sp) :: resolution !< gridcell resolution
    type(shr_gridBounds) :: limits !< limits of the gridcell n, s, e, w
    logical :: enabled !< data is used, otherwise it is ignored
  contains
    procedure :: contains
    procedure :: getNorth, getSouth
    procedure :: getEast, getWest
    procedure :: getSpatialIdxs 
    procedure :: toString => toString_gridcell

    !< deferred as (==)
    procedure :: eq_object => gridcell_eq
  end type shr_gridcell

  interface shr_gridcell
    module procedure :: gridcell_constructor
  end interface


contains

  pure elemental logical function gridcell_eq(self, other) result(res)
    ! `datetime` comparison operator that returns `.true.` if `d0` is
    ! equal to `d1` and `.false.` otherwise. Overloads the operator `==`.
    class(shr_gridcell), intent(in) :: self
    !class(shr_gridcell), intent(in) :: gc1
    class(SHR_eqObject_abs), intent(in) :: other
    class(shr_gridcell), pointer :: pGridcell

    logical :: sameIdx, sameCenter, sameRes, sameLimits, sameStatus

    select type(eqgc => other)
    type is (shr_gridcell)
      pGridcell => eqgc
    class default
      res = .false.
      return
    end select

    sameIdx = (self % idx == pGridcell % idx) 
    sameCenter = ( self % center == pGridcell % center )
    sameRes = (self % resolution == pGridcell % resolution)
    sameLimits = ( self % limits == pGridcell % limits )
    sameStatus = ( self % enabled .eqv. pGridcell % enabled )

    res = sameIdx .and. sameCenter .and. sameRes .and. sameLimits .and. sameStatus
  end function gridcell_eq


  function getSpatialIdxs(self, nlons) result (idxs)
    !< it returns the spatial array indices from the current gridcell
    class(shr_gridcell), intent(in) :: self
    integer, intent(in) :: nlons
    real(kind=sp) :: idxs(2) !< output (lat,lon)

    integer :: idxlat, idxlon

    idxlat = ((self % idx-1) / nlons) + 1
    idxlon = mod((self % idx-1), nlons) + 1

    idxs(1) = idxlat 
    idxs(2) = idxlon
        
  end function getSpatialIdxs


  real(kind=sp) function getEast(self)
    !< it returns the south latitude from the gridcell
    class(shr_gridcell), intent(in) :: self

    getEast = self % limits % east
  end function getEast

  real(kind=sp) function getWest(self)
    !< it returns the south latitude from the gridcell
    class(shr_gridcell), intent(in) :: self

    getWest = self % limits % west
  end function getWest

  real(kind=sp) function getSouth(self)
    !< it returns the south latitude from the gridcell
    class(shr_gridcell), intent(in) :: self

    getSouth = self % limits % south
  end function getSouth

  real(kind=sp) function getNorth(self)
    !< it returns the north latitude from the gridcell
    class(shr_gridcell), intent(in) :: self

    getNorth = self % limits % north
  end function getNorth


  function toString_gridcell(self) result (str)
    !< character representation of the gridcell class
    class(shr_gridcell), intent(inout) :: self
    character(:), allocatable :: str
    character(100) :: idxStr, resStr
    write( idxStr, *) self % idx
    write( resStr, *) self % resolution

    str = "(" //trim(idxStr) // ") center=" // trim(self % center % toString()) // &
            ", res=" // trim(resStr) // &
            ", limits=" // trim(self % limits % toString()) !// self % enabled
  end function toString_gridcell


  logical function contains(self, c)
    !< true if the given coordinate 'c' fits the gridcell
    class(shr_gridcell), intent(in) :: self
    type(shr_coord), intent(in) :: c

    !write(*,*) "grid_mod::contains:: c=", c % toString()
    !write(*,*) "grid_mod::contains:: north, below north=", self % getNorth(), self % getNorth() >= c % lat
    !write(*,*) "grid_mod::contains:: north, above south=", self % getSouth(), self % getSouth() <= c % lat
    !write(*,*) "grid_mod::contains:: east, before east=", self % getEast(), self % getEast() >= c % lon
    !write(*,*) "grid_mod::contains:: west, after west=", self % getWest(), self % getWest() <= c % lon
    contains = .false.
    if (self % getNorth() >= c % lat .and. &
        self % getSouth() <= c % lat .and. &
        self % getEast() >= c % lon .and. &
        self % getWest() <= c % lon) then

      contains = .true.
    endif
    !write(*,*) "grid_mod::contains:: contains? ", contains
  end function contains


  type(shr_gridcell) function gridcell_constructor(idx, resolution, center, enabled)
    !< constructs a new gridcell given the grid resolution and coordinate center of 
    !< the gridcell. 
    !< By default a gridcell is enabled. When enabled it's expect data from the variables
    integer, intent(in) :: idx !< gridcell index number in respect to the whole grid
    real(kind=sp), intent(in) :: resolution !< Grid's resolution. It helps to calculates
                                            !< gridcell's limits.
    type(shr_coord), intent(in) :: center !< gridcell's center coordindate
    !type(pGridcell), intent(in), optional :: neighbours(GRIDCELL_NNEIGHS) !< 
    logical, intent(in), optional :: enabled !< gridcell is enabled (e.g: land)

    real(kind=sp) :: halfres

    halfres = resolution / 2.0

    gridcell_constructor % idx = idx
    gridcell_constructor % center = center
    gridcell_constructor % resolution = resolution
    gridcell_constructor % limits = shr_gridBounds(center % lat + halfres, & ! north
                                     center % lat - halfres, & ! south
                                     center % lon + halfres, & ! east
                                     center % lon - halfres)  ! west
    gridcell_constructor % enabled = .true.
    if (present(enabled)) then
      gridcell_constructor % enabled = enabled
    endif

  end function gridcell_constructor




end module shr_gridcell_mod
