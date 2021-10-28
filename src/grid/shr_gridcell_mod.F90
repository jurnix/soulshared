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
!>
!------------------------------------------------------------------------------


module shr_gridcell_mod

  use SHR_precision_mod, only: sp 
!  use SHR_error_mod, only: raiseError
!  use SHR_arrayUtils_mod, only: unique, closestNumber2Index, PREFER_LAST, &
!                        initArrayRange
  use shr_coord_mod, only: coord
!  use shr_gridPartition_mod, only: grid_partition_type
  use shr_gridBounds_mod, only: shr_gridBounds
!  use shr_gridBounds_mod, only: SHR_GRIDBOUNDS_NCOORDS, SHR_GRIDBOUNDS_NORTH, &
!             SHR_GRIDBOUNDS_SOUTH, SHR_GRIDBOUNDS_EAST, SHR_GRIDBOUNDS_WEST

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



  type shr_gridcell !< gridcell from the map
    integer :: idx !< gridcell index in repect to the overall grid (starting from top-left with 1 to ...)
    type(coord) :: center !< gridcell center
    real(kind=sp) :: resolution !< gridcell resolution
    type(shr_gridBounds) :: limits !< limits of the gridcell n, s, e, w
    logical :: enabled !< data is used, otherwise it is ignored
  contains
    procedure :: contains
    procedure :: getNorth, getSouth
    procedure :: getEast, getWest
    procedure :: getSpatialIdxs 
    procedure :: toString => toString_gridcell

    procedure, private :: gridcell_eq
    generic :: operator(==) => gridcell_eq
  end type shr_gridcell

  interface shr_gridcell
    module procedure :: gridcell_constructor
  end interface


contains

  pure elemental logical function gridcell_eq(gc0,gc1) result(res)
    ! `datetime` comparison operator that returns `.true.` if `d0` is
    ! equal to `d1` and `.false.` otherwise. Overloads the operator `==`.
    class(shr_gridcell), intent(in) :: gc0, gc1

    logical :: sameIdx, sameCenter, sameRes, sameLimits, sameStatus

    sameIdx = (gc0 % idx == gc1 % idx) 
    sameCenter = ( gc0 % center == gc1 % center )
    sameRes = (gc0 % resolution == gc1 % resolution)
    sameLimits = ( gc0 % limits == gc1 % limits )
    sameStatus = ( gc0 % enabled .eqv. gc1 % enabled )

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
    type(coord), intent(in) :: c

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
    type(coord), intent(in) :: center !< gridcell's center coordindate
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
