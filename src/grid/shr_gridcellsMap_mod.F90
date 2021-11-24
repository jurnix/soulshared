!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridcellsMap_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> 
!> 
!------------------------------------------------------------------------------
module shr_gridcellsMap_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gridcell_mod, only: shr_gridcell


  implicit none

  public :: shr_gridcellsMap

  logical, parameter :: ISDEBUG = .false.

  type shr_gridcellsMapIterator
  contains
!    procedure :: hasMore -> yes/no
!    procedure :: getNext -> shr_gridcellsMap
  end type shr_gridcellsMapIterator


  type shr_gridcellsMap
    type(shr_gridcell), allocatable :: gridcells(:)

!    real(kind=sp) :: resolution
!    class(shr_arrayDim), allocatable :: latitudes
!    class(shr_arrayDim), allocatable :: longitudes
!    type(shr_gridMask), allocatable :: mask
  contains
    procedure :: init => gridcellsMap_initialize 
    ! procedure :: createGridMapIterator() -> shr_gridcellsMapIterator
  end type shr_gridcellsMap

contains

  subroutine gridcellsMap_initialize(self) !, resolution, latitudes, longitudes, mask)
    !< gridcellsMap initialization
    !<
    class(shr_gridcellsMap), intent(inout) :: self
!    class(shr_arrayDim), intent(in) :: latitudes
!    class(shr_arrayDim), intent(in) :: longitudes
!    type(shr_gridMask), intent(in) :: mask

!    integer :: nlats, nlons, ngridcells
!    integer :: nidx, idxlat, idxlon
!    real(kind=sp) :: latVal, lonVal
!    type(shr_coord) :: ccentre

!    self % resolution = resolution
!    allocate(self % latitudes, source = latitudes)
!    allocate(self % longitdues, source = longitudes)
!    if (present(mask)) then
!      allocate(self % mask)
!      self % mask = mask
!    endif !< present(mask)

    ! initialize gridcells
!    nlats = latitudes % getSize()
!    nlons = longitudes % getSize()
!    ngridcells = nlats * nlons
!    nidx = 1
!    allocate(self % gridcells(ngridcells))
    ! iterate the grid
!    do idxlat = 1, nlats
!      do idxlon = nlons, 1, -1
        ! create all gridcells
!        latVal = self % lats % getValue(idxlat)
!        lonVal = self % lons % getValue(idxlon)
!        ccentre = shr_coord(latVal, lonVal)
!        self % gridcells(nidx) = shr_gridcell(nidx, resolution, ccentre)
!        nidx = nidx + 1 ! provide an unique number to each gridcell
!      enddo !< idxlon = nlons, 1, -1
!    enddo !< idxlat = 1, nlats

!    if (.not. present(mask)) then
!      call self % setAllEnabledGridcells(enabledGridcells)
!    endif !< .not. present(mask)
  end subroutine gridcellsMap_initialize


end module shr_gridcellsMap_mod 

