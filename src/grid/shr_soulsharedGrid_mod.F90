!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGrid_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> main soulshared grid interface
!>
!------------------------------------------------------------------------------
module shr_soulsharedGrid_mod

  use shr_precision_mod, only: sp
  use shr_error_mod, only: raiseError

  use shr_gGrid_mod, only: shr_igGrid
  use shr_strings_mod, only: string
  use shr_gridDomain_mod, only: shr_iGridDomain, shr_gridDomainBuilder

  implicit none

  private

  character(*), parameter :: SHR_GRID_TYPE_REGULAR_SQUARED = "squared"

  public :: shr_soulsharedGrid

  type shr_soulsharedGrid
    class(shr_igridDomain), allocatable :: gridDomain
  contains
    procedure :: init
    !< combine
  end type shr_soulsharedGrid


contains

  subroutine init(self, resolution, bounds, type)
    !< shr_soulsharedGrid initialization
    class(shr_soulsharedGrid), intent(inout) :: self
    real(kind=sp), intent(in) :: resolution
    real(kind=sp), intent(in) :: bounds(4) !< n, s, e, w
    type(string), intent(in) :: type

    if (type == SHR_GRID_TYPE_REGULAR_SQUARED) then
      self % gridDomain = shr_gridDomainBuilder(resolution, bounds)
    else
      call raiseError(__FILE__, "init", &
          "Unknown/unsupported grid type")
    end if

!    call partitionMethod % init(current, total)
!    call gdPartitioning % init(gloDomain, method=partitionMethod same Number of gridcells)
!    self % domains = gdPartitioning % get()
  end subroutine init

end module shr_soulsharedGrid_mod
