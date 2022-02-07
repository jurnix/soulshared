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
!> main soulshared_grid interface
!>
!------------------------------------------------------------------------------
module shr_soulsharedGrid_mod

  use shr_precision_mod, only: sp
  use shr_error_mod, only: raiseError

  use shr_gGrid_mod, only: shr_igGrid
  use shr_gridPartition_mod, only: shr_gridPartition
  use shr_strings_mod, only: string
  use shr_gridDomain_mod, only: shr_iGridDomain

  implicit none

  private

  public :: shr_soulsharedGrid

  type shr_soulsharedGrid
    class(shr_igGrid), allocatable :: grid
    class(shr_igridDomain), allocatable :: domains(:) !< domain Collection?
    type(string), allocatable :: type
    type(shr_gridPartition), allocatable :: partitions
  contains
    procedure :: init
  end type shr_soulsharedGrid


contains

  subroutine init(self, resolution, bounds, type, partitions)
    !< shr_soulsharedGrid initialization
    class(shr_soulsharedGrid), intent(inout) :: self
    real(kind=sp), intent(in) :: resolution
    real(kind=sp), intent(in) :: bounds !< n, s, e, w
    type(string), intent(in) :: type
    type(shr_gridPartition), intent(in) :: partitions

    ! structure
    !type(shr_gridDomain) :: gloDomain
    !type(shr_gridDomainPartitioning) :: gdPartitioning

!    gloDomain = gridDomainFactory(resolution, bounds, type)

!    call partitionMethod % init(current, total)
!    call gdPartitioning % init(gloDomain, method=partitionMethod same Number of gridcells)
!    self % domains = gdPartitioning % get()
  end subroutine init

end module shr_soulsharedGrid_mod
