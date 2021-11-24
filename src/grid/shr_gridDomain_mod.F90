!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridDomain_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gridDomain is a part of a grid
!> 
!------------------------------------------------------------------------------
module shr_gridDomain_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  implicit none

  public :: shr_gridDomain

  logical, parameter :: ISDEBUG = .false.


  type shr_gridDomain
!    class(shr_gridMask), allocatable :: mask(:,:) !< some gridcells are not used in this partition
!    class(shr_grid), allocatable :: grid !< local grid
  contains
    procedure :: init => gridDomain_initialize 
!    procedure :: isSquared !< yes if the current domain is squared
!    procedure :: toSquaredDomains !< it divides the current domain into smaller with
                                  !< but all squared

!    procedure :: toGrid !< transfrom shr_gridDomain into shr_grid 

!    procedure :: gridDomain_combine !< +
!    generic, operator(+) :: gridDomain_combine
!    procedure :: gridDomain_difference !< -
!    generic, operator(-) :: gridDomain_difference
!    procedure :: gridDomain_copy !< -

!    generic, assignment(=) :: gridDomain_copy
!    procedure :: gridDomain_equal !< -
!    generic, operator(==) :: gridDomain_equal
  end type shr_gridDomain

contains


  subroutine gridDomain_initialize(self)!, grid, mask)
    !< grid domain initialization
    class(shr_gridDomain), intent(inout) :: self
!    class(shr_grid), intent(in) :: grid
!    type(shr_gridMask), intent(in) :: mask
!    allocate(self % grid, source = grid)
!    if (present(mask)) allocate(self % mask, source = mask)
  end subroutine gridDomain_initialize


end module shr_gridDomain_mod 

