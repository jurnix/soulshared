!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridDomainPartitioningMethodBySquares_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> shr_gridDomain is partitioned into squared grids
!>
!> Example:
!>
!> (gridDomain) 			found								gridDomainSquared
!>
!> - x x -					-> 1st - x x -  ->  x x
!> x x - -					-> 2d  x x - -  ->  x x
!> - - - -     ->
!> x x x x					-> 3rd x x x x  ->  x x x x
!> x x x x 								 x x x x 			x x x x
!>
!> gridDomainSquare include the correct gridDescriptor.
!> They can be put into place in case those are combined.
!>
!> Such feature is useful when writing/reading netcdf data.
!> Due to parallelism the grid is divided for each proc.
!> This causes non regular grids. Consequently IO data must be written
!> into chunks.
!> 
!------------------------------------------------------------------------------
module shr_gridDomainPartitioningMethodBySquares_mod

  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gridDomainPartitioningMethod_abs, only: shr_gridDomainPartitioningMethod

  use shr_gridMask_mod, only: shr_gridMask_cast, shr_IgridMask
  use shr_gridMaskFindClustersMethod_mod, only: shr_IGridMaskFindClustersMethod, &
          shr_gridMaskSimpleSquaresFindClustersMethod
  use shr_gridDomain_mod, only: shr_gridDomain, shr_iGridDomain
  use shr_gridMaskFindClustersIterator_mod, only: shr_gridMaskFindClustersIterator

  implicit none

  public :: shr_gridDomainPartitioningMethodBySquares, GRID_DOMAIN_PARTITION_SQUARED

  logical, parameter :: ISDEBUG = .false.

  character(*), parameter :: GRID_DOMAIN_PARTITION_SQUARED = "squared"



  type, extends(shr_gridDomainPartitioningMethod) :: shr_gridDomainPartitioningMethodBySquares
    class(shr_igridDomain), allocatable :: domain !< input
    class(shr_IGridMaskFindClustersMethod), allocatable :: squaredPartitioning !< method

    class(shr_igridDomain), allocatable :: newDomains(:) !< output
  contains
    procedure :: init
    procedure :: calculate
    procedure, private :: calculate_clusters
    procedure :: get
  end type shr_gridDomainPartitioningMethodBySquares

contains

  subroutine init(self, domain)
    !< initialize
    class(shr_gridDomainPartitioningMethodBySquares), intent(inout) :: self
    class(shr_iGridDomain), intent(in) :: domain
    allocate(self % domain, source = domain)
    allocate(shr_gridMaskSimpleSquaresFindClustersMethod :: self % squaredPartitioning)
    call self % calculate()
  end subroutine init


  subroutine calculate(self)
    !< compute partitions
    class(shr_gridDomainPartitioningMethodBySquares), intent(inout) :: self
    self % newDomains = self % calculate_clusters(self % squaredPartitioning)
  end subroutine calculate


  function calculate_clusters(self, clustersMethod) result (gDomains)
    !< in case it is not squared:
    !< -it returns multiple domains with squared property
    !<  (the domain is partitioned into multiple squared grid domains)
    !< otherwise it returns the new gridDomainSquared
    class(shr_gridDomainPartitioningMethodBySquares), intent(in) :: self
    class(shr_IGridMaskFindClustersMethod), intent(in) :: clustersMethod
    class(shr_igridDomain), allocatable :: gDomains(:) !< output

    type(shr_gridMaskFindClustersIterator) :: clustersIterator
    class(shr_igridMask), allocatable :: newGMaskClustered
    class(*) , allocatable :: obj

    integer :: ndomains, idomain

    ndomains = clustersMethod % getSize()
    allocate(shr_gridDomain :: gDomains(ndomains))

    call clustersIterator % init(clustersMethod)
    idomain = 1
    do while (clustersIterator % hasNext())
      obj = clustersIterator % getNext()
      !< change type
      call shr_gridMask_cast(obj, newGMaskClustered)
      gDomains(idomain) = self % domain % filter(newGMaskClustered)

      idomain = idomain + 1
    enddo

  end function calculate_clusters


  function get(self) result (newDomains)
    !< it returns partitioned domains
    class(shr_gridDomainPartitioningMethodBySquares), intent(in) :: self
    class(shr_igridDomain), allocatable :: newDomains(:) !< output
    allocate(newDomains, source = self % newDomains)
  end function get

end module shr_gridDomainPartitioningMethodBySquares_mod

