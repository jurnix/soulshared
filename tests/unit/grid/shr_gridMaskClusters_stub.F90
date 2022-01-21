!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskClusters_stub 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gridMaskClusters stub
!------------------------------------------------------------------------------
module shr_gridMaskClusters_stub
  use shr_error_mod, only: raiseError

  use shr_gridMaskFindClustersMethod_mod, only: shr_IGridMaskFindClustersMethod
  use shr_gridMask_mod, only: shr_IgridMask, shr_gridMask
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
  use shr_gGridMap_mod, only: shr_gGridMap
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_gAxisMapping_mod, only: shr_gAxisMapping

  use shr_precision_mod, only: sp
  use shr_gGridAxes_mod, only: shr_gGridAxes

  use shr_gGrid_mod, only: shr_gGrid
  use shr_strings_mod, only: string

  implicit none

  private
  public :: shr_gridMaskClustersStub


  type, extends(shr_IGridMaskFindClustersMethod) :: shr_gridMaskClustersStub
  contains
    !< overload
    procedure :: init
    procedure :: getSize
    procedure :: get

    !< custom
    procedure :: getFirst
    procedure :: getSecond
    procedure :: getThird
  end type shr_gridMaskClustersStub

contains

  type(shr_gGridMap) function getNewGridMap(gDescriptor)
    !< generate new gridmap
    type(shr_gGridDescriptor), intent(in) :: gDescriptor
    real(kind=sp) :: res
    type(shr_gGridAxes) :: laxis, lonxis
    type(shr_gGridAxesBounds) :: laxisBounds, lonxisBounds
    type(shr_gAxisMapping) :: laxisMapping, lonxisMapping
    type(shr_gridBounds) :: bounds
    res = gDescriptor % getResolution()
    bounds = gDescriptor % getBounds()

    call laxisBounds % init(bounds % getNorth(), bounds % getSouth())
    call lonxisBounds % init(bounds % getEast(), bounds % getWest())

    call laxis % init(string("lats"), res, laxisBounds)
    call lonxis % init(string("lons"), res, lonxisBounds)

    call laxisMapping % init(laxis)
    call lonxisMapping % init(lonxis)

    call getNewGridMap % init(gDescriptor, laxisMapping, lonxisMapping)
  end function getNewGridMap


  type(shr_gGrid) function getNewGrid(gDescriptor) result (newGrid)
    !< generate a new grid
    type(shr_gGridDescriptor), intent(in) :: gDescriptor
    type(shr_gGridMap) :: gmap
    gmap = getNewGridMap(gDescriptor)
    call newGrid % init(gDescriptor, gmap)
  end function getNewGrid


  type(shr_gGridDescriptor) function genNewGridDescriptor() result (newGDescriptor)
    !< generate a new gridDescriptor
    type(shr_gridBounds) :: bounds
    call bounds % init(4.,0.,3.,0.)
    call newGDescriptor % init(1., bounds)
  end function genNewGridDescriptor

  !< 
  subroutine init(self, gridMask)    
    !< gridMask initialization
    class(shr_gridMaskClustersStub), intent(inout) :: self
    class(shr_IgridMask), intent(in) :: gridMask
    !< (TTT,TTT,TFF,FTT)
  end subroutine init


  integer function getSize(self)    
    !< returns how many groups found
    class(shr_gridMaskClustersStub), intent(in) :: self
    getSize = 3
  end function getSize


  type(shr_gridMask) function get(self, pos)    
    !< it returns selected gridMask requested for position 'pos'
    class(shr_gridMaskClustersStub), intent(in) :: self
    integer, intent(in) :: pos
    type(shr_gGridDescriptor) :: gDescriptor
    type(shr_gGrid) :: grid

    gDescriptor = genNewGridDescriptor()
    grid = getNewGrid(gDescriptor)
    call get % init(grid)

    if (pos == 1) then
      get = self % getFirst()
    else if (pos == 2) then
      get = self % getSecond()
    else if (pos == 3) then
      get = self % getThird()
    else
      !< error here
      call raiseError(__FILE__, "get", &
          "Requested element position not found")
    end if
  end function get


  type(shr_gridMask) function getFirst(self)
    !< returns first faked gridmask
    !< (TTT,TTT,FFF,FFF)
    class(shr_gridMaskClustersStub), intent(in) :: self
    type(shr_gGrid) :: grid
    type(shr_gGridDescriptor) :: gDesc
    logical :: lmask(4,3)
    gDesc = genNewGridDescriptor()
    grid = getNewGrid(gDesc)
    lmask = .false.
    lmask(1:2,:) = .true.
    call getFirst % init(grid, lmask)
  end function getFirst


  type(shr_gridMask) function getSecond(self)
    !< returns first faked gridmask
    !< (FFF,FFF,TFF,FFF)
    class(shr_gridMaskClustersStub), intent(in) :: self
    type(shr_gGrid) :: grid
    type(shr_gGridDescriptor) :: gDesc
    logical :: lmask(4,3)
    gDesc = genNewGridDescriptor()
    grid = getNewGrid(gDesc)
    lmask = .false.
    lmask(3,1) = .true.
    call getSecond % init(grid, lmask)
  end function getSecond


  type(shr_gridMask) function getThird(self)
    !< returns first faked gridmask
    !< (FFF,FFF,FFF,FTT)
    class(shr_gridMaskClustersStub), intent(in) :: self
    type(shr_gGrid) :: grid
    type(shr_gGridDescriptor) :: gDesc
    logical :: lmask(4,3)

    gDesc = genNewGridDescriptor()
    grid = getNewGrid(gDesc)
    lmask = .false.
    lmask(3,1) = .true.
    call getThird % init(grid, lmask)
  end function getThird

end module shr_gridMaskClusters_stub

