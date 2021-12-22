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

  use shr_gridMaskClusters_mod, only: shr_IGridMaskClusters
  use shr_gridMask_mod, only: shr_IgridMask, shr_gridMask
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor

  implicit none

  private
  public :: shr_gridMaskClustersStub


  type, extends(shr_IGridMaskClusters) :: shr_gridMaskClustersStub
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
    logical :: lmask(4,3)

    gDescriptor = genNewGridDescriptor()
    call get % init(gDescriptor)

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
    type(shr_gGridDescriptor) :: gDescriptor
    logical :: lmask(4,3)
    gDescriptor = genNewGridDescriptor() !gmStub % getGridDescriptor()
    lmask = .false.
    lmask(1:2,:) = .true.
    call getFirst % init(gDescriptor, lmask)
  end function getFirst


  type(shr_gridMask) function getSecond(self)
    !< returns first faked gridmask
    !< (FFF,FFF,TFF,FFF)
    class(shr_gridMaskClustersStub), intent(in) :: self
    type(shr_gGridDescriptor) :: gDescriptor
    logical :: lmask(4,3)
    gDescriptor = genNewGridDescriptor() !gmStub % getGridDescriptor()
    lmask = .false.
    lmask(3,1) = .true.
    call getSecond % init(gDescriptor, lmask)
  end function getSecond


  type(shr_gridMask) function getThird(self)
    !< returns first faked gridmask
    !< (FFF,FFF,FFF,FTT)
    class(shr_gridMaskClustersStub), intent(in) :: self
    type(shr_gGridDescriptor) :: gDescriptor
    logical :: lmask(4,3)
    gDescriptor = genNewGridDescriptor() !gmStub % getGridDescriptor()
    lmask = .false.
    lmask(3,1) = .true.
    call getThird % init(gDescriptor, lmask)
  end function getThird

end module shr_gridMaskClusters_stub

