!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMask_stub 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_gridMask stub
!------------------------------------------------------------------------------
module shr_gridMask_stub

  use shr_gridMask_mod, only: shr_IgridMask
  !< dependencies
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor

  implicit none

  private
  public :: shr_gridMaskStub


  !< stub
  type, extends(shr_IgridMask) :: shr_gridMaskStub
  contains
    procedure :: getRaw
    procedure :: getGridDescriptor
  end type shr_gridMaskStub


contains

  !<
  !< shr_gridMaskStub
  !<
  function getRaw(self) result (outMask)
    !< returns current mask
    class(shr_gridMaskStub), intent(in) :: self
    logical, allocatable :: outMask(:,:) !< output
    allocate(outMask(4,3))
    outMask(1,:) = [.true., .true., .true.]
    outMask(2,:) = [.true., .true., .true.]
    outMask(3,:) = [.true., .false., .false.]
    outMask(4,:) = [.false., .true., .true.]
  end function getRaw


  type(shr_gGridDescriptor) function getGridDescriptor(self)
    !< returns self gridDescriptor
    class(shr_gridMaskStub), intent(in) :: self
    type(shr_gridBounds) :: bounds !< n, s, e, w
    call bounds % init(4.,0.,3.,0.)
    call getGridDescriptor % init(1., bounds)
  end function getGridDescriptor

end module shr_gridMask_stub

