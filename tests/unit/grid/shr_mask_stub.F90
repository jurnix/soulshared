!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_mask_stub
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_mask2d empty stub
!> Inhereted classes will overload desired methods instead of declaring
!> all of them.
!------------------------------------------------------------------------------
module shr_mask_stub

  use shr_mask_mod, only: shr_imask2d, shr_maskIndices_2d
  use shr_arrayIndices_mod, only: shr_arrayGridcellIndex

  implicit none

  private
  public :: shr_mask2dEmptyStub


  !< stub
  type, extends(shr_imask2d) :: shr_mask2dEmptyStub
  contains
    procedure :: getShape
    procedure :: getSize

    procedure :: getByGridcellIndex
    procedure :: getByMaskIndices
    procedure :: getSimple
  end type shr_mask2dEmptyStub

contains

  function getShape(self) result (s)
    !< get 'mask' shape
    class(shr_mask2dEmptyStub), intent(in) :: self
    integer :: s(2) !< row=1, col=2
  end function getShape


  integer function getSize(self) result (s)
    !< gets lmask size
    class(shr_mask2dEmptyStub), intent(in) :: self
  end function getSize


  function getByGridcellIndex(self, arrayGridcellIndex) result (l)
    !< gets unique value requested by
    class(shr_mask2dEmptyStub), intent(in) :: self
    type(shr_arrayGridcellIndex) :: arrayGridcellIndex
    logical :: l !< output
  end function getByGridcellIndex


  function getByMaskIndices(self, mIndices) result (m)
    !< returns internal mask
    class(shr_mask2dEmptyStub), intent(in) :: self
    type(shr_maskIndices_2d), intent(in) :: mIndices
    logical, allocatable :: m(:,:) !< output
  end function getByMaskIndices


  function getSimple(self) result (m)
    !< returns internal mask
    class(shr_mask2dEmptyStub), intent(in) :: self
    logical, allocatable :: m(:,:) !< output
  end function getSimple

end module shr_mask_stub

