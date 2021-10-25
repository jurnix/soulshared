!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  shr_arrayPointer_mod
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!> 
!>
!> 
!> 
!------------------------------------------------------------------------------


module SHR_arrayPointer_mod

  use SHR_precision_mod, only: sp! dp, eqReal
!  use SHR_error_mod, only: raiseError 
!  use SHR_strings_mod, only: string

  implicit none

  private


!  type, extends(shr_arrayContainer) :: shr_arrayRspPointer !< allocatable single precision array
!    real(kind=sp), pointer, contiguous :: r1(:)
!    real(kind=sp), pointer, contiguous :: r2(:,:)
!    real(kind=sp), pointer, contiguous :: r3(:,:,:)
!    real(kind=sp), pointer, contiguous :: r4(:,:,:,:)
!    real(kind=sp), pointer, contiguous :: r5(:,:,:,:,:) ! MAXRANK
!  contains
!    procedure :: add_arrayContainer => add_arrayRspPointer
!  end type shr_arrayRspPointer



contains


end module SHR_arrayPointer_mod
