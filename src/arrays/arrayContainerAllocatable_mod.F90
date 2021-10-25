!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  shr_arrayAllocatable_mod
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!> Common Array subroutines
!>
!> array class (real sp, real dp, int)
!> 
!------------------------------------------------------------------------------


module SHR_arrayContainerAllocatable_mod

  use SHR_precision_mod, only: sp! dp, eqReal
  use shr_arrayContainer_mod, only: shr_arrayContainer
!  use SHR_error_mod, only: raiseError 
!  use SHR_strings_mod, only: string

  implicit none

  private

  public :: shr_arrayContainerAllocatable

  !< allocatable single precision array
  type, extends(shr_arrayContainer) :: shr_arrayContainerAllocatable 
    real(kind=sp), allocatable :: r1(:)
    real(kind=sp), allocatable :: r2(:,:)
    real(kind=sp), allocatable :: r3(:,:,:)
    real(kind=sp), allocatable :: r4(:,:,:,:)
    real(kind=sp), allocatable :: r5(:,:,:,:,:) ! MAXRANK
  contains
    !< available: add, sub, div, mull
    !< kind: sp, dp
    !< rank: 1 to MAXRANK
    procedure :: add_scalar_rsp
    procedure :: add_array_raw_rsp_1
    procedure :: add_arrayContainer => add_arrayContainerAllocatable

    procedure :: copy_scalar_rsp
    procedure :: copy_array_raw_rsp_1
    procedure :: copy_arrayContainer
  end type shr_arrayContainerAllocatable


contains


  pure function add_arrayContainerAllocatable(left, right) Result(total)
    !< addition of arrayCA + arrayC 
    class(shr_arrayContainerAllocatable), intent(in) :: left
    class(shr_arrayContainer), intent(in) :: right
    class(shr_arrayContainer), allocatable :: total !< output

    allocate(shr_arrayContainerAllocatable :: total)

    select type(rightArray => right)
    type is (shr_arrayContainerAllocatable)
      if (allocated(left % r1) .and. allocated(rightArray % r1)) then
!        total = left % r1 + rightArray % r1
!      else
        !< unexpected :-/
      endif

!    type is (shr_arrayRspPointer)
    class default
      !< unexpected :-/
    end select
  end function add_arrayContainerAllocatable


  pure function add_array_raw_rsp_1(left, right) Result(total)
    !<
    class(shr_arrayContainerAllocatable), intent(in) :: left
    real(kind=sp), intent(in) :: right(:)
    class(shr_arrayContainer), allocatable :: total(:)
  end function add_array_raw_rsp_1


  pure function add_scalar_rsp(left, right) Result(total)
      !< copy scalar value into array
      !< arrayCA = 24.1
      class(shr_arrayContainerAllocatable), intent(in) :: left 
      real(kind=sp), intent(in) :: right 
      class(shr_arrayContainer), allocatable :: total !< output
  end function add_scalar_rsp


  pure subroutine copy_scalar_rsp(self, other)
      !< Copy to current array container allocatable
      class(shr_arrayContainerAllocatable), intent(inout) :: self
      real(kind=sp), intent(in) :: other
  end subroutine copy_scalar_rsp


  pure subroutine copy_array_raw_rsp_1(self, other)
    !< Copy to current array 'self' into 'other' rsp array
    class(shr_arrayContainerAllocatable), intent(inout) :: self
    real(kind=sp), intent(in) :: other(:)
  end subroutine copy_array_raw_rsp_1


  pure subroutine copy_arrayContainer(self, other)
      !< Copy to current array container allocatable
      !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
      class(shr_arrayContainerAllocatable), intent(inout) :: self
      class(shr_arrayContainer), intent(in) :: other
  end subroutine copy_arrayContainer

end module SHR_arrayContainerAllocatable_mod
