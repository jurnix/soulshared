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


!  type :: arrayRsp
!    class(shr_arrayContainer), allocatable :: container
!  contains
!    procedure :: add_arrayRsp
!    generic, public :: operator(+) => add_arrayRsp

!    procedure :: copy_arrayRsp
!    generic, public :: assignment(=) => copy_arrayRsp

!    procedure :: equal_arrayRsp
!    generic, public :: operator(==) => equal_arrayRsp
!  end type arrayRsp

  !< -------


!  type, abstract :: shr_arrayContainer
 ! contains
 !   procedure(iface_add_arrayContainer), deferred :: add_arrayContainer
 !   generic, public :: operator(+) => add_arrayContainer
!    procedure(iface_sub_arrayContainer), deferred :: sub_arrayContainer
!    procedure(iface_div_arrayContainer), deferred :: div_arrayContainer
!    procedure(iface_mul_arrayContainer), deferred :: mul_arrayContainer
 ! end type


 ! abstract interface
 !   pure function iface_add_arrayContainer(left, right) Result(total)
 !     import :: shr_arrayContainer
 !     class(shr_arrayContainer), intent(in) :: left, right
!      class(shr_arrayContainer), allocatable :: total
!    end function iface_add_arrayContainer
!  end interface


  ! data types
  type, extends(shr_arrayContainer) :: shr_arrayContainerAllocatable !< allocatable single precision array
    real(kind=sp), allocatable :: r1(:)
    real(kind=sp), allocatable :: r2(:,:)
    real(kind=sp), allocatable :: r3(:,:,:)
    real(kind=sp), allocatable :: r4(:,:,:,:)
    real(kind=sp), allocatable :: r5(:,:,:,:,:) ! MAXRANK
  contains
    procedure :: add_arrayContainer => add_arrayContainerAllocatable

    procedure :: copy_scalar
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


  pure subroutine copy_scalar(self, other)
      !< copy scalar value into array
      !< arrayCA = 24.1
      class(shr_arrayContainerAllocatable), intent(inout) :: self
      real(kind=sp), intent(in) :: other
  end subroutine copy_scalar


  pure subroutine copy_arrayContainer(self, other)
      !< Copy to current array container allocatable
      !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
      class(shr_arrayContainerAllocatable), intent(inout) :: self
      class(shr_arrayContainer), intent(in) :: other
  end subroutine copy_arrayContainer

end module SHR_arrayContainerAllocatable_mod
