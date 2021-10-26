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
  use shr_arrayDim_mod, only: shr_arrayDim
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
    procedure :: init

    !< available: add, sub, div, mull
    !< kind: sp, dp
    !< rank: 1 to MAXRANK
    procedure :: add_scalar_rsp
    procedure :: add_array_raw_rsp_1
    procedure :: add_arrayContainer => add_arrayContainerAllocatable

    procedure :: copy_scalar_rsp
    procedure :: copy_array_raw_rsp_1
    procedure :: copy_arrayContainer

    final :: destroy_class
  end type shr_arrayContainerAllocatable


contains


  pure subroutine init(self, dimensions)
    !< initialize shr_arrayContainer, overload parent subroutine
    !< to customize child initialiation
    class(shr_arrayContainerAllocatable), intent(inout) :: self
    class(shr_arrayDim), intent(in) :: dimensions(:)

    integer, allocatable :: alldims(:)
    integer :: first, second, third, fourth, fifth

    !< common to all arrayContainer
    self % dimensions = dimensions
    self % ndims = size(dimensions)

    !< customized init
    first = alldims(1)
    if (self % ndims <= 2) second = alldims(2)
    if (self % ndims <= 3) third = alldims(3)
    if (self % ndims <= 4) fourth = alldims(4)
    if (self % ndims <= 5) fifth = alldims(5)

    if (self % ndims == 1) then
      allocate(self % r1(first))
    else if (self % ndims == 2) then
      allocate(self % r2(first, second))
    else if (self % ndims == 3) then
      allocate(self % r3(first, second, third))
    else if (self % ndims == 4) then
      allocate(self % r4(first, second, third, fourth))
    else if (self % ndims == 5) then ! MAXRANK
      allocate(self % r5(first, second, third, fourth, fifth))
    else
      !< unexpected
    endif
  end subroutine init


  pure function add_arrayContainerAllocatable(left, right) Result(total)
    !< addition of arrayCA + arrayC 
    class(shr_arrayContainerAllocatable), intent(in) :: left
    class(shr_arrayContainer), intent(in) :: right
    class(shr_arrayContainer), allocatable :: total !< output

    if (.not. allocated(total)) deallocate(total)
    allocate(shr_arrayContainerAllocatable :: total)

    !
    select type(rightArray => right)
    type is (shr_arrayContainerAllocatable)
      if (left % getSize() == 1 .and. right % getSize() == 1) then
        total = left % r1 + rightArray % r1
!      else if (left % getSize() == 2 .and. right % getSize() == 2) then
!        total = left % r2 + rightArray % r2
!      else if (left % getSize() == 3 .and. right % getSize() == 3) then
!        total = left % r3 + rightArray % r3
!      else if (left % getSize() == 4 .and. right % getSize() == 4) then
!        total = left % r4 + rightArray % r4
!      else if (left % getSize() == 5 .and. right % getSize() == 5) then !< MAXRANK
!        total = left % r5 + rightArray % r5
      else
        !< unexpected, inconsistency found
      endif
    class default
      !< unexpected, type not found
    end select

  end function add_arrayContainerAllocatable


  pure function add_array_raw_rsp_1(left, right) Result(total)
    !<
    class(shr_arrayContainerAllocatable), intent(in) :: left
    real(kind=sp), intent(in) :: right(:)
    class(shr_arrayContainer), allocatable :: total
    total = left % r1 + right
  end function add_array_raw_rsp_1


  pure function add_scalar_rsp(left, right) Result(total)
    !< add scalar value into array
    !< arrayCA = 24.1
    class(shr_arrayContainerAllocatable), intent(in) :: left 
    real(kind=sp), intent(in) :: right 
    class(shr_arrayContainer), allocatable :: total !< output
    if (left % getSize() == 1) then
      total = left % r1 + right
!    else if (left % getSize() == 2) then
!      total = left % r2 + right
!    MAXRANK
    else
      !< unexpected, inconsistency found
    endif
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
    if (self % getSize() == 1) then
      self % r1 = other
    else
      !< unexpected, inconsistency found
    endif
  end subroutine copy_array_raw_rsp_1


  pure subroutine copy_arrayContainer(self, other)
    !< Copy to current array container allocatable
    !< arrayCA = arrayC (arrayCA % r2 = arrayC % r2...) 
    class(shr_arrayContainerAllocatable), intent(inout) :: self
    class(shr_arrayContainer), intent(in) :: other

    select type(otherArray => other)
    type is (shr_arrayContainerAllocatable)
      if (self % getSize() == 1 .and. other % getSize() == 1) then
        self % r1 = otherArray % r1
!      else if (self % getSize() == 2 .and. other % getSize() == 2) then
!        self % r2 = otherArray % r2
      !< MAXRANK
      endif
    class default
      !< unexpected type found
    end select
  end subroutine copy_arrayContainer


  subroutine destroy_class(self)
    !< destroy class
    type(shr_arrayContainerAllocatable), intent(inout) :: self

    deallocate(self % dimensions)

    if (allocated(self % r1)) deallocate(self % r1)
    if (allocated(self % r2)) deallocate(self % r2)
    if (allocated(self % r3)) deallocate(self % r3)
    if (allocated(self % r4)) deallocate(self % r4)
    if (allocated(self % r5)) deallocate(self % r5) ! MAXRANK
  end subroutine destroy_class

end module SHR_arrayContainerAllocatable_mod
