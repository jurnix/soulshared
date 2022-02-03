!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_mask_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> mask unit tests
!------------------------------------------------------------------------------
module shr_mask_test
  use shr_testSuite_mod, only: testSuite

  use shr_mask_mod, only: shr_mask1d, shr_mask2d
  use shr_maskIndices_mod, only: shr_maskIndices_1d, shr_maskIndices_2d
  !use shr_strings_mod, only: string

  implicit none

  private
  public :: testSuiteMask

  type, extends(testSuite) :: testSuiteMask

  contains
    procedure :: define => defineTestCases
    procedure :: testMask1d
    procedure :: testMask2d
  end type
contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteMask), intent(inout) :: self

    call self % testMask1d()
    call self % testMask2d()
  end subroutine defineTestCases


  subroutine testMask1d(self)
    !< shr_mask1d unit tests
    class(testSuiteMask), intent(inout) :: self

    type(shr_mask1d) :: m1, mfiltered
    type(shr_maskIndices_1d) :: idx

    !procedure :: mask1d_initialize_bySize!(4)
    call m1 % init(5, default = .false.)
    call self % assert( .true., "m1 % init(5, false) = T" )

    !procedure :: mask1d_initialize_byArray!(4)
    call m1 % init([.true., .false., .true.])
    call self % assert( .true., "m1 % init(TFT, true) = T" )

    !procedure :: getSize => mask1d_getSize
    call m1 % init([.true., .false., .true.])
    call self % assert( m1 % getSize() == 3, &
        "m1(TFT) % getSize() .eq. 3 = T" )

    !procedure :: get => mask1d_get !< get mask
    call m1 % init([.true., .false., .true.])
    idx = shr_maskIndices_1d(2,3)
    call self % assert( all(m1 % get(idx) .eqv. [.false., .true.]), &
        "m1(TFT) % get([2,3]) .eq. (FT) = T" )

    !procedure :: set => mask1d_set !< set mask
    call m1 % init([.true., .false., .true.])
    idx = shr_maskIndices_1d(2,3)
    call m1 % set([.false., .false.], idx)
    call self % assert( all(m1 % get(idx) .eqv. [.false., .false.]), &
        "m1(TFT) % set([2,3]) .eq. (FF) = T" )

    !procedure :: filter => mask1d_filter !< new mask with selected indices
    call m1 % init([.true., .false., .true.])
    idx = shr_maskIndices_1d(3,3)
    mfiltered = m1 % filter(idx)
    call self % assert( all(mfiltered % get() .eqv. [.false., .false., .true.]), &
        "m1(TFT) % filter([3,3]) .eq. (FFT) = T" )
  end subroutine testMask1d


  subroutine testMask2d(self)
    !< shr_mask2d unit tests
    class(testSuiteMask), intent(inout) :: self

    type(shr_mask2d) :: m2, mfiltered, m1
    type(shr_maskIndices_2d) :: idx
    logical :: rmask(2,3), smask(2,2)
    logical, allocatable :: tmp(:,:), expMask(:,:)

    !procedure :: mask2d_initialize_bySize!(4)
    call m2 % init(2, 3, default = .false.)
    call self % assert( .true., "m2 % init(2,3,false) = T" )

    !procedure :: mask2d_initialize_byArray!(4)
    rmask(1,:) = [.true., .true., .true.]
    rmask(2,:) = [.false., .false., .false.]
    call m2 % init(rmask)
    call self % assert( .true., "m2 % init([TTT,FFF]) = T" )

    !procedure :: getSize => mask2d_getSize
    rmask(1,:) = [.true., .true., .true.]
    rmask(2,:) = [.false., .false., .false.]
    call m2 % init(rmask)
    call self % assert(all(m2 % getSize() == [2,3]), &
          "m2 % getSize() .eq. (2,3) = T" )


    !procedure :: get => mask2d_get
    call idx % init(1,2,2,3) !<row, col
    rmask(1,:) = [.true., .false., .true.]
    rmask(2,:) = [.false., .false., .false.]
    call m2 % init(rmask)
    tmp = m2 % get(idx)
    allocate(expMask(2,2))
    expMask(1,:) = [.false., .true.]
    expMask(2,:) = [.false., .false.]
    call self % assert(all(tmp .eqv. expMask), &
        "m2(TFT, FFF) % get() .eq. ((FF),(TF)) = T" )
    deallocate(expMask)


    !procedure :: set => mask2d_set
    call idx % init(1,2,2,3)
    rmask(1,:) = [.true., .false., .true.]
    rmask(2,:) = [.false., .false., .false.]
    call m2 % init(rmask)

    smask(1,:) = [.true., .true.]
    smask(2,:) = [.true., .false.]
    call m2 % set(smask, idx)

    tmp = m2 % get(idx)
    allocate(expMask(2,2))
    expMask(1,:) = [.true.,.true.]
    expMask(2,:) = [.true., .false.]
    call self % assert(all(tmp .eqv. expMask), &
          "m2(TFT,FFF) % set(TT,TF) .eq. ((TTT),(FTF)) = T" )
    deallocate(expMask)

    !procedure :: filter => mask2d_filter !< new mask with selected indices
    call idx % init(1,1,2,3)
    rmask(1,:) = [.true., .false., .true.]
    rmask(2,:) = [.false., .false., .false.]
    call m2 % init(rmask)

    mfiltered = m2 % filter(idx)
    allocate(expMask(2,3))
    expMask(1,:) = [.false., .false., .true.]
    expMask(2,:) = [.false., .false., .false.]

    call self % assert( all(mfiltered % get() .eqv. expMask), &
        "m2(TFT,FFF) % filter([1,1,2,3]) .eq. (FFT,FFF) = T" )

    ! procedure :: mask2d_eq
    !< same object
    rmask(1,:) = [.true., .false., .true.]
    rmask(2,:) = [.false., .false., .false.]
    call m2 % init(rmask)
    call self % assert( m2 == m2 , "m2 .eq. m2 = T" )

    !< different object (same mask)
    rmask(1,:) = [.true., .false., .true.]
    rmask(2,:) = [.false., .false., .false.]
    call m2 % init(rmask)
    call m1 % init(rmask)
    call self % assert( m2 == m1 , "m2 .eq. m1 = T" )

    !< different object (same mask)
    rmask(1,:) = [.true., .false., .true.]
    rmask(2,:) = [.false., .false., .false.]
    call m2 % init(rmask)
    rmask(1,:) = [.true., .false., .true.]
    rmask(2,:) = [.false., .true., .false.]
    call m1 % init(rmask)
    call self % assert(.not. m2 == m1 , "m2 .eq. m1 = F" )

    !< count
    rmask(1,:) = [.true., .false., .true.]
    rmask(2,:) = [.false., .false., .false.]
    call m2 % init(rmask)
    call self % assert( m2 % count() == 2 , "m2 % count(TFTFFF) .eq. 2 = T" )
  end subroutine testMask2d


end module shr_mask_test