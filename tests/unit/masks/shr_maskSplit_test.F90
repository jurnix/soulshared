!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_maskSplit_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_maskSplit unit tests
!------------------------------------------------------------------------------
module shr_maskEnabledEqualSplit_test

  use shr_testSuite_mod, only: testSuite

  use shr_maskEnabledEqualSplit_mod, only: shr_maskEnabledEqualSplit
  use shr_mask_mod, only: shr_mask2d
  use shr_strings_mod, only: string !< debug only


  implicit none

  private
  public :: testSuiteMaskEnabledEqualSplit

  type, extends(testSuite) :: testSuiteMaskEnabledEqualSplit

  contains
    procedure :: define => defineTestCases
    procedure, private :: testCaseMaskSplitFullEnabled
    procedure, private :: testCaseMaskSplitSomeEnabled
  end type
contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteMaskEnabledEqualSplit), intent(inout) :: self

    !< basic subroutines
    call self % testCaseMaskSplitFullEnabled()
    call self % testCaseMaskSplitSomeEnabled()
  end subroutine defineTestCases


  subroutine testCaseMaskSplitFullEnabled(self)
    !< split a full mask (unit test)
    !> (16 gcs)
    !> x x x x  3 parts  	x x x x		 - - - -		 - - - -
    !> x x x x  			-> 	x x	- -	-> - - x x  -> - - - -
    !> x x x x						- - - -		 x x x -		 - - - x
    !> x x x x						- - - -		 - - - -		 x x x x
    class(testSuiteMaskEnabledEqualSplit), intent(inout) :: self
    type(shr_maskEnabledEqualSplit) :: maskSplit
    type(shr_mask2d) :: fullMask

    type(shr_mask2d) :: expMasks(3)
    type(shr_mask2d), allocatable :: foundMasks(:)
    logical :: lmask(4,4)

    !< setup
    lmask = .true.
    call fullMask % init(lmask)
    call maskSplit % init(3, fullMask)
    !< build expected output
    lmask = .false.
    lmask(1,:) = .true.
    lmask(2,1:2) = .true.
    call expMasks(1) % init(lmask)
    lmask = .false.
    lmask(2,3:4) = .true.
    lmask(3,1:3) = .true.
    call expMasks(2) % init(lmask)
    lmask = .false.
    lmask(3,4) = .true.
    lmask(4,:) = .true.
    call expMasks(3) % init(lmask)

    call maskSplit % calculate()
    foundMasks = maskSplit % get()
    call self % assert(size(foundMasks) == 3, &
          "maskSplit(T...T) size .eq. 3 = T")
    call self % assert(foundMasks(1) == expMasks(1), &
            "maskSplit(TT..TT) % get(1) .eq. = T")
    call self % assert(foundMasks(2) == expMasks(2), &
        "maskSplit(TT..TT) % get(2) .eq. = T")
    call self % assert(foundMasks(3) == expMasks(3), &
        "maskSplit(TT..TT) % get(3) .eq. = T")

  end subroutine testCaseMaskSplitFullEnabled


  subroutine testCaseMaskSplitSomeEnabled(self)
    !< split a mask with some enabled gcs (unit test)
    !> x - - -  3 parts  	x x x x			 - - - -			- - - -
    !> - - x x  			-> 	x x x x		-> - - - -   -> - - - -
    !> - - x -						- - - -			 x x x x			- - - -
    !> x - x x						- - - -			 x - - -			-	x	x x
    class(testSuiteMaskEnabledEqualSplit), intent(inout) :: self
    type(shr_maskEnabledEqualSplit) :: maskSplit
    type(shr_mask2d) :: fullMask

    type(shr_mask2d) :: expMasks(3)
    type(shr_mask2d), allocatable :: foundMasks(:)
    logical :: lmask(4,4)
    type(string) :: tmp

    !< setup
    lmask = .false.
    lmask(1,1) = .true.
    lmask(2,3:4) = .true.
    lmask(3,3) = .true.
    lmask(4,:) = [.true., .false., .true. , .true.]
    call fullMask % init(lmask)
    call maskSplit % init(3, fullMask)

    !< build expected output
    lmask = .false.
    lmask(1:2,:) = .true.
    call expMasks(1) % init(lmask)
    lmask = .false.
    lmask(3,:) = .true.
    lmask(4,1) = .true.
    call expMasks(2) % init(lmask)
    lmask = .false.
    lmask(4,2:4) = .true.
    call expMasks(3) % init(lmask)

    call maskSplit % calculate()
    foundMasks = maskSplit % get()

    call self % assert(size(foundMasks) == 3, &
        "maskSplit(T...T) size .eq. 3 = T")
    tmp = foundMasks(1) % toString()
    write(*,*) "foundMasks(1) =", tmp % toString()
    call self % assert(foundMasks(1) == expMasks(1), &
        "maskSplit(TT..TT) % get(1) .eq. = T")

    tmp = foundMasks(2) % toString()
    write(*,*) "foundMasks(2) =", tmp % toString()
    call self % assert(foundMasks(2) == expMasks(2), &
        "maskSplit(TT..TT) % get(2) .eq. = T")

    tmp = foundMasks(3) % toString()
    write(*,*) "foundMasks(3) =", tmp % toString()
    call self % assert(foundMasks(3) == expMasks(3), &
        "maskSplit(TT..TT) % get(3) .eq. = T")
  end subroutine testCaseMaskSplitSomeEnabled


end module shr_maskEnabledEqualSplit_test