module shr_mask_test

	!use shr_precision_mod, only: sp
  use shr_testSuite_mod, only: testSuite
  use shr_mask_mod, only: shr_mask_calc_SquaredGroups

  implicit none

  private
  public :: testSuiteMask

  type, extends(testSuite) :: testSuiteMask

  contains
    procedure :: define => defineTestCases
  end type
contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteMask), intent(inout) :: self
    logical :: mask2d(4, 6)

    !< 1d mask
    !write(*,*) shr_mask_calc_SquaredGroups([.true., .false.])
    call self % assert(shr_mask_calc_SquaredGroups([.true., .false.]) == 1, &
            "shr_mask_calc_groups(T, F) .eq. 1 = T")

    !write(*,*) shr_mask_calc_SquaredGroups([.true., .false., .true., .false.])
    call self % assert(shr_mask_calc_SquaredGroups([.true., .false., .true., .false.]) == 2, &
        "shr_mask_calc_groups(T, F, T, T) .eq. 2 = T")

    !write(*,*) shr_mask_calc_SquaredGroups([.false., .false., .false., .true.])
    call self % assert(shr_mask_calc_SquaredGroups([.false., .false., .false., .true.]) == 1, &
        "shr_mask_calc_groups(F, F, F, T) .eq. 1 = T")

    !write(*,*) shr_mask_calc_SquaredGroups([.true., .true., .true., .false., .true.])
    call self % assert(shr_mask_calc_SquaredGroups([.true., .true., .true., .false., .true.]) == 2, &
        "shr_mask_calc_groups(T, T, T, F, T) .eq. 2 = T")

    !< 2d mask
    !< (horizontal)
    !< T T F F T T = 2
    !< T T T T T T = 1
    !< T T T T T T
    !< T F F F F T = 2		-> Total  5
    mask2d(1,:) = [.true., .true., .false., .false., .true., .true.]
    mask2d(2,:) = [.true., .true., .true., .true., .true., .true.]
    mask2d(3,:) = [.true., .true., .true., .true., .true., .true.]
    mask2d(4,:) = [.true., .false., .false., .false., .false., .true.]
    call self % assert(shr_mask_calc_SquaredGroups(mask2d) == 5, &
        "shr_mask_calc_groups_l2(...) .eq. 5 = T")

    !< (horizontal)
    !< T T F F T T = 2
    !< T T T T T F = 1
    !< T T T T T T = 1
    !< T F F F F T = 2		-> Total  6
    mask2d(1,:) = [.true., .true., .false., .false., .true., .true.]
    mask2d(2,:) = [.true., .true., .true., .true., .true., .false.]
    mask2d(3,:) = [.true., .true., .true., .true., .true., .true.]
    mask2d(4,:) = [.true., .false., .false., .false., .false., .true.]
    call self % assert(shr_mask_calc_SquaredGroups(mask2d) == 6, &
        "shr_mask_calc_groups_l2(...) .eq. 6 = T")
  end subroutine defineTestCases

end module shr_mask_test