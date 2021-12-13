module shr_mask_test

	!use shr_precision_mod, only: sp
  use shr_testSuite_mod, only: testSuite
  use shr_mask_mod, only: shr_mask_calc_groups

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

    write(*,*) shr_mask_calc_groups([.true., .false.])
    call self % assert(shr_mask_calc_groups([.true., .false.]) == 1, &
            "shr_mask_calc_groups(T, F) .eq. 1 = T")

    write(*,*) shr_mask_calc_groups([.true., .false., .true., .false.])
    call self % assert(shr_mask_calc_groups([.true., .false., .true., .false.]) == 2, &
        "shr_mask_calc_groups(T, F, T, T) .eq. 2 = T")

    write(*,*) shr_mask_calc_groups([.false., .false., .false., .true.])
    call self % assert(shr_mask_calc_groups([.false., .false., .false., .true.]) == 1, &
        "shr_mask_calc_groups(F, F, F, T) .eq. 1 = T")

    write(*,*) shr_mask_calc_groups([.true., .true., .true., .false., .true.])
    call self % assert(shr_mask_calc_groups([.true., .true., .true., .false., .true.]) == 2, &
        "shr_mask_calc_groups(T, T, T, F, T) .eq. 2 = T")
  end subroutine defineTestCases

end module shr_mask_test