!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_maskClusters_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_maskClusters unit tests
!------------------------------------------------------------------------------
module shr_maskClusters_test

  use shr_testSuite_mod, only: testSuite
  use shr_strings_mod, only: string

  use shr_maskClusters_mod, only: shr_mask_calc_groups_indices_l1, shr_mask_calc_groups_indices_l2
  use shr_maskClusters_mod, only: shr_mask_count_groups_l1, shr_mask_count_groups_l2
  use shr_maskIndices_mod, only: shr_maskIndices_1d, shr_maskIndices_2d


  implicit none

  private
  public :: testSuiteMaskClusters

  type, extends(testSuite) :: testSuiteMaskClusters

  contains
    procedure :: define => defineTestCases
    procedure, private :: testMaskClusters1d
    procedure, private :: testMaskClusters2d
  end type
contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteMaskClusters), intent(inout) :: self

    call self % testMaskClusters1d()
    call self % testMaskClusters2d()
  end subroutine defineTestCases


  subroutine testMaskClusters1d(self)
    !< tests for shr_maskcluster_1d
    class(testSuiteMaskClusters), intent(inout) :: self

    logical :: mask1d(6)
    type(shr_maskIndices_1d) :: expIndices(2)
    type(shr_maskIndices_1d), allocatable :: mIndices(:)

    !< 1d mask
    !write(*,*) shr_mask_calc_SquaredGroups([.true., .false.])
    call self % assert(shr_mask_count_groups_l1([.true., .false.]) == 1, &
        "shr_mask_count_groups_l1(T, F) .eq. 1 = T")

    !write(*,*) shr_mask_calc_SquaredGroups([.true., .false., .true., .false.])
    call self % assert(shr_mask_count_groups_l1([.true., .false., .true., .false.]) == 2, &
        "shr_mask_count_groups_l1(T, F, T, T) .eq. 2 = T")

    !write(*,*) shr_mask_calc_SquaredGroups([.false., .false., .false., .true.])
    call self % assert(shr_mask_count_groups_l1([.false., .false., .false., .true.]) == 1, &
        "shr_mask_count_groups_l1(F, F, F, T) .eq. 1 = T")

    !write(*,*) shr_mask_calc_SquaredGroups([.true., .true., .true., .false., .true.])
    call self % assert(shr_mask_count_groups_l1([.true., .true., .true., .false., .true.]) == 2, &
        "shr_mask_count_groups_l1(T, T, T, F, T) .eq. 2 = T")


    !< 1d calc mask indices
    !< T T F F T T = 2
    mask1d = [.true., .true., .false., .false., .true. ,.true.]
    call expIndices(1) % init(1,2)
    call expIndices(2) % init(5,6)
    mIndices = shr_mask_calc_groups_indices_l1(mask1d)
    call self % assert(all(mIndices == expIndices), &
        "shr_mask_calc_groups_indices_l1(TTFFTT) .eq. [(1,2),(5,6)] = T")

    !< T T F F F T = 2
    mask1d = [.true., .true., .false., .false., .false. ,.true.]
    call expIndices(1) % init(1,2)
    call expIndices(2) % init(6,6)
    mIndices = shr_mask_calc_groups_indices_l1(mask1d)
    call self % assert(all(mIndices == expIndices), &
        "shr_mask_calc_groups_indices_l1(TTFFTT) .eq. [(1,2),(6,6)] = T")
  end subroutine testMaskClusters1d


  subroutine testMaskClusters2d(self)
    !<
    class(testSuiteMaskClusters), intent(inout) :: self
    logical :: mask2d(4, 6)

    type(shr_maskIndices_2d), allocatable :: mMatrixIndices(:)
    type(shr_maskIndices_2d) :: expMatrixIndices(5)

    integer :: istr
    type(string), allocatable :: tmpStrs(:)

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
    call self % assert(shr_mask_count_groups_l2(mask2d) == 5, &
        "shr_mask_count_groups_l2(...) .eq. 5 = T")

    !< (horizontal)
    !< T T F F T T = 2
    !< T T T T T F = 1
    !< T T T T T T = 1
    !< T F F F F T = 2		-> Total  6
    mask2d(1,:) = [.true., .true., .false., .false., .true., .true.]
    mask2d(2,:) = [.true., .true., .true., .true., .true., .false.]
    mask2d(3,:) = [.true., .true., .true., .true., .true., .true.]
    mask2d(4,:) = [.true., .false., .false., .false., .false., .true.]
    call self % assert(shr_mask_count_groups_l2(mask2d) == 6, &
        "shr_mask_count_groups_l2(...) .eq. 6 = T")

    !< 2d calc mask indices
    !< (horizontal)
    !< T T F F T T -> 2
    !< T T T T T T -> 1
    !< T T T T T T
    !< T F F F F T -> 2		-> Total  5
    mask2d(1,:) = [.true., .true., .false., .false., .true., .true.]
    mask2d(2,:) = [.true., .true., .true., .true., .true., .true.]
    mask2d(3,:) = [.true., .true., .true., .true., .true., .true.]
    mask2d(4,:) = [.true., .false., .false., .false., .false., .true.]
    call expMatrixIndices(2) % init([1,2,1,1])
    call expMatrixIndices(3) % init([5,6,1,1])
    call expMatrixIndices(1) % init([1,6,2,3])
    call expMatrixIndices(4) % init([1,1,4,4])
    call expMatrixIndices(5) % init([6,6,4,4])
    mMatrixIndices = shr_mask_calc_groups_indices_l2(mask2d)

    call self % assert(size(mMatrixIndices) == 5, &
        "shr_mask_calc_groups_indices_l2(TTFFTT,T,T,T,FFFFT) size .eq. 5 = T")

!    allocate(tmpStrs(size(mMatrixIndices)))
!    do istr = 1, size(mMatrixIndices) ! debug only
      !write(*,*) "col allocated? ", allocated(mMatrixIndices(istr) % col)
      !write(*,*) "row allocated? ", allocated(mMatrixIndices(istr) % row)
      !write(*,*) "col: ", mMatrixIndices(istr) % col % start, mMatrixIndices(istr) % col % end
      !write(*,*) "row: ", mMatrixIndices(istr) % row % start, mMatrixIndices(istr) % row % end
!      tmpStrs = mMatrixIndices(istr) % toString()
!      write(*,*) "MatrixIndices: ", tmpStrs(1) % toString()
!      tmpStrs = expMatrixIndices(istr) % toString()
!      write(*,*) "Expected: ", tmpStrs(1) % toString()
!      tmpStrs = mMatrixIndices(istr) % toString()
!      write(*,*) "same? ", mMatrixIndices(istr) == expMatrixIndices(istr)
!    end do
    call self % assert(all(mMatrixIndices == expMatrixIndices), &
        "shr_mask_calc_groups_indices_l2(TTFFTT,T,T,T,FFFFT) .eq. (...) = T")
  end subroutine testMaskClusters2d

end module shr_maskClusters_test