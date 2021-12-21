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
  use shr_maskClusters_mod, only: shr_maskClusters_1d, shr_maskClusters_2d
  use shr_maskIndices_mod, only: shr_maskIndices_1d, shr_maskIndices_2d
  use shr_mask_mod, only: shr_mask1d, shr_mask2d


  implicit none

  private
  public :: testSuiteMaskClusters

  type, extends(testSuite) :: testSuiteMaskClusters

  contains
    procedure :: define => defineTestCases
    procedure, private :: testMaskClusters1d
    procedure, private :: testMaskClusters2d

    procedure, private :: testMaskClusters1dClasses
    procedure, private :: testMaskClusters2dClasses
  end type
contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteMaskClusters), intent(inout) :: self

    !< basic subroutines
    call self % testMaskClusters1d()
    call self % testMaskClusters2d()

    !< classes
    call self % testMaskClusters1dClasses()
    call self % testMaskClusters2dClasses()
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
    !< test cases for mask clusters 2d related subroutines
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
    call expMatrixIndices(2) % init(1,1,1,2)
    call expMatrixIndices(3) % init(1,1,5,6)
    call expMatrixIndices(1) % init(2,3,1,6)
    call expMatrixIndices(4) % init(4,4,1,1)
    call expMatrixIndices(5) % init(4,4,6,6)
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


  subroutine testMaskClusters1dClasses(self)
    !< test cases for maskClusters_1d class
    class(testSuiteMaskClusters), intent(inout) :: self
    type(shr_maskClusters_1d) :: c
    type(shr_mask1d) :: mask
    type(shr_mask1d) :: mfirst, msecond !< results
    logical :: lmask(6)

    lmask = [.true., .false., .false., .true., .true., .true.]
    call mask % init(lmask)
    call c % init(mask)

    !procedure :: init => mask1dClusters_initialize
    call self % assert(.true., "c % init(TFFTTT) = T")

    !procedure :: getSize => mask1d_getSize
    call self % assert(c % getSize() == 2, "c(TFFTTT) % getSize() .eq. 2 = T")

    !procedure :: get => mask1dClusters_get
    ! create c
    lmask = [.true., .false., .false., .true., .true., .true.]
    call mask % init(lmask)
    call c % init(mask)

    ! create mfirst
    lmask = [.true., .false., .false., .false., .false., .false.]
    !call mask % init(lmask)
    call mfirst % init(lmask)

    call self % assert(c % get(1) == mfirst, &
        "c(TFFTTT) % get(1) .eq. shr_mask(TFFFFF) = T")

    ! create msecond
    lmask = [.false., .false., .false., .true., .true., .true.]
    !call mask % init(lmask)
    call msecond % init(lmask)

    call self % assert(c % get(2)  == msecond, &
        "c(TFFTTT) % get(2) .eq. shr_mask(FFFTTT) = T")
  end subroutine testMaskClusters1dClasses


  subroutine testMaskClusters2dClasses(self)
    !< test cases for maskClusters_2d class
    class(testSuiteMaskClusters), intent(inout) :: self
    type(shr_maskClusters_2d) :: c
    type(shr_mask2d) :: mask
    type(shr_mask2d) :: mfirst, msecond, mthrid, mfourth, mfifth, msixth
    logical :: lmask(4,6) !< row, cols
    type(shr_mask2d) :: tmpMask
    type(string) :: tmpStr

    lmask(1,:) = [.true., .true., .false., .false., .true., .true.] ! -> 2
    lmask(2,:) = [.true., .true., .true., .true., .true., .false.] ! -> 1
    lmask(3,:) = [.true., .true., .true., .true., .true., .true.] ! -> 1
    lmask(4,:) = [.true., .false., .false., .false., .false., .true.] ! -> 2
    call mask % init(lmask)

    !procedure :: init => mask2dClusters_initialize
    call c % init(mask)
    call self % assert(.true., "c % init(...) = T")

    !procedure :: getSize => mask2d_getSize
    call self % assert(c % getSize() == 6, "c % geSize() .eq. 6 = T")

    !procedure :: get => mask2dClusters_get
    !< 1st
    lmask(1,:) = [.true., .true., .false., .false., .true., .true.] ! -> 2
    lmask(2,:) = [.true., .true., .true., .true., .true., .false.] ! -> 1
    lmask(3,:) = [.true., .true., .true., .true., .true., .true.] ! -> 1
    lmask(4,:) = [.true., .false., .false., .false., .false., .true.] ! -> 2
    call mask % init(lmask)
    call c % init(mask)
    lmask = .false. !< reuse
    lmask(1,:) = [.true., .true., .false., .false., .false., .false.] ! -> 2
    call mfirst % init(lmask)
    tmpMask = c % get(2)
    tmpStr = tmpMask % toString()
    write(*,*) "shr_maskClusters_test:: found = ", tmpStr % toString()
    write(*,*) "shr_maskClusters_test:: expected(1) = ", lmask(1,:)
    write(*,*) "shr_maskClusters_test:: expected(2) = ", lmask(2,:)
    write(*,*) "shr_maskClusters_test:: expected(3) = ", lmask(3,:)
    write(*,*) "shr_maskClusters_test:: expected(4) = ", lmask(4,:)
    call self % assert(tmpMask == mfirst, "c % get(2) .eq. mask(TTFFFF,...) = T")

    !< 2nd
    lmask = .false.
    lmask(1,:) = [.false., .false., .false., .false., .true., .true.] ! -> 2
    call msecond % init(lmask)
    call self % assert(c % get(3) == msecond, "c % get(3) .eq. mask(FFFFTT,...) = T")

    !< 3rd
    lmask = .false.
    lmask(2,:) = [.true., .true., .true., .true., .true., .false.] ! -> 1
    call mthrid % init(lmask)
    call self % assert(c % get(4) == mthrid, "c % get(4) .eq. mask(...,TTTTTF,...) = T")

    !< 4th
    lmask = .false.
    lmask(3,:) = [.true., .true., .true., .true., .true., .true.] ! -> 1
    call mfourth % init(lmask)
    call self % assert(c % get(1) == mfourth, "c % get(4) .eq. mask(...,TTTTTT,...) = T")

    !< 5th
    lmask = .false.
    lmask(4,:) = [.true., .false., .false., .false., .false., .false.] ! -> 1
    call mfifth % init(lmask)
    call self % assert(c % get(5) == mfifth, "c % get(5) .eq. mask(...,TFFFFF,...) = T")

    !< 6th
    lmask = .false.
    lmask(4,:) = [.false., .false., .false., .false., .false., .true.] ! -> 1
    call msixth % init(lmask)
    call self % assert(c % get(6) == msixth, "c % get(6) .eq. mask(...,FFFFFT,...) = T")

  end subroutine testMaskClusters2dClasses

end module shr_maskClusters_test