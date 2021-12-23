!------------------------------------------------------------------------------
!						Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        :  shr_maskClusters_mod
!
!> @author
!> jurnix
!
! DESCRIPTION:
!>
!> Mask related subroutines
!>
!> Find groups of clusters.
!>
!> Clusters include the following conditions:
!> - only true gridcells allowed
!> - can be represented in 2d squared (grid)
!>
!> Clusters:
!> - Same mask dimensions as the original
!> - Only enabled found clusters
!------------------------------------------------------------------------------
module shr_maskClusters_mod

	use shr_error_mod, only: raiseError
	use shr_strings_mod, only: string
	use shr_mask_mod, only: shr_mask1d, shr_mask2d
	use shr_maskIndices_mod, only: shr_maskIndices_1d, shr_maskIndices_2d

	implicit none


  !< discover how many cluster and return each one
	type :: shr_maskClusters_1d
		type(shr_mask1d), allocatable :: mask
		type(shr_maskIndices_1d), allocatable :: mIndices(:)
		type(shr_mask1d), allocatable :: mClusters(:)
	contains
		procedure :: init => mask1dClusters_initialize
		procedure :: get => mask1dClusters_get
		procedure, private :: calculateIndices => mask1dClusters_calculateIndices ! shr_gridMask
		procedure, private :: count => mask1dClusters_count !< integer
		procedure :: getSize => mask1d_getSize
	end type shr_maskClusters_1d


	type :: shr_maskClusters_2d
		type(shr_mask2d), allocatable :: mask
		type(shr_maskIndices_2d), allocatable :: mIndices(:)
		type(shr_mask2d), allocatable :: mClusters(:)
	contains
		procedure :: init => mask2dClusters_initialize
		procedure :: get => mask2dClusters_get
		procedure, private :: calculateIndices => mask2dClusters_calculateIndices ! shr_gridMask
		procedure, private :: count => mask2dClusters_count !< integer
		procedure :: getSize => mask2d_getSize
	end type shr_maskClusters_2d

contains

	subroutine mask1dClusters_initialize(self, mask)
		!< initialize mask1dCluster with new mask
		class(shr_maskClusters_1d), intent(inout) :: self
		type(shr_mask1d), intent(in) :: mask

		integer :: nclusters, icluster
		type(shr_maskIndices_1d) :: mIdx
		logical, allocatable :: lmask(:)

		!< allow reuse
		if (allocated(self % mask)) deallocate(self % mask)
		if (allocated(self % mIndices)) deallocate(self % mIndices)
		if (allocated(self % mClusters)) deallocate(self % mClusters)


		allocate(self % mask, source = mask)
		lmask = self % mask % get()
		nclusters = self % count(lmask) !shr_mask_count_groups_indices_l1(self % mask)

		!< discover all clusters
		allocate(self % mClusters(nclusters))
		self % mIndices =  self % calculateIndices(lmask)

		!< each cluster found as new shr_mask
		do icluster = 1, nclusters
			mIdx = self % mIndices(icluster)
			self % mClusters(icluster) = self % mask % filter(mIdx)
		end do
	end subroutine mask1dClusters_initialize


	integer function mask1dClusters_count(self, lmask) result (ngroups)
		!< calculate (compute) how many cluters found in 'lmask'
		!< (wrap to shr_mask_count_groups_l1)
		class(shr_maskClusters_1d), intent(in) :: self
		logical, intent(in) :: lmask(:)
		ngroups = shr_mask_count_groups_l1(lmask)
	end function  mask1dClusters_count


	integer function mask1d_getSize(self)
		!<return the numbe of clusters found
		class(shr_maskClusters_1d), intent(in) :: self
		mask1d_getSize = size(self % mClusters)
	end function mask1d_getSize


	function mask1dClusters_calculateIndices(self, lmask) result (midxs)
		!< calculates each cluster from 'lmask'
		class(shr_maskClusters_1d), intent(in) :: self
		logical, intent(in) :: lmask(:)
		type(shr_maskIndices_1d), allocatable :: midxs(:)
		midxs = shr_mask_calc_groups_indices_l1(lmask)
	end function mask1dClusters_calculateIndices


	type(shr_mask1d) function mask1dClusters_get(self, ipos) result (m)
		!< returns a shr_mask at th 'ipos' th position
		class(shr_maskClusters_1d), intent(in) :: self
		integer, intent(in) :: ipos
		m = self % mClusters(ipos)
	end function mask1dClusters_get


	subroutine mask2dClusters_initialize(self, mask)
		!< initialize mask2dCluster with new mask
		class(shr_maskClusters_2d), intent(inout) :: self
		type(shr_mask2d), intent(in) :: mask

		integer :: nclusters, icluster
		type(shr_maskIndices_2d) :: mIdx
		logical, allocatable :: lmask(:,:)
		!type(string) :: tmpStr

		! allow reuse
		if ( allocated(self % mask)) deallocate(self % mask)
		if ( allocated(self % mIndices)) deallocate(self % mIndices)
		if ( allocated(self % mClusters)) deallocate(self % mClusters)

		allocate(self % mask, source = mask)
		lmask = self % mask % get()
		nclusters = self % count(lmask)

		!< discover all clusters
		allocate(self % mClusters(nclusters))
		self % mIndices = self % calculateIndices(lmask)

		!< each cluster found as new shr_mask
		do icluster = 1, nclusters
			!write(*,* ) "mask2dClusters_initialize:: icluster =", icluster
			mIdx = self % mIndices(icluster)
			!tmpStr = mIdx % toString()
			!write(*,* ) "mask2dClusters_initialize:: indices found =", tmpStr % toString()
			self % mClusters(icluster) = self % mask % filter(mIdx)
			!tmpStr = self % mClusters(icluster) % toString()
			!write(*,* ) "mask2dClusters_initialize:: indices found =", tmpStr % toString()
		end do
	end subroutine mask2dClusters_initialize


	integer function mask2dClusters_count(self, lmask) result (ngroups)
		!< calculate (compute) how many cluters found in 'lmask'
		!< (wrap to shr_mask_count_groups_l1)
		class(shr_maskClusters_2d), intent(in) :: self
		logical, intent(in) :: lmask(:,:)
		ngroups = shr_mask_count_groups_l2(lmask)
	end function  mask2dClusters_count


	integer function mask2d_getSize(self)
		!<return the numbe of clusters found
		class(shr_maskClusters_2d), intent(in) :: self
		mask2d_getSize = size(self % mClusters)
	end function mask2d_getSize


	function mask2dClusters_calculateIndices(self, lmask) result (midxs)
		!< calculates each cluster from 'lmask'
		class(shr_maskClusters_2d), intent(in) :: self
		logical, intent(in) :: lmask(:,:)
		type(shr_maskIndices_2d), allocatable :: midxs(:)
		midxs = shr_mask_calc_groups_indices_l2(lmask)
	end function mask2dClusters_calculateIndices


	type(shr_mask2d) function mask2dClusters_get(self, ipos) result (m)
		!< returns a shr_mask at th 'ipos' th position
		class(shr_maskClusters_2d), intent(in) :: self
		integer, intent(in) :: ipos
		!type(string) :: tmpStr
		!tmpStr = self % mClusters(ipos) % toString()
		!write(*,*) "shr_maskClusters_mod:: mClusters =", tmpStr % toString()
		m = self % mClusters(ipos)
	end function mask2dClusters_get


	integer function shr_mask_count_groups_l1(mask) result (ngroups)
		!< it returns how many groups (true) found in a 1d logical array
		!< T T F F T T = 2
		!< T F T F T F = 3
		!< F F T T F F = 1
		!< T F F F F T  = 2
		logical, intent(in) :: mask(:)
		integer :: ipos
		logical :: hasNewGroup
		logical :: currentStatus, prevStatus

		ngroups = 0
		hasNewGroup = mask(1)

		!< Has a group at the begining? then count it
		if (hasNewGroup) ngroups = ngroups + 1

		do ipos = 2, size(mask)
			currentStatus = mask(ipos)
			prevStatus = mask(ipos - 1)
			!< from F to T
			hasNewGroup = (currentStatus .and. .not. prevStatus)
			if (hasNewGroup) ngroups = ngroups + 1
		end do

	end function shr_mask_count_groups_l1


	integer function shr_mask_count_groups_l2(mask) result (ngroups)
		!< it returns how many groups (true) found in a 1d logical array
		!< (horizontal)
		!< T T F F T T = 2
		!< T T T T T T = 1
		!< T T T T T T
		!< T F F F F T = 2		-> Total  5
		logical, intent(in) :: mask(:,:)
		integer :: nRows, nCols
		integer :: iRow
		logical, allocatable :: isFullRow(:), rowMask(:)
		integer, allocatable :: groupsFoundByRow(:) !< non full lines
		integer :: nFullGroups
		nRows = size(mask, dim=1)
		nCols = size(mask, dim=2)
		ngroups = 0
		allocate(groupsFoundByRow(nRows))
		allocate(isFullRow(nRows))

		!< full lines into mask 1d
		do iRow = 1, nRows
			isFullRow(iRow) = all(mask(iRow,:))
		end do
		!< count grouped full lines
		nfullGroups = shr_mask_count_groups_l1(isFullRow)

		!< count non full lines
		groupsFoundByRow(:) = 0
		allocate(rowMask(nCols))
		do iRow = 1, nRows
			if (isFullRow(iRow)) cycle !< skip full line
			rowMask(:) = mask(iRow, :)
			groupsFoundByRow(iRow) = shr_mask_count_groups_l1(rowMask)
		end do
		ngroups = sum(groupsFoundByRow) + nfullGroups

	end function shr_mask_count_groups_l2


	function shr_mask_calc_groups_indices_l1(lmask) result (indices)
		!< get indices (start, end) relative to 'lmask' for each group found
		!< Naive algorithm
		!< T T F F T T = 1:2, 5:6
		logical, intent(in) :: lmask(:) !< row, cols
		type(shr_maskIndices_1d), allocatable :: indices(:)

		integer :: npos, ipos, lastPos
		logical :: prevStatus, currentStatus
		logical :: hasNewGroup, hasEndGroup
		logical :: isLastTrue, isLastGroupOpen
		integer :: iStart, iEnd
		integer :: ngroups, igroup
		integer, parameter :: UNDEF = -1

		igroup = 1
		ngroups = shr_mask_count_groups_l1(lmask)
		allocate(indices(ngroups))

		npos = size(lmask)

		!< full rows
		iStart = 1
		iEnd = npos
		lastPos = npos
		hasNewGroup = lmask(1)
		hasEndGroup = .false.

		if (hasNewGroup) then
			iStart = 1
			hasNewGroup = .false. !< reinit
		end if

		do ipos = 2, npos
			!< update loop status
			prevStatus = lmask(ipos-1)
			currentStatus = lmask(ipos)
			!write(*,*) "shr_mask_calc_groups_indices_l1:: start, status (prec, current)=", prevStatus, currentStatus

			!> transition from F to T
			if ((.not. prevStatus) .and. currentStatus) hasNewGroup = .true.
			!> transition from T to F
			if (prevStatus .and. (.not. currentStatus)) hasEndGroup = .true.

			!< register index
			if (hasNewGroup) then
				!write(*,*) "shr_mask_calc_groups_indices_l1:: hasnewGroup, ipos=", ipos
				iStart = ipos
			end if
			if (hasEndGroup) then
				!write(*,*) "shr_mask_calc_groups_indices_l1:: hasEndGroup, ipos, posEnd=", ipos, ipos-1
				iEnd = ipos-1
			end if

			!< save indices
			if (hasEndGroup) then
				!write(*,*) "pos:", iStart,":" , iEnd
				call indices(igroup) % init(iStart, iEnd)
				igroup = igroup + 1 !< next position
				iStart = UNDEF
				iEnd = UNDEF
			end if

			!< init values
			hasNewGroup = .false.
			hasEndGroup = .false.
		end do !< ipos = 2, npos

		!< last iteration?
		isLastGroupOpen = (iStart /= UNDEF)

		if (isLastGroupOpen) then
			isLastTrue = lmask(lastPos)
			if (isLastTrue) then
				!< close new group
				call indices(igroup) % init(iStart, lastPos)
			else
				call raiseError(__FILE__, "shr_mask_calc_groups_indices_l1", &
							"Inconsistency found")
			end if !< isLastTrue
		end if !< isLastGroupOpen
	end function shr_mask_calc_groups_indices_l1


	function shr_mask_calc_groups_indices_l2(lmask) result(mIndices)
		!< discover each group and create and new gridMask from it
		!< Naive algorithm
		logical, intent(in) :: lmask(:,:) !< row, cols
		type(shr_maskIndices_2d), allocatable :: mIndices(:) !< output

		logical, allocatable :: isFullRow(:)
		integer :: nRows, irow
		integer :: nCols
		integer :: ngroups, igroup
		type(shr_maskIndices_1d), allocatable :: fullRowIndex(:)
		type(shr_maskIndices_1d), allocatable :: colIndxs(:)
		type(shr_maskIndices_1d) :: fullColIndex, rowIndex
		integer :: idx, icol
		type(string) :: tmp

		igroup = 1
		ngroups = shr_mask_count_groups_l2(lmask)
		allocate(mIndices(ngroups))
		!< cols = 3
		!< x x x
		!< x x x rows=2
		nRows = size(lmask, dim=1) !< 4
		nCols = size(lmask, dim=2) !< 6
		allocate(isFullRow(nRows))
		!< shrink to rows
		do irow = 1, nRows
			isFullRow(irow) = all(lmask(irow,:))
			!write(*,*) "shr_mask_calc_groups_indices_l2:: row =", irow, isFullRow(irow), " -> ", lmask(irow,:)
		end do

		!write(*,*) "shr_mask_calc_groups_indices_l2:: full rows =", isFullRow

		!< characterize full rows with indices
		!< search them
		fullRowIndex = shr_mask_calc_groups_indices_l1(isFullRow)
		call fullColIndex % init(1, nCols)
		!< combine into 2d mask indices type
		do idx = 1, size(fullRowIndex)
			call mIndices(igroup) % init(fullColIndex, fullRowIndex(idx))
			!tmp = mIndices(igroup) % toString()
			!write(*,*) "shr_mask_calc_groups_indices_l2:: found full rows =", tmp % toString()
			igroup = igroup + 1 !< next position
		end do

		!< partial rows (no full rows)
		!< for each non full row, discover each group of T's
		do irow = 1, nRows
			if (isFullRow(irow)) cycle !< skip full row
			colIndxs = shr_mask_calc_groups_indices_l1(lmask(irow, :))
			!< current row indices
			call rowIndex % init(irow, irow)
			!< for each col found:
			do icol = 1, size(colIndxs)
				!< combine into 2d mask indices type
				call mIndices(igroup) % init(colIndxs(icol), rowIndex)
				igroup = igroup + 1 !< next position
			end do !< icol = 1, size(colIndxs)
		end do !< irow = 1, nRows

#ifndef NDEBUG
		!< consistency check (debug only)
 		if (igroup - 1 /= ngroups) then
			write(*,*) "Expected ",  ngroups, " but found ", (igroup-1)
			call raiseError(__FILE__, "shr_mask_calc_groups_indices_l2", &
					"Inconsistency found", &
					"The number of indices found does not match the expected")
		end if
#endif
	end function shr_mask_calc_groups_indices_l2


end module shr_maskClusters_mod