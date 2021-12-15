!------------------------------------------------------------------------------
!						Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        :  shr_mask_mod
!
!> @author
!> jurnix
!
! DESCRIPTION:
!>
!> Mask related subroutines
!> 
!------------------------------------------------------------------------------
module shr_mask_mod

	use shr_error_mod, only: raiseError
	use shr_maskIndices_mod, only: shr_maskIndices_1d, shr_maskIndices_2d

	implicit none

	public :: shr_mask_calc_SquaredGroups, shr_mask_calc_groups_indices


	interface shr_mask_calc_SquaredGroups
		module procedure shr_mask_calc_groups_l1, 	shr_mask_calc_groups_l2
	end interface shr_mask_calc_SquaredGroups

	interface shr_mask_calc_groups_indices
		module procedure shr_mask_calc_groups_indices_l1, shr_mask_calc_groups_indices_l2
	end interface shr_mask_calc_groups_indices

contains

	integer function shr_mask_calc_groups_l1(mask) result (ngroups)
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

	end function shr_mask_calc_groups_l1


	integer function shr_mask_calc_groups_l2(mask) result (ngroups)
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
		nfullGroups = shr_mask_calc_SquaredGroups(isFullRow)

		!< count non full lines
		groupsFoundByRow(:) = 0
		allocate(rowMask(nCols))
		do iRow = 1, nRows
			if (isFullRow(iRow)) cycle !< skip full line
			rowMask(:) = mask(iRow, :)
			groupsFoundByRow(iRow) = shr_mask_calc_SquaredGroups(rowMask)
		end do
		ngroups = sum(groupsFoundByRow) + nfullGroups

	end function shr_mask_calc_groups_l2


	function shr_mask_calc_groups_indices_l1(lmask) result (indices)
		!< get indices (start, end) relative to 'lmask' for each group found
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
		ngroups = shr_mask_calc_SquaredGroups(lmask)
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
			write(*,*) "shr_mask_calc_groups_indices_l1:: start, status (prec, current)=", prevStatus, currentStatus

			!> transition from F to T
			if ((.not. prevStatus) .and. currentStatus) hasNewGroup = .true.
			!> transition from T to F
			if (prevStatus .and. (.not. currentStatus)) hasEndGroup = .true.

			!< register index
			if (hasNewGroup) then
				write(*,*) "shr_mask_calc_groups_indices_l1:: hasnewGroup, ipos=", ipos
				iStart = ipos
			end if
			if (hasEndGroup) then
				write(*,*) "shr_mask_calc_groups_indices_l1:: hasEndGroup, ipos, posEnd=", ipos, ipos-1
				iEnd = ipos-1
			end if

			!< save indices
			if (hasEndGroup) then
				write(*,*) "pos:", iStart,":" , iEnd
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
		logical, intent(in) :: lmask(:,:) !< row, cols
		type(shr_maskIndices_2d), allocatable :: mIndices(:) !< output

		logical, allocatable :: isFullRow(:)
		integer :: nRows, irow
		integer :: ngroups, igroup
		type(shr_maskIndices_1d), allocatable :: fullColIndex(:)
		type(shr_maskIndices_1d), allocatable :: colIndxs(:)
		type(shr_maskIndices_1d) :: fullRowIndex, rowIndex
		integer :: idx, icol

		igroup = 0
		ngroups = shr_mask_calc_SquaredGroups(lmask)
		allocate(mIndices(ngroups))

		nRows = size(lmask, dim=2)
		!< shrink to rows
		do irow = 1, nRows
			isFullRow = all(lmask(irow, :))
		end do

		!< characterize full rows with indices
		fullColIndex = shr_mask_calc_groups_indices_l1(isFullRow)
		call fullRowIndex % init(1, nRows)
		!< combine into 2d mask indices type
		do idx = 1, size(fullColIndex)
			call mIndices(igroup) % init(fullColIndex(idx), fullRowIndex)
			igroup = igroup + 1 !< next position
		end do

		!< slim rows
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
	end function shr_mask_calc_groups_indices_l2

end module shr_mask_mod