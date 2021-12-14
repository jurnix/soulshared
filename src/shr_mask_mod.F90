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

	implicit none

	public :: shr_mask_calc_SquaredGroups

	interface shr_mask_calc_SquaredGroups
		module procedure shr_mask_calc_groups_l1, 	shr_mask_calc_groups_l2
	end interface shr_mask_calc_SquaredGroups

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

end module shr_mask_mod