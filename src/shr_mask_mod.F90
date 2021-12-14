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

	interface shr_mask_calc_groups
		module procedure shr_mask_calc_groups_l1
	end interface shr_mask_calc_groups

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

end module shr_mask_mod