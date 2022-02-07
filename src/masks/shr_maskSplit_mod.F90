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
!> It splits the mask into several parts
!> - Each has the same amount of enabled mask cells
!> 
!------------------------------------------------------------------------------
module shr_maskSplit_mod

	use shr_error_mod, only: raiseError
	use shr_strings_mod, only: string

	use shr_mask_mod, only: shr_mask2d
	use shr_maskIterator_mod, only: shr_maskIterator


	implicit none

	public :: shr_maskSplit


	integer, parameter :: ROW_SHAPE_INDEX = 1
	integer, parameter :: COL_SHAPE_INDEX = 2


	type :: shr_maskSplit
		!< input
		integer, allocatable :: current, total
		type(shr_mask2d), allocatable :: mask
		!< output
		type(shr_mask2d), allocatable :: foundMasks(:)
	contains
		procedure :: init
		procedure :: calculate
		procedure :: get
	end type shr_maskSplit

contains

	subroutine init(self, current, total, mask)
		!< initialization
		class(shr_maskSplit), intent(inout) :: self
		integer, intent(in) :: current
		integer, intent(in) :: total
		type(shr_mask2d), intent(in) :: mask
	end subroutine init


	subroutine calculate(self)
		!< Computes all parts
		class(shr_maskSplit), intent(inout) :: self
		type(shr_maskIterator) :: miterator
		integer :: count

		nenabled = self % mask % count()

		count = 0
		do while(miterator % hasNext())
			lval = miterator % getNext()
			if (lval) then
				count = count + 1
			end if
		end do
	end subroutine calculate


	function get(self) result (masks)
		!< it returns splitted masks
		class(shr_maskSplit), intent(inout) :: self
		type(shr_maskSplit), allocatable :: masks

	!	masks  = self % foundMasks
	end function get

end module shr_maskSplit_mod