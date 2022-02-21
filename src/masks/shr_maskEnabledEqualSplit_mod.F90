!------------------------------------------------------------------------------
!						Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        :  shr_maskSplit_mod
!
!> @author
!> jurnix
!
! DESCRIPTION:
!>
!> It splits the mask into several parts
!< - Horizontal/row order
!> - Each has the same amount of enabled mask cells
!> - Inbetween gridcells are considered out of scope
!>
!>
!> (16 gcs)
!> x x x x  3 parts  	x x x x		 - - - -		 - - - -
!> x x x x  			-> 	x x	- -	-> - - x x  -> - - - -
!> x x x x						- - - -		 x x x -		 - - - x
!> x x x x						- - - -		 - - - -		 x x x x
!>
!> (13 gcs)
!> x - x x  3 parts  	x x x x      - - - - 			- - - -
!> x x x x  			-> 	x x - -		-> - - x x  ->  - - - -
!> x - x x						- - - -			 x x x -  		- - - x
!> x x - x					  - - - -      - - - -			x	x	x x
!>
!> (7 gcs)
!> x - - -  3 parts  	x x x x			 - - - -			- - - -
!> - - x x  			-> 	x x x x		-> - - - -   -> - - - -
!> - - x -						- - - -			 x x x x			- - - -
!> x - x x						- - - -			 x - - -			-	x	x x
!> 
!------------------------------------------------------------------------------
module shr_maskEnabledEqualSplit_mod

	use shr_error_mod, only: raiseError
	use shr_strings_mod, only: string

	use shr_arrayIndices_mod, only: shr_arrayGridcellIndex
	use shr_mask_mod, only: shr_mask2d
	use shr_mask_mod, only: ROW_SHAPE_INDEX, COL_SHAPE_INDEX
	use shr_maskIndices_mod, only: shr_maskIndices_2d
	use shr_maths_mod, only: absoluteDivision


	implicit none

	public :: shr_maskEnabledEqualSplit


	type :: shr_maskEnabledEqualSplit
		!< input
		integer, allocatable :: total
		type(shr_mask2d), allocatable :: mask
		!< output
		type(shr_mask2d), allocatable :: parts(:)
	contains
		procedure :: init
		procedure :: calculate
		procedure :: get
	end type shr_maskEnabledEqualSplit

contains

	subroutine init(self, total, mask)
		!< initialization
		class(shr_maskEnabledEqualSplit), intent(inout) :: self
		integer, intent(in) :: total
		type(shr_mask2d), intent(in) :: mask
		allocate(self % mask)
		self % mask = mask
		allocate(self % total)
		self % total = total

	end subroutine init


	subroutine calculate(self)
		!< Computes all parts
		class(shr_maskEnabledEqualSplit), intent(inout) :: self

		!integer :: i
		integer :: nrows, ncols
		integer :: nEnabled
		integer :: count
		logical, allocatable :: lmask(:,:)
		integer :: dims(2)
		integer, allocatable :: nparts(:)

		type(shr_arrayGridcellIndex) :: cArrayIndex
		type(shr_mask2d), allocatable :: gcsParts(:)

		integer :: ipart, irow, icol
		integer :: currentPart

		!< equal partition
		!< distribute remaining values equally
		!< 11 into 4 parts -> 3, 3, 3, 2
		nenabled = self % mask % count()
		nparts = absoluteDivision(nenabled, self % total)

		lmask = self % mask % get()
		dims = self % mask % getShape()
		nrows = dims(ROW_SHAPE_INDEX)
		ncols = dims(COL_SHAPE_INDEX)

		!< initialize output
		allocate(self % parts(self % total))
		do ipart = 1, self % total
			!< all false by default
			call self % parts(ipart) % init(nrows, ncols, default = .false.)
		end do

		!< discover each gridcell for each part
		currentPart = 1
		count = 0

		do irow = 1, nrows
			do icol = 1, ncols
				cArrayIndex % row = irow
				cArrayIndex % col = icol
				!< enable each gridcell for current part
				call self % parts(currentPart) % set(cArrayIndex, .true.)

				!< count found enabled gridcells
				if (lmask(irow, icol)) then
					count = count + 1
				end if

				!< is part at the end?
				if (nparts(currentPart) == count ) then
					!< is the very last group?
					if (size(self % parts) > currentPart) then
					  !< no, so move to next part
					  currentPart = currentPart + 1
					end if
					count = 0
				end if
			enddo !< for each mask column
		enddo !< for each mask row

	end subroutine calculate


	function get(self) result (masks)
		!< it returns splitted masks
		class(shr_maskEnabledEqualSplit), intent(inout) :: self
		type(shr_mask2d), allocatable :: masks(:)
		allocate(masks, source = self % parts)
	end function get

end module shr_maskEnabledEqualSplit_mod
