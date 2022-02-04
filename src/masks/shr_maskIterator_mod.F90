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
!> Iterates over shr_mask(2d)
!> 
!------------------------------------------------------------------------------
module shr_maskIterator_mod

	use shr_error_mod, only: raiseError
	use shr_mask_mod, only: shr_imask2d
	use shr_iterator_mod, only: shr_iterator_abs
	use shr_logical_mod, only: shr_logical_cast
	use shr_arrayUtils_mod, only: shr_arrayCalculatorIndices
	use shr_arrayIndices_mod, only: shr_arrayGridcellIndex

	implicit none

	public :: shr_maskIterator

	type, extends(shr_iterator_abs) :: shr_maskIterator
		class(shr_imask2d), allocatable :: mask
		integer :: icounter = 0
	contains
		procedure :: init
		procedure :: hasNext
		procedure :: getNext
		procedure :: getLogicalNext
	end type shr_maskIterator

contains

	subroutine init(self, mask)
		!< initialization
		class(shr_maskIterator), intent(inout) :: self
		class(shr_imask2d), intent(in) :: mask
		allocate(self % mask, source = mask)
		self % icounter = 0
	end subroutine init


	logical function hasNext(self)
		!< true if there is another gridcell to iterate
		class(shr_maskIterator), intent(in) :: self
		hasNext = (self % mask % getSize() > self % icounter)
	end function hasNext


	function getNext(self) result (obj)
		!< returns next object and move to next position
		!< It calculates current grid indices
		!<
		!< Example:
		!<
		!< x x x x -> 7 (2,3) =>  7-1/4 = 1, 7-1 mod 4 = 2 =>
		!< x x x x -> 11 (3,3) => 11-1/4 = 2, 11 mod 4 = 2 =>
		!< x x x x -> 1 (1,1) => 1-1/4 = 0, 1-1 mod 4 = 0 =>
		!<						12 (3,4) => 12-1/4 = 2, 12-1 mod 4 = 3 =>
		!<
		class(shr_maskIterator), intent(inout) :: self
		class(*), allocatable :: obj
		logical :: value
		type(shr_arrayGridcellIndex) :: gridcellIndices
		integer :: dim1, dim2
		integer :: maskSize(2)
		!< find value
		maskSize = self % mask % getShape()
		dim1 = maskSize(1) !size(self % mask, dim=1)
		dim2 = maskSize(2) !size(self % mask, dim=2)
		gridcellIndices = shr_arrayCalculatorIndices(dim1, dim2, self % icounter)

		!< get value
		value = self % mask % get(gridcellIndices)
		allocate(obj, source=value)

		!< move next
		self % icounter = self % icounter + 1
	end function getNext


	logical function getLogicalNext(self) result (l)
		!< wrapper of getNext with expected output type (logical)
		class(shr_maskIterator), intent(inout) :: self
		class(*), allocatable :: obj
		obj = self % getNext()
		l = shr_logical_cast(obj)
	end function getLogicalNext

end module shr_maskIterator_mod