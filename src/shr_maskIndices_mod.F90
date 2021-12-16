!------------------------------------------------------------------------------
!						Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        :  shr_maskIndices_mod
!
!> @author
!> jurnix
!
! DESCRIPTION:
!>
!>  Mask indices classes
!> 
!------------------------------------------------------------------------------
module shr_maskIndices_mod

	use shr_objects_mod, only: shr_eqObject_abs
	use shr_strings_mod, only: string, int2string

	implicit none

	public :: shr_maskIndices_1d, shr_maskIndices_2d

	!< shr_maskIndices_2d as an array(4)
	integer, parameter :: MASK_INDICES_ARRAY_ROW_START = 3
	integer, parameter :: MASK_INDICES_ARRAY_ROW_END= 4
	integer, parameter :: MASK_INDICES_ARRAY_COL_START = 1
	integer, parameter :: MASK_INDICES_ARRAY_COL_END= 2


	!< 1d mask indices
	type, extends(shr_eqObject_abs) :: shr_maskIndices_1d
		integer :: start, end
	contains
		procedure :: init => maskIndices_1d_init
		procedure :: eq_object => eq_maskIndices_1d
		procedure :: toString => toString_maskIndices_1d
	end type shr_maskIndices_1d

	!< 2d mask indices
	type, extends(shr_eqObject_abs) :: shr_maskIndices_2d
		type(shr_maskIndices_1d), allocatable :: row, col
	contains
		procedure :: maskIndices_2d_init_by_array
		procedure :: maskIndices_2d_init_by_1d
		generic :: init => maskIndices_2d_init_by_array, maskIndices_2d_init_by_1d
		procedure :: eq_object => eq_maskIndices_2d
		procedure :: toString => toString_maskIndices_2d
	end type shr_maskIndices_2d


contains

	subroutine maskIndices_1d_init(self, start, end)
		!< maskIndices_1d constructor
		class(shr_maskIndices_1d), intent(inout) :: self
		integer, intent(in) :: start
		integer, intent(in) :: end
		self % start = start
		self % end = end
	end subroutine maskIndices_1d_init


	subroutine maskIndices_2d_init_by_array(self, array)
		!< maskIndices_2d constructor with array
		class(shr_maskIndices_2d), intent(inout) :: self
		integer, intent(in) :: array(4) !< col start, col end, row start, row end,

		type(shr_maskIndices_1d) :: colIndices, rowIndices
		call colIndices % init(array(MASK_INDICES_ARRAY_COL_START), &
				array(MASK_INDICES_ARRAY_COL_END))
		call rowIndices % init(array(MASK_INDICES_ARRAY_ROW_START), &
				array(MASK_INDICES_ARRAY_ROW_END))

		call self % maskIndices_2d_init_by_1d(colIndices, rowIndices)
	end subroutine maskIndices_2d_init_by_array


	subroutine maskIndices_2d_init_by_1d(self, colIndices, rowIndices)
		!< maskIndices_2d constructor with shr_maskIndices_1d
		class(shr_maskIndices_2d), intent(inout) :: self
		type(shr_maskIndices_1d), intent(in) :: colIndices
		type(shr_maskIndices_1d), intent(in) :: rowIndices
		allocate(self % col, source = colIndices)
		allocate(self % row, source = rowIndices)
	end subroutine maskIndices_2d_init_by_1d


	elemental logical function eq_maskIndices_1d(self, other)
		!< true if 'self' and 'other' have the same attributes
		class(shr_maskIndices_1d), intent(in) :: self
		!type(shr_maskIndices_1d), intent(in) :: other
		class(shr_eqObject_abs), intent(in) :: other
		logical :: hasSameStart, hasSameEnd
		logical :: hasSameType

		select type(o => other)
		type is(shr_maskIndices_1d)
			hasSameType = .true.
			hasSameStart = (self % start == o % start)
			hasSameEnd = (self % end == o % end)
		class default
			hasSameType = .false.
			hasSameEnd = .false.
			hasSameStart = .false.
		end select

		eq_maskIndices_1d = (hasSameType .and. hasSameStart .and. hasSameEnd)
	end function eq_maskIndices_1d


	elemental type(string) function toString_maskIndices_1d(self) result (s)
		!< string representation of maskIndices_1d
		clasS(shr_maskIndices_1d), intent(in) :: self
		type(string) :: strStart, strEnd

		strStart = int2string(self % start)
		strEnd = int2string(self % end)
		s = string("(") + self % start + ":" + self % end + ")"
	end function toString_maskIndices_1d


	elemental logical function eq_maskIndices_2d(self, other)
		!< true if 'self' and 'other' have the same attributes
		class(shr_maskIndices_2d), intent(in) :: self
		class(shr_eqObject_abs), intent(in) :: other
		logical :: hasSameCol, hasSameRow
		logical :: hasSameType

		select type(o => other)
		type is(shr_maskIndices_2d)
			hasSameType = .true.
			hasSameCol = (self % col == o % col)
			hasSameRow = (self % row == o % row)
		class default
			hasSameType = .false.
			hasSameCol = .false.
			hasSameRow = .false.
		end select

		eq_maskIndices_2d = (hasSameType .and. hasSameCol .and. hasSameRow)
	end function eq_maskIndices_2d


	elemental type(string) function toString_maskIndices_2d(self) result (s)
		!< string representation of maskIndices_2d
		class(shr_maskIndices_2d), intent(in) :: self
		type(string) :: strCol, strRow
		strCol = self % col % toString()
		strRow = self % row % toString()
		s = string("[") + strCol + ", " + strRow + "]"
	end function toString_maskIndices_2d

end module shr_maskIndices_mod