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

	implicit none

	public :: shr_maskIndices_1d, shr_maskIndices_2d

	!< shr_maskIndices_2d as an array(4)
	integer, parameter :: MASK_INDICES_ARRAY_ROW_START = 1
	integer, parameter :: MASK_INDICES_ARRAY_ROW_END= 2
	integer, parameter :: MASK_INDICES_ARRAY_COL_START = 3
	integer, parameter :: MASK_INDICES_ARRAY_COL_END= 4


	!< 1d mask indices
	type shr_maskIndices_1d
		integer :: start, end
	contains
		procedure :: init => maskIndices_1d_init
	end type shr_maskIndices_1d

	!< 2d mask indices
	type shr_maskIndices_2d
		type(shr_maskIndices_1d), allocatable :: row, col
	contains
		procedure :: maskIndices_2d_init_by_array
		procedure :: maskIndices_2d_init_by_1d
		generic :: init => maskIndices_2d_init_by_array, maskIndices_2d_init_by_1d
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
		integer, intent(in) :: array(4) !< row start, row end, col start, col end

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

end module shr_maskIndices_mod