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
	use shr_strings_mod, only: string
	use shr_maskIndices_mod, only: shr_maskIndices_1d, shr_maskIndices_2d

	implicit none

	public :: shr_mask1d, shr_mask2d


	type :: shr_mask1d
		logical, allocatable :: lmask(:)
	contains
		procedure :: mask1d_initialize_bySize!(4)
		procedure :: mask1d_initialize_byArray!(4)
		generic :: init =>  mask1d_initialize_bySize, mask1d_initialize_byArray
		procedure :: getSize => mask1d_getSize

		procedure :: get => mask1d_get !< raw mask
		procedure :: filter => mask1d_filter !< new mask with selected indices
		!procedure :: getVal(mIndices, values)
		! procedure :: setVal(mIndices, values)

		! set (=)
	end type shr_mask1d


	type :: shr_mask2d
		logical, allocatable :: lmask(:,:)
	contains
!		procedure :: init => mask2d_initialize
		procedure :: get => mask2d_get
	end type shr_mask2d

contains

	subroutine mask1d_initialize_bySize(self, size, default)
		!< initialize with new size
		!< By default true is assigned,
		class(shr_mask1d), intent(inout) :: self
		integer, intent(in) :: size
		logical, intent(in), optional :: default
		logical :: inDefault
		allocate(self % lmask(size))

		inDefault = .true.
		if (present(default)) inDefault = default
		self % lmask = inDefault
	end subroutine mask1d_initialize_bySize


	subroutine mask1d_initialize_byArray(self, larray)
		!< initialize with new size with logical array
		class(shr_mask1d), intent(inout) :: self
		logical, intent(in) :: larray(:)
		allocate(self % lmask, source =  larray)
	end subroutine mask1d_initialize_byArray


	type(shr_mask1d) function mask1d_filter(self, mIndices) result (m)
		!< filter those mask values found in mIndices in a new shr_mask
		class(shr_mask1d), intent(inout) :: self
		type(shr_maskIndices_1d), intent(in) :: mIndices

		integer :: start, end, sz

		!< init as empty
		sz = self % getSize()
		call m % init(sz, .false.)
		start = mIndices % start !< getStart
		end = mIndices % end !< getEnd
		!< copy values (get(mindices, values), set(mindices, values))
		m % lmask(start:end) = self % lmask(start:end)
	end function mask1d_filter


	integer function mask1d_getSize(self)
		!< mask size
		class(shr_mask1d), intent(in) :: self
		mask1d_getSize = size(self % lmask)
	end function mask1d_getSize


	function mask1d_get(self) result (m)
		!< returns internal mask
		class(shr_mask1d), intent(in) :: self
		logical, allocatable :: m(:) !< output
		allocate(m, source = self % lmask)
	end function mask1d_get


	function mask2d_get(self) result (m)
		!< returns internal mask
		class(shr_mask2d), intent(in) :: self
		logical, allocatable :: m(:,:) !< output
		allocate(m, source = self % lmask)
	end function mask2d_get


end module shr_mask_mod