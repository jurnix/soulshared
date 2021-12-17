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

		procedure :: get => mask1d_get !< get mask
		procedure :: set => mask1d_set !< set mask
		procedure :: filter => mask1d_filter !< new mask with selected indices
	end type shr_mask1d


	type :: shr_mask2d
		logical, allocatable :: lmask(:,:)
	contains
		procedure :: mask2d_initialize_bySize!(4)
		procedure :: mask2d_initialize_byArray!(4)
		generic :: init =>  mask2d_initialize_bySize, mask2d_initialize_byArray
		procedure :: getSize => mask2d_getSize

		procedure :: get => mask2d_get
		procedure :: set => mask2d_set
		procedure :: filter => mask2d_filter !< new mask with selected indices
	end type shr_mask2d

contains

	subroutine mask1d_initialize_bySize(self, dim1, default)
		!< initialize with new size
		!< By default true is assigned,
		class(shr_mask1d), intent(inout) :: self
		integer, intent(in) :: dim1
		logical, intent(in), optional :: default
		logical :: inDefault

		inDefault = .true.
		if (present(default)) inDefault = default
		allocate(self % lmask(dim1))
		self % lmask = inDefault
	end subroutine mask1d_initialize_bySize


	subroutine mask1d_initialize_byArray(self, larray)
		!< initialize with new size with logical array
		class(shr_mask1d), intent(inout) :: self
		logical, intent(in) :: larray(:)
		if (allocated(self % lmask)) deallocate(self % lmask)
		allocate(self % lmask, source =  larray)
	end subroutine mask1d_initialize_byArray


	type(shr_mask1d) function mask1d_filter(self, mIndices) result (m)
		!< filter those mask values found in mIndices in a new shr_mask
		class(shr_mask1d), intent(inout) :: self
		type(shr_maskIndices_1d), intent(in) :: mIndices

		integer :: inStart, inEnd

		logical, allocatable :: tmp(:)
		integer :: sz
		tmp = self % get(mIndices)

		!< get lmask indices
		inStart = mIndices % start
		inEnd = mIndices % end

		!< initialize output with same size as lmask
		sz = self % getSize()
		call m % init(sz, default = .false.)
		!< copy values
		m % lmask(inStart:inEnd) = tmp
	end function mask1d_filter


	integer function mask1d_getSize(self)
		!< mask size
		class(shr_mask1d), intent(in) :: self
		mask1d_getSize = size(self % lmask)
	end function mask1d_getSize


	function mask1d_get(self, mIndices) result (m)
		!< returns internal mask
		class(shr_mask1d), intent(in) :: self
		type(shr_maskIndices_1d), intent(in), optional :: mIndices
		logical, allocatable :: m(:) !< output
		integer :: inStart, inEnd

		inStart = 1
		inEnd = self % getSize()
		if (present(mIndices)) then
			inStart = mIndices % start
			inEnd = mIndices % end
		end if

		allocate(m(inEnd))
		m = self % lmask(inStart:inEnd)
	end function mask1d_get


	subroutine mask1d_set(self, rmask, mIndices)
		!< returns internal mask
		class(shr_mask1d), intent(inout) :: self
		logical, intent(in) :: rmask(:)
		type(shr_maskIndices_1d), intent(in), optional :: mIndices
		integer :: inStart, inEnd

		inStart = 1
		inEnd = self % getSize()
		if (present(mIndices)) then
			inStart = mIndices % start
			inEnd = mIndices % end
		end if

		self % lmask(inStart:inEnd) = rmask
	end subroutine mask1d_set


	function mask2d_get(self, mIndices) result (m)
		!< returns internal mask
		class(shr_mask2d), intent(in) :: self
		type(shr_maskIndices_2d), intent(in), optional :: mIndices
		logical, allocatable :: m(:,:) !< output

		integer :: inColStart, inColEnd
		integer :: inRowStart, inRowEnd
		type(shr_maskIndices_1d) :: colIdx, rowIdx
		integer, allocatable :: sh(:)
		inColStart = 1
		inRowStart = 1

		if (present(mIndices)) then
			colIdx = mIndices % getCol()
			rowIdx = mIndices % getRow()
			inRowStart = rowIdx % start
			inRowEnd = rowIdx % end
			inColStart = colIdx % start
			inColEnd = colIdx % end
		else
			sh = self % getSize()
			inColEnd = sh(1)
			inRowEnd = sh(2)
		end if
		allocate(m(inColEnd, inRowEnd))
		m = self % lmask(inColStart:inColEnd, inRowStart:inRowEnd)
	end function mask2d_get


	subroutine mask2d_set(self, rmask, mIndices)
		!< sets internal mask
		!< if mIndices defined then:
		!< - rmask must have the same shape
		!<
		!< It mIndices not defined:
		!< - rmask must have the same shape as self % lmask
		class(shr_mask2d), intent(inout) :: self
		logical, intent(in) :: rmask(:,:) !< raw mask
		type(shr_maskIndices_2d), intent(in), optional :: mIndices

		integer :: inColStart, inColEnd
		integer :: inRowStart, inRowEnd
		type(shr_maskIndices_1d) :: colIdx, rowIdx
		integer, allocatable :: sh(:)
		inColStart = 1
		inRowStart = 1

		!< check rmask is consistent with self
		if (.not. present(mIndices)) then
			if (any(shape(rmask) /= shape(self % lmask))) then
				write(*,*) "shr_mask_mod:: mask2d_set:: lmask shape?", shape(self % lmask)
				write(*,*) "shr_mask_mod:: mask2d_set:: rmask shape?", shape(rmask)
				call raiseError(__FILE__, "mask2d_set", &
							"Given rmask does not have the same dimensions as current mask")
			end if
		end if

		if (present(mIndices)) then
			colIdx = mIndices % getCol()
			rowIdx = mIndices % getRow()
			inRowStart = rowIdx % start
			inRowEnd = rowIdx % end
			inColStart = colIdx % start
			inColEnd = colIdx % end
		else
			sh = self % getSize()
			inColEnd = sh(1)
			inRowEnd = sh(2)
		end if
		write(*,*) "mask2d_set:: lmask shape?", shape(self % lmask)
		write(*,*) "mask2d_set:: rmask shape?", shape(rmask)
		write(*,*) "mask2d_set:: col indices?", inColStart, ":", inColEnd
		write(*,*) "mask2d_set:: row indices?", inRowStart, ":", inRowEnd
		self % lmask(inColStart:inColEnd, inRowStart:inRowEnd) = rmask
	end subroutine mask2d_set


	subroutine mask2d_initialize_bySize(self, dim1, dim2, default)
		!< initialize with new size
		!< By default true is assigned,
		class(shr_mask2d), intent(inout) :: self
		integer, intent(in) :: dim1, dim2
		logical, intent(in), optional :: default
		logical :: inDefault

		inDefault = .true.
		if (present(default)) inDefault = default
		allocate(self % lmask(dim1, dim2))
		self % lmask = inDefault
	end subroutine mask2d_initialize_bySize


	subroutine mask2d_initialize_byArray(self, larray)
		!< initialize with new size with logical array
		class(shr_mask2d), intent(inout) :: self
		logical, intent(in) :: larray(:,:)
		if (allocated(self % lmask)) deallocate(self % lmask)
		allocate(self % lmask, source =  larray)
	end subroutine mask2d_initialize_byArray


	type(shr_mask2d) function mask2d_filter(self, mIndices) result (m)
		!< m has the same shape as self % lmask but with selected values from mIndices
		!< the remaining values are set to false
		class(shr_mask2d), intent(inout) :: self
		type(shr_maskIndices_2d), intent(in) :: mIndices

		integer :: inRowStart, inRowEnd
		integer :: inColStart, inColEnd
		type(shr_maskIndices_1d) :: colIdx, rowIdx
		logical, allocatable :: tmp(:,:)
		integer, allocatable :: sz(:)
		tmp = self % get(mIndices)

		!< get lmask indices
		colIdx = mIndices % getCol()
		rowIdx = mIndices % getRow()
		inRowStart = rowIdx % start
		inRowEnd = rowIdx % end
		inColStart = colIdx % start
		inColEnd = colIdx % end

		!< initialize output with same size as lmask
		sz = self % getSize()
		call m % init(sz(1), sz(2), default = .false.)
		!< copy values
		m % lmask(inRowStart:inRowEnd, inColStart:inColEnd) = tmp
	end function mask2d_filter


	function mask2d_getSize(self) result (s)
		!< get 'mask' shape
		class(shr_mask2d), intent(in) :: self
		integer, allocatable :: s(:)
		s = shape(self % lmask)
	end function mask2d_getSize

end module shr_mask_mod