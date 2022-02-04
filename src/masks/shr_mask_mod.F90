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
	use shr_arrayIndices_mod, only: shr_arrayGridcellIndex

	implicit none

	public :: shr_imask2d
	public :: shr_mask1d, shr_mask2d


	integer, parameter :: ROW_SHAPE_INDEX = 1
	integer, parameter :: COL_SHAPE_INDEX = 2


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

		procedure :: mask1d_eq
		generic :: operator(==) => mask1d_eq
	end type shr_mask1d


	type, abstract :: shr_imask2d
	contains
		procedure(iface_getShape), deferred :: getShape
		procedure(iface_getSize), deferred :: getSize

		procedure(iface_get_byGridcellIndex), deferred :: getByGridcellIndex
		procedure(iface_get_maskIndices), deferred :: getByMaskIndices
		procedure(iface_get_simple), deferred :: getSimple
		generic :: get => getByGridcellIndex, getByMaskIndices, getSimple
	end type shr_imask2d


	abstract interface
		function iface_getShape(self) result (s)
			import :: shr_imask2d
			!< get 'mask' shape
			class(shr_imask2d), intent(in) :: self
			integer :: s(2) !< row=1, col=2
		end function iface_getShape

		integer function iface_getSize(self) result (s)
			import :: shr_imask2d
			!< gets lmask size
			class(shr_imask2d), intent(in) :: self
		end function iface_getSize

		function iface_get_byGridcellIndex(self, arrayGridcellIndex) result (l)
			import :: shr_imask2d, shr_arrayGridcellIndex
			!< gets unique value requested by
			class(shr_imask2d), intent(in) :: self
			type(shr_arrayGridcellIndex) :: arrayGridcellIndex
			logical :: l !< output
		end function iface_get_byGridcellIndex

		function iface_get_maskIndices(self, mIndices) result (m)
			import :: shr_imask2d, shr_maskIndices_2d
			!< returns internal mask
			class(shr_imask2d), intent(in) :: self
			type(shr_maskIndices_2d), intent(in) :: mIndices
			logical, allocatable :: m(:,:) !< output
		end function iface_get_maskIndices

		function iface_get_simple(self) result (m)
			import :: shr_imask2d
			!< returns internal mask
			class(shr_imask2d), intent(in) :: self
			logical, allocatable :: m(:,:) !< output
		end function iface_get_simple
	end interface


	type, extends(shr_imask2d) :: shr_mask2d
		logical, allocatable :: lmask(:,:)
	contains
		procedure :: mask2d_initialize_bySize!(4)
		procedure :: mask2d_initialize_byArray!(4)
		generic :: init =>  mask2d_initialize_bySize, mask2d_initialize_byArray
		procedure :: getShape => mask2d_getShape
		procedure :: getSize => mask2d_getSize

		procedure :: getByGridcellIndex => mask2d_get_byGridcellIndex
		procedure :: getByMaskIndices => mask2d_get_maskIndices
		procedure :: getSimple => mask2d_get_simple

		procedure :: set => mask2d_set
		procedure :: filter => mask2d_filter !< new mask with selected indices
		procedure :: toString => mask2d_toString
		procedure :: count => mask2d_count

		procedure :: mask2d_eq
		generic :: operator(==) => mask2d_eq
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


	elemental logical function mask1d_eq(self, other)
		!< true if self and other have the same attributes
		!< (both must be initialized)
		class(shr_mask1d), intent(in) :: self
		type(shr_mask1d), intent(in) :: other
		mask1d_eq = all(self % lmask .eqv. other % lmask)
	end function mask1d_eq


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


	function mask2d_get_byGridcellIndex(self, arrayGridcellIndex) result (l)
		!< gets unique value requested by
		class(shr_mask2d), intent(in) :: self
		type(shr_arrayGridcellIndex) :: arrayGridcellIndex
		logical :: l !< output
		integer :: col, row
		row = arrayGridcellIndex % row
		col = arrayGridcellIndex % col
		l = self % lmask(row, col)
	end function mask2d_get_byGridcellIndex


	function mask2d_get_simple(self) result (m)
		!< returns internal mask
		class(shr_mask2d), intent(in) :: self
		logical, allocatable :: m(:,:) !< output

		integer :: inColStart, inColEnd
		integer :: inRowStart, inRowEnd
		type(shr_maskIndices_1d) :: colIdx, rowIdx
		integer :: colSize, rowSize
		integer, allocatable :: sh(:)

		inColStart = 1
		inRowStart = 1
		sh = self % getShape()
		inColEnd = sh(COL_SHAPE_INDEX)
		inRowEnd = sh(ROW_SHAPE_INDEX)

		colSize = inColEnd - inColStart + 1
		rowSize = inRowEnd - inRowStart + 1
		allocate(m(rowSize, colSize))
		!write(*,*) "mask2d_get:: rowSize, colSize = ", rowSize, colSize
		!write(*,*) "mask2d_get:: row = ", inRowStart, inRowEnd
		!write(*,*) "mask2d_get:: col = ", inColStart, inColEnd
		!write(*,*) "mask2d_get:: self % lmask shape? ", shape(self % lmask)
		!write(*,*) "mask2d_get:: m shape? ", shape(m)
		m = self % lmask(inRowStart:inRowEnd, inColStart:inColEnd)
	end function mask2d_get_simple


	function mask2d_get_maskIndices(self, mIndices) result (m)
		!< returns internal mask
		class(shr_mask2d), intent(in) :: self
		type(shr_maskIndices_2d), intent(in) :: mIndices
		logical, allocatable :: m(:,:) !< output

		integer :: inColStart, inColEnd
		integer :: inRowStart, inRowEnd
		type(shr_maskIndices_1d) :: colIdx, rowIdx
		integer :: colSize, rowSize
		integer, allocatable :: sh(:)

		colIdx = mIndices % getCol()
		rowIdx = mIndices % getRow()
		inRowStart = rowIdx % start
		inRowEnd = rowIdx % end
		inColStart = colIdx % start
		inColEnd = colIdx % end

		colSize = inColEnd - inColStart + 1
		rowSize = inRowEnd - inRowStart + 1
		allocate(m(rowSize, colSize))
		!write(*,*) "mask2d_get:: rowSize, colSize = ", rowSize, colSize
		!write(*,*) "mask2d_get:: row = ", inRowStart, inRowEnd
		!write(*,*) "mask2d_get:: col = ", inColStart, inColEnd
		!write(*,*) "mask2d_get:: self % lmask shape? ", shape(self % lmask)
		!write(*,*) "mask2d_get:: m shape? ", shape(m)
		m = self % lmask(inRowStart:inRowEnd, inColStart:inColEnd)
	end function mask2d_get_maskIndices


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
			sh = self % getShape()
			inColEnd = sh(COL_SHAPE_INDEX)
			inRowEnd = sh(ROW_SHAPE_INDEX)
		end if
		self % lmask(inRowStart:inRowEnd, inColStart:inColEnd) = rmask
	end subroutine mask2d_set


	subroutine mask2d_initialize_bySize(self, nrows, ncols, default)
		!< initialize with new size
		!< By default true is assigned,
		class(shr_mask2d), intent(inout) :: self
		integer, intent(in) :: nrows
		integer, intent(in) :: ncols
		logical, intent(in), optional :: default
		logical :: inDefault

		inDefault = .true.
		if (present(default)) inDefault = default
		allocate(self % lmask(nrows, ncols))
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
		!type(string) :: tmpStr ! debug
		!tmpStr = mIndices % toString()
		!write(*,*) "mask2d_filter:: mIndices =", tmpStr % toString()
		tmp = self % get(mIndices)

		!< get lmask indices
		colIdx = mIndices % getCol()
		rowIdx = mIndices % getRow()
		inRowStart = rowIdx % start
		inRowEnd = rowIdx % end
		inColStart = colIdx % start
		inColEnd = colIdx % end

		!< initialize output with same size as lmask
		sz = self % getShape()
		call m % init(sz(ROW_SHAPE_INDEX), sz(COL_SHAPE_INDEX), default = .false.)
		!< copy values
		m % lmask(inRowStart:inRowEnd, inColStart:inColEnd) = tmp
	end function mask2d_filter


	function mask2d_getShape(self) result (s)
		!< get 'mask' shape
		class(shr_mask2d), intent(in) :: self
		integer :: s(2) !< row=1, col=2
		s = shape(self % lmask)
	end function mask2d_getShape


	integer function mask2d_getSize(self) result (s)
		!< gets lmask size
		class(shr_mask2d), intent(in) :: self
		s = size(self % lmask)
	end function mask2d_getSize


	elemental logical function mask2d_eq(self, other)
		!< true if self and other have the same attributes
		!< (both must be initialized)
		class(shr_mask2d), intent(in) :: self
		type(shr_mask2d), intent(in) :: other
		if (.not. allocated(self % lmask))  then
			mask2d_eq = .false.
			return
		end if
		if (.not. allocated(other % lmask))  then
			mask2d_eq = .false.
			return
		end if
		mask2d_eq = all(self % lmask .eqv. other % lmask)
	end function mask2d_eq


	type(string) function mask2d_toString(self) result (s)
		!< string representation of shr_mask2d
		class(shr_mask2d), intent(in) :: self
		integer :: irow, nRows
		character(200) :: tmpStr
		s = ""
		nRows = size(self % lmask, dim=1)
		do irow = 1, nRows
			write(tmpStr,*) self % lmask(irow,:)
			s = s + "'" + trim(adjustl(tmpStr)) + "'" + new_line('A')
		end do
	end function mask2d_toString


	integer function mask2d_count(self) result (c)
		!< count how many true cells found
		class(shr_mask2d), intent(in) :: self
		c = count(self % lmask)
	end function mask2d_count

end module shr_mask_mod