!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridShape_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> grid shape
!>
!------------------------------------------------------------------------------
module shr_gridShape_mod

	implicit none

	public :: shr_gridShape

	type shr_gridShape
		integer :: nlats !< rows
		integer :: nlons !< cols
	contains
		procedure :: getLats
		procedure :: getLons

		procedure :: equal
		generic :: operator(==) => equal
	end type shr_gridShape

contains

	integer function getLats(self)
		!< nlats getter
		class(shr_gridShape), intent(in) :: self
		getLats = self % nlats
	end function getLats


	integer function getLons(self)
		!< nlons getter
		class(shr_gridShape), intent(in) :: self
		getLons = self % nlons
	end function getLons


	logical function equal(self, other)
		!< true if self and other have the same attributes
		class(shr_gridShape), intent(in) :: self
		type(shr_gridShape), intent(in) :: other
		logical :: hasSameNlats, hasSameNlons
		hasSameNlats = (self % nlats == other % getLats())
		hasSameNlons = (self % nlons == other % getLons())
		equal = (hasSameNlats .and. hasSameNlons)
	end function equal

end module shr_gridShape_mod