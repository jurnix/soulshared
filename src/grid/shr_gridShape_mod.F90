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


	type shr_gridShape
		integer :: nlats !< rows
		integer :: nlons !< cols
	contains
		procedure :: getLats
		procedure :: getLons
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

end module shr_gridShape_mod