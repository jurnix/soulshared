!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : adarray_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!------------------------------------------------------------------------------
module SHR_adarray_mod 
  use SHR_precision_mod, only: sp

  implicit none


  type adarray(k, elems) !< generic real array
    integer, kind :: k = sp !< set precission
    integer, len :: elems !< array size
    real(kind=k) :: array(elems)
  end type adarray

  contains

end module SHR_adarray_mod

