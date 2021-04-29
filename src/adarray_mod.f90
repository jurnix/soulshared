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
module adarray_mod 
  use precision_mod, only: sp

  implicit none


  type adarray(k, elems) !< generic real array
    integer, kind :: k = sp !< set precission
    integer, len :: elems !< array size
    real(kind=k) :: array(elems)
  end type adarray

  contains

end module adarray_mod

