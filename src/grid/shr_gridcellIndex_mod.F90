!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridcellIndex_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> 2D Grid array indices
!> 
!------------------------------------------------------------------------------
module shr_gridcellIndex_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_objects_mod, only: shr_equal_iface !< soulshared base object
  use shr_strings_mod, only: string, int2string
  use shr_gridcell_mod, only: shr_gridcell

  implicit none

  public :: shr_gridcellIndex

  logical, parameter :: ISDEBUG = .false.



  type, extends(shr_equal_iface) :: shr_gridcellIndex
    integer :: idxlat
    integer :: idxlon
  contains
    procedure :: init => gridcellIndex_initialize 

    procedure :: getIndices
    !< implemented
    procedure :: equal => eq_gridcellIndex
!    generic :: operator(==) => eq_object

    procedure :: toString
  end type shr_gridcellIndex

contains

  subroutine gridcellIndex_initialize(self, idxlat, idxlon)
    !< gridcellIndex initialization
    class(shr_gridcellIndex), intent(inout) :: self
    integer, intent(in) :: idxlat, idxlon
    self % idxlat = idxlat
    self % idxlon = idxlon
  end subroutine gridcellIndex_initialize


  function getIndices(self) result (indices)
    !< it returns x and y array indices
    class(shr_gridcellIndex), intent(in) :: self
    integer :: indices(2) !< output
    indices(1) = self % idxlat
    indices(2) = self % idxlon
  end function getIndices


  elemental logical function eq_gridcellIndex(self, other)
    !< true if 'self' and 'other' have the same attributes 
    !< in case 'other' is not the same it is false
    class(shr_gridcellIndex), intent(in) :: self
    class(shr_equal_iface), intent(in) :: other
    logical :: hasSameIdxLat, hasSameIdxLon

    type(shr_gridcellIndex), pointer :: otherWrap

    !< extract type from 'other'
    select type (obj => other)
    type is (shr_gridcellIndex) !< type found
      otherWrap => obj 
    class default 
      !< not found so it fails
      eq_gridcellIndex = .false.
      return
    end select

    !< comparision as usual
    hasSameIdxLat = (self % idxlat == otherWrap % idxlat)
    hasSameIdxLon = (self % idxlon == otherWrap % idxlon)
    eq_gridcellIndex = (hasSameIdxLat .and. hasSameIdxLon)
  end function eq_gridcellIndex


  elemental type(string) function toString(self)
    !< string representation of 'self'
    class(shr_gridcellIndex), intent(in) :: self
    type(string) :: latStr, lonStr
    latStr = int2string(self % idxlat)
    lonStr = int2string(self % idxlon)
    toString = string("(idxlat="//latStr % toString()//", idxlon="//&
            lonStr % toString()//")")
  end function toString

end module shr_gridcellIndex_mod 

