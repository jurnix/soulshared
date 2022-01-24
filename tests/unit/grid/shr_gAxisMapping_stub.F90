!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridDescriptor_stub 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_gAxisMapping stub
!------------------------------------------------------------------------------
module shr_gAxisMapping_stub

  use shr_gAxisMapping_mod, only: shr_IGAxisMapping

  !< dependencies
  use shr_precision_mod, only: sp
  use shr_strings_mod, only: string
  use shr_gGridAxesCell_mod, only: shr_gGridAxesCell

  implicit none

  private
  public :: shr_gAxisMappingEmptyStub

  !< stub
  type, extends(shr_igAxisMapping) :: shr_gAxisMappingEmptyStub
  contains
    procedure :: getIndexByCoord
    procedure :: getIndexByGridAxisCell

    procedure :: getSize

    procedure :: equal

    procedure :: toString
  end type shr_gAxisMappingEmptyStub

contains

  type(string) function toString(self)
    !< string representation of shr_gAxisMapping
    class(shr_gAxisMappingEmptyStub), intent(in) :: self
  end function toString


  function getIndexByCoord(self, axisCoord) result (foundIdxs)
    !< it returns the index(s) which matches with axisCoord
    class(shr_gAxisMappingEmptyStub), intent(in) :: self
    real(kind=sp), intent(in):: axisCoord
    integer, allocatable :: foundIdxs(:)
  end function getIndexByCoord


  integer function getIndexByGridAxisCell(self, gAxisCell)
    !< it returns the index(s) which matches with gAxisCell
    !< only 1 index can be returned
    !< -1 in case is not found
    class(shr_gAxisMappingEmptyStub), intent(in) :: self
    type(shr_gGridAxesCell), intent(in) :: gAxisCell
  end function getIndexByGridAxisCell


  integer function getSize(self)
    !< axis size
    class(shr_gAxisMappingEmptyStub), intent(in) :: self
  end function getSize


  logical function equal(self, other)
    !< true if self and equal have the same attributes
    class(shr_gAxisMappingEmptyStub), intent(in) :: self
    class(shr_igAxisMapping), intent(in) :: other
  end function equal

end module shr_gAxisMapping_stub

