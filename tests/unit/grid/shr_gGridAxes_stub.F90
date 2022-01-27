!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : shr_gGridAxes_stub
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gGridAxes stub
!------------------------------------------------------------------------------
module shr_gGridAxes_stub

  use shr_gGridAxes_mod, only: shr_igGridAxes
  use shr_strings_mod, only: string
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_precision_mod, only: sp
  use shr_gGridAxesCell_mod, only: shr_gGridAxesCell

  implicit none

  private
  public :: shr_gGridAxesEmptyStub

  type, extends(shr_igGridAxes) :: shr_gGridAxesEmptyStub
  contains
    procedure :: getName
    procedure :: getBounds
    procedure :: getResolution
    procedure :: getCells
    procedure :: getSize
    procedure :: toString
    procedure :: equal
  end type shr_gGridAxesEmptyStub

contains

  elemental type(string) function getName(self)
    !< it returns the name attribute
    class(shr_gGridAxesEmptyStub), intent(in) :: self
  end function getName


  elemental type(shr_gGridAxesBounds) function getBounds(self)
    !< it returns the bounds attribute
    class(shr_gGridAxesEmptyStub), intent(in) :: self
  end function getBounds


  elemental real(kind=sp) function getResolution(self)
    !< it returns the resolution attribute
    class(shr_gGridAxesEmptyStub), intent(in) :: self
  end function getResolution


  function getCells(self, axisCoord) result (gcells)
    !< given an coordinate from the current axis, it returns itss shr_gGridAxesCell(s)
    class(shr_gGridAxesEmptyStub), intent(in) :: self
    real(kind=sp), intent(in) :: axisCoord
    type(shr_gGridAxesCell), allocatable :: gcells(:)
  end function getCells


  integer function getSize(self)
    !< it returns how many grid axes cells has
    class(shr_gGridAxesEmptyStub), intent(in) :: self
  end function getSize


  type(string) function toString(self)
    !< string representation of gGridAxes
    class(shr_gGridAxesEmptyStub), intent(in) :: self
  end function toString


  elemental impure logical function equal(self, other)
    !< true if 'self' and 'other' have the same attributes
    class(shr_gGridAxesEmptyStub), intent(in) :: self
    class(shr_igGridAxes), intent(in) :: other
  end function equal

end module shr_gGridAxes_stub

