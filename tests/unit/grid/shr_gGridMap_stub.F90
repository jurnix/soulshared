!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridMap_stub
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> shr_gridMap_stub defines an empty stub.
!> Use it along a unit test.
!> When extended only overload those required procedures.
!------------------------------------------------------------------------------
module shr_gGridMap_stub

  use shr_gGridDescriptor_mod, only: shr_igGridDescriptor
  use shr_gAxisMapping_mod, only: shr_igAxisMapping
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_coord_mod, only: shr_coord
!  use shr_gridShape_mod, only: shr_gridShape

  use shr_strings_mod, only: string
!  use shr_precision_mod, only: sp
!  use shr_gridBounds_mod, only: shr_gridBounds
!  use shr_gGridAxes_mod, only: shr_gGridAxes
!  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_gGridMap_mod, only: shr_igGridMap
  use shr_gridShape_mod, only: shr_gridShape

  implicit none

  private


  !< implementation
  type, extends(shr_igGridMap) :: shr_gGridMapEmptyStub
  contains
    procedure :: equal

    !< getters
    procedure :: getGridDescriptor
    procedure :: getLatAxis
    procedure :: getLonAxis

    procedure :: getIndex
    procedure :: getShape

    !<
    procedure :: toString
  end type shr_gGridMapEmptyStub

contains

  
  type(shr_gridShape) function getShape(self)
    !< grid shape
    class(shr_gGridMapEmptyStub), intent(in) :: self
  end function getShape


  function getIndex(self, coord) result (gIndices)
    !< Seach gridcell indices from given 'coord'
    !< Multiple values can be returned if given coord
    !< is found in-between multiple gridcells
    class(shr_gGridMapEmptyStub), intent(in) :: self
    type(shr_coord), intent(in) :: coord
    type(shr_gridcellIndex), allocatable :: gIndices(:)!< output
  end function getIndex


  type(string) function toString(self)
    !< string representation of shr_gGridMap
    class(shr_gGridMapEmptyStub), intent(in) :: self
  end function toString


  logical function equal(self, other)
    !< true if self and other have the same attributes
    class(shr_gGridMapEmptyStub), intent(in) :: self
    class(shr_igGridMap), intent(in) :: other
  end function equal


  function getGridDescriptor(self) result (gDesc)
    !< getGridDescriptor getter
    class(shr_gGridMapEmptyStub), intent(in) :: self
    class(shr_igGridDescriptor), allocatable :: gDesc
  end function getGridDescriptor


  function getLatAxis(self) result (laxisMapping)
    !< latAxis getter
    class(shr_gGridMapEmptyStub), intent(in) :: self
    class(shr_igAxisMapping), allocatable :: laxisMapping !< output
  end function getLatAxis


  function getLonAxis(self) result (laxisMapping)
    !< latAxis getter
    class(shr_gGridMapEmptyStub), intent(in) :: self
    class(shr_igAxisMapping), allocatable :: laxisMapping !< output
  end function getLonAxis

end module shr_gGridMap_stub
