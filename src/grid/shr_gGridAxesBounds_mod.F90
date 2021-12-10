!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridAxesBounds_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gGridAxesBounds defines an axes bounds 
!> 
!------------------------------------------------------------------------------
module shr_gGridAxesBounds_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_strings_mod, only: string, real2string
  use shr_gridBounds_mod, only: shr_gridBounds

  implicit none

  public :: shr_gGridAxesBounds

  logical, parameter :: ISDEBUG = .false.


  type shr_gGridAxesBounds
    real(kind=sp) :: start, end 
  contains
    procedure :: init => gGridAxesBounds_initialize 
    procedure :: getStart
    procedure :: getEnd
    procedure :: isInByCoord, isInByGridAxesBounds
    generic :: isIn => isInbyCoord, isInByGridAxesBounds
    procedure :: isOverlapped

    procedure :: equal_gGridAxesBounds, equal_byArray2d
    generic :: operator(==) => equal_gGridAxesBounds, equal_byArray2d

    procedure :: create_gridBounds
    generic :: operator(*) => create_gridBounds

    procedure :: toString
  end type shr_gGridAxesBounds

contains

  subroutine gGridAxesBounds_initialize(self, startCoord, endCoord)
    !< gGridAxesBounds initialization
    !< startCoord > endCoord
    class(shr_gGridAxesBounds), intent(inout) :: self
    real(kind=sp), intent(in) :: startCoord, endCoord 

    if (startCoord <= endCoord) then
      call raiseError(__FILE__, "gGridAxesBounds_initialize", &
              "endCoord must be smaller than startCoord")
    end if

    self % start = startCoord
    self % end = endCoord
  end subroutine gGridAxesBounds_initialize


  elemental logical function isInByCoord(self, axisCoord) result (isIn)
    !< true if 'axisCoord' is inbetween start and end axisBounds
    class(shr_gGridAxesBounds), intent(in) :: self
    real(kind=sp), intent(in) :: axisCoord
    isIn = (axisCoord <= self % start) .and. (axisCoord >= self % end)
  end function isInByCoord


  elemental logical function isInByGridAxesBounds(self, gridAxesBounds) result (isIn)
    !< true if 'gridAxisBounds' start and end fits inside in 'self'
    class(shr_gGridAxesBounds), intent(in) :: self
    type(shr_gGridAxesBounds), intent(in) :: gridAxesBounds
    logical :: hasInnerStart, hasInnerEnd
    hasInnerStart = (gridAxesBounds % getStart() <= self % start)
    hasInnerEnd = (gridAxesBounds % getEnd() >= self % end)
    isIn = (hasInnerStart .and. hasInnerEnd)
  end function isInByGridAxesBounds


  elemental real(kind=sp) function getStart(self)
    !< it returns start axies coordinate
    class(shr_gGridAxesBounds), intent(in) :: self
    getStart = self % start
  end function getStart


  elemental real(kind=sp) function getEnd(self)
    !< it returns start axies coordinate
    class(shr_gGridAxesBounds), intent(in) :: self
    getEnd = self % end
  end function getEnd


  elemental logical function equal_gGridAxesBounds(self, other)
    !< true if 'self' and 'other' are the same
    class(shr_gGridAxesBounds), intent(in) :: self
    type(shr_gGridAxesBounds), intent(in) :: other
    equal_gGridAxesBounds = (self % start == other % start) &
                      .and. (self % end == other % end)
  end function equal_gGridAxesBounds


  pure logical function equal_byArray2d(self, other)
    !< true if 'self' and 'other' are the same
    !< Other is an array composed of 'start' and 'end'
    !< in that specific order.
    class(shr_gGridAxesBounds), intent(in) :: self
    real(kind=sp), intent(in) :: other(2) !< start, end
    integer, parameter :: SHR_GGRIDAXES_BOUNDS_START = 1
    integer, parameter :: SHR_GGRIDAXES_BOUNDS_END = 2
    equal_byArray2d = (self % start == other(SHR_GGRIDAXES_BOUNDS_START)) &
                      .and. (self % end == other(SHR_GGRIDAXES_BOUNDS_END))
  end function equal_byArray2d


  elemental type(shr_gridBounds) function create_gridBounds(latBounds, lonBounds) result (newGb)
    !< The combination of latBounds and lonBounds creates a shr_gridBounds
    class(shr_gGridAxesBounds), intent(in) :: latBounds
    type(shr_gGridAxesBounds), intent(in) :: lonBounds
    newGb = shr_gridBounds(latBounds % getStart(), &
                           latBounds % getEnd(), &
                           lonBounds % getStart(), &
                           lonBounds % getEnd())
  end function create_gridBounds


  type(string) function toString(self)
    !< string representation of 'self'
    !< toString() -> (2.0, -1.0)
    class(shr_gGridAxesBounds), intent(in) :: self
    type(string) :: startStr, endStr
    startStr = real2string(self % start)
    endStr = real2string(self % end)
    toString = string("("//startStr % toString() // ", " // &
                endStr % toString() // ")")
  end function toString


  logical function isOverlapped(self, other)
    !< true if other overlaps with self axis
    !< (upper) - self(4,0), other(5,3)
    !< (lower) - self(4,0), other(1,-5)
    class(shr_gGridAxesBounds), intent(in) :: self
    type(shr_gGridAxesBounds), intent(in) :: other
    logical :: isOtherStartIn, isOtherEndIn

    isOtherStartIn = self % isIn(other % start)
    isOtherEndIn = self % isIn(other % end)
    isOverlapped = (isOtherStartIn .or. isOtherEndIn)
  end function isOverlapped

end module shr_gGridAxesBounds_mod 

