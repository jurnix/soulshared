!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskBorder_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gridMaskBorder defines which gridcells are border
!> A border gridcell won't be used as it is outside of bounds
!>
!------------------------------------------------------------------------------
module shr_gridMaskBorder_mod
  use SHR_error_mod, only: raiseError
  !use SHR_precision_mod, only: sp

  use shr_strings_mod, only: string
  use shr_gridMask_mod, only: shr_gridMask
  use shr_gridMaskEnabled_mod, only: shr_gridMaskEnabled

  implicit none

  public :: shr_gridMaskBorder

  logical, parameter :: ISDEBUG = .false.

  type, extends(shr_gridMask) :: shr_gridMaskBorder
  contains
    procedure :: isValid_gridMask !< gridMaskEnabled
    procedure :: isValid_gridMaskEnabled
    generic :: isValid => isValid_gridMask, isValid_gridMaskEnabled
  end type shr_gridMaskBorder

contains

  logical function isValid_gridMaskEnabled(self, other)
    !< true if valid gridMaskEnabled properly match non border
    !<
    !< gridMaskBorder(TTFFTT) % isValid(FFTFFF) = T
    !< gridMaskBorder(TTFFTT) % isValid(FFTFFT) = F
    class(shr_gridMaskBorder), intent(in) :: self
    type(shr_gridMaskEnabled), intent(in) :: other
    type(shr_gridMaskBorder) :: reversed

    !reversed = other % reverse()
    !call self % isValid_GridMask(reversed)
  end function isValid_gridMaskEnabled


  logical function isValid_gridMask(self, other)
    !< true if 'other' enabled gridcell fit into self valud gridcells
    !< true if 'other' gridMask true gridcells also match self mask array
    !<
    !< self (TT,FF) % isMaskIncluded(TF,FF) -> true
    !< self (TT,FF) % isMaskIncluded(TF,FT) -> false
    !<
    !< 'self' matches with 'other' gridcells?
    !< ('self' gridcells are disabled in 'other')
    !< partition self
    !< T T F F F T -> True where border
    !<
    !< land grid cells -> True where land grid cells
    !< F F T F T F   -> rev (select disabled) -> T T F T F T
    !<                                              .and.
    !<                                 (border)  T T - - - T
    !< all true? -> yes
    class(shr_gridMaskBorder), intent(in) :: self
    type(shr_gridMask), intent(in) :: other

    type(shr_gridMask) :: reversedMask, disabledMask
    type(string) :: tmp

    !select type (o => other)
    !type is (shr_gridMask)
    !  reversedMask = o
    !class default
    !  call raiseError(__FILE__, &
    !      "gridMask_isIncluded", &
    !      "Unexpected type found instead of 'shr_gridMask'")
    !end select
    tmp = self % toString()
    write(*,*) "gridMask_mod:: gridMask_isIncluded:: self =", tmp % toString()
    !tmp = reversedMask % toString()
    tmp = other % toString()
    write(*,*) "gridMask_mod:: gridMask_isIncluded:: other =", tmp % toString()

    !< select potential border cells
    reversedMask = other
    call reversedMask % reverse()
    tmp = reversedMask % toString()
    write(*,*) "gridMask_mod:: gridMask_isIncluded:: other % reversedMask =", tmp % toString()
    !< potential border cells match with chosen 'border'?
    disabledMask = (self .and. reversedMask)
    tmp = disabledMask % toString()
    write(*,*) "gridMask_mod:: gridMask_isIncluded:: (self and reversed) disabledMask =", tmp % toString()
    !< all match?
    tmp = other % toString()
    write(*,*) "gridMask_mod:: gridMask_isIncluded:: other =", tmp % toString()
    isValid_gridMask = (disabledMask == other)
  end function isValid_gridMask

end module shr_gridMaskBorder_mod

