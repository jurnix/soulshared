!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  shr_arrayDim_mod 
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!> Dimension array descriptor 
!>
!> 
!------------------------------------------------------------------------------
module SHR_arrayDim_mod

  use SHR_precision_mod, only: sp!, dp, eqReal
  use SHR_objects_mod, only: shr_eqObject_abs
  use SHR_strings_mod, only: string
  use SHR_error_mod, only: raiseError 
  use SHR_array_mod, only: initArrayRange

  implicit none

  public :: shr_arrayRealDim

  private


  type, extends(shr_eqObject_abs), abstract :: shr_arrayDim
    type(string) :: name
    integer :: size
  contains
    procedure :: getName
    procedure :: getSize

    procedure :: isInBounds_dummy
    generic :: isInBounds => isInBounds_dummy

    procedure(iface_copy), deferred :: copy
    generic, public :: assignment(=) => copy

    procedure :: getIndex_dummy
    generic :: getIndex => getIndex_dummy
  end type shr_arrayDim


  abstract interface

    pure subroutine iface_copy(self, from)
      !< copy ncOtherDim_mod to another
      import :: shr_arrayDim
      class(shr_arrayDim), intent(inout) :: self
      class(shr_arrayDim), intent(in) :: from
    end subroutine iface_copy

  end interface


  type, extends(shr_arrayDim) :: shr_arrayRealDim
    real(kind=sp) :: start, end, step
    real(kind=sp), allocatable :: values(:)
  contains
    procedure :: init => init_arrayRealDim

    procedure :: copy => copy_arrayRealDim
    procedure :: eq_object => equal_arrayRealDim

    procedure :: getIndex_arrayRealDim
    generic :: getIndex => getIndex_arrayRealDim

    procedure :: isInBounds_arrayRealDim
    generic :: isInBounds => isInBounds_arrayRealDim

    procedure :: getStart_arrayRealDim
    procedure :: getEnd_arrayRealDim
    procedure :: getStep_arrayRealDim
  end type shr_arrayRealDim

contains


  elemental function getName(self) result (name)
    !< it returns the dimenion's name
    class(shr_arrayDim), intent(in) :: self
    type(string) :: name !< output
    name = string(self % name)
  end function getName


  elemental integer function getSize(self) 
    !< it returns the dimension size
    class(shr_arrayDim), intent(in) :: self
    getSize = self % size
  end function getSize


  integer function getIndex_dummy(self)
    !< it returns an index given a 'value'
    !< dummy version to ensure its declared
    class(shr_arrayDim), intent(in) :: self
    call raiseError(__FILE__, "getIndex_dummy", "Dummy subroutine")
  end function getIndex_dummy


  logical function isInBounds_dummy(self)
    !< true if the given 'newValue' fits in arrayDim
    !< dummy
    class(shr_arrayDim), intent(in) :: self
    isInBounds_dummy = .false.
    call raiseError(__FILE__, "isInBounds_dummy", "Dummy subroutine")
  end function isInBounds_dummy


  !
  !========= arrayRealDim ===========
  !


  pure subroutine copy_arrayRealDim(self, from)
    !< copy ncOtherDim_mod to another
    class(shr_arrayRealDim), intent(inout) :: self
    class(shr_arrayDim), intent(in) :: from
  end subroutine copy_arrayRealDim 


  logical function equal_arrayRealDim(self, other)
    !< true if self and other are not the same
    class(shr_arrayRealDim), intent(in) :: self
    class(SHR_eqObject_abs), intent(in) :: other
  end function equal_arrayRealDim


  elemental integer function getIndex_arrayRealDim(self, value) result (idx)
    !< Given value comprised in between start and end
    !< it returns its position in an array (1 to N)
    !< it returns -1 if value is outside bounds
    !<
    !< 2, 4, 6, 8, 10, 12
    !< getIndex(2) -> 1
    !< getIndex(10) -> 5
    !< getIndex(9) -> 4
    class(shr_arrayRealDim), intent(in) :: self
    real(kind=sp), intent(in) :: value

    real(kind=sp) :: calc

    if (.not. self % isInBounds(value)) then
      idx = -1
      return
    endif

    calc = self % start
    idx = 0 !< array 1st position
    do while (calc <= value)
!      write(*,*) "getIndex_ncRealDim: calc, value =", calc, value
      calc = calc + self % step
      idx = idx + 1
    end do
!    write(*,*) "getIndex_ncRealDim: idx =", idx
  end function getIndex_arrayRealDim


  elemental real(kind=sp) function getStart_arrayRealDim(self)
    !< it returns its start value
    class(shr_arrayRealDim), intent(in) :: self
    getStart_arrayRealDim = self % start
  end function getStart_arrayRealDim


  elemental real(kind=sp) function getEnd_arrayRealDim(self)
    !< end value
    class(shr_arrayRealDim), intent(in) :: self
    getEnd_arrayRealDim = self % end
  end function getEnd_arrayRealDim


  elemental real(kind=sp) function getStep_arrayRealDim(self)
    !< it returs step
    class(shr_arrayRealDim), intent(in) :: self
    getStep_arrayRealDim = self % step
  end function getStep_arrayRealDim


  subroutine init_arrayRealDim(self, name, start, end, step)
    !< initialize ncRealDim
    class(shr_arrayRealDim), intent(inout) :: self
    character(*), intent(in) :: name
    real(kind=sp), intent(in) :: start
    real(kind=sp), intent(in) :: end
    real(kind=sp), intent(in), optional :: step !< default = 1.0

    real(kind=sp) :: inStep
    character(5) :: negVal

    inStep = 1.0
    if (present(step)) inStep = step

    if (inStep < 0.) then
      write(negVal, '(F5.1)') inStep
      call raiseError(__FILE__, "init_ncRealDim", &
              "'step' argument cannot be negative", &
              "Found= "// negVal)
    endif

    self % name = name
    self % values = initArrayRange(start, end, inStep)
    self % size = size(self % values)
    self % start = start
    self % end = end
    self % step = inStep
  end subroutine init_arrayRealDim


  elemental logical function isInBounds_arrayRealDim(self, value)
    !< true if given 'value' is in between start and end bounds
    class(shr_arrayRealDim), intent(in) :: self
    real(kind=sp), intent(in) :: value

    isInBounds_arrayRealDim = .true.
    if (self % start > value) then
      isInBounds_arrayRealDim = .false.
      return
    endif

    if (self % end < value) then
      isInBounds_arrayRealDim = .false.
      return
    endif
  end function isInBounds_arrayRealDim

end module SHR_arrayDim_mod
