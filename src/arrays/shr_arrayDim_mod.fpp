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
#:include "../common.fpp"

module SHR_arrayDim_mod

  use SHR_precision_mod, only: sp, dp !, eqReal
  use SHR_objects_mod, only: shr_eqObject_abs
  use SHR_strings_mod, only: string
  use SHR_error_mod, only: raiseError 
  use SHR_arrayUtils_mod, only: initArrayRange

  implicit none

  public :: shr_arrayDim, shr_arrayDimContainer
#:for IKIND, ITYPE, IHEADER in ALL_KINDS_TYPES

  public :: shr_array${IHEADER}$Dim

#:endfor

  private


  type, extends(shr_eqObject_abs) :: shr_arrayDimContainer
    class(shr_arrayDim), allocatable :: arraydim
  contains
    procedure :: getSize => getSize_arrayDimContainer
    procedure :: eq_object => equal_arrayDimContainer
    procedure :: copy_arrayDimContainer
    generic, public :: assignment(=) => copy_arrayDimContainer
  end type shr_arrayDimContainer


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

    procedure :: getValue_dummy
    generic :: getValue => getValue_dummy
  end type shr_arrayDim


  abstract interface

    elemental pure subroutine iface_copy(self, from)
      !< copy ncOtherDim_mod to another
      import :: shr_arrayDim
      class(shr_arrayDim), intent(inout) :: self
      class(shr_arrayDim), intent(in) :: from
    end subroutine iface_copy

  end interface


#:for IKIND, ITYPE, IHEADER in ALL_KINDS_TYPES

  type, extends(shr_arrayDim) :: shr_array${IHEADER}$Dim
    ${ITYPE}$ :: start, end, step
    ${ITYPE}$, allocatable :: values(:)
  contains
    procedure :: init => init_array${IHEADER}$Dim

    procedure :: copy => copy_array${IHEADER}$Dim
    procedure :: eq_object => equal_array${IHEADER}$Dim

    procedure :: getIndex_array${IHEADER}$Dim
    generic :: getIndex => getIndex_array${IHEADER}$Dim

    procedure :: getValue_array${IHEADER}$Dim
    generic :: getValue => getValue_array${IHEADER}$Dim

    procedure :: getAllValues => getAllValues_array${IHEADER}$Dim

    procedure :: isInBounds_array${IHEADER}$Dim
    generic :: isInBounds => isInBounds_array${IHEADER}$Dim

    procedure :: getStart_array${IHEADER}$Dim
    procedure :: getEnd_array${IHEADER}$Dim
    procedure :: getStep_array${IHEADER}$Dim
  end type shr_array${IHEADER}$Dim

#:endfor

contains
  !
  ! shr_arrayDimContainer
  !
  subroutine copy_arrayDimContainer(left, right) 
    !< true if self and other are the same
    class(shr_arrayDimContainer), intent(inout) :: left
    class(shr_arrayDimContainer), intent(in) :: right

    if (allocated(left % arrayDim)) then
      ! different types?
      if (.not. same_type_as(left % arrayDim, right % arrayDim)) &
            deallocate(left % arrayDim)
    endif

    ! is allocated?
    if (.not. allocated(left % arrayDim)) then
      ! allocate only with its type
      allocate(left % arrayDim, mold = right % arrayDim)
    end if
    left % arrayDim = right % arrayDim
  end subroutine copy_arrayDimContainer


  elemental logical function equal_arrayDimContainer(self, other)
    !< true if self and other are the same
    class(shr_arrayDimContainer), intent(in) :: self
    class(SHR_eqObject_abs), intent(in) :: other

    select type(other)
      type is (shr_arrayDimContainer)
        equal_arrayDimContainer = (self % arrayDim == other % arrayDim)
      class default
        equal_arrayDimContainer = .false.
    end select
  end function equal_arrayDimContainer


  elemental integer function getSize_arrayDimContainer(self) 
    !< it returns the dimension size
    class(shr_arrayDimContainer), intent(in) :: self
    getSize_arrayDimContainer = self % arrayDim % getSize()
  end function getSize_arrayDimContainer

  !
  ! shr_arrayDim
  !

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
    call raiseError(__FILE__, "getIndex_dummy", &
            "Dummy subroutine")
  end function getIndex_dummy


  integer function getValue_dummy(self)
    !< it returns an value given the index from array
    !< dummy version only to ensure its declared
    class(shr_arrayDim), intent(in) :: self
    call raiseError(__FILE__, "getValue_dummy", &
            "Dummy subroutine")
  end function getValue_dummy


  logical function isInBounds_dummy(self)
    !< true if the given 'newValue' fits in arrayDim
    !< dummy
    class(shr_arrayDim), intent(in) :: self
    isInBounds_dummy = .false.
    call raiseError(__FILE__, "isInBounds_dummy", &
            "Dummy subroutine")
  end function isInBounds_dummy


  !
  !========= arrayRealDim ===========
  !

#:for IKIND, ITYPE, IHEADER in ALL_KINDS_TYPES

  elemental pure subroutine copy_array${IHEADER}$Dim(self, from)
    !< copy ncOtherDim_mod to another
    class(shr_array${IHEADER}$Dim), intent(inout) :: self
    class(shr_arrayDim), intent(in) :: from

    select type (from)
      type is (shr_array${IHEADER}$Dim)
        self % name = from % name !< parent
        self % size = from % size !< parent
        self % start = from % start
        self % end = from % end
        self % step = from % step
        self % values = from % values
      class default
        !< unexpected type found
    end select
  end subroutine copy_array${IHEADER}$Dim 


  elemental logical function equal_array${IHEADER}$Dim(self, other)
    !< true if self and other are not the same
    class(shr_array${IHEADER}$Dim), intent(in) :: self
    class(SHR_eqObject_abs), intent(in) :: other

    logical :: hasSameAttrs, hasSameValues
    logical :: areBothValuesAllocated, areBothValuesDeallocated
    logical :: haveSameSize

    hasSameValues = .false.
    hasSameAttrs = .false.
    areBothValuesAllocated = .false.
    areBothValuesDeallocated = .false.
    equal_array${IHEADER}$Dim = .false.

    select type(other)
      type is (shr_array${IHEADER}$Dim)
        hasSameAttrs = (self % name == other % name) .and. &
                            self % size == other % size .and. &
                            self % start == other % start .and. &
                            self % end == other % end .and. &
                            self % step == other % step

        ! todo, compare allocatable arrays
        areBothValuesAllocated = (allocated(self % values) .and. allocated(other % values))
        areBothValuesDeallocated = (.not. allocated(self % values) .and. .not. allocated(other % values))

        if (areBothValuesAllocated) then
          haveSameSize = (size(self % values) == size(other % values)) ! same size?
          if (haveSameSize) hasSameValues = ( all(self % values == other % values) )
        else if (areBothValuesDeallocated) then
          hasSameValues = .true.
        else
          hasSameValues = .false.
        endif
      class default
        !< unexpected type error
    end select

    equal_array${IHEADER}$Dim = (hasSameAttrs .and. hasSameValues)
  end function equal_array${IHEADER}$Dim


  elemental impure integer function getIndex_array${IHEADER}$Dim(self, value) result (idx)
    !< Given value comprised in between start and end
    !< it returns its position in an array (1 to N)
    !< it returns -1 if value is outside bounds
    !<
    !< 2, 4, 6, 8, 10, 12
    !< getIndex(2) -> 1
    !< getIndex(10) -> 5
    !< getIndex(9) -> 4
    class(shr_array${IHEADER}$Dim), intent(in) :: self
    ${ITYPE}$, intent(in) :: value
    logical :: isPositive

    real(kind=sp) :: calc

    write(*,*) "getIndex_ncRealDim: starting (", value, ")..."
    write(*,*) "getIndex_ncRealDim: is in bounds? ", self % isInBounds(value), self % start, self % end
    if (.not. self % isInBounds(value)) then
      idx = -1
      return
    endif

    ! decide how to loop due to step sign (increase or decrease)
    isPositive = (self % step > 0)

    if (isPositive) then
      write(*,*) "getIndex_ncRealDim: positive steps (", self % step, ")"
      calc = self % start 
      idx = 0 !< array 1st position
      do while (calc <= value)
        write(*,*) "getIndex_ncRealDim: calc, value =", calc, value
        calc = calc + self % step
        idx = idx + 1
      end do
    else !< is negative 5 .3. 1
      write(*,*) "getIndex_ncRealDim: negative steps (", self % step, ")"
      calc = self % start !5
      idx = 0 !< array 1st position
      do while (calc >= value)
        write(*,*) "getIndex_ncRealDim: calc, value =", calc, value
        calc = calc + self % step
        idx = idx + 1
      end do
    endif

    write(*,*) "getIndex_ncRealDim: idx =", idx
    write(*,*) "getIndex_ncRealDim: starting... DONE"
  end function getIndex_array${IHEADER}$Dim


  elemental function getValue_array${IHEADER}$Dim(self, index) result (value)
    !< it returns the value of a given index of the array 
    class(shr_array${IHEADER}$Dim), intent(in) :: self
    integer, intent(in) :: index
    ${ITYPE}$ :: value
    value = self % values(index)
  end function getValue_array${IHEADER}$Dim


  function getAllValues_array${IHEADER}$Dim(self) result (allValues)
    !< it returns the value of a given index of the array 
    class(shr_array${IHEADER}$Dim), intent(in) :: self
    ${ITYPE}$, allocatable :: allValues(:)
    allValues = self % values
  end function getAllValues_array${IHEADER}$Dim


  elemental ${ITYPE}$ function getStart_array${IHEADER}$Dim(self)
    !< it returns its start value
    class(shr_array${IHEADER}$Dim), intent(in) :: self
    getStart_array${IHEADER}$Dim = self % start
  end function getStart_array${IHEADER}$Dim


  elemental ${ITYPE}$ function getEnd_array${IHEADER}$Dim(self)
    !< end value
    class(shr_array${IHEADER}$Dim), intent(in) :: self
    getEnd_array${IHEADER}$Dim = self % end
  end function getEnd_array${IHEADER}$Dim


  elemental ${ITYPE}$ function getStep_array${IHEADER}$Dim(self)
    !< it returs step
    class(shr_array${IHEADER}$Dim), intent(in) :: self
    getStep_array${IHEADER}$Dim = self % step
  end function getStep_array${IHEADER}$Dim


  subroutine init_array${IHEADER}$Dim(self, name, start, end, step)
    !< initialize ncRealDim
    class(shr_array${IHEADER}$Dim), intent(inout) :: self
    character(*), intent(in) :: name
    ${ITYPE}$, intent(in) :: start
    ${ITYPE}$, intent(in) :: end
    ${ITYPE}$, intent(in), optional :: step !< default = 1.0

    ${ITYPE}$ :: inStep
    character(5) :: negVal

    inStep = 1.0
    if (present(step)) inStep = step

!    if (inStep < 0.) then
!      write(negVal, '(F5.1)') inStep
!      call raiseError(__FILE__, "init_ncRealDim", &
!              "'step' argument cannot be negative", &
!              "Found= "// negVal)
!    endif

    self % name = name
    self % values = initArrayRange(start, end, inStep)
    self % size = size(self % values)
    self % start = start
    self % end = end
    self % step = inStep
  end subroutine init_array${IHEADER}$Dim


  elemental logical function isInBounds_array${IHEADER}$Dim(self, value)
    !< true if given 'value' is in between start and end bounds
    !< 
    !< is positive
    !< start(small) <= value <= end(big)
    !<
    !< is negative
    !< start(big) => value => end(small)
    class(shr_array${IHEADER}$Dim), intent(in) :: self
    ${ITYPE}$, intent(in) :: value
    ${ITYPE}$ :: ismall, ibig
    logical :: isPositive

    isInBounds_array${IHEADER}$Dim = .true.
    isPositive = (self % step > 0)
    if (isPositive) then
      ismall = self % start
      ibig = self % end
    else ! is negative
      ibig = self % start
      ismall = self % end
    endif

    if (ismall > value) then
      isInBounds_array${IHEADER}$Dim = .false.
      return
    endif

    if (ibig < value) then
      isInBounds_array${IHEADER}$Dim = .false.
      return
    endif
  end function isInBounds_array${IHEADER}$Dim

#:endfor

end module SHR_arrayDim_mod
