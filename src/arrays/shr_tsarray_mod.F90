!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  SHR_shr_tsArray_mod
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!> shr_tsArray or timestepArray
!> Given an array it provides data with another timestep.
!> Those steps must always be bigger than the given clock.
!>
!> timestep   1 2 3 4 5 6 7 8 9 10  ...
!> data       2 3 4 3 5 2 4 3 3 2   ...
!> 20 min ts  - - - - - - - - - -   ... -> simulation timestep
!> 1  h   ts  -     -     -     -   ... -> shr_tsArray timestep
!> data (ave) 3     3.3   3,3   ? (lost values)
!> data (min) 2     2     3     ?
!> data (max) 4     5     4     ?
!> data (frst)2     3     4     ?
!> data (lst) 4     2     3     ?
!>
!> isFirstTs  T     T     T     T   ...
!> isLastTs       T     T     T     ...
!>
!> 
!------------------------------------------------------------------------------
module shr_tsArray_mod

  use SHR_precision_mod, only: sp, dp
  use SHR_error_mod, only: raiseError 
  use SHR_strings_mod, only: string
  use SHR_datetime_mod, only: clock, timedelta
  use SHR_array_mod, only: shr_array!, pAbsArray, allocAbsArray, sp_rArray, sp_rpArray, min, max
  use shr_arrayDim_mod, only: shr_arrayDim, shr_arrayDimContainer

  implicit none

  private

  public :: shr_tsArray, SHR_TSARRAY_OP_AVE, SHR_TSARRAY_OP_MIN, SHR_TSARRAY_OP_MAX, SHR_TSARRAY_OP_FIRST, SHR_TSARRAY_OP_LAST

  ! created only to compile
  type sp_rArray
  end type

  ! created only to compile
  type sp_rpArray
  end type


  ! shr_tsArray operation modes
  integer, parameter :: SHR_TSARRAY_OP_AVE = 1 !< average
  integer, parameter :: SHR_TSARRAY_OP_MIN = 2 !< minimum
  integer, parameter :: SHR_TSARRAY_OP_MAX = 3 !< maximum 
  integer, parameter :: SHR_TSARRAY_OP_FIRST = 4 !< first value
  integer, parameter :: SHR_TSARRAY_OP_LAST = 5 !< last value 

  !< timeScaled Array. It only support data pointer
!  type, extends(shr_array) :: shr_tsArray !< temporal array (clock), infered todo- support >1 ts variable for the same
  type :: shr_tsArray !< temporal array (clock), infered todo- support >1 ts variable for the same
    type(clock) :: internalTime !< sync from simulations clock
    integer :: operation !< what to do when rescaling to another time sacle
    type(timedelta) :: newTs !< new variable time scale (always bigger the clock timestep)
    type(timedelta) :: counterTs !< accumulated timesteps from clock interval. 
                                 !< It is initialized to 0 when counterTs => newTs 

    logical :: isFirstTs        !< true at the beginning of the shr_tsArray timestep
    logical :: isLastTs         !< true at the end of the shr_tsArray timestep

!    class(pAbsArray), allocatable :: pdata !< feeding data(pointer)
!    class(allocAbsArray), allocatable :: opData !< temporary data calculated according to 'operation'

    integer :: aveCounter !< accumulate counter to do average
    logical :: hasStarted 
  contains
    procedure :: init_array => init_array_rsp

    procedure :: update !< simulation clock update 
    procedure :: getData_r1_shr_tsArray
    generic :: getData => getData_r1_shr_tsArray !< it returns the timescaled data

   ! procedure :: getTimeScale !< timescaled timestep

    procedure :: getInitVal_r1_sp !< it returns the default initial value. it depends on the operation mode
    generic :: getInitVal => getInitVal_r1_sp

    procedure :: hasNewDataAvail  !< true if there is new data available 

    ! private
    procedure :: getInitCounterTs !< initial default value to reinitiaze accumulative operations
  end type shr_tsArray

  interface shr_tsArray
    module procedure :: shr_tsArray_constructor_r1
  end interface shr_tsArray

contains

  real(kind=sp) function getInitVal_r1_sp(self, op)
    !< find initial value to set up according to the selection operation
    class(shr_tsArray), intent(inout) :: self
    integer, intent(in) :: op !< operation selected 

    real(kind=sp) :: initVal !< defaut initial value
    real(kind=sp), parameter :: rspNum = 0. !< real sp number. Type matters

    ! initial value
    if (op == SHR_TSARRAY_OP_AVE) then
      initVal = real(0., kind=sp)
    else if (op == SHR_TSARRAY_OP_MIN) then
      initVal = huge(rspNum) ! biggest possible real(sp) number
    else if (op == SHR_TSARRAY_OP_MAX) then
      initVal = -huge(rspNum) ! smallest possible real(sp) number
    else ! default value
      initVal = real(0., kind=sp)
    endif
    getInitVal_r1_sp = initVal

  end function getInitVal_r1_sp


  pure subroutine init_array_rsp(self, name, dimensions, units, description)
    !< shr_arrayRsp initialization
    class(shr_tsArray), intent(inout) :: self
    type(string), intent(in) :: name
    type(shr_arrayDimContainer), intent(in) :: dimensions(:)
    type(string), intent(in) :: units
    type(string), intent(in) :: description

    ! todo, check vs constructor
    ! decouple from shr_array?

  end subroutine init_array_rsp


  type (timedelta) function getInitCounterTs(self, ts)
    !< get initial value for counterTs
    class(shr_tsArray), intent(inout) :: self
    type(timedelta), intent(in) :: ts

    getInitCounterTs = ts - ts
  end function getInitCounterTs


  subroutine getData_r1_shr_tsArray(self, r)
    class(shr_tsArray), intent(inout) :: self
    real(kind=sp), allocatable, intent(out) :: r(:)

!    class(allocAbsArray), pointer :: wrap

    if (.not. self % hasNewDataAvail()) then
      call raiseError(__FILE__, "getData_r1_shr_tsArray", &
              "There is no new data available yet")
    endif
!    write(*,*) "array_mod:: getData_r1_shr_tsArray:: allocated(opData)? ", allocated(self % opData)

!    select type(wrap => self % opData)
!    type is (sp_rArray)
!      call wrap % getData_r1_sp_rArray(r)
!    class default
      ! not implemented error
!      call raiseError(__FILE__, "getData_r1_shr_tsArray", "Class/Type not found")
!    end select ! self
     ! todo, enable unit tests properly run
     allocate(r(1))
     r = -1

!    write(*,*) "array_mod:: getData_r1_shr_tsArray:: r=", r
  end subroutine getData_r1_shr_tsArray


  type(shr_tsArray) function shr_tsArray_constructor_r1(name, dimsName, sClock, operation, timeScale, pdata)

    character(len=*), intent(in) :: name !< array name
    type(string), intent(in) :: dimsName(:)
    type(clock),intent(in) :: sClock !< simulation clock
    integer, intent(in) :: operation !< operation to do
    type(timedelta) , intent(in) :: timeScale !< new time scale info. it has to be multiple of sClock ts
    real(kind=sp), contiguous, target, intent(in) :: pdata(:) !< pointer to data to rescale

    integer, parameter ::  NDIMS=1
    integer :: dimsSize(NDIMS)
    class(sp_rArray), pointer :: wrap
    class(sp_rpArray), pointer :: wrapPoint
    real(kind=sp) :: initVal !< default value to initialize shr_tsArray

!    allocate(shr_tsArray_constructor_r1 % name, source = string(name))
!    shr_tsArray_constructor_r1 % dimsName = dimsName
    shr_tsArray_constructor_r1 % internalTime = sClock
    shr_tsArray_constructor_r1 % operation = operation
    shr_tsArray_constructor_r1 % newTs = timeScale
    shr_tsArray_constructor_r1 % hasStarted = .false.
  
    ! check arguments are consistent 
    if (sClock % tickInterval > timeScale ) then
      call raiseError(__FILE__, "shr_tsArray_constructor_r1", &
              "Given timeStep cannot be smaller than current clock timeStep", &
              "Given timestep: "//timeScale % toString(), &
              "Clock timestep: "//sClock % tickInterval % toString())
    endif

    ! initial value
    initVal = shr_tsArray_constructor_r1 % getInitVal(operation)

    dimsSize(1) = size(dimsName, dim=1)
!    shr_tsArray_constructor_r1 % pData = sp_rpArray(name, dimsName, pdata)
!    shr_tsArray_constructor_r1 % opData = sp_rArray(name//"Ts", dimsName, dimsSize, initVal) !< init accum to 0
    shr_tsArray_constructor_r1 % aveCounter = 0
    shr_tsArray_constructor_r1 % isFirstTs = .false. 
    shr_tsArray_constructor_r1 % isLastTs = .false.
    shr_tsArray_constructor_r1 % counterTs  = shr_tsArray_constructor_r1 % getInitCounterTs(sClock % tickInterval) 

  end function shr_tsArray_constructor_r1


  subroutine update(self)
    !< update internal time step, update data when required
    class(shr_tsArray), intent(inout) :: self

    character(len=10) :: tmp
    class(sp_rArray), pointer :: wrap
    class(sp_rpArray), pointer :: wrapPoint

    !write (*,*) "Array_mod:: update:: starting ", self % name, " ..."

    if (.not. self % hasStarted) self % hasStarted = .true.

    ! is last Ts? 
    self % isLastTs = (self % counterTs + self % internalTime % tickInterval) >= self % newTs

    ! is first Ts?
    self % isFirstTs = (self % counterTs == self % getInitCounterTs(self % internalTime % tickInterval))

    ! reinitialize calculated data
!    if (self % isFirstTs) self % opData = self % getInitVal(self % operation)

    ! apply next time step
    call self % internalTime % tick()
    self % counterTs = self % counterTs + self % internalTime % tickInterval

    if (self % operation == SHR_TSARRAY_OP_AVE) then
!      self % opData = self % opData + self % pData
      self % aveCounter = self % aveCounter + 1

    else if (self % operation == SHR_TSARRAY_OP_MIN) then
!      self % opData = min(self % opData, self % pData)

    else if (self % operation == SHR_TSARRAY_OP_MAX) then
!      self % opData = max(self % opData, self % pData)

    else if (self % operation == SHR_TSARRAY_OP_FIRST) then
      if (self % isFirstTs) then
!        self % opData = self % pData
      endif

    else if (self % operation == SHR_TSARRAY_OP_LAST) then
!      if (self % isLastTs) self % opData = self % pData

    else
      ! not supported
      write(tmp,*) self % operation
      call raiseError(__FILE__, "update", &
                      "Operation mode not recognized (1 to 5)", &
                      "Operation found: "//trim(adjustl(tmp)))
    endif

    if (self % isLastTs) then
       if (self % operation == SHR_TSARRAY_OP_AVE) then
         ! apply average
!         self % opData = self % opData / self % aveCounter
         self % aveCounter = 0
       endif
       self % counterTs = self % internalTime % tickInterval - self % internalTime % tickInterval
    endif
    !write(*,*) "tsarray_mod::update:: last, name=", self % name
    !write(*,*) "tsarray_mod::update:: last, counterTs=", self % counterTs % toString()
    !write(*,*) "tsarray_mod::update:: last, tickInterval=", self % internalTime % tickInterval % toString()
    !write(*,*) "tsarray_mod::update:: last, operation=", self % operation
    !write(*,*) "tsarray_mod::update:: last, newTs=", self % newTs % toString()
    !write(*,*) "tsarray_mod::update:: last, aveCounter=", self % aveCounter 
    !write(*,*) "tsarray_mod::update:: last, isFirstTs", self % isFirstTs
    !write(*,*) "tsarray_mod::update:: last, isLastTs", self % isLastTs
     
  end subroutine update


  logical function hasNewDataAvail(self)
    !< true if there is new data available
    !< New data is available at the very beggining of the simulation
    !< or when the new timestep 
    class(shr_tsArray) , intent(inout) :: self


    hasNewDataAvail = (self % isLastTs .or. .not. self % hasStarted )

    !write(*,*) "shr_tsArray_mod:: hasNewDataAvail:: name=", self % name
    !write(*,*) "shr_tsArray_mod:: hasNewDataAvail:: isFirstTs=", self % isFirstTs
    !write(*,*) "shr_tsArray_mod:: hasNewDataAvail:: isLastTs=", self % isLastTs
    !write(*,*) "shr_tsArray_mod:: hasNewDataAvail:: hasStarted=", self % hasStarted 
    !write(*,*) "shr_tsArray_mod:: hasNewDataAvail=", hasNewDataAvail

  end function hasNewDataAvail

end module shr_tsArray_mod
