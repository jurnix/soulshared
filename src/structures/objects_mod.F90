!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : SHR_objects_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!
!> Interface SHR_eqObject_abs enforces classes to implement '=='
!>
!> wrapObject is a generic type used by structures. It controls
!> which types are allowed.
!------------------------------------------------------------------------------
!
module SHR_objects_mod

  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp, dp

  implicit none 

  public :: SHR_eqObject_abs, wrapObject


  !< current wrapObject type
  integer, parameter :: WRAP_OBJ_TYPE_NONE=-1
  integer, parameter :: WRAP_OBJ_TYPE_INT=0
  integer, parameter :: WRAP_OBJ_TYPE_RSP=1
  integer, parameter :: WRAP_OBJ_TYPE_RDP=2
  integer, parameter :: WRAP_OBJ_TYPE_CHAR=3
  integer, parameter :: WRAP_OBJ_TYPE_EQO=4 !< eqObject


  !< interface
  type, abstract :: SHR_eqObject_abs !< equal unlimited polymorphic type
                                 !< for non basic types
  contains
    procedure(iface_eq_object), deferred :: eq_object
    generic :: operator(==) => eq_object
  end type SHR_eqObject_abs


  abstract interface
    logical function iface_eq_object(self, other)
      import SHR_eqObject_abs
      !< true if self and other are not the same
      class(SHR_eqObject_abs), intent(in) :: self
      class(SHR_eqObject_abs), intent(in) :: other
    end function iface_eq_object
  end interface 


  !< wrapObject is used by data structures to hold
  !< Either basic types or SHR_eqObject_abs sub types
  !<
  !< supported wrapObject types
  integer, parameter :: SHR_WRAP_OBJ_TYPE_NONE=-1
  integer, parameter :: SHR_WRAP_OBJ_TYPE_INT=0
  integer, parameter :: SHR_WRAP_OBJ_TYPE_RSP=1
  integer, parameter :: SHR_WRAP_OBJ_TYPE_RDP=2
  integer, parameter :: SHR_WRAP_OBJ_TYPE_CHAR=3
  integer, parameter :: SHR_WRAP_OBJ_TYPE_EQO=4 !< eqObject


  ! setObject generic container for 'set' data structure
  type, extends(SHR_eqObject_abs) :: wrapObject
    class(*), pointer :: obj => null()

    !< cast value
    class(SHR_eqObject_abs), pointer :: eqObj => null()
    integer, pointer :: intObj => null()
    character(:), pointer :: chrObj => null()
    real(kind=sp), pointer :: rspObj => null()
    real(kind=dp), pointer :: rdpObj => null()

    integer :: type !< NONE=-1, 0=int, 1=rsp, 2=rdp, 3=char, 4=SHR_eqObject_abs
  contains
    procedure :: init => init_wrapObject !< constructor_wrapObject
    procedure :: eq_object => eq_wrapObject
  end type wrapObject


  interface wrapObject
    module procedure constructor_wrapObject
  end interface wrapObject

contains


  logical function eq_wrapObject(self, other)
    !< true if self and other are the same type and have the same values
    !< false if same type and different values
    !< false if different types
    !< error if other is not an otherWrapObj
    class(wrapObject), intent(in) :: self
    class(SHR_eqObject_abs), intent(in) :: other

    class(wrapObject), pointer :: otherWrapObj

    ! cast 'other' to wrapObject
    select type(wrap => other)
    class is (wrapObject)
      otherWrapObj => wrap
    class default
      !< error
      call raiseError(__FILE__, "eq_wrapObject", &
              "Unexpected 'other' class type found", &
              "It is only allowed wrapObject type")
    end select

    if (self % type /= otherWrapObj % type) then
      eq_wrapObject = .false.
      return
    endif

    if (self % type == WRAP_OBJ_TYPE_INT) then
      eq_wrapObject = (self % intObj == otherWrapObj % intObj)
    else if (self % type == WRAP_OBJ_TYPE_CHAR) then
      eq_wrapObject = (self % chrObj == otherWrapObj % chrObj)
    else if (self % type == WRAP_OBJ_TYPE_RSP) then
      eq_wrapObject = (self % rspObj == otherWrapObj % rspObj)
    else if (self % type == WRAP_OBJ_TYPE_RDP) then
      eq_wrapObject = (self % rdpObj == otherWrapObj % rdpObj)
    else if (self % type == WRAP_OBJ_TYPE_EQO) then
      eq_wrapObject = (self % eqObj == otherWrapObj % eqObj)
    else
      !< assert
      call raiseError(__FILE__, "eq_wrapObject", &
              "Unexpected error found", &
              "Non supported and misterious type found")
    endif

  end function eq_wrapObject


  type(wrapObject) function constructor_wrapObject(obj)
    !< wrap unlimited polymorhpic type
    class(*), intent(in), pointer :: obj
    call constructor_wrapObject % init(obj)
  end function constructor_wrapObject


  subroutine init_wrapObject(self, obj)
    !< initialize wrapObject
    !< Wrap unlimited polymorhpic type
    class(wrapObject), intent(inout) :: self
    class(*), intent(in), pointer :: obj

    self % obj => obj
    self % type = WRAP_OBJ_TYPE_NONE

    ! discover and cast type
    select type(wrap => obj) 
    type is (integer)
      write(*,*) "set_mod:: self:: integer type found"
      self % type = WRAP_OBJ_TYPE_INT
      self % intObj => wrap
    type is (character(*))
      write(*,*) "set_mod:: self:: char type found"
      self % type = WRAP_OBJ_TYPE_CHAR
      self % chrObj => wrap
    type is (real(kind=sp))
      write(*,*) "set_mod:: self:: real sp type found"
      self % type = WRAP_OBJ_TYPE_RSP
      self % rspObj => wrap
    type is (real(kind=dp))
      write(*,*) "set_mod:: self:: real dp type found"
      self % type = WRAP_OBJ_TYPE_RDP
      self % rdpObj => wrap
    class is (SHR_eqObject_abs)
      write(*,*) "set_mod:: self:: SHR_eqObject_abs type found"
      self % type = WRAP_OBJ_TYPE_EQO
      self % eqObj => wrap
    class default
      !< assert
      call raiseError(__FILE__, "init_wrapObject", &
              "Unsupported type found", &
              "Supported types: int, char, rsp, dsp and SHR_eqObject_abs")
    end select
  end subroutine init_wrapObject

end module SHR_objects_mod
