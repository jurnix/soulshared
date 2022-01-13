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
!> Interface shr_equal_iface enforces classes to implement '=='
!> Interface shr_string_iface enforces classes to implement 'toString()'
!>
!> shr_genericObject is a generic type used across the model.
!>
!------------------------------------------------------------------------------
!
module shr_objects_mod

  use shr_error_mod, only: raiseError
  use shr_precision_mod, only: sp, dp

  implicit none 

  public :: shr_string_iface
  public :: shr_equal_iface
  public :: shr_genericObject

  logical, parameter :: IS_DEBUG = .false.


  !< string interface
  type, abstract :: shr_string_iface
  contains
    procedure(iface_toString), deferred :: toString
  end type shr_string_iface


  abstract interface
    subroutine iface_toString(self)
      import :: shr_string_iface
      !< toString interface
      class(shr_string_iface), intent(in) :: self
    end subroutine iface_toString
  end interface


  !< interface
  type, abstract :: shr_equal_iface !< equal unlimited polymorphic type
                                 !< for non basic types
  contains
    procedure(iface_equal), deferred :: equal
    generic :: operator(==) => equal

    !procedure :: toWrapObject
  end type shr_equal_iface


  abstract interface
    elemental logical function iface_equal(self, other)
      import shr_equal_iface
      !< true if self and other are not the same
      class(shr_equal_iface), intent(in) :: self
      class(shr_equal_iface), intent(in) :: other
    end function iface_equal
  end interface 


  !< shr_wrapObject is used by data structures to hold
  !< Either basic types or SHR_eqObject_abs sub types
  !<
  !< supported wrapObject types
  integer, parameter :: SHR_GENERIC_OBJ_TYPE_NONE=-1
  integer, parameter :: SHR_GENERIC_OBJ_TYPE_INT=0
  integer, parameter :: SHR_GENERIC_OBJ_TYPE_RSP=1
  integer, parameter :: SHR_GENERIC_OBJ_TYPE_RDP=2
  integer, parameter :: SHR_GENERIC_OBJ_TYPE_CHAR=3
  integer, parameter :: SHR_GENERIC_OBJ_TYPE_EQO=4 !< eqObject


  ! setObject generic container for 'set' data structure
  type, extends(shr_equal_iface) :: shr_genericObject
    class(*), pointer :: obj => null()

    !< cast value
    class(shr_equal_iface), pointer :: eqObj => null()
    integer, pointer :: intObj => null()
    character(:), pointer :: chrObj => null()
    real(kind=sp), pointer :: rspObj => null()
    real(kind=dp), pointer :: rdpObj => null()

    integer :: type !< NONE=-1, 0=int, 1=rsp, 2=rdp, 3=char, 4=SHR_eqObject_abs
  contains
    procedure :: init => init_genericObject !< constructor_wrapObject
    procedure :: equal => equal_genericObject
  end type shr_genericObject


  interface shr_genericObject
    module procedure constructor_genericObject
  end interface shr_genericObject

contains


  elemental logical function equal_genericObject(self, other)
    !< true if self and other are the same type and have the same values
    !< false if same type and different values
    !< false if different types
    !< error if other is not an otherWrapObj
    class(shr_genericObject), intent(in) :: self
    class(shr_equal_iface), intent(in) :: other

    class(shr_genericObject), pointer :: otherWrapObj

    ! cast 'other' to shr_genericObject
    select type(wrap => other)
    class is (shr_genericObject)
      otherWrapObj => wrap
    class default
      !< error
!      call raiseError(__FILE__, "eq_wrapObject", &
!              "Unexpected 'other' class type found", &
!              "It is only allowed shr_genericObject type")
    end select

    if (self % type /= otherWrapObj % type) then
      equal_genericObject = .false.
      return
    endif

    if (self % type == SHR_GENERIC_OBJ_TYPE_INT) then
      equal_genericObject = (self % intObj == otherWrapObj % intObj)
    else if (self % type == SHR_GENERIC_OBJ_TYPE_CHAR) then
      equal_genericObject = (self % chrObj == otherWrapObj % chrObj)
    else if (self % type == SHR_GENERIC_OBJ_TYPE_RSP) then
      equal_genericObject = (self % rspObj == otherWrapObj % rspObj)
    else if (self % type == SHR_GENERIC_OBJ_TYPE_RDP) then
      equal_genericObject = (self % rdpObj == otherWrapObj % rdpObj)
    else if (self % type == SHR_GENERIC_OBJ_TYPE_EQO) then
      equal_genericObject = (self % eqObj == otherWrapObj % eqObj)
    else
      !< assert
!      call raiseError(__FILE__, "eq_wrapObject", &
!              "Unexpected error found", &
!              "Non supported and misterious type found")
      equal_genericObject = .false.
    endif

  end function equal_genericObject


  type(shr_genericObject) function constructor_genericObject(obj)
    !< wrap unlimited polymorhpic type
    class(*), intent(in), pointer :: obj
    call constructor_genericObject % init(obj)
  end function constructor_genericObject


  subroutine init_genericObject(self, obj)
    !< initialize shr_genericObject
    !< Wrap unlimited polymorhpic type
    class(shr_genericObject), intent(inout) :: self
    class(*), intent(in), pointer :: obj

    self % obj => obj
    self % type = SHR_GENERIC_OBJ_TYPE_NONE

    ! discover and cast type
    select type(wrap => obj) 
    type is (integer)
      if (IS_DEBUG) write(*,*) "set_mod:: self:: integer type found"
      self % type = SHR_GENERIC_OBJ_TYPE_INT
      self % intObj => wrap
    type is (character(*))
      if (IS_DEBUG) write(*,*) "set_mod:: self:: char type found"
      self % type = SHR_GENERIC_OBJ_TYPE_CHAR
      self % chrObj => wrap
    type is (real(kind=sp))
      if (IS_DEBUG) write(*,*) "set_mod:: self:: real sp type found"
      self % type = SHR_GENERIC_OBJ_TYPE_RSP
      self % rspObj => wrap
    type is (real(kind=dp))
      if (IS_DEBUG) write(*,*) "set_mod:: self:: real dp type found"
      self % type = SHR_GENERIC_OBJ_TYPE_RDP
      self % rdpObj => wrap
    class is (SHR_genericObject)
      if (IS_DEBUG) write(*,*) "set_mod:: self:: SHR_eqObject_abs type found"
      self % type = SHR_GENERIC_OBJ_TYPE_EQO
      self % eqObj => wrap
    class default
      !< assert
      call raiseError(__FILE__, "init_genericObject", &
              "Unsupported type found", &
              "Supported types: int, char, rsp, dsp and SHR_eqObject_abs")
    end select
  end subroutine init_genericObject


  !elemental impure function toWrapObject(self) result (generic)
    !< convert a specific class to shr_wrapObject generic 
  !  class(shr_eqObject_Abs), target, intent(in) :: self
  !  type(shr_wrapObject) :: generic !< output
  !  class(*), pointer :: wrap
  !  wrap => self
  !  call generic % init(wrap)
  !end function toWrapObject

end module shr_objects_mod
