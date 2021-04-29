!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : SHR_typeOptions_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> 
!> Options
!> It allows to define which options expect, how many values and its type.
!> 
!> An option is caracterized by:
!> key, description, number of expected arguments (nargs), type (int, real, ...), 
!> default value and minimum and maximum (depending of the type).
!>
!> key:
!> description:
!> default (optional): used when value is not defined
!> nargs: how many values are required (0 to OPT_NARGS_N)
!>      0 to OPT_NARGS_N
!>      1 to OPT_NARGS_N
!>      fixed args (2)
!>      multiple args (start, end)
!>
!> Allowed options types:
!> - str (string)
!> - int
!> - files (file_t)
!> - reals (single precision)
!> - datetime (datetime_mod)
!> - timedelta (datetime_mod)
!> - choice: only allowed with the previous options types (exclucing itself)
!>
!------------------------------------------------------------------------------
module SHR_typeOptions_mod

  use SHR_precision_mod, only: sp
  use SHR_datetime_mod, only: datetime, timedelta, DT_MAX_LEN, TD_MAX_LEN
  use SHR_strings_mod, only: string, equalStringsList, stringCollection, int2string, real2string, sanitize
  use SHR_strings_mod, only: findloc 
  use SHR_oopExtends_mod, only: all_same_type_as 
  use SHR_error_mod, only: raiseError, raiseUnexpectedError

  use SHR_file_mod, only: file_t, FILE_NAME_MAX_LEN

  implicit none

  private

  public :: option_abs, option_str, option_files, option_reals, option_ints, &
          option_datetimes, option_timedeltas, option_bools
  public :: CFG_AVAILABLE
  public :: CFG_STR, CFG_FILES, CFG_REALS, CFG_INTS, &
            CFG_TIMEDELTAS, CFG_DATETIMES, CFG_BOOLS
  public :: argsRange, OPT_NARGS_N


  ! available config fields
  integer, parameter :: CFG_STR = 1
  integer, parameter :: CFG_FILES = 2
  integer, parameter :: CFG_REALS = 3
  integer, parameter :: CFG_INTS = 4
  integer, parameter :: CFG_TIMEDELTAS = 5
  integer, parameter :: CFG_DATETIMES = 6
  integer, parameter :: CFG_BOOLS = 7

  integer, parameter :: CFG_NELEMS = 7
  character(len=15), parameter :: CFG_NAMES(CFG_NELEMS) = [ &
          "CFG_STR       ", &
          "CFG_FILES     ", &
          "CFG_REALS     ", &
          "CFG_INTS      ", &
          "CFG_TIMEDELTAS", &
          "CFG_DATETIMES ", &
          "CFG_BOOLS     "]

  character(len=*), parameter :: STR_SPLIT = "," !< char to define how to split a character


  ! list all config fields avaiable
  integer, parameter :: CFG_AVAILABLE(7) = [CFG_STR, CFG_FILES, &
                CFG_REALS, CFG_INTS, &
                CFG_TIMEDELTAS, CFG_DATETIMES, CFG_BOOLS]

  !< nargs parameters
  integer, parameter :: OPT_NARGS_N = -1 !< as many args as desired
  character(len=*), parameter :: EMPTY_VAL = "undefined"


  type, abstract :: option_abs
    character(len=:), allocatable :: key 
    character(len=:), allocatable :: description 
    type(argsRange), allocatable :: argumentsRange !< how many args 
  contains
!    procedure(iface_option_get), deferred :: getVal
    procedure :: dummy_SetVal
    generic :: setVal => dummy_setVal

    procedure :: dummy_getVal
    generic :: getVal => dummy_getVal

    procedure(equiv_option_iface), deferred :: equiv_option
    generic :: operator(==) => equiv_option

    procedure :: checkNargs !< error if not satisfied

    procedure(toString_iface), deferred :: toString
    procedure :: toNiceString
  end type option_abs

  
  abstract interface
    impure elemental logical function equiv_option_iface(a, b)
      import option_abs
      class(option_abs), intent(in) :: a
      class(option_abs), intent(in) :: b
    end function equiv_option_iface

    function toString_iface(this) result(r)
      import option_abs
      class(option_abs), intent(in) :: this
      character(len=:), allocatable :: r
    end function toString_iface
  end interface


  ! bool(s)
  type, extends(option_abs) :: option_bools
    logical, allocatable :: values(:) !< defined strings
    logical, allocatable :: defVal(:) !< default string
  contains 
    procedure :: initialize => initialize_option_bools
    procedure :: setVal_bools
    procedure :: setVal_many_bools
    generic :: setVal => setVal_bools, setVal_many_bools

    procedure :: equiv_option => equiv_option_bool

    procedure :: getVal_option_bools
    generic :: getVal => getVal_option_bools

    procedure :: toString => toString_option_bool
  end type option_bools


  ! string(s)
  type, extends(option_abs) :: option_str
    type(string), allocatable :: choices(:) !< available choices
    type(string), allocatable :: values(:) !< defined strings
    type(string), allocatable :: defVal(:) !< default string
  contains 
    procedure :: initialize => initialize_option_str
    procedure :: setVal_str_chars
    procedure :: setVal_str_string
    procedure :: setVal_str_many_strings
    generic :: setVal => setVal_str_chars, setVal_str_string, setVal_str_many_strings

    procedure :: equiv_option => equiv_option_str

    procedure :: getVal_option_str
    generic :: getVal => getVal_option_str

    procedure :: toString => toString_option_str

    procedure :: isValueAllowed => isValueAllowed_str
  end type option_str


  interface option_str
    module procedure option_str_constructor
  end interface option_str


  ! file(s) 
  type, extends(option_abs) :: option_files
    type(file_t), allocatable :: choices(:)
    type(file_t), allocatable :: files(:) !< defined files
    type(file_t), allocatable :: defFile(:) !< default file
  contains
    procedure :: initialize => initialize_option_files !< same as constructor (to use with pointers)
    procedure :: setVal_files_chars
    procedure :: setVal_files_string
    procedure :: setVal_files_many_strings
    procedure :: setVal_files_scalar
    procedure :: setVal_files_many_files
    generic :: setVal => setVal_files_chars, setVal_files_string, &
            setVal_files_many_strings, setVal_files_scalar, &
            setVal_files_many_files

    procedure :: equiv_option => equiv_option_files

    procedure :: getVal_option_files
    generic :: getVal => getVal_option_files

    procedure :: toString => toString_option_files
    procedure :: isValueAllowed => isValueAllowed_file
  end type option_files


  interface option_files
    module procedure option_files_constructor
  end interface option_files


  ! real(s)
  type, extends(option_abs) :: option_reals
    real(kind=sp), allocatable :: choices(:)
    real(kind=sp), allocatable :: values(:) !< chosen reals
    real(kind=sp), allocatable :: defValue(:) !< default values
    real(kind=sp), allocatable :: minvalue
    real(kind=sp), allocatable :: maxvalue 
  contains
    procedure :: initialize => initialize_option_reals !< same as constructor (to use with pointers)
    procedure :: isMinEnabled !< true if min value is defined
    procedure :: isMaxEnabled !< true if max value is defined

    procedure :: checkMinError !< error if given value is below defined min value
    procedure :: checkMaxError !< error if given value is above defined max value

    procedure :: setVal_real_sp
    procedure :: setVal_many_reals_sp
    generic :: setVal => setVal_real_sp, setVal_many_reals_sp

    procedure :: equiv_option => equiv_option_reals

    procedure :: getVal_option_reals
    generic :: getVal => getVal_option_reals

    procedure :: toString => toString_option_reals
    procedure :: isValueAllowed => isValueAllowed_real
  end type option_reals


  interface option_reals
    module procedure option_reals_constructor
  end interface option_reals


  ! integer(s) (mirror from option_reals)
  type, extends(option_abs) :: option_ints
    integer, allocatable :: choices(:)
    integer, allocatable :: values(:)
    integer, allocatable :: defValue(:)
    integer, allocatable :: minvalue
    integer, allocatable :: maxvalue
  contains
    procedure :: initialize => initialize_option_ints !< same as constructor (to use with pointers)
    procedure :: isMinEnabled => isMinEnabled_optionInt !< true if min value is defined
    procedure :: isMaxEnabled => isMaxEnabled_optionInt!< true if max value is defined

    procedure :: checkMinError => checkMinError_optionInt !< error if given value is below defined min value
    procedure :: checkMaxError => checkMaxError_optionInt !< error if given value is above defined max value

    procedure :: setVal_int_sp
    procedure :: setVal_many_ints_sp
    generic :: setVal => setVal_int_sp, setVal_many_ints_sp

    procedure :: equiv_option => equiv_option_ints

    procedure :: getVal_option_ints
    generic :: getVal => getVal_option_ints

    procedure :: toString => toString_option_ints
    procedure :: isValueAllowed => isValueAllowed_int
  end type option_ints


  interface option_ints
    module procedure option_ints_constructor
  end interface option_ints


  ! datetime(s)
  type, extends(option_abs) :: option_datetimes
    type(datetime), allocatable :: choices(:)
    type(datetime), allocatable :: dates(:)
    type(datetime), allocatable :: defDate
    type(datetime), allocatable :: minDate
    type(datetime), allocatable :: maxDate
  contains
    procedure :: initialize => initialize_option_datetimes
    procedure :: isMinEnabled => isMinEnabled_Datetime !< true if min value is defined
    procedure :: isMaxEnabled => isMaxEnabled_Datetime !< true if max value is defined

    procedure :: checkMinError => checkMinError_Datetime !< error if given value is below defined min value
    procedure :: checkMaxError => checkMinError_Datetime !< error if given value is above defined max value

    procedure :: setVal_datetime
    procedure :: setVal_many_datetimes
    generic :: setVal => setVal_datetime, setVal_many_datetimes

    procedure :: equiv_option => equiv_option_datetimes

    procedure :: getVal_option_datetimes
    generic :: getVal => getVal_option_datetimes

    procedure :: toString => toString_option_datetimes
    procedure :: isValueAllowed => isValueAllowed_datet
  end type option_datetimes


  interface option_datetimes
    module procedure option_datetimes_constructor
  end interface option_datetimes


  ! timedelta(s)
  type, extends(option_abs) :: option_timedeltas
    type(timedelta), allocatable :: choices(:)
    type(timedelta), allocatable :: values(:)
    type(timedelta), allocatable :: defValue(:)
    type(timedelta), allocatable :: mintd, maxtd
  contains
    procedure :: initialize => initialize_option_timedeltas
    procedure :: setVal_timedelta
    procedure :: setVal_many_timedeltas
    generic :: setVal => setVal_timedelta, setVal_many_timedeltas

    procedure :: equiv_option => equiv_option_timedeltas    

    procedure :: getVal_option_timedeltas
    generic :: getVal => getVal_option_timedeltas

    procedure :: toString => toString_option_timedeltas
    procedure :: isValueAllowed => isValueAllowed_tdelta
  end type option_timedeltas


  interface option_timedeltas
    module procedure option_timedeltas_constructor
  end interface option_timedeltas


  !< argsRange constant values
  integer, parameter :: ARGSRANGE_MAX_ELEMS = 2 !< count start and end positions

  type argsRange
    !< how many args (list-0 to N-, list-1 to N-, fixed number(3), 1)
    !< defined from (no negatives numbers allowed): 
    !< 2 pos: start, end (or default if not specified)
    !< 1 pos: fixed number (or default if not specified)
    !< use ARGS_N to define any number given
    integer, allocatable :: range(:)
  contains
!    procedure :: isFixed !< true if only unique number
    procedure :: isAllowed !< true if the given a number of arguments is valid according to its rules
    procedure :: toString => argsRange_toString !< string class representation
    procedure :: getStartRange !< minimum number of elements to contains
    procedure :: getEndRange !< max number of elements to contains

    procedure :: equiv_argsRange
    generic :: operator(==) => equiv_argsRange
  end type argsRange


  interface argsRange
    module procedure argsRange_constructor
  end interface argsRange


  !< Subroutines to deal with allocatable vars
  !< Wrapper to equiv_... subroutines
  interface hasSameDatetime
    module procedure hasSameDatetime_scalar, hasSameDatetime_array
  end interface hasSameDatetime

  interface hasSameTimedelta
    module procedure hasSameTimedelta_scalar, hasSameTimedelta_array
  end interface hasSameTimedelta

  interface hasSameFile
    module procedure hasSameFile_scalar, hasSameFile_array
  end interface hasSameFile

  interface hasSameInts
    module procedure hasSameInts_scalar, hasSameInts_array
  end interface hasSameInts

  interface hasSameReals
    module procedure hasSameReals_sp_scalar, hasSameReals_sp_array
  end interface hasSameReals

  interface hasSameBools
    module procedure hasSameBools_scalar, hasSameBools_array
  end interface hasSameBools

  interface hasSameStr
    module procedure equalStringsList, hasSameStr_scalar
  end interface hasSameStr

  interface hasSameOptions
    module procedure hasSameOptions_scalar, hasSameOptions_array
  end interface hasSameOptions
  
contains

  elemental logical function equiv_argsRange(self, other)
    !< true if self and other have the same characteristics
    !< (range must be allocated)
    class(argsRange), intent(in) :: self
    class(argsRange), intent(in) :: other

    ! do they have the same size?
    if (size(self % range) /= size(other % range)) then
      equiv_argsRange = .false.
      return
    endif

    equiv_argsRange = all(self % range == other % range)
  end function equiv_argsRange


  type(argsRange) function argsRange_constructor(nargs)
    !< argsRange constructor 
    integer, intent(in) :: nargs(:)
    integer :: nelems !< number of elements found in nargs 
    character(len=*), parameter :: SNAME = "argsRange_constructor"
    character(len=20) :: tmp !< auxiliary var
    logical :: isEndUnlimited

    ! check values 
    nelems = size(nargs)

    if (nelems == 0) then
      call raiseError(__FILE__, SNAME, &
              "nargs cannot be empty ( 0 )")

    else if (nelems == ARGSRANGE_MAX_ELEMS) then
      isEndUnlimited = (nargs(2) == OPT_NARGS_N)
      if (.not. isEndUnlimited .and. nargs(2) < nargs(1)) then
        call raiseError(__FILE__, SNAME, &
                "end is smaller than starting value")
      endif

    else if (nelems > ARGSRANGE_MAX_ELEMS) then
      call raiseError(__FILE__, SNAME, &
              "nargs can only have 1 or 2 elemements", &
              "But found: "//tmp)

    endif

    if (any(nargs < OPT_NARGS_N)) then
      call raiseError(__FILE__, SNAME, &
              "nargs arguments cannot be smaller than 0 (except for OPT_NARGS_N)")
    endif

    ! init values
    allocate(argsRange_constructor % range(2))
    if (nelems == 1) then
      if (nargs(1) == OPT_NARGS_N) then
        argsRange_constructor % range = [0, OPT_NARGS_N]
      else
        argsRange_constructor % range = [nargs(1), nargs(1)]
      endif
    else if (nelems == 2) then
      argsRange_constructor % range = nargs
    endif

  end function argsRange_constructor


  integer function getStartRange(self)
    !< it returns the minimum number of elements it can contain
    class(argsRange), intent(in) :: self
    integer, parameter :: START = 1
    getStartRange = self % range(START)
  end function getStartRange


  integer function getEndRange(self)
    !< it returns the maximum number of elements it can contain
    class(argsRange), intent(in) :: self
    integer, parameter :: END = 2
    getEndRange = self % range(END)
  end function getEndRange


  subroutine checkNargs(self, nelems)
    !< check the number of given arguments is correct
    !< if ok, nothing happens 
    !< otherwise an error message is raised
    class(option_abs), intent(in) :: self
    integer, intent(in) :: nelems

    character(len=20) :: tmp, allowedArgs

    if (.not. self % argumentsRange % isAllowed(nelems)) then
      write (tmp, *) nelems
      allowedArgs = self % argumentsRange % toString()
      call raiseError(__FILE__, "checkNargs", &
              "The number of manyStrings found is not allowed", &
              "Given arguments: "//trim(tmp), &
              "Allowed arguments: "//allowedArgs)
    endif
  end subroutine checkNargs


  subroutine initialize_option_timedeltas(this, key, nargs, description, default, min, max, choices)
    !< initialize option_timedeltas class
    class(option_timedeltas), intent(inout) :: this
    character(len=*), intent(in) :: key 
    type(timedelta), intent(in), optional :: default(:)
    integer, intent(in) :: nargs(:) !< 1 (fixed) or 2 (range) elements allowed
    character(len=*), intent(in) :: description
    type(timedelta), intent(in), optional :: min
    type(timedelta), intent(in), optional :: max
    type(timedelta), intent(in), optional :: choices(:)
    logical :: allDefault

    this % key = key
    this % description = description

    allocate(this % argumentsRange)
    this % argumentsRange = argsRange(nargs)

    if (present(default)) then
      this % defValue = default
    endif

    if (present(min)) then
      allocate(this % minTd)
      this % minTd = min
    endif

    if (present(max)) then
      allocate(this % maxTd)
      this % maxTd = max
    endif

    if (present(choices)) then
      if (present(default)) then ! let's make sure default is elegible in-between choices
        allDefault = this % isValueAllowed(this % defValue)
        if (.not. allDefault) then
          call raiseError(__FILE__, "initialize_option_timedeltas", &
                "'default' value not found in 'choices'", &
                "default(s): ", &!//this % defValue, & ! todo
                "Choices: ")!// this % choices ) 
        endif
      endif

      this % choices = choices
    endif

  end subroutine initialize_option_timedeltas


  type(option_timedeltas) function option_timedeltas_constructor(key, nargs, description, default, &
                  min, max, choices) result (r)
    !< build option_timedeltas class
    character(len=*), intent(in) :: key 
    type(timedelta), intent(in), optional :: default(:)
    integer, intent(in) :: nargs(:) !< 1 (fixed) or 2 (range) elements allowed
    character(len=*), intent(in) :: description
    type(timedelta), intent(in), optional :: min
    type(timedelta), intent(in), optional :: max
    type(timedelta), intent(in), optional :: choices(:)

    call r % initialize(key, nargs, description, default, min, max, choices)
  end function option_timedeltas_constructor


  subroutine initialize_option_datetimes(this, key, nargs, description, default, min, max, choices)
    !< option_reals constructor
    class(option_datetimes), intent(inout) :: this
    character(len=*), intent(in) :: key 
    character(len=*), intent(in) :: description
    integer, intent(in) :: nargs(:) !< 1 (fixed) or 2 (range) elements allowed
    type(datetime), intent(in), optional :: default(:)
    type(datetime), intent(in), optional :: min
    type(datetime), intent(in), optional :: max 
    type(datetime), intent(in), optional :: choices(:)
    logical :: allDefault

    this % key = key
    this % description = description

    allocate(this % argumentsRange)
    this % argumentsRange = argsRange(nargs)

    if (present(default)) then
      allocate(this % defDate)
      this % defDate = default(1)
    endif

    if (present(min)) then
      allocate(this % minDate)
      this % minDate = min
    endif

    if (present(max)) then
      allocate(this % maxDate)
      this % maxDate = max
    endif

    if (present(choices)) then
      if (present(default)) then ! let's make sure default is elegible in-between choices
        allDefault = this % isValueAllowed([this % defDate])
        if (.not. allDefault) then
          call raiseError(__FILE__, "initialize_option_datetimes", &
                "'default' value not found in 'choices'", &
                "default(s): ", &!//this % defValue, & ! todo
                "Choices: ")!// this % choices ) 
        endif
      endif

      this % choices = choices
    endif
  end subroutine initialize_option_datetimes


  type(option_datetimes) function option_datetimes_constructor(key, nargs, description, default, min, max, choices)  result (r)
    !< option_reals constructor
    character(len=*), intent(in) :: key 
    character(len=*), intent(in) :: description
    integer, intent(in) :: nargs(:) !< 1 (fixed) or 2 (range) elements allowed
    type(datetime), intent(in), optional :: default(:)
    type(datetime), intent(in), optional :: min
    type(datetime), intent(in), optional :: max 
    type(datetime), intent(in), optional :: choices(:)

    call r % initialize(key, nargs, description, default, min, max, choices)
  end function option_datetimes_constructor


  function option_ints_constructor(key, nargs, description, default, min, max, choices) result (r)
    !< build option_reals class
    character(len=*), intent(in) :: key 
    integer, intent(in) :: nargs(:)
    character(len=*), intent(in) :: description
    integer, intent(in), optional :: default(:)
    integer, intent(in), optional :: min
    integer, intent(in), optional :: max
    integer, intent(in), optional :: choices(:)
    type(option_ints) :: r !< output

    call r % initialize(key, nargs, description, default, min, max, choices)

  end function option_ints_constructor


  subroutine initialize_option_ints(this, key, nargs, description, default, min, max, choices)
    !<  initialize option_int class. Same as option_ints_constructor
    !< to use when option_ints is a pointer
    class(option_ints), intent(inout) :: this
    character(len=*), intent(in) :: key 
    integer, intent(in) :: nargs(:)
    character(len=*), intent(in) :: description
    integer, intent(in), optional :: default(:)
    integer, intent(in), optional :: min
    integer, intent(in), optional :: max
    integer, intent(in), optional :: choices(:)

    logical :: allDefault

    ! same implementation as constructor
    this % key = key
    this % description = description

    allocate(this % argumentsRange)
    this % argumentsRange = argsRange(nargs)

    if (present(default)) then
      this % defValue = default
    endif

    if (present(min)) then
      allocate(this % minValue)
      this % minValue = min
    endif
    if (present(max)) then
      allocate(this % maxValue)
      this % maxValue = max
    endif

    if (present(min) .and. present(max)) then
      call checkMinMax(real(min, sp), real(max, sp))
    endif

    if (present(choices)) then
      if (present(default)) then ! let's make sure default is elegible in-between choices
        allDefault = this % isValueAllowed(this % defValue)
        if (.not. allDefault) then
          call raiseError(__FILE__, "initialize_option_ints", &
                "'default' value not found in 'choices'", &
                "default(s): ", &!//this % defValue, & ! todo
                "Choices: ")!// this % choices ) 
        endif
      endif

      this % choices = choices
    endif

  end subroutine initialize_option_ints


  subroutine initialize_option_reals(this, key, nargs, description, default, min, max, choices)
    !<  initialize option_int class. Same as option_ints_constructor
    !< to use when option_ints is a pointer
    class(option_reals), intent(inout) :: this
    character(len=*), intent(in) :: key 
    integer, intent(in) :: nargs(:)
    character(len=*), intent(in) :: description
    real(kind=sp), intent(in), optional :: default(:)
    real(kind=sp), intent(in), optional :: min
    real(kind=sp), intent(in), optional :: max
    real(kind=sp), intent(in), optional :: choices(:)

    logical :: allDefault

    ! same implementation as constructor
    this % key = key
    this % description = description

    allocate(this % argumentsRange)
    this % argumentsRange = argsRange(nargs)

    if (present(default)) then
      this % defValue = default
    endif

    if (present(min)) then
      allocate(this % minValue)
      this % minValue = min
    endif
    if (present(max)) then
      allocate(this % maxValue)
      this % maxValue = max
    endif

    if (present(min) .and. present(max)) then
      call checkMinMax(min, max)
    endif

    if (present(choices)) then
      if (present(default)) then ! let's make sure default is elegible in-between choices
        allDefault = this % isValueAllowed(this % defValue)
        if (.not. allDefault) then
          call raiseError(__FILE__, "initialize_option_reals", &
                "'default' value not found in 'choices'", &
                "default(s): ", &!//this % defValue, & ! todo
                "Choices: ")!// this % choices ) 
        endif
      endif

      this % choices = choices
    endif

  end subroutine initialize_option_reals


  function option_reals_constructor(key, nargs, description, min, max, default, choices) result (r)
    !< build option_reals class
    character(len=*), intent(in) :: key 
    integer, intent(in) :: nargs(:)
    character(len=*), intent(in) :: description
    real(kind=sp), intent(in), optional :: min
    real(kind=sp), intent(in), optional :: max
    real(kind=sp), intent(in), optional :: default(:)
    real(kind=sp), intent(in), optional :: choices(:)
    type(option_reals) :: r !< output

    call r % initialize(key, nargs, description, default, min, max, choices)
  end function option_reals_constructor


  subroutine checkMinMax(minValue, maxValue)
    !< it raises an error when minValue > maxValue
    !< otherwise nothing happens
    real(kind=sp), intent(in) :: minValue
    real(kind=sp), intent(in) :: maxValue
    character(len=50) :: tmp, tmp1

    if (minValue > maxValue) then
        write(tmp, *) minValue
        write(tmp1, *) maxValue
        call raiseError(__FILE__, "checkMinMax", &
                "minValue cannot be bigger than maxValue", &
                "minValue = "//trim(tmp), &
                "maxValue = "//trim(tmp1))
    endif
  end subroutine checkMinMax


  subroutine initialize_option_str(this, key, nargs, description, default, choices)
    !< intialize option_str
    class(option_str), intent(inout) :: this 
    character(len=*), intent(in) :: key 
    integer, intent(in) :: nargs(:)
    character(len=*), intent(in) :: description
    type(string), intent(in), optional :: default(:)
    type(string), intent(in), optional :: choices(:)

    logical :: allDefault
    type(stringCollection), allocatable :: manystrings, defStrings

    this % key = key
    this % description = description

    allocate(this % argumentsRange)
    this % argumentsRange = argsRange(nargs)

    if (present(default)) then
      this % defVal = string(default)
    endif

    if (present(choices)) then
      if (present(default)) then ! let's make sure default is elegible in-between choices
        allDefault = this % isValueAllowed(default)
        if (.not. allDefault) then
          allocate(manyStrings, defStrings)
          manyStrings = stringCollection(choices)
          defStrings = stringCollection(default)
          call raiseError(__FILE__, "initialize_option_bools", &
                "'default' value not found in 'choices'", &
                "default(s): "//defStrings % toString(), &
                "Choices: "//manyStrings % toString()) 
        endif
      endif

      this % choices = choices
    endif
  end subroutine initialize_option_str


  type(option_str) function option_str_constructor(key, nargs, description, default, choices) result (r)
    !< option_str constructor 
    character(len=*), intent(in) :: key 
    integer, intent(in) :: nargs(:)
    character(len=*), intent(in) :: description
    type(string), intent(in), optional :: default(:)
    type(string), intent(in), optional :: choices(:) 

    call r % initialize(key, nargs, description, default, choices)
  end function option_str_constructor


  subroutine initialize_option_files(this, key, nargs, description, default, choices)
    !< initialize option_files class
    class(option_files), intent(inout) :: this
    character(len=*), intent(in) :: key 
    integer, intent(in) :: nargs(:)
    character(len=*), intent(in) :: description
    type(string), intent(in), optional :: default(:)
    type(string), intent(in), optional :: choices(:)

    logical :: allDefault
    type(string), allocatable :: def(:), defStrings(:)
    type(stringCollection), allocatable :: manystrings, defCollection

    this % key = key
    this % description = description

    allocate(this % argumentsRange)
    this % argumentsRange = argsRange(nargs)

    if (present(default)) then
      this % defFile = file_t(default) 
    endif

    if (present(choices)) then
      if (present(default)) then ! let's make sure default is elegible in-between choices
        allDefault = this % isValueAllowed(this % defFile)
        if (.not. allDefault) then
          allocate(manyStrings, defCollection)
          manyStrings = stringCollection(choices)
          defStrings = string(this % defFile % getName())
          defCollection = stringCollection(defStrings)
          call raiseError(__FILE__, "initialize_option_files", &
                "'default' value not found in 'choices'", &
                "default(s): "//defCollection % toString(), &
                "Choices: "//manyStrings % toString()) 
        endif
      endif

      this % choices = file_t(choices)
    endif
  end subroutine initialize_option_files


  type(option_files) function option_files_constructor(key, nargs, description, default, choices) result (r)
    !< build option_files class
    character(len=*), intent(in) :: key 
    integer, intent(in) :: nargs(:)
    character(len=*), intent(in) :: description
    type(string), intent(in), optional :: default(:)
    type(string), intent(in), optional :: choices(:) 

    call r % initialize(key, nargs, description, default, choices)
  end function option_files_constructor


  subroutine dummy_setVal(self)
    !< dummy_setVal, overload to add functionalities
    class(option_abs), intent(inout) :: self
  end subroutine dummy_setVal


  subroutine dummy_getVal(self)
    !< dummy_getVal, overload to add functionalities
    class(option_abs), intent(inout) :: self
  end subroutine dummy_getVal


  subroutine setVal_str_chars(self, chars)
    !< setVal for character
    !< it removes previous content
    class(option_str), intent(inout) :: self
    character(len=*), intent(in) :: chars

    type(string) :: tmp(1)
    tmp(1) = string(chars)
    call self % setVal_str_many_strings(tmp)
    
  end subroutine setVal_str_chars


  logical function isValueAllowed_str(self, vals)
    !< true if all the given vals are found in 'choices'
    !< if no choices is defined, it is always true as there in no restriction
    class(option_str), intent(inout) :: self
    type(string), intent(in) :: vals(:)

    logical :: hasChoices
    integer :: i
    integer, allocatable :: elemsFound(:)
    logical :: hasDefault

    hasChoices = allocated(self % choices)

    isValueAllowed_str = .true.
    if (hasChoices) then ! let's make sure default is elegible in-between choices
      do i = 1, size(vals)
        elemsFound = findloc(self % choices, vals(i))
        hasDefault = (size(elemsFound) > 0)
        if (.not. hasDefault) then
          isValueAllowed_str = .false.
          exit
        endif
      enddo
    endif
  end function isValueAllowed_str


  logical function isValueAllowed_file(self, vals)
    !< true if all the given vals are found in 'choices'
    !< if no choices is defined, it is always true as there in no restriction
    class(option_files), intent(inout) :: self
    type(file_t), intent(in) :: vals(:)

    logical :: hasChoices
    integer :: i
    integer, allocatable :: elemsFound(:)
    logical :: hasDefault
    type(string) :: filename
    type(string), allocatable :: fnChoices(:)
    character(FILE_NAME_MAX_LEN), allocatable :: strChoices(:)

    hasChoices = allocated(self % choices)

    isValueAllowed_file = .true.
    if (hasChoices) then ! let's make sure default is elegible in-between choices
      do i = 1, size(vals)
        filename = string(vals(i) % getName())
        fnChoices = string(self % choices % getName())
        elemsFound = findloc(fnChoices, filename)
        hasDefault = (size(elemsFound) > 0)
        if (.not. hasDefault) then
          isValueAllowed_file = .false.
          exit
        endif
      enddo
    endif
  end function isValueAllowed_file


  logical function isValueAllowed_real(self, vals)
    !< true if all the given vals are found in 'choices'
    !< if no choices is defined, it is always true as there in no restriction
    class(option_reals), intent(inout) :: self
    real(kind=sp), intent(in) :: vals(:)

    logical :: hasChoices
    integer :: i
    integer, allocatable :: elemsFound(:)
    logical :: hasDefault

    hasChoices = allocated(self % choices)

    isValueAllowed_real = .true.
    if (hasChoices) then ! let's make sure default is elegible in-between choices
      do i = 1, size(vals)
        elemsFound = findloc(self % choices, vals(i))
        hasDefault = (size(elemsFound) > 0)
        if (.not. hasDefault) then
          isValueAllowed_real = .false.
          exit
        endif
      enddo
    endif
  end function isValueAllowed_real


  logical function isValueAllowed_int(self, vals)
    !< true if all the given vals are found in 'choices'
    !< if no choices is defined, it is always true as there in no restriction
    class(option_ints), intent(inout) :: self
    integer, intent(in) :: vals(:)

    logical :: hasChoices
    integer :: i
    integer, allocatable :: elemsFound(:)
    logical :: hasDefault

    hasChoices = allocated(self % choices)

    isValueAllowed_int = .true.
    if (hasChoices) then ! let's make sure default is elegible in-between choices
      do i = 1, size(vals)
        elemsFound = findloc(self % choices, vals(i))
        hasDefault = (size(elemsFound) > 0)
        if (.not. hasDefault) then
          isValueAllowed_int = .false.
          exit
        endif
      enddo
    endif
  end function isValueAllowed_int


  logical function isValueAllowed_datet(self, vals)
    !< true if all the given vals are found in 'choices'
    !< if no choices is defined, it is always true as there in no restriction
    class(option_datetimes), intent(inout) :: self
    type(datetime), intent(in) :: vals(:)

    logical :: hasChoices
    integer :: i
    integer, allocatable :: elemsFound(:)
    logical :: hasDefault

    hasChoices = allocated(self % choices)

    isValueAllowed_datet = .true.
    if (hasChoices) then ! let's make sure default is elegible in-between choices
      do i = 1, size(vals)
!        elemsFound = findloc(self % choices, vals(i)) ! TODO
        hasDefault = (size(elemsFound) > 0)
        if (.not. hasDefault) then
          isValueAllowed_datet = .false.
          exit
        endif
      enddo
    endif
  end function isValueAllowed_datet


  logical function isValueAllowed_tdelta(self, vals)
    !< true if all the given vals are found in 'choices'
    !< if no choices is defined, it is always true as there in no restriction
    class(option_timedeltas), intent(inout) :: self
    type(timedelta), intent(in) :: vals(:)

    logical :: hasChoices
    integer :: i
    integer, allocatable :: elemsFound(:)
    logical :: hasDefault

    hasChoices = allocated(self % choices)

    isValueAllowed_tdelta = .true.
    if (hasChoices) then ! let's make sure default is elegible in-between choices
      do i = 1, size(vals)
!        elemsFound = findloc(self % choices, vals(i)) ! TODO
        hasDefault = (size(elemsFound) > 0)
        if (.not. hasDefault) then
          isValueAllowed_tdelta = .false.
          exit
        endif
      enddo
    endif
  end function isValueAllowed_tdelta


  subroutine setVal_str_string(self, singleString)
    !< setVal for character
    !< it removes previous content
    class(option_str), intent(inout) :: self
    type(string), intent(in) :: singleString

    type(string) :: tmp(1)

    tmp(1) = string(singleString)
    call self % setVal_str_many_strings(tmp)
  end subroutine setVal_str_string


  subroutine setVal_str_many_strings(self, manyStrings)
    !< setVal for character
    !< it removes previous content
    class(option_str), intent(inout) :: self
    type(string), intent(in) :: manyStrings(:)

    integer :: nelems
    logical :: hasChoices
    logical :: areValidValues
    type(stringCollection), allocatable :: sc, chs

    hasChoices = allocated(self % choices)

    ! check the number of given arguments is correct
    nelems = size(manyStrings)
    call self % checkNargs(nelems)

    if (hasChoices) then
      areValidValues = self % isValueAllowed(manyStrings)
      if (.not. areValidValues) then
        allocate(sc, chs)
        sc = stringCollection(manyStrings)
        chs = stringCollection(self % choices)
        call raiseError(__FILE__, "setVal_str_many_strings", &
                "Given values do not match with the possible choices", &
                "Value: "//sc % toString(), &
                "Choices: "//chs % toString())
      endif
    endif

    ! reallocate values
    if (allocated(self % values)) deallocate(self % values)
    allocate(self % values(size(manyStrings)))

    ! save values
    self % values = manyStrings
  end subroutine setVal_str_many_strings


  subroutine setVal_files_chars(self, newpath)
    !< set character's path for options_file
    class(option_files), intent(inout) :: self
    character(len=*), intent(in) :: newpath

    if (allocated(self % files)) deallocate(self % files)
    allocate(self % files(1))
    self % files = file_t(newpath)
  end subroutine setVal_files_chars


  subroutine setVal_files_string(self, newpath)
    !< set character's path for options_file
    class(option_files), intent(inout) :: self
    type(string), intent(in) :: newpath

    if (allocated(self % files)) deallocate(self % files)
    allocate(self % files(1))
    self % files = file_t(newpath)
  end subroutine setVal_files_string


  subroutine setVal_files_many_strings(self, newpaths)
    !< set character's path for options_file
    class(option_files), intent(inout) :: self
    type(string), intent(in) :: newpaths(:)
    integer :: i

    if (allocated(self % files)) deallocate(self % files)
    allocate(self % files(size(newpaths)))

    do i = 1, size(newpaths)
      self % files(i) = file_t(newpaths(i))
    enddo
  end subroutine setVal_files_many_strings


  subroutine setVal_files_scalar(self, newFile)
    !< set character's path for options_file
    class(option_files), intent(inout) :: self
    type(file_t), intent(in) :: newFile

    type(file_t) :: wrapper(1) ! array wrapper

    wrapper(1) = newFile
    call self % setVal_files_many_files(wrapper)
  end subroutine setVal_files_scalar


  subroutine setVal_files_many_files(self, newFiles)
    !< set character's path for options_file
    class(option_files), intent(inout) :: self
    type(file_t), intent(in) :: newFiles(:)
    integer :: i, nelems

    nelems = size(newFiles)

    if (allocated(self % files)) deallocate(self % files)
    allocate(self % files(nelems))

    do i = 1, nelems
      self % files(i) = newFiles(i) 
    enddo
  end subroutine setVal_files_many_files


  pure logical function isMaxEnabled(self)
    !< true is maximum value is defined
    class(option_reals), intent(in) :: self
    isMaxEnabled = allocated(self % maxValue)
  end function isMaxEnabled


  pure logical function isMinEnabled(self)
    !< true is maximum value is defined
    class(option_reals), intent(in) :: self
    isMinEnabled = allocated(self % minValue)
  end function isMinEnabled


  impure elemental subroutine checkMaxError(self, val)
    !< error if val value is above maxValue 
    class(option_reals), intent(in) :: self
    real(kind=sp), intent(in) :: val ! value to check
    character(len=20) :: tmp, tmp1 ! temporary 

    if (self % maxValue < val) then
      write(tmp, *) val 
      write(tmp1, *) self % maxValue

      call raiseError(__FILE__, "checkMaxErro", &
              "val must be bigger the maxValue", &
              "New value: "//tmp, &
              "Max value: "//tmp1)
    endif
  end subroutine checkMaxError


  impure elemental subroutine checkMinError(self, val)
    !< error if val value is below minValue 
    class(option_reals), intent(in) :: self
    real(kind=sp), intent(in) :: val ! value to check
    character(len=20) :: tmp, tmp1 ! temporary 

    if (self % minValue > val) then
      write(tmp, *) val 
      write(tmp1, *) self % minValue

      call raiseError(__FILE__, "checkMinError", &
              "newValue must be bigger the minValue", &
              "new value: "//tmp, &
              "min value: "//tmp1)
    endif
  end subroutine checkMinError


  subroutine setVal_real_sp(self, newValue)
    !< set new values for options_real_sp
    class(option_reals), intent(inout) :: self
    real(kind=sp), intent(in) :: newValue

    if (self % isMinEnabled()) then
      call self % checkMinError(newValue)
    endif

    if (self % isMaxEnabled()) then
      call self % checkMaxError(newValue)
    endif

    if (allocated(self % values)) deallocate(self % values)
    allocate(self % values(1))
    self % values = newValue 
  end subroutine setVal_real_sp


  subroutine setVal_many_reals_sp(self, newValues)
    !< set new real sp values for options_real_sp
    class(option_reals), intent(inout) :: self
    real(kind=sp), intent(in) :: newValues(:)

    logical :: areValidValues, hasChoices

    if (self % isMinEnabled()) then
      call self % checkMinError(newValues)
    endif

    if (self % isMaxEnabled()) then
      call self % checkMaxError(newValues)
    endif

    hasChoices = (allocated(self % choices))
    if (hasChoices) then
      areValidValues = self % isValueAllowed(newValues)
      if (.not. areValidValues) then
        call raiseError(__FILE__, "setVal_many_reals_sp", &
                "Given values do not match with the possible choices", &
                "Value: ", & ! TODO sc % toString(), &
                "Choices: ")! //chs % toString())
      endif
    endif

    ! reallocate values
    if (allocated(self % values)) deallocate(self % values)
    allocate(self % values(size(newValues)))
    self % values = newValues
  end subroutine setVal_many_reals_sp


  pure logical function isMaxEnabled_optionInt(self)
    !< true is maximum value is defined
    class(option_ints), intent(in) :: self
    isMaxEnabled_optionInt = allocated(self % maxValue)
  end function isMaxEnabled_optionInt


  pure logical function isMinEnabled_optionInt(self)
    !< true is maximum value is defined
    class(option_ints), intent(in) :: self
    isMinEnabled_optionInt = allocated(self % minValue)
  end function isMinEnabled_optionInt


  impure elemental subroutine checkMaxError_optionInt(self, val)
    !< error if val value is above maxValue 
    class(option_ints), intent(in) :: self
    integer, intent(in) :: val ! value to check
    character(len=20) :: tmp, tmp1 ! temporary 

    if (self % maxValue < val) then
      write(tmp, *) val 
      write(tmp1, *) self % maxValue

      call raiseError(__FILE__, "checkMaxError_optionInt", &
              "val must be bigger the maxValue", &
              "New value: "//tmp, &
              "Max value: "//tmp1)
    endif
  end subroutine checkMaxError_optionInt


  impure elemental subroutine checkMinError_optionInt(self, val)
    !< error if val value is below minValue 
    class(option_ints), intent(in) :: self
    integer, intent(in) :: val ! value to check
    character(len=20) :: tmp, tmp1 ! temporary 

    if (self % minValue > val) then
      write(tmp, *) val 
      write(tmp1, *) self % minValue

      call raiseError(__FILE__, "checkMinError_optionInt", &
              "newValue must be bigger the minValue", &
              "new value: "//tmp, &
              "min value: "//tmp1)
    endif
  end subroutine checkMinError_optionInt


  subroutine setVal_int_sp(self, newValue)
    !< set new values for options_real_sp
    class(option_ints), intent(inout) :: self
    integer, intent(in) :: newValue

    if (self % isMinEnabled()) then
      call self % checkMinError(newValue)
    endif

    if (self % isMaxEnabled()) then
      call self % checkMaxError(newValue)
    endif

    if (allocated(self % values)) deallocate(self % values)
    allocate(self % values(1))
    self % values = newValue 
  end subroutine setVal_int_sp


  subroutine setVal_many_ints_sp(self, newValues)
    !< set new real sp values for options_real_sp
    class(option_ints), intent(inout) :: self
    integer, intent(in) :: newValues(:)
    logical :: hasChoices, areValidValues

    if (self % isMinEnabled()) then
      call self % checkMinError(newValues)
    endif

    if (self % isMaxEnabled()) then
      call self % checkMaxError(newValues)
    endif

    hasChoices = (allocated(self % choices))
    if (hasChoices) then
      areValidValues = self % isValueAllowed(newValues)
      if (.not. areValidValues) then
        call raiseError(__FILE__, "setVal_many_ints_sp", &
                "Given values do not match with the possible choices", &
                "Value: ", & ! TODO sc % toString(), &
                "Choices: ")! //chs % toString())
      endif
    endif

    if (allocated(self % values)) deallocate(self % values)
    allocate(self % values(size(newValues)))
    self % values = newValues
  end subroutine setVal_many_ints_sp


  pure logical function isMaxEnabled_datetime(self)
    !< true is maximum value is defined
    class(option_datetimes), intent(in) :: self
    isMaxEnabled_datetime = allocated(self % maxDate)
  end function isMaxEnabled_datetime


  pure logical function isMinEnabled_datetime(self)
    !< true is maximum value is defined
    class(option_datetimes), intent(in) :: self
    isMinEnabled_datetime = allocated(self % minDate)
  end function isMinEnabled_datetime


  impure elemental subroutine checkMaxError_datetime(self, val)
    !< error if val value is above maxValue 
    class(option_datetimes), intent(in) :: self
    type(datetime), intent(in) :: val ! value to check

    if (self % maxDate < val) then
      call raiseError(__FILE__, "checkMaxError_datetime", &
              "val must be bigger the maxValue", &
              "New date: "//val % toString(), &
              "Max date: "//self % maxDate % toString())
    endif
  end subroutine checkMaxError_datetime


  impure elemental subroutine checkMinError_datetime(self, val)
    !< error if val value is below minValue 
    class(option_datetimes), intent(in) :: self
    type(datetime), intent(in) :: val ! value to check

    if (self % minDate > val) then
      call raiseError(__FILE__, "checkMinError_datetime", &
              "newValue must be bigger the minValue", &
              "New value: "//val % toString(), &
              "Min value: "//self % minDate % toString())
    endif
  end subroutine checkMinError_datetime


  subroutine setVal_datetime(self, newValue)
    !< set new values for options_real_sp
    class(option_datetimes), intent(inout) :: self
    type(datetime), intent(in) :: newValue

    if (self % isMinEnabled()) then
      call self % checkMinError(newValue)
    endif

    if (self % isMaxEnabled()) then
      call self % checkMaxError(newValue)
    endif

    if (allocated(self % dates)) deallocate(self % dates)
    allocate(self % dates(1))
    self % dates = newValue 
  end subroutine setVal_datetime


  subroutine setVal_many_datetimes(self, newValues)
    !< set new real sp values for options_real_sp
    class(option_datetimes), intent(inout) :: self
    type(datetime), intent(in) :: newValues(:)
    logical :: hasChoices, areValidValues

    if (self % isMinEnabled()) then
      call self % checkMinError(newValues)
    endif

    if (self % isMaxEnabled()) then
      call self % checkMaxError(newValues)
    endif

    hasChoices = (allocated(self % choices))
    if (hasChoices) then
      areValidValues = self % isValueAllowed(newValues)
      if (.not. areValidValues) then
        call raiseError(__FILE__, "setVal_many_datetimes", &
                "Given values do not match with the possible choices", &
                "Value: ", & ! TODO sc % toString(), &
                "Choices: ")! //chs % toString())
      endif
    endif

    if (allocated(self % dates)) deallocate(self % dates)
    allocate(self % dates(size(newValues)))
    self % dates = newValues
  end subroutine setVal_many_datetimes


  subroutine setVal_timedelta(self, newValue)
    !< set new values for options_real_sp
    class(option_timedeltas), intent(inout) :: self
    type(timedelta), intent(in) :: newValue

    if (allocated(self % values)) deallocate(self % values)
    allocate(self % values(1))
    self % values = newValue 
  end subroutine setVal_timedelta


  subroutine setVal_many_timedeltas(self, newValues)
    !< set new real sp values for options_real_sp
    class(option_timedeltas), intent(inout) :: self
    type(timedelta), intent(in) :: newValues(:)
    logical :: hasChoices, areValidValues

    hasChoices = (allocated(self % choices))
    if (hasChoices) then
      areValidValues = self % isValueAllowed(newValues)
      if (.not. areValidValues) then
        call raiseError(__FILE__, "setVal_many_timedeltas", &
                "Given values do not match with the possible choices", &
                "Value: ", & ! TODO sc % toString(), &
                "Choices: ")! //chs % toString())
      endif
    endif

    if (allocated(self % values)) deallocate(self % values)
    allocate(self % values(size(newValues)))
    self % values = newValues
  end subroutine setVal_many_timedeltas


  impure elemental logical function equiv_option_str(a, b) 
    !< compare option_str vs option_str
    !< true if both types hae the same values
    !< error if b is not option_str type
    class(option_str), intent(in) :: a
    class(option_abs), intent(in) :: b

    class(option_str), pointer :: pOptStr
    logical :: hasSameStrings
    logical :: hasSameDefString
    logical :: hasSameArgs

    equiv_option_str = .false.
    select type (pOptStr =>  b)
    type is (option_str)

      hasSameStrings = hasSameStr(a % values, pOptStr % values)
      hasSameDefString = hasSameStr(a % defVal, pOptStr % defVal)
      hasSameArgs = hasSameArgsRange(a % argumentsRange, pOptStr % argumentsRange)

      if (a % key == pOptStr % key .and. &
          a % description == pOptStr % description .and. &
          hasSameArgs .and. &
          hasSameDefString .and. &
          hasSameStrings) then
        equiv_option_str = .true.
      endif
    class default
      call raiseError(__FILE__, "equiv_option_str", "Not implemented type")
    end select
    
  end function equiv_option_str


  impure elemental logical function equiv_option_reals(a, b) 
    !< compare option_reals to option_reals
    !< true if both type have the same values
    !< error if b is not a option_reals type
    class(option_reals), intent(in) :: a
    class(option_abs), intent(in) :: b

    class(option_reals), pointer :: pOptReals
    logical :: hasSameValues
    logical :: hasSameDefValue
    logical :: hasSameArgs
    logical :: hasSameMin
    logical :: hasSameMax

    equiv_option_reals = .false.
    select type (pOptReals =>  b)
    type is (option_reals)

      hasSameValues = hasSameReals(a % values, pOptReals % values)
      hasSameDefValue = hasSameReals(a % defValue, pOptReals % defValue)
      hasSameMin = (a % minValue == pOptReals % minValue) 
      hasSameMax = (a % maxValue == pOptReals % maxValue) 
      hasSameArgs = hasSameArgsRange(a % argumentsRange, pOptReals % argumentsRange)

      if (a % key == pOptReals % key .and. &
          a % description == pOptReals % description .and. &
          hasSameArgs .and. &
          hasSameValues .and. &
          hasSameDefValue .and. &
          hasSameMin .and. &
          hasSameMax ) then
        equiv_option_reals = .true.
      endif

    class default
      call raiseError(__FILE__, "equiv_option_reals", "Not implemented type")
    end select
    
  end function equiv_option_reals


  impure elemental logical function equiv_option_ints(a, b) 
    !< compare option_reals to option_ints
    !< true if both type have the same values
    !< error if b is not a option_reals type
    class(option_ints), intent(in) :: a
    class(option_abs), intent(in) :: b

    class(option_ints), pointer :: pOptInts
    logical :: hasSameValues
    logical :: hasSameDefValue
    logical :: hasSameMin
    logical :: hasSameMax
    logical :: hasSameArgs

    equiv_option_ints = .false.
    select type (pOptInts =>  b)
    type is (option_ints)

      hasSameValues = hasSameInts(a % values, pOptInts % values)
      hasSameDefValue = hasSameInts(a % defValue, pOptInts % defValue)
      hasSameMin = hasSameInts(a % minValue, pOptInts % minValue)
      hasSameMax = hasSameInts(a % maxValue, pOptInts % maxValue)
      hasSameArgs = hasSameArgsRange(a % argumentsRange, pOptInts % argumentsRange)

      if (a % key == pOptInts % key .and. &
          a % description == pOptInts % description .and. &
          hasSameArgs .and. &
          hasSameValues .and. &
          hasSameDefValue .and. &
          hasSameMin .and. &
          hasSameMax ) then
        equiv_option_ints = .true.
      endif

    class default
      call raiseError(__FILE__, "equiv_option_ints", "Not implemented type")
    end select
    
  end function equiv_option_ints


  impure elemental logical function equiv_option_files(a, b) 
    !< compare option_reals to option_ints
    !< true if both type have the same values
    !< error if b is not a option_reals type
    class(option_files), intent(in) :: a
    class(option_abs), intent(in) :: b

    class(option_files), pointer :: pOptFiles

    logical :: hasSameFiles
    logical :: hasSameArgs
    logical :: hasSameDefFile

    equiv_option_files = .false.
    select type (pOptFiles =>  b)
    type is (option_files)

      hasSameFiles = hasSameFile(a % files, pOptFiles % files)
      hasSameDefFile = hasSameFile(a % defFile, pOptFiles % defFile)
      hasSameArgs = hasSameArgsRange(a % argumentsRange, pOptFiles % argumentsRange)

      if (a % key == pOptFiles % key .and. &
          a % description == pOptFiles % description .and. &
          hasSameArgs .and. &
          hasSameDefFile .and. &
          hasSameFiles) then
        equiv_option_files = .true.
      endif
    class default
      call raiseError(__FILE__, "equiv_option_files", "Not implemented type")
    end select
    
  end function equiv_option_files


  impure elemental logical function equiv_option_datetimes(a, b) 
    !< compare option_reals to option_ints
    !< true if both type have the same values
    !< error if b is not a option_reals type
    class(option_datetimes), intent(in) :: a
    class(option_abs), intent(in) :: b

    class(option_datetimes), pointer :: pOptDts
    logical :: hasSameDts
    logical :: hasSameDefDate
    logical :: hasSameKey
    logical :: hasSameDescription
    logical :: hasSameMinDate
    logical :: hasSameMaxDate
    logical :: hasSameArgs

    equiv_option_datetimes = .false.
    select type (pOptDts =>  b)
    type is (option_datetimes)

      hasSameKey = (a % key == pOptDts % key)
      hasSameDescription = (a % description == pOptDts % description)
      hasSameDts = hasSameDatetime(a % dates, pOptDts % dates)
      hasSameDefDate = hasSameDatetime(a % defDate, pOptDts % defDate)
      hasSameMinDate = hasSameDatetime(a % minDate, pOptDts % minDate)
      hasSameMaxDate = hasSameDatetime(a % maxDate, pOptDts % maxDate)
      hasSameArgs = hasSameArgsRange(a % argumentsRange, pOptDts % argumentsRange)

      if (hasSameKey .and. &
          hasSameDescription .and. &
          hasSameDts .and. &
          hasSameArgs .and. &
          hasSameDefDate .and. &
          hasSameMinDate .and. &
          hasSameMaxDate  ) then
        equiv_option_datetimes = .true.
      endif

    class default
      call raiseError(__FILE__, "equiv_option_datetimes", "Not implemented type")
    end select
    
  end function equiv_option_datetimes


  impure elemental logical function equiv_option_timedeltas(a, b) 
    !< compare option_reals to option_ints
    !< true if both type have the same values
    !< error if b is not a option_reals type
    class(option_timedeltas), intent(in) :: a
    class(option_abs), intent(in) :: b

    class(option_timedeltas), pointer :: pOptTds
    logical :: hasSameValues
    logical :: hasSameDefValue
    logical :: hasSameMin
    logical :: hasSameMax
    logical :: hasSameArgs

    equiv_option_timedeltas = .false.

    select type (pOptTds =>  b)
    type is (option_timedeltas)

      hasSameValues = hasSameTimedelta(a % values, pOptTds % values)
      hasSameDefValue = hasSameTimedelta(a % defValue, pOptTds % defValue)
      hasSameMin = hasSameTimedelta(a % mintd, pOptTds % mintd)
      hasSameMax = hasSameTimedelta(a % maxtd, pOptTds % maxtd)
      hasSameArgs = hasSameArgsRange(a % argumentsRange, pOptTds % argumentsRange)

      if (a % key == pOptTds % key .and. &
          a % description == pOptTds % description .and. &
          hasSameArgs .and. &
          hasSameValues .and. &
          hasSameDefValue .and. &
          hasSameMin .and. &
          hasSameMax ) then
        equiv_option_timedeltas = .true.
      endif

    class default
      call raiseError(__FILE__, "equiv_option_timedeltas", "Not implemented type")
    end select
    
  end function equiv_option_timedeltas


  logical function hasSameArgsRange(a, b)
    !< equal comparision for argsRange allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    type(argsRange), allocatable, intent(in) :: a, b

    if (allocated(a) .and. allocated(b) ) then
        hasSameArgsRange = all(a % range == b % range)
    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameArgsRange = .true.
    else
        hasSameArgsRange = .false.
    endif

  end function hasSameArgsRange


  logical function hasSameDatetime_scalar(a, b)
    !< equal comparision for argsRange allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    type(datetime), allocatable, intent(in) :: a, b

    if (allocated(a) .and. allocated(b) ) then
        hasSameDatetime_Scalar = ( a == b )
    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameDatetime_scalar = .true.
    else
        hasSameDatetime_scalar = .false.
    endif

  end function hasSameDatetime_scalar


  logical function hasSameDatetime_array(a, b)
    !< equal comparision for argsRange allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    type(datetime), allocatable, intent(in) :: a(:), b(:)

    ! both allocated?
    if (allocated(a) .and. allocated(b) ) then
        if (size(a) == size(b)) then ! same size?
          hasSameDatetime_array = all( a == b )
        else
          hasSameDatetime_array = .false.
        endif

    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameDatetime_array = .true.

    else
        hasSameDatetime_array = .false.
    endif

  end function hasSameDatetime_array


  logical function hasSameTimedelta_scalar(a, b)
    !< equal comparision for argsRange allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    type(timedelta), allocatable, intent(in) :: a, b

    if (allocated(a) .and. allocated(b) ) then
        hasSameTimedelta_Scalar = ( a == b )
    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameTimedelta_scalar = .true.
    else
        hasSameTimedelta_scalar = .false.
    endif

  end function hasSameTimedelta_scalar


  logical function hasSameTimedelta_array(a, b)
    !< equal comparision for argsRange allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    type(timedelta), allocatable, intent(in) :: a(:), b(:)

    ! both allocated?
    if (allocated(a) .and. allocated(b) ) then
        if (size(a) == size(b)) then ! same size?
          hasSametimedelta_array = all( a == b )
        else
          hasSametimedelta_array = .false.
        endif

    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSametimedelta_array = .true.

    else
        hasSametimedelta_array = .false.
    endif

  end function hasSameTimedelta_array


  logical function hasSameFile_scalar(a, b)
    !< equal comparision for file_t allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    type(file_t), allocatable, intent(in) :: a, b

    if (allocated(a) .and. allocated(b) ) then
        hasSameFile_Scalar = ( a == b )
    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameFile_scalar = .true.
    else
        hasSameFile_scalar = .false.
    endif

  end function hasSameFile_scalar


  logical function hasSameFile_array(a, b)
    !< equal comparision for argsRange allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    type(file_t), allocatable, intent(in) :: a(:), b(:)

    ! both allocated?
    if (allocated(a) .and. allocated(b) ) then
        if (size(a) == size(b)) then ! same size?
          hasSameFile_array = all( a == b )
        else
          hasSameFile_array = .false.
        endif

    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameFile_array = .true.

    else
        hasSameFile_array = .false.
    endif

  end function hasSameFile_array


  logical function hasSameInts_scalar(a, b)
    !< equal comparision for file_t allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    integer, allocatable, intent(in) :: a, b

    if (allocated(a) .and. allocated(b) ) then
        hasSameInts_Scalar = ( a == b )
    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameInts_scalar = .true.
    else
        hasSameInts_scalar = .false.
    endif

  end function hasSameInts_scalar


  logical function hasSameInts_array(a, b)
    !< equal comparision for argsRange allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    integer, allocatable, intent(in) :: a(:), b(:)

    ! both allocated?
    if (allocated(a) .and. allocated(b) ) then
        if (size(a) == size(b)) then ! same size?
          hasSameInts_array = all( a == b )
        else
          hasSameInts_array = .false.
        endif

    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameInts_array = .true.

    else
        hasSameInts_array = .false.
    endif

  end function hasSameInts_array


  logical function hasSameReals_sp_scalar(a, b)
    !< equal comparision for file_t allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    real(kind=sp), allocatable, intent(in) :: a, b

    if (allocated(a) .and. allocated(b) ) then
        hasSameReals_sp_Scalar = ( a == b )
    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameReals_sp_scalar = .true.
    else
        hasSameReals_sp_scalar = .false.
    endif

  end function hasSameReals_sp_scalar


  logical function hasSameReals_sp_array(a, b)
    !< equal comparision for argsRange allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    real(kind=sp), allocatable, intent(in) :: a(:), b(:)

    ! both allocated?
    if (allocated(a) .and. allocated(b) ) then
        if (size(a) == size(b)) then ! same size?
          hasSameReals_sp_array = all( a == b )
        else
          hasSameReals_sp_array = .false.
        endif

    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameReals_sp_array = .true.

    else
        hasSameReals_sp_array = .false.
    endif

  end function hasSameReals_sp_array


  logical function hasSameStr_scalar(a, b)
    !< equal comparision for file_t allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    type(string), allocatable, intent(in) :: a, b

    if (allocated(a) .and. allocated(b) ) then
        hasSameStr_Scalar = ( a == b )
    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameStr_scalar = .true.
    else
        hasSameStr_scalar = .false.
    endif

  end function hasSameStr_scalar


  logical function hasSameOptions_scalar(a, b)
    !< equal comparision for file_t allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    class(option_abs), allocatable, intent(in) :: a, b

    if (allocated(a) .and. allocated(b) ) then
        hasSameOptions_Scalar = ( a == b )
    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameOptions_scalar = .true.
    else
        hasSameOptions_scalar = .false.
    endif

  end function hasSameOptions_scalar


  logical function hasSameOptions_array(a, b)
    !< equal comparision for argsRange allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    class(option_abs), allocatable, intent(in) :: a(:), b(:)

    ! both allocated?
    if (allocated(a) .and. allocated(b) ) then
        if (size(a) == size(b)) then ! same size?
          hasSameOptions_array = all( a == b )
        else
          hasSameOptions_array = .false.
        endif

    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameOptions_array = .true.

    else
        hasSameOptions_array = .false.
    endif

  end function hasSameOptions_array


  logical function isAllowed(self, nelems)
    !< true if nelems fits in argsRange conditions
    !< (2)true if nelems is between start - end(OPT_NARGS_N is unlimited)
    !< (1)true if nelems is the same as range
    class(argsRange), intent(in) :: self
    integer, intent(in) :: nelems !< number of elements to test

    logical :: isFixed !< true if self only allows 1 value
    logical :: isRange !< true if self has a range of valid values
    logical :: isEndUnlimited !< true if max value is unlimited (ARGS_N)

    isAllowed = .false.
    isFixed = (size(self % range) == 1)
    isRange = (size(self % range) == 2)

    if (isFixed) then ! fixed number
      if (nelems == self % range(1)) isAllowed = .true.

    else if (isRange) then !< range of allowed values
      isEndUnlimited = (self % range(2) == OPT_NARGS_N)

      if (isEndUnlimited) then
        ! only check start range
        if (self % range(1) <= nelems) isAllowed = .true.
      else
        ! check start and end range
        if (self % range(1) <= nelems .and. &
            self % range(2) >= nelems ) isAllowed = .true.
      endif

    else
      call raiseUnexpectedError()
    endif
  end function isAllowed


  function argsRange_toString(self) result (r)
    !< argsRange string representation
    !< toString([2]) -> 2
    !< toString([N_ARGS]) -> *
    !< toString([2,2]) -> 2
    !< toSTring([2,N_ARGS]) -> 2, *
    class(argsRange), intent(in) :: self
    character(len=:), allocatable :: r !< output

    logical :: isFixed
    logical :: isEndUnlimited
    character(len=5) :: strStart, strEnd
    character(len=:), allocatable :: adjStrStart, adjStrEnd

    isFixed = (self % range(1) == self % range(2))

    if (isFixed) then
      write(strStart, '(I5)') self % range(1)
      r = trim(adjustl(strStart))//" allowed"

    else ! has range of values
      isEndUnlimited = (self % range(2) == OPT_NARGS_N)
      if (isEndUnlimited) then
        strEnd = "*"
      else
        write(strEnd, '(I5)') self % range(2)
      endif

      write(strStart, '(I5)') self % range(1)

      r = trim(adjustl(strStart))//" to "//trim(adjustl(strEnd))//" allowed"

    endif
  end function argsRange_toString


  subroutine getVal_option_str(self, r)
    !< return the all the strings values
    class(option_str), intent(in) :: self
    type(string), allocatable, intent(out) :: r(:) !< output

    integer :: nelems

    if (allocated(r)) deallocate(r)

    if (allocated(self % values)) then
      r = self % values
    else !< no values allocated
      if (allocated(self % defVal)) then
        nelems = self % argumentsRange % getStartRange()

        allocate(r(nelems))
        r = self % defVal
      else
        call raiseError(__FILE__, "getVal_option_str", &
                "No value defined yet from key", &
                self % key)
      endif
    endif

  end subroutine getVal_option_str


  subroutine getVal_option_files(self, r)
    !< return the all the strings values
    class(option_files), intent(in) :: self
    type(file_t), allocatable, intent(out) :: r(:) !< output

    integer :: nelems

    if (allocated(r)) deallocate(r)

    if (allocated(self % files)) then
      r = self % files
    else !< no values allocated
      if (allocated(self % defFile)) then
        nelems = self % argumentsRange % getStartRange()

        allocate(r(nelems))
        r(:) = self % defFile
      else
        call raiseError(__FILE__, "getVal_option_files", &
                "No value defined from key", &
                self % key)
      endif
    endif

  end subroutine getVal_option_files


  subroutine getVal_option_ints(self, r)
    !< return the all the strings values
    class(option_ints), intent(in) :: self
    integer, allocatable, intent(out) :: r(:) !< output

    integer :: nelems

    if (allocated(r)) deallocate(r)

    if (allocated(self % values)) then
      r = self % values
    else !< no values allocated
      if (allocated(self % defValue)) then
        nelems = self % argumentsRange % getStartRange()

        allocate(r(nelems))
        r(:) = self % defValue
      else
        call raiseError(__FILE__, "getVal_option_ints", &
                "No value defined from key", &
                self % key)
      endif
    endif

  end subroutine getVal_option_ints


  subroutine getVal_option_reals(self, r)
    !< return the all the strings values
    class(option_reals), intent(in) :: self
    real(kind=sp), allocatable, intent(out) :: r(:) !< output
    integer :: nelems

    if (allocated(r)) deallocate(r)

    if (allocated(self % values)) then
      r = self % values
    else !< no values allocated
      if (allocated(self % defValue)) then
        nelems = self % argumentsRange % getStartRange()
        r = self % defValue
      else
        call raiseError(__FILE__, "getVal_option_reals", &
                "No value defined from key", &
                self % key)
      endif
    endif

  end subroutine getVal_option_reals


  subroutine getVal_option_datetimes(self, r)
    !< Return the all datetimes values
    !< if no values then it returns the default
    !< in case of no default, an error is raised
    class(option_datetimes), intent(in) :: self
    type(datetime), allocatable, intent(out) :: r(:) !< output
    integer :: nelems

    if (allocated(r)) deallocate(r)

    if (allocated(self % dates)) then
      r = self % dates
    else !< no values allocated
      if (allocated(self % defDate)) then
        nelems = self % argumentsRange % getStartRange()

        allocate(r(nelems))
        r(:) = self % defDate
      else
        call raiseError(__FILE__, "getVal_option_datetimes", &                
                "No value defined from key", &
                self % key)
      endif
    endif

  end subroutine getVal_option_datetimes


  subroutine getVal_option_timedeltas(self, r)
    !< Return the all datetimes values
    !< if no values then it returns the default
    !< in case of no default, an error is raised
    !<
    !< For scalar values wrap into an array
    class(option_timedeltas), intent(in) :: self
    type(timedelta), allocatable, intent(out) :: r(:) !< output

    integer :: nelems

    if (allocated(r)) deallocate(r)

    if (allocated(self % values)) then
      r = self % values
    else !< no values allocated
      if (allocated(self % defValue)) then
        nelems = self % argumentsRange % getStartRange()

        allocate(r(nelems))
        r(:) = self % defValue
      else
        call raiseError(__FILE__, "getVal_option_timedeltas", &
                "No value defined from key", &
                self % key)
      endif
    endif

  end subroutine getVal_option_timedeltas


  subroutine raiseUnexpectedTypeFoundError(strType)
    !< raise an exception 
    character(len=*), intent(in) :: strType
    
    call raiseError(__FILE__, "raiseUnexpectedTypeFoundError", &
            "Unexpected option_xxx type found", &
            "Expected: "//strType)
  end subroutine raiseUnexpectedTypeFoundError


  function toString_option_str(this) result(r)
    !< string representation of option_str
    class(option_str), intent(in) :: this
    character(len=:), allocatable :: r !< output

    character(len=:), allocatable :: argsString
    type(stringCollection), allocatable :: sc

    r = "key: "//this % key//new_line('A')
    r = r // "Type: CFG_STR" // new_line('A')
    r = r // "Description: "//this % description//new_line('A')

    if (allocated(this % argumentsRange)) &
      r = r //"Range: "// this % argumentsRange % toString() // new_line('A')

    if (allocated(this % values)) then
      allocate(sc)
      sc = stringCollection(this % values) !< wrap an array of string 
      r = r // "Values= "// sc % toString()
      deallocate(sc)
    else
      r = r // "Values= "// EMPTY_VAL
    endif

    if (allocated(this % defVal)) then
      if (allocated(sc)) then
        deallocate(sc)
        allocate(sc)
      endif
      sc = stringCollection(this % defVal) !< wrap an array of string 
      r = r // new_line('A') // "Default= " // sc % toString() 
    endif
    
  end function toString_option_str


  function toString_option_files(this) result(r)
    !< string representation of option_str
    class(option_files), intent(in) :: this
    character(len=:), allocatable :: r !< output
    character(len=FILE_NAME_MAX_LEN), allocatable :: fileNames(:)
    type(stringCollection) :: sc

    r = "key: "//this % key//new_line('A')
    r = r // "Type: CFG_FILES" // new_line('A')
    r = r // "Description: "//this % description//new_line('A')

    if (allocated(this % argumentsRange)) &
      r = r //"Range: "// this % argumentsRange % toString() // new_line('A')

    if (allocated(this % files)) then
      fileNames = this % files % getName()
      sc = stringCollection(fileNames)
      r = r // "Values= "// sc % toString()
    else
      r = r // "Values= "// EMPTY_VAL
    endif

    if (allocated(this % defFile)) then
      fileNames = this % defFile % getName()
      sc = stringCollection(fileNames)
      r = r // new_line('(A)') // "Default= " // sc % toString()
    endif
    
  end function toString_option_files


  function toString_option_reals(this) result(r)
    !< string representation of option_str
    class(option_reals), intent(in) :: this
    character(len=:), allocatable :: r !< output

    character(len=50), allocatable :: strReals(:)
    character(len=50) :: strReal
    type(string), allocatable :: stringsReal(:)
    integer :: i, nelems
    character(len=*), parameter :: REAL_FORMAT = '(F10.3)'
    type(stringCollection) :: sc

    r = "key: "//this % key//new_line('A')
    r = r // "Type: CFG_REALS" // new_line('A')
    r = r // "Description: "//this % description//new_line('A')

    if (allocated(this % argumentsRange)) &
      r = r //"Range: "// this % argumentsRange % toString() // new_line('A')

    if (allocated(this % values)) then
      nelems = size(this % values)
      ! real to string
      allocate(strReals(nelems))
      do i = 1, nelems 
!        write(*,*) "toString_option_reals:: this % values =", this % values(i)
        write(strReals(i), REAL_FORMAT) this % values(i)
      enddo
      stringsReal = sanitize(strReals)
      sc = stringCollection(stringsReal)
      r = r // "Values= "// sc % toString()
      deallocate(strReals)
    else
      r = r // "Values= "// EMPTY_VAL
    endif

    if (allocated(this % defValue)) then
      nelems = size(this % defValue)
      allocate(strReals(nelems))
      ! real to string
      do i = 1, nelems
        write(strReals(i), REAL_FORMAT) this % defValue(i)
      enddo
      stringsReal = sanitize(strReals)
      sc = stringCollection(stringsReal)
      r = r // new_line('(A)') // "Default= " // sc % toString() !trim(adjustl(strReal))
      deallocate(strReals)
    endif

    ! minValue
    if (allocated(this % minValue)) then
      write(strReal, REAL_FORMAT) this % minValue
      r = r // new_line('(A)') // "Min= " // trim(adjustl(strReal))
    endif
    ! maxValue
    if (allocated(this % maxValue)) then
      write(strReal, REAL_FORMAT) this % maxValue
      r = r // new_line('(A)') // "Max= " // trim(adjustl(strReal))
    endif
    
  end function toString_option_reals


  function toString_option_ints(this) result(r)
    !< string representation of option_str
    class(option_ints), intent(in) :: this
    character(len=:), allocatable :: r !< output

    character(len=20), allocatable :: strInts(:)
    character(len=20) :: strInt
    type(string), allocatable :: stringsInt(:)
    character(len=*), parameter :: DEF_FORMAT = '(I10)'
    integer :: i, nelems
    type(stringCollection) :: sc

    r = "key: "//this % key//new_line('A')
    r = r // "Type: CFG_INTS" // new_line('A')
    r = r // "Description: "//this % description//new_line('A')

    if (allocated(this % argumentsRange)) &
      r = r //"Range: "// this % argumentsRange % toString() // new_line('A')

    if (allocated(this % values)) then
      nelems = size(this % values)
      ! int to string
      allocate(strInts(nelems))
      do i = 1, nelems 
        write(strInts(i), DEF_FORMAT) this % values(i)
      enddo
      stringsInt = sanitize(strInts)
      sc = stringCollection(stringsInt)
      r = r // "Values= "// sc % toString()
    else
      r = r // "Values= "// EMPTY_VAL
    endif

    if (allocated(this % defValue)) then
      write(strInt, DEF_FORMAT) this % defValue
      r = r // new_line('A') // "Default= " // trim(adjustl(strInt)) 
    endif

    ! minValue
    if (allocated(this % minValue)) then
      write(strInt, DEF_FORMAT) this % minValue
      r = r // new_line('A') // "Min= " // trim(adjustl(strInt))
    endif
    ! maxValue
    if (allocated(this % maxValue)) then
      write(strInt, DEF_FORMAT) this % maxValue
      r = r // new_line('A') // "Max= " // trim(adjustl(strInt))
    endif
    
  end function toString_option_ints


  function toString_option_datetimes(this) result(r)
    !< string representation of option_str
    class(option_datetimes), intent(in) :: this
    character(len=:), allocatable :: r !< output

    character(len=DT_MAX_LEN), allocatable :: strDts(:)
    character(len=DT_MAX_LEN) :: strDt
    type(string), allocatable :: stringDts(:)
    integer :: i, nelems
    type(stringCollection) :: sc

    r = "key: "//this % key//new_line('A')
    r = r // "Type: CFG_DATETIMES" // new_line('A')
    r = r // "Description: "//this % description//new_line('A')

    if (allocated(this % argumentsRange)) &
      r = r //"Range: "// this % argumentsRange % toString() // new_line('A')

    if (allocated(this % dates)) then
      strDTs = this % dates % toString()
      stringDts = sanitize(strDts)
      sc = stringCollection(stringDts)
      r = r // "Values= "// sc % toString()
    else
      r = r // "Values= "// EMPTY_VAL
    endif

    if (allocated(this % defDate)) then
      strDt = this % defDate % toString()
      r = r // new_line('(A)') // "Default= " // trim(adjustl(strDt))
    endif

    ! minValue
    if (allocated(this % minDate)) then
      strDt = this % minDate % toString()
      r = r // new_line('(A)') // "Min= " // trim(adjustl(strDt))
    endif
    ! maxValue
    if (allocated(this % maxDate)) then
      strDt = this % maxDate % toString()
      r = r// new_line('(A)') // "Max= " // trim(adjustl(strDt))
    endif
    
  end function toString_option_datetimes


  function toString_option_timedeltas(this) result(r)
    !< string representation of option_str
    class(option_timedeltas), intent(in) :: this
    character(len=:), allocatable :: r !< output

    character(len=TD_MAX_LEN), allocatable :: strTds(:)
    character(len=TD_MAX_LEN) :: strTd
    type(string), allocatable :: stringTds(:)
    integer :: i, nelems
    type(stringCollection) :: sc

    r = "key: "//this % key//new_line('A')
    r = r // "Type: CFG_TIMEDELTAS" // new_line('A')
    r = r // "Description: "//this % description//new_line('A')

    if (allocated(this % argumentsRange)) &
      r = r //"Range: "// this % argumentsRange % toString() // new_line('A')

    if (allocated(this % values)) then
      strTds = this % values % toString()
      stringTds = sanitize(strTds)
      sc = stringCollection(stringTds)
      r = r // "Values= "// sc % toString()
    else
      r = r // "Values= "// EMPTY_VAL
    endif

    if (allocated(this % defValue)) then
      if (allocated(strTds)) deallocate(strTds)
      if (allocated(stringTds)) deallocate(stringTds)
      strTds = this % defValue % toString()
      stringTds = sanitize(strTds)
      sc = stringCollection(stringTds)
      r = r // new_line('A') // "Default= " // sc % toString()
    endif

    ! minValue
    if (allocated(this % minTd)) then
      strTd = this % minTd % toString()
      r = r // new_line('A') // "Min= " // trim(adjustl(strTd))
    endif
    ! maxValue
    if (allocated(this % maxTd)) then
      strTd = this % maxTd % toString()
      r = r // new_line('A') // "Max= " // trim(adjustl(strTd))
    endif
    
  end function toString_option_timedeltas


  impure elemental function optionsValues_to_string(options) result (r)
    !< returns all options values into a string
    class(option_abs), intent(in) :: options
    type(string) :: r !< output

    type(stringCollection) :: sc
    type(string), allocatable :: strs(:)
    character(len=:), allocatable :: tmp

    class(*), pointer :: popt

    integer, allocatable :: ints(:)
    real(kind=sp), allocatable :: reals(:)
    character(len=FILE_NAME_MAX_LEN), allocatable :: filenames(:)
    character(len=DT_MAX_LEN), allocatable :: strDatetimes(:)
    character(len=TD_MAX_LEN), allocatable :: strTimedeltas(:)
    type(file_t), allocatable :: files(:)
    type(datetime), allocatable :: datetimes(:)
    type(timedelta), allocatable :: timedeltas(:)

    select type(popt => options)
    type is (option_str)
        call popt % getVal(strs)
        sc = stringCollection(strs)
        tmp = sc % toString()

    type is (option_ints)
        call popt % getVal(ints)
        strs = int2string(ints)
        sc = stringCollection(strs)
        tmp = sc % toString()

    type is (option_reals)
        call popt % getVal(reals)
        strs = real2string(reals)
        sc = stringCollection(strs)
        tmp = sc % toString()

    type is (option_files)
        call popt % getVal(files)
        filenames = files % getName()
        sc = stringCollection(filenames)
        tmp = sc % toString()

    type is (option_datetimes)
        call popt % getVal(datetimes)
        strDatetimes = datetimes % toString()
        sc = stringCollection(strDatetimes)
        tmp = sc % toString()

    type is (option_timedeltas)
        call popt % getVal(timedeltas)
        strTimedeltas = timedeltas % toString()
        sc = stringCollection(strTimedeltas)
        tmp = sc % toString()

    class default
        call raiseUnexpectedError()
    end select

    r = string(trim(adjustl(tmp)))
    
  end function optionsValues_to_string


  function toString_argsRange(this) result (r)
    !< string representation of arguments range
    class(argsRange), intent(in) :: this
    character(len=:), allocatable :: r !< output

    integer, parameter :: START = 1, END = 2
    character(len=20) :: tmp, tmp1

    write(tmp,*) this % range(START)
    write(tmp1,*) this % range(END)
  
    if (this % range(START) == this % range(END)) then
      r = trim(adjustl(tmp)) // " to " // trim(adjustl(tmp1))
    else
      r = trim(adjustl(tmp))
    endif
  end function toString_argsRange


  subroutine initialize_option_bools(this, key, nargs, description, default)
    !< initialize option_files class
    !< arguments
    !< key: unique value 
    !< nargs: how many values expect
    !< description: explain the purpose of this key
    !< default: if has no value specified it uses this one if defined
    class(option_bools), intent(inout) :: this
    character(len=*), intent(in) :: key 
    integer, intent(in) :: nargs(:)
    character(len=*), intent(in) :: description
    logical, intent(in), optional :: default(:)

    this % key = key
    this % description = description

    allocate(this % argumentsRange)
    this % argumentsRange = argsRange(nargs)

    if (present(default)) then
      this % defVal = default
    endif

  end subroutine initialize_option_bools


  subroutine setVal_bools(self, newBool)
    !< setVal for character
    !< it removes previous content
    class(option_bools), intent(inout) :: self
    logical, intent(in) :: newBool

    integer :: nelems

    nelems = 1

    ! reallocate values
    if (allocated(self % values)) deallocate(self % values)
    allocate(self % values(nelems))

    ! save values
    self % values = newBool
  end subroutine setVal_bools


  subroutine setVal_many_bools(self, manyBools)
    !< setVal for character
    !< it removes previous content
    class(option_bools), intent(inout) :: self
    logical, intent(in) :: manyBools(:)

    integer :: nelems

    ! check the number of given arguments is correct
    nelems = size(manyBools)
    call self % checkNargs(nelems)

    ! reallocate values
    if (allocated(self % values)) deallocate(self % values)
    allocate(self % values(nelems))

    ! save values
    self % values = manyBools
  end subroutine setVal_many_bools


  impure elemental logical function equiv_option_bool(a, b) 
    !< compare option_str vs option_str
    !< true if both types hae the same values
    !< error if b is not option_str type
    class(option_bools), intent(in) :: a
    class(option_abs), intent(in) :: b

    class(option_bools), pointer :: pOptBool
    logical :: hasSameStrings
    logical :: hasSameDefString
    logical :: hasSameArgs

    equiv_option_bool = .false.
    select type (pOptBool =>  b)
    type is (option_bools)

      hasSameStrings = hasSameBools(a % values, pOptBool % values)
      hasSameDefString = hasSameBools(a % defVal, pOptBool % defVal)
      hasSameArgs = hasSameArgsRange(a % argumentsRange, pOptBool % argumentsRange)

      if (a % key == pOptBool % key .and. &
          a % description == pOptBool % description .and. &
          hasSameArgs .and. &
          hasSameDefString .and. &
          hasSameStrings) then
        equiv_option_bool = .true.
      endif
    class default
      call raiseError(__FILE__, "equiv_option_bool", "Not implemented type")
    end select
    
  end function equiv_option_bool


  logical function hasSameBools_scalar(a, b)
    !< equal comparision for file_t allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    logical, allocatable, intent(in) :: a, b

    if (allocated(a) .and. allocated(b) ) then
        hasSameBools_Scalar = ( a .eqv. b )
    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameBools_scalar = .true.
    else
        hasSameBools_scalar = .false.
    endif

  end function hasSameBools_scalar


  logical function hasSameBools_array(a, b)
    !< equal comparision for argsRange allocatable arguments 
    !< true if a and b are the same
    !< true if both unallocated 
    logical, allocatable, intent(in) :: a(:), b(:)

    ! both allocated?
    if (allocated(a) .and. allocated(b) ) then
        if (size(a) == size(b)) then ! same size?
          hasSameBools_array = all( a .eqv. b )
        else
          hasSameBools_array = .false.
        endif

    else if (.not. allocated(a) .and. .not. allocated(b) ) then
        hasSameBools_array = .true.

    else
        hasSameBools_array = .false.
    endif

  end function hasSameBools_array


  subroutine getVal_option_bools(self, r)
    !< return the all the strings values
    class(option_bools), intent(in) :: self
    logical, allocatable, intent(out) :: r(:) !< output

    integer :: nelems

    if (allocated(r)) deallocate(r)

    if (allocated(self % values)) then
      r = self % values
    else !< no values allocated
      if (allocated(self % defVal)) then
        nelems = self % argumentsRange % getStartRange()

        allocate(r(nelems))
        r(:) = self % defVal
      else
        call raiseError(__FILE__, "getVal_option_bools", "No value defined yet")
      endif
    endif

  end subroutine getVal_option_bools


  function toString_option_bool(this) result(r)
    !< string representation of option_str
    class(option_bools), intent(in) :: this
    character(len=:), allocatable :: r !< output

    character(len=10), allocatable :: tmp(:)
    type(stringCollection), allocatable :: sc
    integer :: i

    r = "key: "//this % key//new_line('A')
    r = r // "Type: CFG_BOOLS" // new_line('A')
    r = r // "Description: "//this % description//new_line('A')

    if (allocated(this % argumentsRange)) &
      r = r //"Range: "// this % argumentsRange % toString() // new_line('A')

    if (allocated(this % values)) then
      allocate(sc)
      allocate(tmp(size(this % values)))
      do i = 1, size(this % values)
        write(tmp(i), *) this % values(i)
      enddo
      sc = stringCollection(tmp) !< wrap an array of string 
      r = r // "Values= "// sc % toString()
      deallocate(sc)
      deallocate(tmp)
    else
      r = r // "Values= "// EMPTY_VAL
    endif

    if (allocated(this % defVal)) then
      allocate(tmp(1))
      allocate(sc)
      write(tmp(1), *) this % defVal
      sc = stringCollection(tmp) !< wrap an array of string 
      r = r // new_line('A') // "Default= " // sc % toString()
      deallocate(tmp)
    endif
    
  end function toString_option_bool


  function toNiceString(self) result (r)
    !< show a slim version of toString for option_abs
    !< key (int, start, end): description
    class(option_abs), intent(inout) :: self
    character(:), allocatable :: r
    character(:), allocatable :: strtype
    character(:), allocatable :: strRange

    select type (ptr => self)
    type is (option_bools)
       strType = "boolean"
    type is (option_ints)
       strType = "int"
    type is (option_reals)
       strType = "real"
    type is (option_files)
       strType = "file"
    type is (option_datetimes)
       strType = "datetime"
    type is (option_timedeltas)
       strType = "timedelta"
    type is (option_str)
       strType = "str"
    class default
       strType = "unknown"
    end select
    strRange = self % argumentsRange % toString() 
    r = self % key//" ("//strType//", "// strRange //"): "//self % description
  end function toNiceString

end module SHR_typeOptions_mod
