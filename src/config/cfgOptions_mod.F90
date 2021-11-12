!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : SHR_cfgOptions_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> Manage cfg options
!> 
!> It allows to define which option rules expect for each section.
!> It matches the defined options in the code with the file options.
!> 
!------------------------------------------------------------------------------
module SHR_cfgOptions_mod

  use SHR_precision_mod, only: sp
  use SHR_datetime_mod, only: datetime, timedelta
  use SHR_strings_mod, only: string
  use SHR_dict_mod, only: dict
  use SHR_file_mod, only: file_t
  use SHR_error_mod, only: raiseError, raiseUnexpectedError, raiseNotImplementedError
  use SHR_configFile_mod, only: configFile
  use SHR_typeOptions_mod, only: CFG_STR, CFG_FILES, CFG_REALS, CFG_INTS, &
            CFG_TIMEDELTAS, CFG_DATETIMES, CFG_BOOLS, CFG_AVAILABLE
  use SHR_typeOptions_mod, only: OPT_NARGS_N
  use SHR_typeOptions_mod, only: option_reals, option_ints, option_abs, &
                option_files, option_str, option_datetimes, &
                option_timedeltas, option_bools

  implicit none

  private

  public :: cfgOptions_t
  public :: CFG_STR, CFG_FILES, CFG_REALS, CFG_INTS, &
            CFG_TIMEDELTAS, CFG_DATETIMES, CFG_BOOLS
  public :: CFG_OPT_ARGS_N


  ! list all config fields avaiable
  integer, parameter :: CFG_OPT_ARGS_N = OPT_NARGS_N


  type component_t !< component options
    character(len=:), allocatable :: name 
    type(dict), allocatable :: options 
  contains
    procedure :: initialize => initialize_component
    procedure :: getName !< get component name
    procedure :: addOption !< add new option
    procedure :: getOption !< get option
    procedure :: hasOption !< true if the option is found
    procedure :: toString => toString_component
    procedure :: getAllOptionsKeys => getAllOptionsKeys_component
  end type component_t


  interface component_t
    module procedure :: component_constructor
  end interface component_t


  type cfgOptions_t
    character(len=:), allocatable :: fileName ! options filename
    type(configFile), allocatable :: cfgFile !< 
    type(dict), allocatable :: components 

  contains
    procedure :: init !< initialize class
    procedure :: def !< define new option (what to expect)
    procedure :: load !< load cfg file
    procedure :: toString => toString_cfgOptions !< show options (all or component)
    procedure :: addComponent
    procedure, private :: getComponent
    procedure :: getAllComponentNames
    procedure :: getAllOptionsKeys =>  getAllOptionsKeys_cfgOptions
    procedure :: hasCfgFile
    procedure :: getOptionType

    procedure, private :: def_int !< define an integer option
    procedure, private :: def_real_sp !< define a real sp option
    procedure, private :: def_files !< define a files option
    procedure, private :: def_str !< define a string option
    procedure, private :: def_timedeltas !< define timedelta option
    procedure, private :: def_datetimes !< define datetime option
    procedure, private :: def_bools !< define datetime option

    ! getters
    procedure :: get_reals
    procedure :: get_ints
    procedure :: get_files
    procedure :: get_strings
    procedure :: get_datetime
    procedure :: get_timedelta
    procedure :: get_many_bools
    procedure :: get_bool
    generic :: get => get_strings, get_datetime, get_timedelta, &
                get_reals, get_ints, get_files, get_bool, get_many_bools

    ! setters
    procedure :: set_reals
    procedure :: set_ints
    procedure :: set_files
    procedure :: set_strings
    procedure :: set_datetimes
    procedure :: set_timedeltas
    procedure :: set_bools
    generic :: set => set_reals, set_ints, set_files, &
                set_strings, set_datetimes, set_timedeltas, &
                set_bools

    final :: finalize

  end type cfgOptions_t

contains


  subroutine raiseUnexpectedOptionTypeError(foundOptionType, expectedType)
    !< Raise an error when the expected option type is not found
    !< while showing the expected (if possible) type
    class(option_abs), pointer, intent(in) :: foundOptionType
    character(len=*), intent(in) :: expectedType
    character(len=:), allocatable :: nameType

    select type (optType => foundOptionType)
    type is (option_str)
      nameType = "option_str"
    type is (option_ints)
      nameType = "option_ints"
    type is (option_reals)
      nameType = "option_reals"
    type is (option_bools)
      nameType = "option_bools"
    type is (option_datetimes)
      nameType = "option_datetimes"
    type is (option_timedeltas)
      nameType = "option_timedeltas"
    type is (option_files)
      nameType = "option_files"
    class default
      nameType = "Unknown"
    end select

    call raiseError(__FILE__ , &
            "raiseUnexpectedOptionTypeError", &
            "Unexpected type found", &
            "Expected type: "//expectedType, &
            "Buf found: "//nameType)

  end subroutine  raiseUnexpectedOptionTypeError


  subroutine initialize_component(this, name)
    !< initialize compoment with 'name'
    class(component_t), intent(inout) :: this
    character(len=*), intent(in) :: name

    this % name = name 
    allocate(this % options)
    this % options = dict()
  end subroutine initialize_component


  type(component_t) function component_constructor(name) result (r)
    !< component_t constructor
    character(len=*), intent(in) :: name
    call r % initialize(name)
  end function component_constructor


  logical function hasOption(this, name)
    !< true if the option 'name' is already defined in the component
    class(component_t), intent(inout) :: this
    character(len=*), intent(in) :: name

    hasOption = this % options % hasKey(name)
  end function hasOption


  function toString_component(this) result (r)
    !< string representation of component
    class(component_t), intent(inout) :: this
    character(len=:), allocatable :: r !< output

    type(string), allocatable :: allkeys(:)
    integer :: i
    class(option_abs), pointer :: option

    allkeys = this % options % getAllKeys()
    do i = 1, size(allkeys)
      option => this % getOption(allkeys(i) % toString())
      r = r // option % toNiceString() // new_line('A')
    enddo
  end function toString_component


  subroutine init(this)
    !< initialize cfgOptions
    class(cfgOptions_t), intent(inout) :: this
    allocate(this % components)
    this % components = dict()
  end subroutine init


  subroutine load(this, filename)
    !< load 'filename' cfg config file 
    class(cfgOptions_t), intent(inout) :: this
    character(*), intent(in) :: filename
    type (file_t) :: optFile
    class(component_t), pointer :: comp

    type(string), allocatable :: keys(:)
    type(string), allocatable :: allOptionsKeys(:)
    integer :: ikey, ioption, optType
    logical :: hasFileSection, hasFileOption
    character(:), allocatable :: compName
    character(:), allocatable :: key

    type(string), allocatable :: svals(:)
    integer, allocatable :: ivals(:)
    real(kind=sp), allocatable :: rvals(:)
    logical, allocatable :: bvals(:)
    type(timedelta), allocatable :: tdvals(:)
    type(datetime), allocatable :: dtvals(:)
    type(file_t), allocatable :: fvals(:)

    optFile = file_t(filename)
    if (.not. optFile % exists()) then
        call raiseError(__FILE__, "init", &
                "Option file not found", &
              "Filename: "// filename, withTraceback=.false.)
    endif

    this % fileName = fileName
    allocate(this % cfgFile)
    call this % cfgFile % init(filename)


    ! update all values for each component/option
    keys = this % components % getAllkeys()
    do ikey = 1, size(keys)
      compName = keys(ikey) % toString()
      comp => this % getComponent(compName)
      hasFileSection = this % cfgFile % existsSection(compName)

      if (hasFileSection) then
        AllOptionsKeys = comp % getAllOptionsKeys()

        do ioption = 1, size(allOptionsKeys)
          key = allOptionsKeys(ioption)
          hasFileOption = this % cfgFile % existsOption(compName, key)
          if (.not. hasFileOption) cycle

          optType = this % getOptionType(compName, key)
          if (optType == CFG_STR) then
            call this % cfgFile % getOption(compName, key, svals)
            call this % set(compName, key, svals)

          else if (optType == CFG_INTS) then
            call this % cfgFile % getOption(compName, key, ivals)
            call this % set(compName, key, ivals)

          else if (optType == CFG_REALS) then
            call this % cfgFile % getOption(compName, key, rvals)
            call this % set(compName, key, rvals)

          else if (optType == CFG_BOOLS) then
            call this % cfgFile % getOption(compName, key, bvals)
            call this % set(compName, key, bvals)

          else if (optType == CFG_DATETIMES) then
            call this % cfgFile % getOption(compName, key, dtvals)
            call this % set(compName, key, dtvals)

          else if (optType == CFG_TIMEDELTAS) then
            call this % cfgFile % getOption(compName, key, tdvals)
            call this % set(compName, key, tdvals)

          else if (optType == CFG_FILES) then
            call this % cfgFile % getOption(compName, key, svals)
            fvals = file_t(svals)
            call this % set(compName, key, fvals)

          endif
        enddo
      endif
    enddo
  end subroutine load 


  subroutine addComponent(this, name)
    !< add component into cfgOptions
    !< if it already exists in the list an error is raised
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: name 

    type(component_t), pointer :: newComponent
    class(*), pointer :: wrapper

    if (this % components % hasKey(name)) then
      call raiseError(__FILE__, "addComponent", &
              "Component already exists", &
              "Component: "//name)
    endif
 
!    write(*,*) "cfgOptions_mod:: addComponent:: new component, name=", name

    allocate(newComponent)
    call newComponent % initialize(name)
    wrapper => newComponent
    call this % components % insert(name, wrapper)

  end subroutine addComponent


  function getComponent(this, name) result (comp)
    !< Given a name it returns the pointer to a component
    !< If not found it raises an error
    !< This subroutine wraps the extraction due to fortran limitations
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: name
    class(component_t), pointer :: comp

    class(*), pointer :: wrap

    wrap => this % components % get(name)
    select type (wrap)
    type is (component_t)
      comp => wrap
    class default
      call raiseUnexpectedError()
    end select

  end function getComponent


  subroutine def(this, comName, optName, description, optType, nargs, &
                 defValS, defValI, defValR, defValDt, defValTd, defValB, &
                 choicesI, choicesR, choicesS, choicesDt, choicesTd, &
                 minI, maxI, minR, maxR, minDT, maxDT, minTD, maxTD)
    !< define a new option
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: comName !< component name where it belongs
    character(len=*), intent(in) :: optName !< option name
    character(len=*), intent(in) :: description !< description info
    integer, intent(in) :: optType !< option type (int, real, ...)
    integer, intent(in), optional :: nargs(:) !< Define how many elements find when declaring CFG_ARRAY_xxx 

    type(string), intent(in), optional :: defValS(:) !< default value (string, files)
    integer, intent(in), optional :: defValI(:) !< default value (integer)
    real(kind=sp), intent(in), optional :: defValR(:) !< default value (real)
    type(datetime), intent(in), optional :: defValDt(:) !< default value (datetime)
    type(timedelta), intent(in), optional :: defValTd(:) !< default value (timedelta)
    logical, intent(in), optional :: defValB(:) !< default value (logical)

    type(string), intent(in), optional :: choicesS(:)
    integer, intent(in), optional :: choicesI(:) 
    real(kind=sp), intent(in), optional :: choicesR(:)
    type(datetime), intent(in), optional :: choicesDt(:) 
    type(timedelta), intent(in), optional :: choicesTd(:) 

    integer, intent(in), optional :: minI, maxI 
    real(kind=sp), intent(in), optional :: minR, maxR
    type(timedelta), intent(in), optional :: minTD, maxTD
    type(datetime), intent(in), optional :: minDT, maxDT

!    write (*,*) "cfgOptions_mod:: def:: new option defined for ",  comName,", " , optName

    if (optType == CFG_INTS) then
      call this % def_int(comName, optName, description, nargs, defValI, minI, maxI, choicesI)
    else if (optType == CFG_REALS) then
      call this % def_real_sp(comName, optName, description, nargs, defValR, minR, maxR, choicesR)
    else if (optType == CFG_FILES) then
      call this % def_files(comName, optName, description, nargs, defValS, choicesS)
    else if (optType == CFG_STR) then
      call this % def_str(comName, optName, description, nargs, defValS, choicesS)
    else if (optType == CFG_DATETIMES) then
      call this % def_datetimes(comName, optName, description, nargs, defValDT, minDT, maxDT, choicesDt)
    else if (optType == CFG_TIMEDELTAS) then
      call this % def_timedeltas(comName, optName, description, nargs, defValTd, minTd, maxTd, choicesTd)
    else if (optType == CFG_BOOLS) then
      call this % def_bools(comName, optName, description, nargs, defValB)
    else
      call raiseUnexpectedError()
    endif

  end subroutine def


  subroutine raiseComponentNotFoundError(name)
    !< component not found error
    character(len=*), intent(in) :: name !< expected component name to find
    call raiseError(__FILE__, &
            "raiseComponentNotFoundError", &
            "Component not found", &
            "Component: "//Name)
  end subroutine raiseComponentNotFoundError


  subroutine raiseOptionAlreadyDefinedError(oName, cName)
    !< option with the key 'oName' is already defined in the compoment 'cName'
    character(len=*), intent(in) :: oName !< option name
    character(len=*), intent(in) :: cName !< component name

    call raiseError(__FILE__ ,&
            "raiseOptionAlreadyDefinedError", &
            "Option is alread defined in the component", &
            "Component: "//cName, &
            "key: "//oName)
  end subroutine raiseOptionAlreadyDefinedError


  subroutine def_bools(this, compName, key, description, nargs, default)
    !< define a new int option
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: compName !< component name
    character(len=*), intent(in) :: key !< option key
    character(len=*), intent(in) :: description !< description info
    integer, intent(in), optional :: nargs(:) !< how many values to expect (def: 1)
    logical, intent(in), optional :: default(:) !< default value 
    integer, allocatable :: nargsIn(:)

    logical :: hasOpt
    class(option_bools), pointer :: newOption
    class(component_t), pointer :: comp

    logical :: hasOptionInCfgFile
    logical :: hasConfigFile
    logical, allocatable :: boolValues(:)

    ! find component
    comp => this % getComponent(compName)

    ! is the option defined?
    hasOpt = comp % hasOption(key)
    if (hasOpt) call raiseOptionAlreadyDefinedError(key, compName)

    nargsIn = [1] ! default value
    if (present(nargs)) nargsIn = nargs

    allocate(newOption)
    call newOption % initialize(key, nargsIn, description, default)

    ! value in config file?
    hasConfigFile = this % hasCfgFile()

    ! get value from config file 
    if (hasConfigFile) then
      hasOptionInCfgFile = this % cfgFile % existsOption(compName, key)
      if (hasOptionInCfgFile) then ! yes
        call this % cfgFile % getOption(compName, key, boolValues) ! load value
        call newOption % setVal(boolValues)
      else ! nope
        if (.not. present(default)) then ! otherwise raise an error
          call raiseError(__FILE__, "def_bools", &
                "No value defined in the configuration file nor default value specified", &
                "Component: "//compName, &
                "Option: "//key)
        endif
      endif
    endif

    call comp % addOption(key, newOption)
  end subroutine def_bools


  subroutine def_int(this, compName, key, description, nargs, default, min, max, choices)
    !< define a new int option
    !< it creates an option and get its value from the config file
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: compName !< component name
    character(len=*), intent(in) :: key !< option key
    character(len=*), intent(in) :: description !< description info
    integer, intent(in), optional :: nargs(:) !< how many values to expect (def: 1)
    integer, intent(in), optional :: default(:) !< default value 
    integer, intent(in), optional :: min
    integer, intent(in), optional :: max
    integer, intent(in), optional :: choices(:) !< it only allows a subset of values to be defined

    integer, allocatable :: nargsIn(:)
    logical :: hasOptionInCfgFile
    logical :: hasOpt
    class(option_ints), pointer :: newOption
    class(component_t), pointer :: comp
    integer, allocatable :: optValues(:)
    logical :: hasConfigFile

    ! find component
    comp => this % getComponent(compName)

    ! is the option defined?
    hasOpt = comp % hasOption(key)
    if (hasOpt) call raiseOptionAlreadyDefinedError(key, compName)

    nargsIn = [1] ! default value
    if (present(nargs)) nargsIn = nargs

    allocate(newOption)
    call newOption % initialize(key, nargsIn, description, default, min, max, choices)

    hasConfigFile = this % hasCfgFile()

    if (hasConfigFile) then
      ! value in config file?
      hasOptionInCfgFile = this % cfgFile % existsOption(compName, key)
      if (hasOptionInCfgFile) then ! yes
        call this % cfgFile % getOption(compName, key, optValues) ! load value
        call newOption % setVal(optValues)
      else ! nope
        if (.not. present(default)) then ! set default value 
          call raiseError(__FILE__, "def_int", &
                "No value defined in the configuration file nor default value specified", &
                "Component: "//compName, &
                "Optione: "//key)
        endif
      endif
    endif

    call comp % addOption(key, newOption)
  end subroutine def_int


  subroutine def_real_sp(this, compName, key, description, nargs, default, min, max, choices)
    !< define a new int option
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: compName !< component name
    character(len=*), intent(in) :: key !< option key
    character(len=*), intent(in) :: description !< description info
    integer, intent(in), optional :: nargs(:) !< how many values to expect (def: 1)
    real(kind=sp), intent(in), optional :: default(:) !< default value 
    real(kind=sp), intent(in), optional :: min
    real(kind=sp), intent(in), optional :: max
    real(kind=sp), intent(in), optional :: choices(:)

    character(len=*), parameter :: SNAME = "def_real_sp"
    logical :: hasOpt
    class(option_reals), pointer :: newOption
    class(component_t), pointer :: comp
    integer, allocatable :: nargsIn(:)

    logical :: hasConfigFile
    logical :: hasOptionInCfgFile
    real(kind=sp), allocatable :: optValues(:)

    ! find component
    comp => this % getComponent(compName)
    ! is the option defined?
    hasOpt = comp % hasOption(key)

    if (hasOpt) call raiseOptionAlreadyDefinedError(key, compName)

    nargsIn = [1] ! default value
    if (present(nargs)) nargsIn = nargs

    allocate(newOption)
    call newOption % initialize(key, nargsIn, description, default, min, max, choices)

    hasConfigFile = this % hasCfgFile()

    if (hasConfigFile) then
      ! value in config file?
      hasOptionInCfgFile = this % cfgFile % existsOption(compName, key)
      if (hasOptionInCfgFile) then ! yes
        call this % cfgFile % getOption(compName, key, optValues) ! load value
        call newOption % setVal(optValues)
      else ! nope
        if (.not. present(default)) then ! rely on default value
          call raiseError(__FILE__, SNAME, &
                "No value defined in the configuration file nor default value specified", &
                "Component: "//compName, &
                "Option: "//key)
        endif
      endif
    endif

    call comp % addOption(key, newOption)
  end subroutine def_real_sp


  subroutine def_files(this, compName, key, description, nargs, default, choices)
    !< define a new int option
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: compName !< component name
    character(len=*), intent(in) :: key !< option key
    character(len=*), intent(in) :: description !< description info
    integer, intent(in), optional :: nargs(:) !< how many values to expect (def: 1)
    type(string), intent(in), optional :: default(:) !< default value 
    type(string), intent(in), optional :: choices(:)

    character(len=*), parameter :: SNAME = "def_files"
    logical :: hasOpt
    class(option_files), pointer :: newOption
    class(component_t), pointer :: comp
    integer, allocatable :: nargsIn(:)

    logical :: hasOptionInCfgFile
    logical :: hasConfigFile
    type(string), allocatable :: optFiles(:)

    ! find component
    comp => this % getComponent(compName)

    ! is the option defined?
    hasOpt = comp % hasOption(key)
    if (hasOpt) call raiseOptionAlreadyDefinedError(key, compName)

    nargsIn = [1] ! default value
    if (present(nargs)) nargsIn = nargs

    allocate(newOption)
    call newOption % initialize(key, nargsIn, description, default, choices)

    ! value in config file?
    hasConfigFile = this % hasCfgFile()

    if (hasConfigFile) then
      hasOptionInCfgFile = this % cfgFile % existsOption(compName, key)
      if (hasOptionInCfgFile) then ! yes
        call this % cfgFile % getOption(compName, key, optFiles) ! load value
        call newOption % setVal(optFiles)
      else ! nope
        ! rely on default value
        if (.not. present(default)) then 
          call raiseError(__FILE__, SNAME, &
                "No value defined in the configuration file nor default value specified", &
                "Component: "//compName, &
                "Option: "//key)
        endif
      endif
    endif

    call comp % addOption(key, newOption)
  end subroutine def_files


  subroutine def_str(this, compName, key, description, nargs, default, choices)
    !< define a new int option
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: compName !< component name
    character(len=*), intent(in) :: key !< option key
    character(len=*), intent(in) :: description !< description info
    integer, intent(in), optional :: nargs(:) !< how many values to expect (def: 1)
    type(string), intent(in), optional :: default(:) !< default value 
    type(string), intent(in), optional :: choices(:)

    logical :: hasOpt
    class(option_str), pointer :: newOption
    class(component_t), pointer :: comp
    integer, allocatable :: nargsIn(:)

    logical :: hasConfigFile
    logical :: hasOptionInCfgFile
    type(string), allocatable :: optStrs(:)

    ! find component
    comp => this % getComponent(compName)
    ! is the option defined?
    hasOpt = comp % hasOption(key)

    if (hasOpt) call raiseOptionAlreadyDefinedError(key, compName)

    nargsIn = [1] ! default value
    if (present(nargs)) nargsIn = nargs

    allocate(newOption)
    call newOption % initialize(key, nargsIn, description, default, choices)

    hasConfigFile = this % hasCfgFile()

    if (hasConfigFile) then
      ! value in config file?
      hasOptionInCfgFile = this % cfgFile % existsOption(compName, key)
      if (hasOptionInCfgFile) then ! yes
        call this % cfgFile % getOption(compName, key, optStrs) ! load value
        call newOption % setVal(optStrs)
      else ! nope
        if (.not. present(default)) then ! rely on default value
          call raiseError(__FILE__, "def_str", &
                "No value defined in the configuration file nor default value specified", &
                "Component: "//compName, &
                "Option: "//key)
        endif
      endif
    endif
    call comp % addOption(key, newOption)
  end subroutine def_str


  subroutine def_datetimes(this, compName, key, description, nargs, default, min, max, choices)
    !< define a new int option
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: compName !< component name
    character(len=*), intent(in) :: key !< option key
    character(len=*), intent(in) :: description !< description info
    integer, intent(in), optional :: nargs(:) !< how many values to expect (def: 1)
    type(datetime), intent(in), optional :: default(:) !< default value 
    type(datetime), intent(in), optional :: min
    type(datetime), intent(in), optional :: max
    type(datetime), intent(in), optional :: choices(:) 

    logical :: hasOpt
    class(option_datetimes), pointer :: newOption
    class(component_t), pointer :: comp
    integer, allocatable :: nargsIn(:)

    logical :: hasConfigFile
    logical :: hasOptionInCfgFile
    type(datetime), allocatable :: optDts(:)

!    write(*,*) "cfgOptions_mod:: def_datetimes:: comp, key =", compName, key
    ! find component
    comp => this % getComponent(compName)

    hasOpt = comp % hasOption(key)
    if (hasOpt) call raiseOptionAlreadyDefinedError(key, compName)

    nargsIn = [1] ! default value
    if (present(nargs)) nargsIn = nargs

    allocate(newOption)
    call newOption % initialize(key, nargsIn, description, default, min, max, choices)

    ! value in config file?
    hasConfigFile = this % hasCfgFile()
!    write(*,*) "cfgOptions_mod:: def_datetimes:: has config file?", hasConfigFile

    if (hasConfigFile) then
      hasOptionInCfgFile = this % cfgFile % existsOption(compName, key)
!      write(*,*) "cfgOptions_mod:: def_datetimes:: has option in config file?", hasOptionInCfgFile
      if (hasOptionInCfgFile) then ! yes
        call this % cfgFile % getOption(compName, key, optDts) ! load value
!        write(*,*) "cfgOptions_mod:: def_datetimes:: setting value ", optDts(1) % toString()
        call newOption % setVal(optDts)
      else ! nope
        if (.not. present(default)) then ! rely on default value
          call raiseError(__FILE__, "def_str", &
                "No value defined in the configuration file nor default value specified", &
                "Component: "//compName, &
                "Option: "//key)
        endif
      endif
    endif
    call comp % addOption(key, newOption)
  end subroutine def_datetimes


  subroutine def_timedeltas(this, compName, key, description, nargs, default, min, max, choices)
    !< define a new int option
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: compName !< component name
    character(len=*), intent(in) :: key !< option key
    character(len=*), intent(in) :: description !< description info
    integer, intent(in), optional :: nargs(:) !< how many values to expect (def: 1)
    type(timedelta), intent(in), optional :: default(:) !< default value 
    type(timedelta), intent(in), optional :: min
    type(timedelta), intent(in), optional :: max
    type(timedelta), intent(in), optional :: choices(:) 

    character(len=*), parameter :: SNAME = "def_timedeltas"
    logical :: hasOpt
    class(option_timedeltas), pointer :: newOption
    class(component_t), pointer :: comp
    integer, allocatable :: nargsIn(:)

    logical :: hasOptionInCfgFile
    logical :: hasConfigFile
    type(timedelta), allocatable :: optTds(:)

    comp => this % getComponent(compName)

    ! find component
    hasOpt = comp % hasOption(key)
    if (hasOpt) call raiseOptionAlreadyDefinedError(key, compName)

    nargsIn = [1] ! default value
    if (present(nargs)) nargsIn = nargs

    allocate(newOption)
    call newOption % initialize(key, nargsIn, description, default, min, max, choices)

    hasConfigFile = this % hasCfgFile()

    ! value in config file?
    if (hasConfigFile) then
      hasOptionInCfgFile = this % cfgFile % existsOption(compName, key)
      if (hasOptionInCfgFile) then ! yes
        call this % cfgFile % getOption(compName, key, optTds) ! load value
        call newOption % setVal(optTds)
       
      else ! nope
        if (.not. present(default)) then ! rely on default value
          call raiseError(__FILE__, SNAME, &
                "No value defined in the configuration file nor default value specified", &
                "Component: "//compName, &
                "Option: "//key)
        endif
      endif
    endif

    call comp % addOption(key, newOption)
  end subroutine def_timedeltas


  subroutine addOption(this, key, newOption)
    !< add new option into the component
    class(component_t), intent(inout) :: this
    character(len=*), intent(in) :: key
    class(option_abs), pointer, intent(in) :: newOption

    logical :: hasOption
    class(*), pointer :: wrap
    hasOption = this % options % hasKey(key)

    if (hasOption) then
      call raiseError(__FILE__, "addOption", &
              "Option already exists in the component", &
              "Component: "//this % name, &
              "Option: "//key)
    endif

    wrap => newOption
    call this % options % insert(key, wrap)
  end subroutine addOption


  function getOption(this, key) result (option)
    !< get option with the key 
    class(component_t), intent(inout) :: this
    character(len=*), intent(in) :: key
    class(option_abs), pointer :: option !< output
    class(*), pointer :: wrap

    wrap => this % options % get(key)
    select type (wrap)
    class is (option_abs)
      option => wrap
    end select
  end function getOption 


  function getName(this) result (name)
    !< returns the component name
    class(component_t), intent(in) :: this
    character(len=:), allocatable :: name !< output
    name = this % name
  end function getName


  subroutine finalize(this)
    !< define a new option
    type(cfgOptions_t), intent(inout) :: this

    if (allocated(this % cfgFile)) deallocate(this % cfgFile)
    if (allocated(this % components)) deallocate(this % components)
  end subroutine finalize


  subroutine get_reals(this, comp, key, values)
    !< it returns the assign 'key' to 'values' from the compoment 'comp'
    !< 
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: comp !< component name
    character(len=*), intent(in) :: key !< component name
    real(kind=sp), allocatable , intent(out) :: values(:)

    class(component_t), pointer :: c

    c => this % getComponent(comp)
    select type (opt => c % getOption(key))
    type is (option_reals)
      call opt % getVal(values)
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_reals")
    end select

  end subroutine get_reals


  subroutine get_ints(this, comp, key, values)
    !< it returns all 'values' from the 'key' which belongs to the compomenent 'comp'
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: comp !< component name
    character(len=*), intent(in) :: key !< component name
    integer, allocatable , intent(out) :: values(:)

    class(component_t), pointer :: c

    c => this % getComponent(comp)
    select type (opt => c % getOption(key))
    type is (option_ints)
      call opt % getVal(values)
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_ints")
    end select

  end subroutine get_ints


  subroutine get_files(this, comp, key, files)
    !< it returns all 'files' from the 'key' which belongs to the compomenent 'comp'
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: comp !< component name
    character(len=*), intent(in) :: key !< component name
    type(file_t), allocatable , intent(out) :: files(:)

    class(component_t), pointer :: c

    c => this % getComponent(comp)
    select type (opt => c % getOption(key))
    type is (option_files)
      call opt % getVal(files)
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_files")
    end select

  end subroutine get_files


  subroutine get_timedelta(this, name, key, tds)
    !< it returns the value of a given key a new option
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: key
    type(timedelta), allocatable, intent(out) :: tds(:)

    class(component_t), pointer :: c

    c => this % getComponent(name)
    select type (opt => c % getOption(key))
    type is (option_timedeltas)
      call opt % getVal(tds)
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_timedeltas")
    end select
  end subroutine get_timedelta


  subroutine get_datetime(this, name, key, dts)
    !< it returns the value of a given key a new option
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: key
    type(datetime), allocatable, intent(out) :: dts(:)

    class(component_t), pointer :: c

    c => this % getComponent(name)
    select type (opt => c % getOption(key))
    type is (option_datetimes)
      call opt % getVal(dts)
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_deltatimes")
    end select
  end subroutine get_datetime


  subroutine get_strings(this, name,  key, strs)
    !< it returns an array of strings by givin the component name
    !< and its key
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: name !< component name where it belongs
    character(len=*), intent(in) :: key !< 
    type(string), allocatable, intent(out) :: strs(:)

    class(component_t), pointer :: c

    c => this % getComponent(name)
    select type (opt => c % getOption(key))
    type is (option_str)
      call opt % getVal(strs)
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_str")
    end select
  end subroutine get_strings


  subroutine get_bool(this, name, key, value)
    !< it returns an array of strings by givin the component name
    !< and its key
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: name !< component name where it belongs
    character(len=*), intent(in) :: key !< 
    logical, intent(out) :: value

    logical, allocatable :: values(:)

    call this % get_many_bools(name, key, values)
    value = values(1)
  end subroutine get_bool


  subroutine get_many_bools(this, name, key, values)
    !< it returns an array of strings by givin the component name
    !< and its key
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: name !< component name where it belongs
    character(len=*), intent(in) :: key !< 
    logical, allocatable, intent(out) :: values(:)

    class(component_t), pointer :: c

    c => this % getComponent(name)
    select type (opt => c % getOption(key))
    type is (option_bools)
      call opt % getVal(values)
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_bools")
    end select

  end subroutine get_many_bools


  function toString_cfgOptions(this, componentName) result (info)
    !< it returns a string showing all components options defined and its information
    !< in case componentName is specified it only shows the option from it
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in), optional :: componentName !< chose component 
    character(len=:), allocatable :: info !< output

    type(string), allocatable :: allComponents(:)
    type(string) :: compName
    class(component_t), pointer :: component
    integer :: iComp

    if (present(componentName)) then ! show only component 
      component => this % getComponent(componentName)
      info = component % toString()
    else ! show all components
      info = ""
      allComponents = this % getAllComponentNames()
      do iComp = 1, size(allComponents)
        compName = allComponents(icomp)
        component => this % getComponent(compName % toString())
        info = info // new_line('A')
        info = info // "============ " // component % getName() // " ============" // new_line('A') // new_line('A')
        info = info // "key (type, number of arguments): description" // new_line('A')
        info = info // component % toString() 
      enddo
    endif

  end function toString_cfgOptions


  subroutine set_reals(this, componentName, key, newValues) 
    !< Define a new value to the option identified with 'key'
    !< which belongs to the component 'componentName'
    !< with the given values from 'newValues'
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: componentName !< component 
    character(len=*), intent(in) :: key !< option key
    real(kind=sp), intent(in) :: newValues(:)

    class(component_t), pointer :: c
    class(option_reals), pointer :: optReals

    c => this % getComponent(componentName)
    select type (opt => c % getOption(key))
    type is (option_reals)
      optReals => opt
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_reals")
    end select

    call optReals % setVal(newValues)
  end subroutine set_reals


  subroutine set_ints(this, componentName, key, newValues) 
    !< Define a new value to the option identified with 'key'
    !< which belongs to the component 'componentName'
    !< with the given values from 'newValues'
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: componentName !< component 
    character(len=*), intent(in) :: key !< option key
    integer, intent(in) :: newValues(:)

    class(component_t), pointer :: c
    class(option_ints), pointer :: optInts

    c => this % getComponent(componentName)
    select type (opt => c % getOption(key))
    type is (option_ints)
      optInts => opt
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_ints")
    end select

    call optInts % setVal(newValues)

  end subroutine set_ints


  subroutine set_strings(this, componentName, key, newValues) 
    !< Define a new value to the option identified with 'key'
    !< which belongs to the component 'componentName'
    !< with the given values from 'newValues'
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: componentName !< component 
    character(len=*), intent(in) :: key !< option key
    type(string), intent(in) :: newValues(:)

    class(component_t), pointer :: c
    class(option_str), pointer :: optStrs

    c => this % getComponent(componentName)
    select type (opt => c % getOption(key))
    type is (option_str)
      optStrs => opt
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_str")
    end select

    call optStrs % setVal(newValues)
  end subroutine set_strings


  subroutine set_files(this, componentName, key, newFiles) 
    !< Define a new value to the option identified with 'key'
    !< which belongs to the component 'componentName'
    !< with the given values from 'newValues'
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: componentName !< component 
    character(len=*), intent(in) :: key !< option key
    type(file_t), intent(in) :: newFiles(:)

    class(component_t), pointer :: c
    class(option_files), pointer :: optFiles

    c => this % getComponent(componentName)
    select type (opt => c % getOption(key))
    type is (option_files)
      optFiles => opt
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_files")
    end select

    call optFiles % setVal(newFiles)
  end subroutine set_files


  subroutine set_datetimes(this, componentName, key, newValues) 
    !< Define a new value to the option identified with 'key'
    !< which belongs to the component 'componentName'
    !< with the given values from 'newValues'
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: componentName !< component 
    character(len=*), intent(in) :: key !< option key
    type(datetime), intent(in) :: newValues(:)

    class(component_t), pointer :: c
    class(option_datetimes), pointer :: optDatetimes

    c => this % getComponent(componentName)
    select type (opt => c % getOption(key))
    type is (option_datetimes)
      optDatetimes => opt
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_datetimes")
    end select

    call optDatetimes % setVal(newValues)
  end subroutine set_datetimes


  subroutine set_timedeltas(this, componentName, key, newValues) 
    !< Define a new value to the option identified with 'key'
    !< which belongs to the component 'componentName'
    !< with the given values from 'newValues'
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: componentName !< component 
    character(len=*), intent(in) :: key !< option key
    type(timedelta), intent(in) :: newValues(:)

    class(component_t), pointer :: c
    class(option_timedeltas), pointer :: optTimedeltas

    c => this % getComponent(componentName)
    select type (opt => c % getOption(key))
    type is (option_timedeltas)
      optTimedeltas => opt
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_timedeltas")
    end select

    call optTimedeltas % setVal(newValues)

  end subroutine set_timedeltas


  subroutine set_bools(this, componentName, key, newValues) 
    !< Define a new value to the option identified with 'key'
    !< which belongs to the component 'componentName'
    !< with the given values from 'newValues'
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: componentName !< component 
    character(len=*), intent(in) :: key !< option key
    logical, intent(in) :: newValues(:)

    class(component_t), pointer :: c
    class(option_bools), pointer :: optBools

    c => this % getComponent(componentName)
    select type (opt => c % getOption(key))
    type is (option_bools)
      optBools => opt
    class default
      call raiseUnexpectedOptionTypeError(opt, "option_bools")
    end select

    call optBools % setVal(newValues)

  end subroutine set_bools


  function getAllComponentNames(this) result (r)
    !< it returns an allocatable string array with all the name
    !< of the components introduced
    class(cfgOptions_t), intent(inout) :: this
    type(string), allocatable :: r(:)

    if (allocated(r)) deallocate(r)
    r = this % components % getAllKeys()
  end function getAllComponentNames


  function getAllOptionsKeys_cfgOptions(this, cName) result (r)
    !< it returns an allocatable string array with all the options name
    !< which belongs to the given 'component'
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: cName !< component name
    type(string), allocatable :: r(:) !< output

    class(*), pointer :: comp

    comp => this % components % get(cName)
    select type(comp)
    type is (component_t)
      r = comp % getAllOptionsKeys()
    class default
      !< unexpected outcome
    end select
  end function getAllOptionskeys_cfgOptions


  logical function hasCfgFile(this)
    !< true if a config file is used
    class(cfgOptions_t), intent(inout) :: this
    hasCfgFile = allocated(this % cfgFile)
  end function hasCfgFile


  function getAllOptionsKeys_component(this) result (r)
    !< it returns an allocatable string array with all the options name
    !< which belongs to the given 'component'
    class(component_t), intent(inout) :: this
    type(string), allocatable :: r(:) !< output

    if (allocated(r)) deallocate(r)
    r = this % options % getAllKeys()
  end function getAllOptionsKeys_component


  integer function getOptionType(this, cName, key) 
    !< it returns the type of the requested type 
    class(cfgOptions_t), intent(inout) :: this
    character(len=*), intent(in) :: cName !< component name
    character(len=*), intent(in) :: key !< option key 

    class(component_t), pointer :: c

    c => this % getComponent(cName)
    select type (opt => c % getOption(key))
    type is (option_datetimes)
      getOptionType = CFG_DATETIMES
    type is (option_ints)
      getOptionType = CFG_INTS
    type is (option_reals)
      getOptionType = CFG_REALS
    type is (option_timedeltas)
      getOptionType = CFG_TIMEDELTAS
    type is (option_str)
      getOptionType = CFG_STR
    type is (option_files)
      getOptionType = CFG_FILES
    type is (option_bools)
      getOptionType = CFG_BOOLS
    class default
      call raiseUnexpectedOptionTypeError(opt, "-") 
    end select

  end function getOptionType

end module SHR_cfgOptions_mod
