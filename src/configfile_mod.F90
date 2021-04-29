!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : SHR_configFile_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> It reads a configuration file with the following format:
!>  [section name]
!>  key=value
!>  key=value1, value2, value3
!>  anotherkey = value5
!>
!>  https://www.docuxplorer.com/WebHelp/INI_File_Format.htm
!>
!>  How it works in tst:
!>
!>  addSection (vegetation)
!>     tst -> "vegetation" => section(name) 
!>  addKey (vegetation,c3,value)
!>     tst -> "vegetation_c3" => option(key,value)
!>  addKey (vegetation,pft,value)
!>     tst -> "vegetation_pft" => option(key,value)
!>
!> It currently supports: string, integer and real (scalar and array)
!>
!> TODO list: include datetime, timedelta, boolean and file
!>
!------------------------------------------------------------------------------

module SHR_configFile_mod

  use SHR_configParser_mod, only: configParser
  use SHR_strings_mod, only: string, is_numeric, str2real, isInteger, str2int, &
          stringCollection, sanitize, countSubstring
  use SHR_tst_mod, only: tst
  use SHR_datetime_mod, only: datetime, timedelta, strptime
  use SHR_error_mod, only: raiseError, raiseUnexpectedError

  implicit none

  private

  public :: configFile

  type, abstract :: Option
    character(len=:), allocatable :: key
    character(len=:), allocatable :: sectionKey !< the option is aware of which section belongs
!  contains
!    procedure :: getInternalKey
  end type Option


  ! arrays
  type, extends(Option) :: OptInteger
       integer, allocatable :: val(:)
  end type OptInteger

  type, extends(Option) :: OptFloat
       real, allocatable :: val(:)
  end type OptFloat

  type, extends(Option) :: OptString
       type(string), allocatable :: val(:)
  end type OptString


  type :: section
      character(len=:), allocatable :: name
      type(tst) :: options
  contains
      procedure :: addOption
      procedure :: getOption => getOption_section
      procedure :: existsOption => existsOption_section
  end type section


  type configFile
      character(len=:), allocatable :: filename 
      type(tst) :: sections
      type(configParser) :: parser
  contains
      ! public
      procedure :: init !< initialize the options with empty or a filename
!      procedure :: write !< into file 
!      procedure :: read !< read from file
      procedure :: addSection !< add a new section
      procedure :: getSection
      procedure :: existsSection !< 
      procedure :: existsOption

      procedure :: getAllOptionsName

      procedure :: addOption_ss, addOption_sr
      procedure :: addOption_is, addOption_ir
      procedure :: addOption_rs, addOption_rr
      generic :: addOption => addOption_ss, addOption_sr, &
                              addOption_is, addOption_ir, &
                              addOption_rs, addOption_rr

      procedure :: getOption_ss, getOption_sr
      procedure :: getOption_is, getOption_ir
      procedure :: getOption_rs, getOption_rr
      procedure :: getOption_bs, getOption_br
      procedure :: getOption_ds, getOption_dr
      procedure :: getOption_ts, getOption_tr
      generic :: getOption => getOption_ss, getOption_sr, &
                              getOption_is, getOption_ir, &
                              getOption_rs, getOption_rr, &
                              getOption_bs, getOption_br, &
                              getOption_ds, getOption_dr, &
                              getOption_ts, getOption_tr


      ! private
      procedure, private :: readAllLines !< read config from file
      procedure, private :: build !< from AST to options
  end type configFile


  contains


  subroutine init(self, filename)
    !< initialize configFile
    class(configFile), intent(inout) :: self

    character(len=*), intent(in) :: filename

    character(len=:), allocatable :: readText
    logical :: isFileExists

    self % filename = trim(adjustl(filename))

    ! does the file exists?
    inquire(file=self % filename, exist=isFileExists)
    if (.not. isFileExists) then ! file exists?
      call raiseError(__FILE__, "init", &
              "Requested cfg file not found", &
              "File '"//self % filename//"'")
    endif

    readText = self % readAllLines()
    call self % parser % init(readText)
    call self % parser % run()
    call self % build()

  end subroutine init


  subroutine build(self)
    !< build config tree
    class(configFile), intent(inout) :: self

    type(string), allocatable :: allkeys(:)

    integer :: i
    character(len=:), allocatable :: sectionName 
    character(len=:), allocatable :: inKey !< internal key
    character(len=:), allocatable :: key !< config file key
    type(string), allocatable :: values(:)
    real, allocatable :: rvalues(:)
    integer, allocatable :: ivalues(:)
    integer, allocatable :: stats(:)
    class(*), pointer :: wrap

!    write(*,*) "configfile_mod:: build: entering... "

    ! iterate all over options found in the file
    allkeys = self % parser % getAllOptionsKeys()

    ! for east AST found
    do i = 1, size(allkeys)
      inKey = allkeys(i) % val ! current internal key

      ! retrieve info about option
      key = self % parser % getOptionKey(inKey)
      values = self % parser % getOptionValues(inKey)
      sectionName = self % parser % getOptionSection(inKey)

      write(*,*) "configfile_mod:: build:: key, nvalues= ", key, size(values), values(1) % toString()

      ! discover its type
      if (all(is_numeric(values))) then ! is a number

        if (all(isInteger(values))) then ! is integer

          call str2int(values, ivalues, stats)
          write(*,*) "configfile_mod:: build:: key= ", key,  ", inkey=", inkey, ", section=", sectionName, " is integer", ivalues
          call self % addOption(sectionName, key, ivalues)

        else ! is real
          call str2real(values, rvalues)
          write(*,*) "configfile_mod:: build:: key= ", key,  ", inkey=", inkey, ", section=", sectionName, " is real", rvalues
          call self % addOption(sectionName, key, rvalues)
        endif
      else !  is a string

        write(*,*) "configfile_mod:: build:: key= ", key,  ", inkey=", inkey, ", section=", sectionName
        write(*,*) "configfile_mod:: build:: values= ", values(1) % toString()
        call self % addOption(sectionName, key, values)
      endif
      
    enddo

!    write(*,*) "configfile_mod:: build: ...end"

  end subroutine build


  function readAllLines(self) result(lines)
    !< read all lines from file and returns a character
    class(configFile), intent(inout) :: self

    character(len=:), allocatable :: lines
    character(len=1000) :: line
    integer, parameter :: read_unit = 999
    integer :: ios, n 

    open(unit=read_unit, file=self % filename, iostat=ios)
    if ( ios /= 0 ) call raiseError(__FILE__, "readAllLines", &
            "Error opening file "//self % filename)

    n = 0
    lines = ""

    ! remove comments and merge lines
    do
        read(read_unit, '(A)', iostat=ios) line       
        if (ios /= 0) exit
        line = adjustl(trim(line))
        !write(*,*) "readAllLines:: ", trim(line)
        lines = lines // adjustl(trim(line)) // new_line('A')
    end do

    !write(*,*) "readAllLines:: all lines= ", adjustl(trim(lines))

    close(read_unit)

  end function readAllLines


  subroutine addSection(self, sectionName)
    !< Add a new section. If it exists it raises an error. 
    !< The new section is added as "sectionName"
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName

    class(*), pointer :: wrap
    type(section), pointer :: newSection

    if (self % sections % search(sectionName)) then
       call raiseError(__FILE__, "addSection", &
            "Section '"//sectionName//"' already exists")
    endif

    allocate(newSection)
    newSection % name = sectionName
    wrap => newSection

    call self % sections % insert(sectionName, wrap)

  end subroutine addSection


  ! str, real, int(scalar, array)
  ! char scalar
  subroutine addOption_ss(self, sectionName, optionName, val)
    !< Add a new option. If it exists it replaces its value.
    !< If the section does not exists, it's added.
    !< The new option is added as "sectionName_optionName" => val
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: optionName
    character(len=*), intent(in) :: val

    type(string), allocatable :: valwrap(:)

    allocate(valwrap(1))
    valwrap(1) = val

    call self % addOption_sr(sectionName, optionName, valwrap)

  end subroutine addOption_ss

  ! string array
  subroutine addOption_sr(self, sectionName, optionName, val)
    !< Add a new option. If it exists it replaces its value.
    !< If the section does not exists, it's added.
    !< The new option is added as "sectionName_optionName" => val
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: optionName
    type(string), allocatable, intent(in) :: val(:)

    character(len=:), allocatable :: internOptName 
    class(optString), pointer :: newOption
    class(section), pointer :: ptrSection

    type(stringCollection) :: sc

    ! does the section exists?
    if (.not. self % existsSection(sectionName)) then
       call self % addSection(sectionName)
    endif

    ptrSection => self % getSection(sectionName)

    allocate(newOption)
    newOption % key = optionName
    newOption % sectionKey = sectionName
    newOption % val = val

    sc = stringCollection(val)
    write(*,*) "configfile_mod:: addOption_sr:: section, key, values=", &
                sectionName, ", ", optionName, ", ", sc % toString()
    call ptrSection % addOption(optionName, newOption)

  end subroutine addOption_sr

  ! integer array
  subroutine addOption_ir(self, sectionName, optionName, val)
    !< Add a new option. If it exists it replaces its value.
    !< If the section does not exists, it's added.
    !< The new option is added as "sectionName_optionName" => val
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: optionName
    integer, allocatable, intent(in) :: val(:)

    character(len=:), allocatable :: internOptName 
    class(optInteger), pointer :: newOption
    class(section), pointer :: ptrSection

    ! does the section exists?
    if (.not. self % existsSection(sectionName)) then
       call self % addSection(sectionName)
    endif

    ptrSection => self % getSection(sectionName)

    allocate(newOption)
    newOption % key = optionName
    newOption % sectionKey = sectionName
    newOption % val = val

    write(*,*) "configfile_mod:: addOption_ir:: section, key, val=", sectionName, optionName, val
    call ptrSection % addOption(optionName, newOption)

  end subroutine addOption_ir

  ! integer scalar
  subroutine addOption_is(self, sectionName, optionName, val)
    !< Add a new option. If it exists it replaces its value.
    !< If the section does not exists, it's added.
    !< The new option is added as "sectionName_optionName" => val
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: optionName
    integer, intent(in) :: val

    integer, allocatable :: internOpt(:) ! encapsulate

    allocate(internOpt(1))
    internOpt(1) = val
    call self % addOption_ir(sectionName, optionName, internOpt )

  end subroutine addOption_is

  ! real array
  subroutine addOption_rr(self, sectionName, optionName, val)
    !< Add a new option. If it exists it replaces its value.
    !< If the section does not exists, it's added.
    !< The new option is added as "sectionName_optionName" => val
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: optionName
    real, allocatable, intent(in) :: val(:)

    character(len=:), allocatable :: internOptName 
    class(optFloat), pointer :: newOption
    class(section), pointer :: ptrSection

    ! does the section exists?
    if (.not. self % existsSection(sectionName)) then
       call self % addSection(sectionName)
    endif

    ptrSection => self % getSection(sectionName)

    allocate(newOption)
    newOption % key = optionName
    newOption % sectionKey = sectionName
    newOption % val = val

    write(*,*) "configfile_mod:: addOption_rr:: section, key, val=", &
            sectionName, ", "  , optionName, ", ", val
    call ptrSection % addOption(optionName, newOption)

  end subroutine addOption_rr

  ! real scalar 
  subroutine addOption_rs(self, sectionName, optionName, val)
    !< Add a new option. If it exists it replaces its value.
    !< If the section does not exists, it's added.
    !< The new option is added as "sectionName_optionName" => val
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: optionName
    real, intent(in) :: val

    real, allocatable :: valwrap(:)

    allocate(valwrap(1))
    valwrap(1) = val

    call self % addOption_rr(sectionName, optionName, valwrap)

  end subroutine addOption_rs


  logical function existsSection(self, sectionName)
    !< returns true if the section exists otherwise false
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName

    class(*), pointer :: wrap
    type(section), pointer :: newSection

    existsSection = .false. 

    ! does the section exists?
    if (self % sections % search(sectionName)) then ! yes
      ! let's ensure it's a 'section' type
      wrap => self % sections % get (sectionName)
      select type(wrap)
      type is (section)
        existsSection = .true.
      class default 
        existsSection = .false.
      end select
    endif
    
  end function existsSection


  logical function existsOption(self, sectionName, optionName)
    !< returns true if the option is found in the section
    !<         false if the option is not found in the section
    !<         false if the section does not exists
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: optionName

    class(*), pointer :: wrap
    type(section), pointer :: newSection
    logical :: hasSection
    type(optString) :: tmpOption
    class(section), pointer :: ptrSection

    if ( .not. self % existsSection(sectionName) ) then
      existsOption = .false.
      write(*,*) "configfile_mod:: existsOption:: section exists? nope"
      return
    endif

    ptrSection => self % getSection(sectionName)
    existsOption = ptrSection % existsOption(optionName)
    
  end function existsOption


  !< scalar char
  subroutine getOption_ss(self, sectionName, key, sstrout)
    !< Get scalar value of the key 'key'
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: key 

    character(len=:), allocatable, intent(out) :: sstrout
    type(string), allocatable :: values(:)

    call self % getOption_sr(sectionName, key, values)
    sstrout = values(1) % val
  end subroutine getOption_ss

  !< array strings
  subroutine getOption_sr(self, sectionName, key, rstrout)
    !< Get scalar value of the key 'key'
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: key 
    type(optString) :: newOption
    !type(string), allocatable :: allkeys(:)

    type(string), allocatable, intent(out) :: rstrout(:)
    class(section), pointer :: ptrSection
    class(option), pointer :: ptrOption
      
    ptrSection => self % getSection(sectionName)
    ptrOption => ptrSection % getOption(key)

    select type(ptrOption)
    class is (optString)
      !write(*,*) "hello, allocated(val)?", allocated(wrap % val)
      rstrout = ptrOption % val
    class default
      call raiseError(__FILE__, "getOption_sr", &
             "Unexpected type found instead of string(s)")
    end select
  end subroutine getOption_sr

  !< scalar integer
  subroutine getOption_is(self, sectionName, key, intout)
    !< Get scalar value of the key 'key'
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: key 
    integer, intent(out) :: intout

    integer, allocatable :: ivalues(:)

    call self % getOption_ir(sectionName, key, ivalues)
    !write(*,*) "getOption_is:: allocated(ivalues)=", allocated(ivalues)
    !write(*,*) "getOption_is:: ivalues=", ivalues
    intout = ivalues(1)
  end subroutine getOption_is

  !< array integer
  subroutine getOption_ir(self, sectionName, key, rinout)
    !< Get scalar value of the key 'key'
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: key 
    integer, allocatable, intent(out) :: rinout(:)

    class(section), pointer :: ptrSection
    class(option), pointer :: ptrOption

    ptrSection => self % getSection(sectionName) 
    ptrOption => ptrSection % getOption(key)
   
    select type(ptrOption) 
    type is (optInteger)
      rinout = ptrOption % val
    class default
      call raiseError(__FILE__, "getOption_ir", &
             "Unexpected type found, but expected optInteger", &
             "Section: "//sectionName, &
             "Key: "//key)
    end select
  end subroutine getOption_ir

  !< scalar real 
  subroutine getOption_rs(self, sectionName, key, realout)
    !< Get scalar value of the key 'key'
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: key 
    real, intent(out) :: realout
    real, allocatable :: rvalues(:)

    call self % getOption_rr(sectionName, key, rvalues)
    realout = rvalues(1) 
  end subroutine getOption_rs

  !< array real 
  subroutine getOption_rr(self, sectionName, key, rrealout)
    !< Get scalar value of the key 'key'
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: key 
    real, allocatable, intent(out) :: rrealout(:)

    class(section), pointer :: ptrSection
    class(option), pointer :: ptrOption

    ptrSection => self % getSection(sectionName)
    ptrOption => ptrSection % getOption(key)
    
    select type(ptrOption) 
    type is (optFloat) !< default
      rrealout = ptrOption % val
    type is (optInteger) !< transform into real
      rrealout = real(ptrOption % val)
    class default
      call raiseError(__FILE__, "getOption_rr", &
             "Unexpected type found instead of optFloat", &
             "Section: "//sectionName, &
             "Key: "//key)
    end select
  end subroutine getOption_rr


  subroutine getOption_bs(self, sectionName, key, boolOut)
    !< returns a boolean (scalar) value from section with the 'key'
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: key 
    logical, intent(out) :: boolOut
    logical, allocatable :: tmp(:)

    call self % getOption_br(sectionName, key, tmp)
    boolOut = tmp(1)
  end subroutine getOption_bs


  subroutine getOption_br(self, sectionName, key, boolsOut)
    !< returns a boolean (array) value from section with the 'key'
    !< it converts the value found as string into boolean
    !< otherwise an error is raised
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: key 
    logical, allocatable, intent(out) :: boolsOut(:)

    integer :: i, nelems
    type(string), allocatable :: strOptions(:)
    character(len=3), parameter :: AVAIL_TRUE(2) = ['  y', 'yes']
    character(len=2), parameter :: AVAIL_FALSE(2) = [' n', 'no']

    type(stringCollection) :: availsTrue
    type(stringCollection) :: availsFalse
    logical :: isFound
    character(len=:), allocatable :: strTrue, strFalse
    type(string), allocatable :: sanitChars(:)

    ! read option as string
    call self % getOption_sr(sectionName, key, strOptions)
    nelems = size(strOptions)

    if (allocated(boolsOut)) deallocate(boolsOut)
    allocate(boolsOut(nelems))

    sanitChars = sanitize(AVAIL_TRUE)
    availsTrue = stringCollection(sanitChars)

    sanitChars = sanitize(AVAIL_FALSE)
    availsFalse = stringCollection(sanitchars)

    do i = 1, nelems
      isFound = .false.

      ! is option a true?
      if (availsTrue % hasString(strOptions(i))) then
        boolsOut(i) = .true.
        isFound = .true.
      endif

      if (.not. isFound) then 
        ! is option a false?
        if ( availsFalse % hasString(strOptions(i))) then
          boolsOut(i) = .false.
          isFound = .true.
        endif
      endif

      if (.not. isFound) then
        strTrue = availsTrue % toString()
        strFalse = availsFalse % toString()
        call raiseError(__FILE__, "getOption_br", &
                "Boolean option cannot be parsed", &
                "Allowed: "// strTrue // " and " // strFalse, &
                "Found: '"//strOptions(i) % toString()//"'")
      endif
    enddo

  end subroutine getOption_br


  subroutine getOption_ds(self, sectionName, key, datetimeOut)
    !< returns a datetime (scalar) value from section with the 'key'
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: key 
    type(datetime), intent(out) :: datetimeOut

    type(datetime), allocatable :: tmp(:)

    call self % getOption_dr(sectionName, key, tmp)
    datetimeOut = tmp(1)
  end subroutine getOption_ds


  subroutine getOption_dr(self, sectionName, key, datetimeOut)
    !< returns a datetime (array) value from section with the 'key'
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: key 
    type(datetime), allocatable, intent(out) :: datetimeOut(:)

    type(string), allocatable :: strOption(:)
    integer :: i, nelems
    character(len=:), allocatable :: SEL_FORMAT  !< selected format
    logical :: hasTime !< true if time format is found

    !< available formats
    character(len=*), parameter :: DATETIME_FORMAT = "%Y/%m/%d %H:%M:%S" !< datetime format
    character(len=*), parameter :: DATE_FORMAT = "%Y/%m/%d" !< date format

    ! read option as string
    call self % getOption_sr(sectionName, key, strOption)
    nelems = size(strOption)

    if (allocated(datetimeOut)) deallocate(datetimeOut)
    allocate(datetimeOut(nelems))

    do i = 1, nelems
      ! has time format? (search for ':' character)
      hasTime = (index(strOption(i) % val, ":") /= 0)
      if (hasTime) then !< yes, has time format
        SEL_FORMAT = DATETIME_FORMAT
      else
        SEL_FORMAT = DATE_FORMAT
      endif

      datetimeOut(i) = strptime(strOption(i) % toString(), SEL_FORMAT)
    enddo
  end subroutine getOption_dr


  subroutine getOption_ts(self, sectionName, key, timedeltaOut)
    !< returns a datetime (scalar) value from section with the 'key'
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: key 
    type(timedelta), intent(out) :: timedeltaOut

    type(timedelta), allocatable :: tmp(:)

    call self % getOption_tr(sectionName, key, tmp)
    timedeltaOut = tmp(1)
  end subroutine getOption_ts


  subroutine getOption_tr(self, sectionName, key, timedeltaOut)
    !< returns a timedelta (array) value from section with the 'key'
    !< It reads the string value from the config files and it is 
    !< parsed according to TIME_FORMAT 
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    character(len=*), intent(in) :: key 
    type(timedelta), allocatable, intent(out) :: timedeltaOut(:)

    type(string), allocatable :: strOption(:)
    integer :: i, nelems
    character(len=*), parameter :: TIME_FORMAT = "%H:%M:%S" !< timedelta format
    type(datetime) :: parsedTime
    type(timedelta) :: tmpTd
    character(len=:), allocatable :: tmp

    ! read option as string
    call self % getOption_sr(sectionName, key, strOption)
    nelems = size(strOption)

    if (allocated(timedeltaOut)) deallocate(timedeltaOut)
    allocate(timedeltaOut(nelems))

    do i = 1, nelems
      tmp = strOption(i) % toString()
      !< format correct? loose check
      if (countSubstring(tmp, ":") /= 2) then
        call raiseError(__FILE__, "getOption_tr", &
                "Unexpected timdelta format found", &
                "Found: "//tmp, &
                "Expected format '"//TIME_FORMAT//"'")
      endif

      parsedTime = strptime(strOption(i) % toString(), TIME_FORMAT)
      tmpTd = timedelta( hours = parsedTime % getHour(), &
                         minutes = parsedTime % getMinute(), &
                         seconds = parsedTime % getSecond())
      timedeltaOut(i) = tmpTd
    enddo
  end subroutine getOption_tr


  function getAllOptionsName(self, sectionName) result (r)
    !< returns a string array with all options names from 
    !< the component 'compName'
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: sectionName
    type(string), allocatable :: r(:)

    class(section), pointer :: pSection

    pSection => self % getSection(sectionName)


  end function getAllOptionsName


  function getSection(self, name) result (ptrSection)
    !< it returns a pointer of the section
    !< an error is raised if section not found
    class(configFile), intent(inout) :: self
    character(len=*), intent(in) :: name
    class(section), pointer :: ptrSection !< output
 
    class(*), pointer :: ptr

    ptr => self % sections % get(name)
    select type (ptr)
    type is (section)
      ptrSection => ptr
    class default
      call raiseUnexpectedError()
    end select 

  end function getSection


  subroutine addOption(self, key, val)
    !< add new option with key 'key' and value 'val' into current section
    class(section), intent(inout) :: self
    character(len=*), intent(in) :: key
    class(option), pointer, intent(in) :: val

    class(*), pointer :: wrapper

    wrapper => val
    call self % options % insert(key, wrapper)
  end subroutine addOption


  function getOption_section(self, key) result (r)
    !< it returns the 'option' pointer given its key
    !< from a section
    class(section), intent(inout) :: self
    character(len=*), intent(in) :: key
    class(option), pointer :: r

    class(*), pointer :: ptr
    ptr => self % options % get(key)

    select type (ptr)
    class is (option)
      r => ptr
    class default
      call raiseUnexpectedError()
    end select
  end function getOption_section


  logical function existsOption_section(self, key) 
    !< true if the given 'key' is found in the section as an option
    class(section), intent(inout) :: self
    character(len=*), intent(in) :: key

    existsOption_section = self % options % search(key)
  end function existsOption_section

end module SHR_configFile_mod
