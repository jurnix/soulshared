!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : SHR_configParser_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> It parses a configuration file with the following format:
!>  [section name]
!>  key=value
!>  key=value1, value2, value3
!>  anotherkey = value5
!>
!>  https://www.docuxplorer.com/WebHelp/INI_File_Format.htm
!>
!> It builds a dictionary of sections and options.
!> All values are separated if are in lists(,).
!> 
!------------------------------------------------------------------------------

module SHR_configParser_mod

  use SHR_strings_mod, only: string, countsubstring
  use SHR_tst_mod, only: tst
  use SHR_datetime_mod, only: datetime, timedelta
  use SHR_error_mod, only: raiseError

  implicit none

  private

  public :: configParser, parseOptionValues
  public :: isOption, isSection
  public :: parseOption, parseSection
  public :: astnode, astsection, astoption

  !< config files tags
  character(len=1), parameter :: TAG_REAL="." 
  character(len=1), parameter :: TAG_ARRAY="," 
  character(len=1), parameter :: TAG_COMMENT="#" 
  character(len=1), parameter :: TAG_SECTION_START="["
  character(len=1), parameter :: TAG_SECTION_END="]"
  character(len=1), parameter :: TAG_NEXT_LINE="&"
  character(len=1), parameter :: TAG_ASSIGN="="

  type, abstract :: astnode
    character(len=:), allocatable :: raw !< original text
  end type astnode

  type, extends(astnode) :: astsection
    character(len=:), allocatable :: name 
  end type astsection

  type, extends(astnode) :: astoption
    character(len=:), allocatable :: key
    type(string), allocatable :: values(:)
    character(len=:), allocatable :: section
  end type astoption

  type configParser
      character(len=:), allocatable :: text
      type(tst) :: sections !< simple AST
      type(tst) :: options !< simple AST
  contains
      ! public
      procedure :: init !< initialize the options with empty or a filename
      procedure :: run !< main call
      procedure :: getAllOptionsKeys !< all keys in 'options'
      procedure :: getOptionSection !< get the section name from a node
      procedure :: getOptionValues !< get the values of the option
      procedure :: getOptionKey !< get the option of the option 

      ! private
!      procedure, private :: readAllLines !< read config from file
      procedure :: removeComment !< remove the comment from a single line
      procedure :: removeAllComments !< remove all comments from the text
      procedure :: mergeLines !< merges all lines with a next tag included
      procedure :: analyse !< discover each lines type from the file
  end type configParser

  contains


  subroutine init(self, intext)
    !< initialize configFile
    class(configParser), intent(inout) :: self

    character(len=*), intent(in) :: intext

    self % text = intext

  end subroutine init


  subroutine run(self)
    !< parse config file 
    class(configParser), intent(inout) :: self

    character(len=:), allocatable :: allLines

    ! read from file
    !allLines = self % readAllLines()
    !write(*,*) "parse:: readAllLines=", self % text
    ! clean 
    allLines = self % removeAllComments(self % text)
    !write(*,*) "parse:: removeAllComments=", allLines
    allLines = self % mergeLines(allLines)
    !write(*,*) "parse:: mergeLines=", allLines
    ! parse
    call self % analyse(allLines)

  end subroutine run


  subroutine analyse(self, intext)
    !< define section and options
    class(configParser), intent(inout) :: self
    character(len=*), intent(in) :: intext

    character(len=:), allocatable :: newline
    type(string) :: newtext
    character(len=:), allocatable :: currentSectionName
    type(astSection), pointer :: newAstSection
    type(astOption), pointer :: newAstOption

    class(*), pointer :: wrap

    call newtext % init(intext)
    !write(*,*) "analyse:: analyse in..."

    ! iterate line by line 
    do while (.not. newtext % isFinished())
      call newtext % nextLine(newline)

      ! empty line
      if (adjustl(trim(newline)) .eq. "") cycle

      if (isSection(newline)) then
        newAstSection => parseSection(newline)
        ! encapsulate
        wrap => newAstSection
        currentSectionName = newAstSection % name

        ! add into sections
        !write(*,*) "analyse:: insert section '", currentSectionName, "'"
        call self % sections % insert(newAstSection % name, wrap)
      else if (isOption(newline)) then
        newAstOption => parseOption(newline)
        ! encapsulate
        newAstOption % section = currentSectionName
        wrap => newAstOption
        ! add into options
        !write(*,*) "analyse:: insert option '", currentSectionName//"_"//newAstOption % key, "'"
        call self % options % insert(currentSectionName//"_"//newAstOption % key, wrap)
      endif

    end do
    !write(*,*) "analyse:: analyse end"

  end subroutine analyse


  function removeComment(self, instr) result (outstr)
    !> remove comment from the instr if found
    !> otherwise it returns the same
    class(configParser), intent(inout) :: self
    character(len=*) :: instr
    character(len=:), allocatable :: outstr
    integer :: comment_idx

    outstr = instr
    ! restore carry line?
    ! is there a comment in the line?
    comment_idx = index(instr, TAG_COMMENT)
    if ( comment_idx .GT. 0 ) then
      ! yes, lets remove it
      outstr = instr(1:comment_idx-1) 
!      outstr = adjustl(trim(outstr))
    endif
    !write(*,*) "removeComment:: output=", adjustl(trim(outstr))

  end function removeComment


  function removeAllComments(self, intext) result (full_line)
    !< parse config file 
    class(configParser), intent(inout) :: self
    character(len=*) :: intext
    character(len=:), allocatable :: full_line !< output

    character(len=:), allocatable :: newline
    type(string) :: newtext

    full_line = ""

    call newtext % init(intext)
    !write(*,*) "removeAllComments:: entering..."

    ! iterate line by line 
    do while (.not. newtext % isFinished())
      call newtext % nextLine(newline)
      !write(*,*) "removeAllComments:: newline=", newline

      newline = self % removeComment(newline)
      !write(*,*) "removeAllComments:: newline(removed comments)=", newline
      full_line = full_line // newline
      if (.not. newtext % isFinished()) full_line = full_line // new_line('A')
    end do
    !write(*,*) "removeAllComments:: end"

  end function removeAllComments 


  function mergeLines(self, intext) result (outstr)
    !< parse config file 
    class(configParser), intent(inout) :: self
    character(len=*), intent(in) :: intext
    character(len=:), allocatable :: outstr

    logical :: hasNextLine, prevNextLine
    integer :: nextline_idx
    character(len=:), allocatable :: newline, accumline
    type(string) :: newtext

    outstr=""
    hasNextLine = .false.
    prevNextLine = .false.

    call newtext % init(intext)

    ! iterate line by line
    do while (.not. newtext % isFinished())
      call newtext % nextLine(newline)

      !if (trim(newline) .eq. "") then
      !  write(*,*) "mergeLines:: empty line found"
      !  cycle
      !endif

      nextline_idx = index(newline, TAG_NEXT_LINE)
      hasNextLine = nextline_idx .ne. 0
      if (hasNextLine .and. .not. prevNextLine) then ! first occurence
        hasNextLine=.true.
        accumLine = newline(1:nextline_idx-1)
      else if (hasNextLine .and. prevNextLine ) then ! middle occurrences
        accumLine = accumLine // newline(1:nextline_idx-1)
      else if (.not. hasNextLine .and. prevNextLine) then ! last occur.
        outstr= outstr // accumLine // newline 
        if (.not. newtext % isFinished()) outstr= outstr // new_line('A')
      else
        outstr= outstr // newline ! nothing found
        if (.not. newtext % isFinished()) outstr= outstr // new_line('A')
      endif

      prevNextLine = hasNextLine
!      write(*,*) "mergeLines:: newline: '", newline, "'"
    enddo


!    write(*,*) "mergeLines:: outstr= '", outstr, "'"
  end function mergeLines 


  logical function isSection(line)
    !< given a raw line it says whether it defines a section
    !<   [section name here] -> expected format
    character(len=*), intent(in) :: line
    integer :: sec_start_tag, sec_end_tag
    logical :: has_section_tags, has_correct_order

    sec_start_tag = index(line, TAG_SECTION_START)
    sec_end_tag = index(line, TAG_SECTION_END)
    has_section_tags = sec_start_tag .ne. 0 .and. sec_end_tag .ne. 0
    has_correct_order = sec_start_tag < sec_end_tag
    isSection = has_section_tags .and. has_correct_order 
  end function isSection


  function parseSection(line) result (newAstSection)
    !< it returens the name of section given a line 
    !< if it is not detected it raises an error
    character(len=*), intent(in) :: line
    type(astSection), pointer :: newAstSection

    integer :: sec_start_tag, sec_end_tag
    character(len=:), allocatable :: sectionName
!    type(astSection), pointer :: newSection

    if (.not. isSection(line)) call raiseError(__FILE__, &
          "parseSection", "No section found in '"//line//"'")

    ! parse
    sec_start_tag = index(line, TAG_SECTION_START)
    sec_end_tag = index(line, TAG_SECTION_END)
    sectionName = line(sec_start_tag+1:sec_end_tag-1)

    ! create node
    allocate(newAstSection)
    newAstSection % raw = line
    newAstSection % name = adjustl(trim(sectionName))

  end function parseSection


  logical function isOption(line)
    !< true if 'line' define's a section 
    character(len=*), intent(in) :: line
    integer :: assign_idx
    character(len=:), allocatable :: key
    logical :: isEmptyKey, isAssignFound
   
    ! is it an option? key = value(s)
    assign_idx = index(line, TAG_ASSIGN)
    key = line(1:assign_idx-1)

    ! is empty key
    isEmptyKey = len_trim(key) == 0

    ! is assign found
    isAssignFound = assign_idx .ne. 0

    isOption = .not. isEmptyKey .and. isAssignFound
  end function isOption


  function parseOption(line) result (newAstOption)
    !< given a string it returns a astOption by parsing the line
    !< if the line is not as expected an error is launched
    !<  Expected format: key = value
    !<                   key = value1, value2, value3
    character(len=*), intent(in) :: line
    type(astOption), pointer :: newAstOption

    integer :: assign_idx
    character(len=:), allocatable :: optionKey
    character(len=:), allocatable :: rawValues
    type(string), allocatable :: values(:)

    if (.not. isOption(line)) call raiseError(__FILE__, &
          "parseOption", "Invalid format for option, but found '"//line//"'")

    ! parse option key
    assign_idx = index(line, TAG_ASSIGN)
    optionKey = adjustl(trim(line(1:assign_idx-1)))

    ! parse values 
    rawValues = line(assign_idx+1:len(line))
    values = parseOptionValues(rawValues)

    allocate(newAstOption)
    newAstOption % raw = line
    newAstOption % key = optionKey 
    newAstOption % values = values
    !newAstOption % section => to add later

  end function parseOption


  function parseOptionValues(line) result (newAstOptValues)
     !< it parses the values from an option
     !< it returns an array of strings with each value found
     !<   Expected input: 'val1, val2, val3' or 'val'
     character(len=*), intent(in) :: line
     type(string), allocatable :: newAstOptValues(:)

     integer :: nelems 
     integer :: next, prev, i

     nelems = countsubstring(line, TAG_ARRAY) + 1
     allocate(newAstOptValues(nelems))
!     write(*,*) "parseOptionValues:: expected n values=", nelems

!     comma_idx = index(line, TAG_ARRAY)
!     is_single_value = comma_idx .eq. 0

     ! TODO -upgrade to split subroutine in string
     prev = 1 
     next = 1
     i = 1
!     p = index(line, TAG_ARRAY) 
!     write(*,*) "parseOptionValues:: first=", prev, p, line(prev:p-1)
     do
!       write(*,*) ""
!       write(*,*) "parseOptionValues:: line(prev:)=", line(prev:)
       next = index(line(prev:), TAG_ARRAY) 
       if (next .eq. 0) exit
       next = next + prev - 1
!       write(*,*) "parseOptionValues:: i, prev, next, line(prev:)=", i, prev, next, line(prev:)
      
!       write(*,*) "parseOptionValues:: word='", line(prev:next - 1), "'"
       newAstOptValues(i) % val = adjustl(trim(line(prev:next - 1)))
       prev = next + 1
       i = i +1
!       next = next + 1
!       write(*,*) "parseOptionValues:: prev, line(prev:)=", prev, " '", line(prev:), "'"
     enddo

!     write(*,*) "parseOptionValues:: prev, next, line(prev:)=", prev, len(line), "'", line(prev:len(line)), "'"
!     write(*,*) "parseOptionValues:: final='", line(prev:len(line)), "'", size(newAstOptValues)
!     write(*,*) "parseOptionValues:: size(newAstOptValues)=", size(newAstOptValues)
     newAstOptValues(nelems) % val = adjustl(trim(line(prev:len(line))))

  end function parseOptionValues


  function getAllOptionskeys(self) result(keys)
    !< It returns all keys in Options 
    class(configParser), intent(inout) :: self
    type(string), allocatable :: keys(:) !< output

    keys = self % options % getAllKeys()

  end function getAllOptionsKeys


  function getOptionSection(self, key) result (name)
    !< Given a key it returns the option's section's name
    class(configParser), intent(inout) :: self
    character(len=*), intent(in) :: key
    character(len=:), allocatable :: name

    class(*), pointer :: wrap

    wrap => self % options % get(key)
    select type (wrap)
    type is (astOption)
       !write(*,*) "configPArser_mod::getOptionSection:: allocated(section)?  ", allocated(wrap % section)
       if (.not. allocated(wrap % section)) then
         call raiseError(__FILE__, "getOptionSection", &
                        "No section defined for key: '"//key//"'" )
       endif
       name = wrap % section 
    class default
       ! unexpected result here
       call raiseError(__FILE__, "getOptionSection", &
               "Unexpected type found")
    end select

  end function getOptionSection


  function getOptionValues(self, key) result (values)
    !< Given a key it returns the option's section's name
    class(configParser), intent(inout) :: self
    character(len=*), intent(in) :: key
    type(string), allocatable :: values(:)

    class(*), pointer :: wrap

    wrap => self % options % get(key)
    select type (wrap)
    type is (astOption)
       values = wrap % values
    class default
       ! unexpected result here
       call raiseError(__FILE__, "getOptionValues", &
               "Unexpected type found")
    end select

  end function getOptionValues


  function getOptionKey(self, internalkey) result (key)
    !< Given a key it returns the option's section's name
    class(configParser), intent(inout) :: self
    character(len=*), intent(in) :: internalkey
    character(len=:), allocatable :: key

    class(*), pointer :: wrap

    wrap => self % options % get(internalKey)
    select type (wrap)
    type is (astOption)
       key = wrap % key
    class default
       ! unexpected result here
       call raiseError(__FILE__, "getOptionSection", &
               "Unexpected type found")
    end select

  end function getOptionKey


end module SHR_configParser_mod
