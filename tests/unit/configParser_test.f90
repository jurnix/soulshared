!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : configParser_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> configFie_mod unit tests 
!------------------------------------------------------------------------------
!
module configParser_test
  use SHR_strings_mod, only: string
  use SHR_testSuite_mod, only: testSuite
  use SHR_configParser_mod, only: configParser, parseOptionValues
  use SHR_configParser_mod, only: astSection, astOption
  use SHR_configParser_mod, only: isOption, isSection, parseSection, parseOption

  implicit none

  private
  public :: testSuiteConfigParser

  type, extends(testSuite) :: testSuiteConfigParser
    contains
      procedure :: setUp
      procedure :: tearDown
      procedure :: define => defineTestCases
  end type 

contains

  subroutine setUp(self)
    class(testSuiteConfigParser), intent(inout) :: self
    integer :: stat

    open(unit=1234, iostat=stat, file='pkulsm.cfg', status='old')
    if (stat == 0) close(1234, status='delete')

    open(1, file = 'pkulsm.cfg', status = 'new')  
    write(1,*) "[pkulsm] # main section" 
    write(1,*) "pku_restart_from=restart1900.nc # restart simulation from this file" 
    write(1,*) "pku_restart_output_name=restartsYYYY.nc" 
    write(1,*) "[solar radiation]" 
    write(1,*) "rad_enable=y  # always enabled" 
    write(1,*) "" 
    write(1,*) "   rad_c4=2.0, 3.0, 4.0, 5.0, &"
    write(1,*) "    5.5 &" 
    write(1,*) "    ,8.5, &" 
    write(1,*) "    0.5 " 
    write(1,*) "rad_c3=2, 3, 4, 5, 5" 
    write(1,*) "[vegetation]" 
    write(1,*) "slope=5"
    write(1,*) " #slope=5"
    write(1,*) "volume=5.2"
    write(1,*) "fixed=1,2,3,4&"
    write(1,*) "     ,5,6,7"

    close(1)

  end subroutine setUp


  subroutine tearDown(self)
    class(testSuiteConfigParser), intent(inout) :: self
    integer :: stat

    ! delete test config file if exists
    open(unit=1234, iostat=stat, file='pkulsm.cfg', status='old')
    if (stat == 0) close(1234, status='delete')
  end subroutine tearDown


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteConfigParser), intent(inout) :: self
    type(configParser) :: c

    character(len=:), allocatable :: intext, expected, found
    type(string), allocatable :: astOptValues(:)
    type(astSection), allocatable :: newSection
    type(astOption), allocatable :: newOption

    call self % setUp()

    call c % init("pkulsm.cfg")

    ! readAllLines

    ! removeComment
    call self % assert (c % removeComment("hello # to remove") .eq. "hello", &
                    "c % removeComment('hello # to remove') .eq. hello == T")
    call self % assert (c % removeComment("hello  ") .eq. "hello", &
                    "c % removeComment(hello) .eq. hello == T")

    ! mergeLines
    ! 3 lines
    intext="this is=0,1,&"// new_line('A') // "  2,3&" // new_line('A') // ",4,5"
    call self % assert (c % mergeLines(intext) .eq. "this is=0,1,  2,3,4,5", &
                    "c % mergeLines(this is=0,1,&  2,3&,4,5) .eq. this is=0,1,  2,3,4,5 == T")

    ! 2 lines
    intext="this is=0,1,&"// new_line('A') // "  2,3"
    call self % assert (c % mergeLines(intext) .eq. "this is=0,1,  2,3", &
                    "c % mergeLines(this is=0,1,&  2,3) .eq. this is=0,1,  2,3 == T")

    ! 2 lines with empty line in the middle
    intext="this is=0,1,&"// new_line('A') // "  &" // new_line('A') // "  2,3"
    call self % assert (c % mergeLines(intext) .eq. "this is=0,1,    2,3", &
                    "c % mergeLines(this is=0,1,&  &  2,3) .eq. this is=0,1,    2,3 == T")

    ! nothing to merge
    intext = "nothing" // new_line('A') // "to do"
    call self % assert (c % mergeLines(intext) .eq. intext, &
        "c % mergeLines(nothing CL to do) .eq. nothing CL to do == T")

    ! nothing to merge, leading spaces
    intext = "nothing" // new_line('A') // "n  to do"
    call self % assert (c % mergeLines(intext) .eq. intext, &
        "c % mergeLines('nothing CL n  to do') .eq. 'nothing CL n  to do' == T")

    ! removeAllComments
    intext = "a # test" // new_line('A') // "b # to disappear" // &
            new_line('A') // "c" 
    expected = "a " // new_line('A') // "b " // new_line('A') // "c" 
    found = c % removeAllComments(intext)
    call self % assert (c % removeAllComments(intext) .eq. expected, &
        "c % removeAllComments('a #comment CL b #comment CL c') .eq. 'a CL b CL c' == T")

    ! parseOptionValues
    astOptValues = parseOptionValues("this, is, one,  more")
    call self % assert (astOptValues(1) .eq. "this", "astOptValues(this, is, one,  more)(1) .eq. 'this' == T")
    call self % assert (astOptValues(2) .eq. "is", "astOptValues(this, is, one,  more)(2) .eq. 'is' == T")
    call self % assert (astOptValues(3) .eq. "one", "astOptValues(this, is, one,  more)(3) .eq. 'one' == T")
    call self % assert (astOptValues(4) .eq. "more", "astOptValues(this, is, one,  more)(4) .eq. 'more' == T")

    ! parseOption
    newOption = parseOption("mykey =this, is")
    call self % assert (newOption % key .eq. "mykey", "parseOption(mykey =this, is)%key .eq. 'mykey' == T")
    call self % assert (newOption % values(1) .eq. "this", "parseOption(mykey =this, is)%values(1) .eq. this == T")
    call self % assert (newOption % values(2) .eq. "is", "parseOption(mykey =this, is)%values(2) .eq. 'is' == T")

    ! parseSection
    newSection = parseSection("[ new  ]")
    call self % assert (newSection % name .eq. "new", "parseSection([new]) .eq. 'new' == T")

    ! isSection
    call self % assert (isSection("[something]"), "isSection([something]) == T")
    call self % assert (.not. isSection(" [something"), "isSection( [something) == F")
    call self % assert (.not. isSection(" something]"), "isSection( something]) == F")
    call self % assert (.not. isSection("] something["), "isSection(] something[) == F")
    call self % assert (.not. isSection(" something"), "isSection( something) == F")

    ! isOption
    call self % assert (.not. isOption("[something]"), "isOption([something]) == F")
    call self % assert (isOption("akey=avalue"), "isOption(akey=avalue) == T")
    call self % assert (isOption("akey=avalue,another"), "isOption(akey=avalue,another) == T")
    call self % assert (.not. isOption("=a,b"), "isOption(=a,b) == F")
    call self % assert ( isOption("key= "), "isOption(key= ) == T")

    call self % tearDown()

  end subroutine defineTestCases

end module configParser_test

