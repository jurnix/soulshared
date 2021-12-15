!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : strings_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> Strings_mod unit tests 
!------------------------------------------------------------------------------
!
module strings_test
  use SHR_strings_mod, only: string, printStrings, mergeStringsArray, &
                     isEmptyStringsArray, equalStringsList, stringsArrayToString, &
                     reverseStringsArray, stringCollection
  use SHR_strings_mod, only: is_numeric, countsubstring, str2real, findloc
  use SHR_strings_mod, only: isInteger, str2int
  use SHR_strings_mod, only: int2string
  use SHR_strings_mod, only: real2string
  use SHR_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuiteStrings

  type, extends(testSuite) :: testSuiteStrings
    contains
      procedure :: define => defineTestCases
  end type 

contains


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteStrings), intent(inout) :: self
 
    type(string), allocatable :: empty(:)
    type(string), allocatable :: str1(:)
    type(string), allocatable :: str2(:)
    type(string), allocatable :: str3(:)
    type(string), allocatable :: strs(:)
    type(string) :: t, t2
    character(len=:), allocatable :: tmp
    character(len=:), allocatable :: indata, line
    real :: val
    real, allocatable :: values(:)
    type(string), allocatable :: inArr(:), outArr(:)
    type(string), allocatable :: expectedArr(:)
    integer, allocatable :: pos(:)
    type(stringCollection) :: sc, scfews
    type(stringCollection) :: expWords
    type(string), allocatable :: words(:)
    type(string), allocatable :: fewWords(:)

    type(string), allocatable :: wordsExpected(:)
    type(string), allocatable :: wordsSplit(:)
    type(stringCollection) :: scSplit, scExpected

    type(string) :: newtest
    type(string), allocatable :: wordsSlim(:), wordsUnordered(:)
    type(stringCollection) :: scSlim, scUnordered
    type(stringCollection) :: subsSc !< debug 
    integer, allocatable :: ivals(:), stats(:)
    type(string) :: merged


    allocate(str1(2))
    str1(1) % val = "h"
    str1(2) % val = "i"
    indata = "this is " // new_line('A') // "  " // new_line('A') // " the end" // new_line('A')

    ! mergeStringsArray
    call mergeStringsArray(empty, str1)
    call self % assert (stringsArrayToString(str1) .eq. "h, i", &
             "mergeStringsArray('', str1) .eq. 'h, i' == T")

    call mergeStringsArray(str1, empty)
    call self % assert (stringsArrayToString(empty) .eq. "h, i", &
              "mergeStringsArray(str1, '') .eq. 'h, i' == T")

    allocate(str2(2))
    str2(1) % val = "t"
    str2(2) % val = "a"
    call mergeStringsArray(str1, str2)
    call self % assert (stringsArrayToString(str2) .eq. "h, i, t, a", &
              "mergeStringsArray('h''i', 't''a') .eq. 'h, i, t, a' == T")


    ! isEmptyStringsArray
    deallocate(str2)
    call self % assert (isEmptyStringsArray(str2), "isEmptyStringsArray(non allocated) == T")

    allocate(str2(1))
    call self % assert (isEmptyStringsArray(str2), "isEmptyStringsArray(allocated) == T")

    deallocate(str2)
    allocate(str2(2))
    str2(1) % val = "t"
    str2(2) % val = "a"
    call self % assert (.not. isEmptyStringsArray(str2), "isEmptyStringsArray('t','a') == F")

    deallocate(str2)
    allocate(str2(2))
    str2(1) % val = ""
    str2(2) % val = ""
    call self % assert (isEmptyStringsArray(str2), "isEmptyStringsArray('','') == T")

    str2(2) % val = "some"
    call self % assert (.not. isEmptyStringsArray(str2), "isEmptyStringsArray('','some') == F")

    ! equalStringsList
    str2(1) % val = "h"
    str2(2) % val = "i"
    str1 = str2
    call self % assert (equalStringsList(str1, str2), "equalStringsList('h','i','h','i') == T")


    str2(1) % val = "h"
    str2(2) % val = ""
    call self % assert (.not. equalStringsList(str1, str2), "equalStringsList('h','' vs 'h','i') == F")

    deallocate(str2)
    call self % assert (.not. equalStringsList(str1, str2), "equalStringsList('h','' vs dealloc) == F")

    call self % assert (.not. equalStringsList(str2, str1), "equalStringsList(dealloc vs 'h','') == F")

    allocate(str2(1))
    str2(1) % val = "h"
    call self % assert (.not. equalStringsList(str1, str2), "equalStringsList(h vs hi) == F")


    ! stringsArrayToArray
    deallocate(str1)
    call self % assert (stringsArrayToString(str1) .eq. '', "stringsArrayToString(no alloc) == '' ")

    allocate(str1(0))
    call self % assert (stringsArrayToString(str1) .eq. '', "stringsArrayToString(alloc(0)) == '' ")

    deallocate(str1)
    allocate(str1(2))
    str1(1) % val = 'hel'
    str1(2) % val = 'lo'
    call self % assert (stringsArrayToString(str1) .eq. 'hel, lo', "stringsArrayToString(hel, lo) == 'hel, lo' ")

    deallocate(str1)
    allocate(str1(1))
    str1(1) % val = 'hel'
    call self % assert (stringsArrayToString(str1) .eq. 'hel', "stringsArrayToString(hel) == 'hel' ")


    ! is_numeric
    call self % assert (is_numeric('25'), "is_numeric(25) == T' ")
    call self % assert (is_numeric('25.2'), "is_numeric(25.2) == T' ")
    call self % assert (.not. is_numeric('hello'), "is_numeric(hello) == F' ")
    call self % assert (.not. is_numeric('1900/0/0'), "is_numeric(1900/0/0) == F' ")
    call self % assert (.not. is_numeric('1900/0/0 00:12:05'), "is_numeric(1900/0/0 00:12:05) == F' ")

    ! string 
    call t % init(indata)

    ! isFinished 
    call self % assert (.not. t % isFinished(), "t % isFinished() == F (start)")

    ! nextLine
    call t % nextLine(line)
    call self % assert (line .eq. "this is ", "t % next() .eq. 'this is ' == T")

    call t % nextLine(line)
    call self % assert (line .eq. "  ", "t % next() .eq. '  ' == T")

    call self % assert (.not. t % isFinished(), "t % isFinished() == F (middle)")

    call t % nextLine(line)
    call self % assert (line .eq. " the end", "t % next() .eq. ' the end' == T")

    call t % nextLine(line)
    call self % assert (line .eq. "", "t % next() .eq. '' == T")

    call self % assert (t % isFinished(), "t % isFinished() == T (end)")

    ! reset
    call t % reset()

    call t % nextLine(line)
    call self % assert (line .eq. "this is ", "t % next() .eq. 'this is ' == T (after reset)")

    call self % assert (.not. t % isFinished(), "t % isFinished() == F (start, after reset)")

    ! substring
    call self % assert (countsubstring("hihihi", "hi") .eq. 3, "countsubstring(hihihi, hi) .eq. 3  == T")
    call self % assert (countsubstring("hihihi", "he") .eq. 0, "countsubstring(hihihi, he) .eq. 0  == T")

    ! comparision eq
    t % val = "hello"
    call self % assert (t .eq. "hello", "t(hello) .eq. hello == T")
    call self % assert (t == "hello", "t(hello) == hello == T")

    ! assignment
    t % val = "nothing"
    t2 % val = "hello2"
    t = t2
    call self % assert (t .eq. "hello2", "t(hello2) .eq. t2(hello2) == T (after type assign)")
    tmp = "hello3"
    t = tmp
    call self % assert (t .eq. "hello3", "t(hello3) .eq. hello3 == T (after char assign)")


    ! string() % split
    allocate(wordsExpected(4))
    wordsExpected(1) = "there"
    wordsExpected(2) = " is"
    wordsExpected(3) = " nothing"
    wordsExpected(4) = " new"
    newtest = string("there, is, nothing, new")
    wordsSplit = newtest % split (',')

    scSplit = stringCollection(wordsSplit)
    scExpected = stringCollection(wordsExpected)
!    write(*,*) "string_test:: wordsSplit =", scSplit % toString()
!    write(*,*) "string_test:: wordsExpected =", scExpected % toString()
    call self % assert (stringCollection(wordsSplit) == stringCollection(wordsExpected), &
            "string('a, b, c') % split .eq. ['a',' b',' c'] == T")

    ! string merge (+) vs string
    t = string("good")
    t2 = string("morning")
    merged = t + t2
    call self % assert(merged == string("goodmorning"), &
          "string(good) + string(morning) .eq. string(goodmorning) = T")

    ! string merge (+) vs chars
    t = string("good")
    merged = t + "morning"
    call self % assert(merged == string("goodmorning"), &
        "string(good) + 'morning' .eq. string(goodmorning) = T")

    ! string merge (+) vs int
    t = string("good")
    merged = t + 4
    call self % assert(merged == string("good4"), &
        "string(good) + 4 .eq. string(good4) = T")

    ! string merge (+) vs real
    t = string("good")
    merged = t + 4.2
    write(*,*) merged % toString()
    call self % assert(merged == string("good4.2000"), &
        "string(good) + 4.2 .eq. string(good4.2000) = T")

    !str2real
    call str2real("23.2", val)
    call self % assert (val .eq. 23.2, "str2real('23.2') .eq. 23.2 == T")

    allocate(strs(3))
    strs(1) % val = "1.2"
    strs(2) % val = "2.3"
    strs(3) % val = "3.4"
    call str2real(strs, values)
    call self % assert (values(1) .eq. 1.2, "str2real('1.2') .eq. 1.2 == T")
    call self % assert (values(2) .eq. 2.3, "str2real('2.3') .eq. 2.3 == T")
    call self % assert (values(3) .eq. 3.4, "str2real('3.4') .eq. 3.4 == T")

    ! reverseStringsArray
    allocate(inArr(4))
    allocate(expectedArr(4))
    inArr(1) = "1"
    inArr(2) = "2"
    inArr(3) = "3"
    inArr(4) = "4"
    expectedArr(4) = "1"
    expectedArr(3) = "2"
    expectedArr(2) = "3"
    expectedArr(1) = "4"
    call reverseStringsArray(inArr, outArr)
    call self % assert (equalStringsList(expectedArr, outArr), "reverseSTringsArray( (1,2,3,4) ) .eq. (4,3,2,1) == T")

    ! findloc_string
    pos = findloc(strs, "2.3")
    call self % assert (all(pos == [2]), "findloc(strs, '2.3') == 2 == T")

    call self % printBreakLine()

    words = [string("a"), string("uppon"), string("c"), string("time")]
    wordsSlim = [string("a"), string("uppon"), string("c")]
    wordsUnordered = [string("a"), string("uppon"), string("time"), string("c")]
    fewWords = [string("uppon"), string("never")]
    sc = stringCollection(words)
    scSlim = stringCollection(wordsSlim)
    scUnordered = stringCollection(wordsUnordered)
    scfews = stringCollection(fewWords)
    
    call self % assert (sc % hasString(string("time")), &
                "stringCollection(one, uppon, a, time) % hasString(string(time))  == T")
    call self % assert (.not. sc % hasString("none"), &
                "stringCollection(one, uppon, a, time) % hasString(none)  == F")


    call self % assert (sc == sc, &
                "stringCollection(a, uppon, c, time) .eq. stringCollection(a, uppon, c, time) == T")

    call self % assert (.not. (sc == scSlim), &
                "stringCollection(a, uppon, c, time) .eq. stringCollection(a, uppon, c) == F")

    call self % assert (.not. (sc == scUnordered), &
                "stringCollection(a, uppon, c, time) .eq. stringCollection(a, uppon, time, c) == F")

    call self % assert (.not. (sc == scfews), &
                "stringCollection(a, uppon, c, time) .eq. stringCollection(uppon, never) == F")

    expWords = stringCollection([string("a"), string("uppon"), string("c"), string("time"), string("uppon"), string("never")])
    call self % assert ((sc + scfews) == expWords, &
            "sc(a, uppon, c, time) + sc(uppon, never) .eq. sc(a, uppon, c, time, uppon, never)  == T")

    expWords = stringcollection([string("a"), string("uppon"), string("time")])
    call self % assert ((sc - string("c")) == expWords, &
            "sc(a, uppon, c, time) - word(c) .eq. sc(a, uppon, time)  == T")   
    subsSc = sc - string("c")

    expWords = stringCollection([string("a"), string("c"), string("time")])
    call self % assert ((sc - scfews) == expWords, &
            "sc(a, uppon, c, time) - sc(uppon, never) .eq. stringCollection(a, c, time)  == T")
    subsSc = sc - scfews


    fewWords = [string("uppon"), string("c")]
    scfews = stringCollection(fewWords)
    expWords = stringCollection([string("a"), string("time")])
    call self % assert ((sc - scfews) == expWords, &
            " words(a, uppon, c, time) - fewWords(uppon, c) .eq. stringCollection(a, time)  == T")


    ! int2string
    call self % assert(int2string(5) == string("5"), "int2string(5) .eq. string('5')  == T")


    ! real2string
    call self % assert(real2string(5.25) == string("5.2500"), "int2string(5.25) .eq. string('5.2500')  == T")


    ! str2int
    call str2int([string("520")], ivals, stats)
    call self % assert(ivals(1) == 520, "str2int('520') .eq. 520 == T")

    call str2int([string("521"), string("522")], ivals, stats)
    call self % assert(all(ivals == [521, 522]), "str2int('521', '522') .eq. 521, 522 == T")

    call str2int([string("521.5")], ivals, stats)
    call self % assert(stats(1) /= 0, "str2int('521.5') ? == F")

    if (allocated(words)) deallocate(words)
    allocate(words(3))
    words(1) = "525"
    words(2) = "523"
    words(3) = "522"
    call str2int(words, ivals, stats)
    call self % assert(all(stats == 0), "str2int(...) .eq. 525, 523, 522 == T")


    ! isInteger
    call self % assert(all(isInteger([string("520")])), "isInteger('520') == T")
    call self % assert(.not. all(isInteger([string("521.0")])), "isInteger('521.0') == F")
    call self % assert(.not. all(isInteger([string("521.5")])), "isInteger('521.5') == F")


  end subroutine defineTestCases

end module strings_test

