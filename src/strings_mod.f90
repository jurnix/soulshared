!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : SHR_strings_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!  Subroutines related to an array of characters. This is wrapped into a string
!  due to Fortran limitations. Several operations are offered into this module
!> 
!------------------------------------------------------------------------------
!
module SHR_strings_mod

  use SHR_precision_mod, only: sp, dp

  implicit none 

  public :: string, printStrings, mergeStringsArray, isEmptyStringsArray, &
             equalStringsList, stringsArrayToString, reverseStringsArray, &
             is_numeric, countsubstring
  public :: isEmpty
  public :: int2string
  public :: sanitize
  public :: str2int, isInteger
  
  interface real2string
    module procedure :: real2string_sp, real2string_dp
  end interface


  interface sanitize
    module procedure :: sanitize_string, sanitize_chars
  end interface sanitize 


  interface str2real
    module procedure :: str2real_s, str2real_a
  end interface

  interface is_numeric
    module procedure :: is_numeric_chr, is_numeric_str
  end interface

  interface findloc
    module procedure :: findloc_string, findloc_char
  end interface


  type string ! characters wrapper, to use in a string array
    character(len=:), allocatable :: val
    logical :: hasNewLine !< More lines to read
    integer :: prevNlIdx !< Previous next line index
    integer :: nextNlIdx !< Current next line index
  contains
    procedure :: isEmpty => isEmptyString !< empty = non allocated, "", "   " (only spaces)
    procedure :: init => init_text !< introduce the whole text
    procedure :: nextLine !< provide the next line
    procedure :: isFinished !< is there another line to read
    procedure :: reset !< start from the beginning
    procedure :: eq_string !< comparision string vs string
    procedure :: eq_chars_right !< comparision string vs character
    procedure, pass(self) :: eq_chars_left !< comparision string vs character
    procedure :: assign_chars !< 
    procedure :: assign_string !<
    procedure, pass(self) :: assign_toChars !< 
    procedure :: neq_string

    procedure :: toString => toString_string

    procedure :: split !< it returns an array of string, separted by ','

    generic :: operator(==) => eq_string, eq_chars_left, eq_chars_right
    generic :: operator(.eq.) => eq_string, eq_chars_left, eq_chars_right
    generic :: operator(.ne.) => neq_string
    generic :: assignment(=) => assign_chars, assign_string, assign_toChars
  end type string

  interface string
    module procedure :: string_constructor_chars, string_constructor_empty, &
            string_constructor_string
  end interface string


  type stringCollection !< wrap an array of strings
    type(string), allocatable :: strings(:)
  contains
    ! public
    procedure :: getSize
    procedure :: get !< it returns a string at the position 'idx'
    procedure :: getAll !< it returns 'strings' variable
    procedure :: toString => toString_stringCollection
    procedure :: hasString_str, hasString_char
    generic :: hasString => hasString_str, hasString_char

    ! private
    procedure :: sc_subs
    procedure :: sc_subs_string
    procedure :: sc_subs_chars
    procedure :: sc_add
    procedure :: sc_eq
    
    generic :: operator(+) => sc_add 
    generic :: operator(-) => sc_subs, sc_subs_string, sc_subs_chars
    generic :: operator(==) => sc_eq
    !generic :: operator(.eq.) => 
  end type stringCollection

  interface stringCollection
    module procedure :: stringCollection_constructor_chars, &
                        stringCollection_constructor_strings, &
                        stringCollection_constructor_empty
  end interface stringCollection

contains

  function getSize(self) result (scSize)
    !< it returns the total number of strings the stringCollection
    !< hold inside.
    class(stringCollection), intent(in) :: self
    integer :: scSize

    scSize = size(self % strings)
  end function getSize


  function get(self, idx) result (str)
    !< it returns 'strings' variable
    class(stringCollection), intent(in) :: self
    integer, intent(in) :: idx !< index from the internal collection
    type(string) :: str !< output

    str = self % strings(idx)
  end function get


  function getAll(self) result (r)
    !< it returns 'strings' variable
    class(stringCollection), intent(in) :: self
    type(string), allocatable :: r(:)

    r = self % strings
  end function getAll


  pure function toString_stringCollection(self) result (r)
    !< String representation of stringCollection 
    class(stringCollection), intent(in) :: self
    character(len=:), allocatable :: r !< output

    integer :: i
    
    ! not allocated
    if (.not. allocated(self % strings)) then
      r = ""
      return
    endif

    ! no strings in the array
    if (size(self % strings) == 0) then
      r = ""
      return
    endif

    r = self % strings(1)
    do i = 2, size(self % strings)
      r = r//", "//self % strings(i) % toString()
    enddo
  end function toString_stringCollection


  logical function sc_eq(self, other)
    !< 'stringCollecton' equal comparision 
    !< true if they have the same strings inside in the same order 
    class(stringCollection), intent(in) :: self
    class(stringCollection), intent(in) :: other

    integer :: i
    logical :: hasSameSize, hasSameOrder

    hasSameOrder = .true.
    hasSameSize = (size(self % strings) .eq. size(other % strings))

    if (.not. hasSameSize) then
      sc_eq = .false.
      return
    endif

    do i = 1, size(self % strings)
      if ( self % strings(i) .ne. other % strings(i) ) then
        hasSameOrder = .false.
        exit
      endif
    enddo

    sc_eq = (hasSameSize .and. hasSameOrder)
  end function sc_eq


  logical function hasString_char(self, word)
    !< true if the word is found inside the stringCollection
    class(stringCollection), intent(in) :: self
    character(len=*), intent(in) :: word

    type(string) :: newstr
    call newstr % init(word)
    hasString_char = self % hasString_str(newstr)
  end function hasString_char


  logical function hasString_str(self, searchStr)
    !< true if the searchSTr is found inside the stringCollection
    class(stringCollection), intent(in) :: self
    type(string), intent(in) :: searchStr
    integer, allocatable :: idx(:)

    hasString_str = .false.

    idx = findloc(self % strings, searchStr)
    if (size(idx) > 0) then
      if (idx(1) > 0) hasString_str = .true.
    endif
  end function hasString_str


  function sc_add(this, that) result (total)
    !< it merges this and that stringCollections into 
    !< total = this + that
    !< it does not care about repeated strings
    class(stringCollection), intent(in) :: this
    class(stringCollection), intent(in) :: that
    type(stringCollection) :: total !< output

    total % strings =  [this % strings, that % strings] 
  end function sc_add

 
  function sc_subs_chars(this, word) result (total)
    !< it removes the string from 'that' found inside 'this'
    !< otherwise it returns the same
    class(stringCollection), intent(in) :: this
    character(len=*), intent(in) ::word 
    type(stringCollection) :: total !< output

!    write(*,*) "string_mod::sc_subs_chars:: word=", word
    total = this - string(word)
  end function sc_subs_chars


  function sc_subs_string(this, word) result (total)
    !< it removes the string from 'that' found inside 'this'
    !< otherwise it returns the same
    class(stringCollection), intent(in) :: this
    class(string), intent(in) :: word
    type(stringCollection) :: total !< output

    type(string), allocatable :: first(:), last(:)
    integer :: i, strslen
    integer, allocatable :: idx(:)

!    write(*,*) "sc_subs_string:: word=", word % toString()

    idx = findloc(this % strings, word)

    if (size(idx) == 0) then ! not found
      total = this
      return
    endif
!    write(*,*) "sc_subs_string:: idx =", idx

    strslen = size(this % strings)
    first = this % strings(1:idx(1)-1)
    last = this % strings(idx(1)+1:strslen)
    total % strings = [first, last]
!    write(*,*) "sc_subs_string:: output =", total % toString()
  end function sc_subs_string


  function sc_subs(this, that) result (total)
    !< it removes all strings from 'that' found inside 'this'
    class(stringCollection), intent(in) :: this
    class(stringCollection), intent(in) :: that
    type(stringCollection) :: total !< output

    integer :: i !< iterator

!    write(*,*) "stdnc_mod::sc_subs:: that=", that % toString()

    total = this
    do i = 1, size(that % strings)
      total = total - that % strings(i)
    enddo
  end function sc_subs


  pure type(stringCollection) function stringCollection_constructor_chars(chars)
    !< stringCollection constructor
    !< strings: array of strings 
    character(len=*), intent(in) :: chars(:) !< input data
    integer :: i
    integer :: nelems

    nelems = size(chars)
    allocate(stringCollection_constructor_chars % strings(nelems))
    stringCollection_constructor_chars % strings = string( chars )
  end function stringCollection_constructor_chars


  pure type(stringCollection) function stringCollection_constructor_strings(strings)
    !< stringCollection constructor
    !< strings: array of strings 
    type(string), intent(in) :: strings(:) !< input data

    stringCollection_constructor_strings % strings = strings
  end function stringCollection_constructor_strings


  pure type(stringCollection) function stringCollection_constructor_empty()
    !< stringCollection constructor with no contect 

    allocate(stringCollection_constructor_empty % strings(0))
  end function stringCollection_constructor_empty


  pure elemental type(string) function string_constructor_empty()
    !< constructor for string
    string_constructor_empty % val = ""
  end function string_constructor_empty


  pure elemental type(string) function string_constructor_chars(chars)
    !< constructor for string
    character(len=*), intent(in) :: chars
    string_constructor_chars % val = chars
  end function string_constructor_chars


  pure elemental type(string) function string_constructor_string(str)
    !< constructor for string
    type(string), intent(in) :: str
    string_constructor_string % val = str % val
  end function string_constructor_string


  pure function toString_string(self) result (r)
    !< character representation of string -_-
    class(string), intent(in) :: self
    character(len=:), allocatable :: r
    
    r = self % val
  end function toString_string

  
  pure subroutine assign_toChars(chars, self)
    !< this, from
    class(string), intent(in) :: self
    character(len=:), allocatable, intent(in out) :: chars
    chars = self % val
  end subroutine assign_toChars


  pure subroutine assign_chars(self, inchars)
    class(string), intent(in out) :: self
    character(len=*), intent(in) :: inchars

    self % val = inchars
  end subroutine assign_chars


  pure subroutine assign_string(self, instring)
    class(string), intent(in out) :: self
    type(string), intent(in) :: instring

    self % val = instring % val
  end subroutine assign_string


  logical function neq_string(self, other)
    !< true if self and other are not the same
    class(string), intent(in) :: self
    class(string), intent(in) :: other

!    write(*,*) "eq_string:: 1st % val= '", self % val, "' vs '", other % val, "'"
    neq_string = ( self % val .ne. other % val )
  end function neq_string


  elemental logical function eq_string(self, other)
    !< 'string' equal comparision vs string
    class(string), intent(in) :: self
    class(string), intent(in) :: other

!    write(*,*) "eq_string:: 1st % val= '", self % val, "' vs '", other % val, "'"
    eq_string = ( self % val == other % val )
  end function eq_string


  elemental logical function eq_chars_left(other, self)
    !< 'string' equal comparision vs character
    class(string), intent(in) :: self
    character(len=*), intent(in) :: other

!    write(*,*) "eq_chars:: 1st % val= '", self % val, "' vs '", other, "'"
    eq_chars_left = ( self % val == other )
  end function eq_chars_left


  elemental logical function eq_chars_right(self, other)
    !< 'string' equal comparision vs character
    class(string), intent(in) :: self
    character(len=*), intent(in) :: other

!    write(*,*) "eq_chars:: 1st % val= '", self % val, "' vs '", other, "'"
    eq_chars_right = ( self % val == other )
  end function eq_chars_right

        
  subroutine reset(self)
    !< set text to the beginning
    class(string), intent(inout) :: self

    self % hasNewLine = .true.
    self % nextNlIdx = 1
    self % prevNlIdx = 1
  end subroutine reset


  subroutine init_text(self, intxt)
    !< initialize text with a character
    class(string), intent(inout) :: self
    character(len=*), intent(in) :: intxt

    self % val = intxt

    call self % reset()
  end subroutine init_text


  logical function isFinished(self) 
    !< true if there are no more lines to read
    class(string), intent(inout) :: self
    
    isFinished = .not. self % hasNewLine

  end function isFinished


  subroutine nextLine(self, newline)
    !< it provides the next line to read
    class(string), intent(inout) :: self
    character(len=:), allocatable, intent(out) :: newline

    integer :: newline_idx
    integer :: last_idx
    logical :: is_last_line    

!    write(*,*) "nextLines:: --------------------------"

!    write(*,*) "nextLines:: checking on '", self % val(self % prevNlIdx:len(self % val)), "'"
    newline_idx = index(self % val(self % prevNlIdx:len(self % val)), new_line('A'))
    self % hasNewLine = newline_idx .ne. 0

    last_idx = self % prevNlIdx + newline_idx - 2 ! -2 = skip new_line
!    write(*,*) "nextLines:: indices:", self % prevNlIdx, last_idx
    is_last_line = .not. self % hasNewLine
    if (is_last_line) then
         last_idx = len(self % val)
!         write(*,*) "nextLines:: last index enabled, set to ", last_idx
!         write(*,*) "nextLines:: indices:", self % prevNlIdx, last_idx
    endif

    newline = self % val(self % prevNlIdx:last_idx)
!    write(*,*) "nextLine:: newline: '", newline, "'"

    self % prevNlIdx = self % prevNlIdx+newline_idx

  end subroutine nextLine

  function stringsArrayToString(strArr) result (str)
    !< It converts an array of strings into a character(*)
    type(string), allocatable, intent(in) :: strArr(:)
    character(len=:), allocatable :: str

    character(len=:), allocatable :: tmp
    integer :: i

    str = ""
    tmp = ""

    if (.not. allocated(strArr)) then
       return
    endif

    if (size(strArr) .eq. 0) then
       return
    endif

    str = strArr(1) % val
    do i = 2, size(strArr)
      str = str//", "//strArr(i) % val
    enddo
  end function stringsArrayToString


  logical function equalStringsList(str1, str2)
    !< compare two arrays of string
    type(string), allocatable, intent(in) :: str1(:)
    type(string), allocatable, intent(in) :: str2(:)
    integer :: i

    if (.not. allocated(str1)) then
      equalStringsList = .false.
      return
    endif

    if (.not. allocated(str2)) then
      equalStringsList = .false.
      return
    endif
 
    equalStringsList = .true.

    !write(*,*) "equalStringsList::", size(str1) , size(str2)
    if (size(str1) .ne. size(str2)) then 
      equalStringsList = .false.
      return
    endif

    do i = 1, size(str1)
      !write(*,*) "equalStringsList::", i, adjustl(str1(i) % val), adjustl(str2(i) % val)
      if (adjustl(str1(i) % val) .ne. adjustl(str2(i) % val)) then
         equalStringsList = .false.
         exit
      endif
    enddo

  end function equalStringsList


  logical function isEmpty(chars)
    !< Given an allocatable character it returns true when it is empty
    !< An empty character is defined as:
    !< - not allocated
    !< - has 0 characters in it
    !< - it only has spaces
    character(len=:), allocatable, intent(in) :: chars

    isEmpty = .false.

    if (.not. allocated(chars) ) then
      isEmpty = .true.
      return
    endif

    !isEmpty = isEmpty_dummy(chars)
    if (len(trim(adjustl(chars))) == 0) then
      isEmpty = .true.
    endif

  end function isEmpty


  logical function isEmptyString(this)
    !< is empty when it is not allocated, "", "   " (only spaces)
    class(string), intent(in) :: this

    !isEmptyString = .false.
    isEmptyString = isEmpty(this % val)
    !if (.not. allocated(this % val)) then
    !  isEmptyString = .true.
    !  return
    !endif

    !if ( trim(adjustl(this % val)) == "") isEmptyString = .true.

  end function isEmptyString


  logical function isEmptyStringsArray(strings)
    !< true if all strings in the array are empty  
    type(string), allocatable, intent(in) :: strings(:)
    integer :: i

    isEmptyStringsArray = .true.

    if (.not. allocated(strings)) then
      return
    endif

    do i = 1, size(strings)
      if (.not. strings(i) % isEmpty()) then
        isEmptyStringsArray = .false.
        exit
      endif
    enddo    

  end function isEmptyStringsArray


  subroutine reverseStringsArray(inArr, outArr)
    !< given an inArr(1,2,3,4) it returns a new outArr
    !> with all its elements reversed outArr(4,3,2,1)
    type(string), allocatable, intent(in) :: inArr(:)
    type(string), allocatable, intent(out) :: outArr(:)

    integer :: i, arrsz

    arrsz = size(inArr)
    if (allocated(outArr)) deallocate(outArr)
    allocate(outArr(arrsz))

    do i = 1, arrsz
      outArr(i) = inARr(arrsz-i+1)
    enddo

  end subroutine reverseStringsArray


  subroutine printStrings(strings)
    !< print an array of strings
    type(string), intent(in) :: strings(:)
    integer :: i

!    write(*,*) "print array of strings:"
    do i=1, size(strings)
      write(*,*) "'", strings(i) % val, "'"
    enddo
  end subroutine printStrings


  subroutine mergeStringsArray(arstr1, arstr2)
    !< given 2 arrys of string, it merges them if those are not empty.
    !< Otherwise it returns the existing array of string
    !< if boths are empty, it returns an empty("") dest string array
    type(string), allocatable, intent(in) :: arstr1(:)
    type(string), allocatable, intent(in out) :: arstr2(:) ! destination

    ! use tmp to allow mergeStringsArray(str1, str2, str1)
    type(string), allocatable :: tmp(:)

    if (isEmptyStringsArray(arstr1) .and. isEmptyStringsArray(arstr2)) then
       if (allocated(arstr2)) deallocate(arstr2)
       allocate(arstr2(1))
       arstr2(1) % val = ""
       return
    endif

    if (isEmptyStringsArray(arstr1)) then
       return
    endif

    if (isEmptyStringsArray(arstr2)) then
       arstr2 = arstr1
       return
    endif

    tmp = [arstr1, arstr2]
    !write(*,*) "mergeStringsArray:: tmp"
    !call printStrings(tmp)
    arstr2 = tmp
    !write(*,*) "mergeStringsArray:: dest"
    !call printStrings(arstr2)
  end subroutine mergeStringsArray


  logical elemental function is_numeric_chr(string)
    !< True if the string 'string' is a number(real/float)
    character(len=*), intent(in) :: string
    real :: x
    integer :: e, n
    character(len=12) :: fmt

!    read(string,*,iostat=e) x
!    is_numeric_chr = (e == 0)
    n=LEN_TRIM(string)
    WRITE(fmt,'("(F",I0,".0)")') n
    READ(string,fmt,IOSTAT=e) x
    is_numeric_chr = (e == 0)
  end function is_numeric_chr


  logical elemental function is_numeric_str(strin)
    !< True if the string 'string' is a number(real/float)
    type(string), intent(in) :: strin
    real :: x
    integer :: e

    is_numeric_str = is_numeric_chr(strin % val)
  end function is_numeric_str


  function countsubstring(s1, s2) result(c)
    !< count the number of substrings s2 found in s1
    !< it returns an integer
    character(len=*), intent(in) :: s1, s2
    integer :: c, p, posn

    c = 0
    if(len(s2) == 0) return
    p = 1
    do
      posn = index(s1(p:), s2)
      if(posn == 0) return
      c = c + 1
      p = p + posn + len(s2) -1
    end do
  end function countsubstring


  subroutine str2real_s(str, num)
    ! string into number
    character(len=*), intent(in) :: str
    real, intent(out) :: num
    read(str , *) num
  end subroutine str2real_s


  subroutine str2real_a(str, num)
    ! string into number
    type(string), intent(in) :: str(:)
    real, allocatable, intent(out) :: num(:)

    integer :: i

    if (allocated(num)) deallocate(num)
    allocate(num(size(str)))

    do i = 1, size(str)
      read(str(i) % val , *) num(i)
    enddo
  end subroutine str2real_a


  function findloc_string(array, value) result (res)
    !< wrapper to findloc_char 
    type(string), intent(in) :: array(:)
    type(string), intent(in) :: value
    integer, allocatable :: res(:)

    res = findloc_char(array, value % val)
  end function findloc_string


  function findloc_char(array, value) result (res)
    !< findloc implementation for character type
    !< https://gcc.gnu.org/onlinedocs/gfortran/FINDLOC.html
    type(string), intent(in) :: array(:) !< data
    character(len=*), intent(in) :: value !< value to search
    integer, allocatable :: res(:) !< output

    integer :: i

    do i = 1, size(array)
      if (array(i) == value) then
        allocate(res(1)) ! found!
        res(1) = i
        exit
      endif
    enddo

    if (.not. allocated(res)) allocate(res(0)) ! not found
  end function findloc_char


  function split(self, token) result (r)
    !< it splits the self text into multiple pieces according to 'splitter'
    !< in case there's nothing, it returns the same text
    !< it returns an array of string
    class(string), intent(in) :: self
    character(len=*), intent(in) :: token
    type(string), allocatable :: r(:)

    integer :: nfound !< number of substrings found
    integer :: idx
    integer :: c, p, posn, pnext

    nfound = countSubstring(self % val, token)
    nfound = nfound + 1 
    if (allocated(r)) deallocate(r)
    allocate(r(nfound))

    idx = 1
    c = 0
    if(len(token) == 0) return
    p = 1
    do
      posn = index(self % val(p:), token)
      if(posn == 0) exit 
      pnext = p + posn - len(token) - 1
!      write (*,*) "strings_mod:: split:: val= '", self % val(p:pnext), "'"
      r(c+1) = self % val(p:pnext)

      c = c + 1
      p = p + posn + len(token) -1
    end do

    ! last position
!    write (*,*) "strings_mod:: split:: last, val= '", self % val(p:len(self % val)), "' ", p, len(self % val)
    r(nfound) = self % val(p:len(self % val))

  end function split


  pure elemental function real2string_sp(value, format) result (r)
    !< converts a sp real into a string
    real(kind=sp), intent(in) :: value !< input values
    character(len=*), intent(in), optional :: format !< string format
    type(string) :: r !< output
    
    character(len=*), parameter :: DEF_FORMAT = '(f10.4)'
    character(len=50) :: tmp
    character(len=:), allocatable :: curFormat

    curFormat = DEF_FORMAT
    if (present(format)) curFormat = format

    write(tmp, curFormat) value
    r % val = trim(adjustl(tmp))
  end function real2string_sp


  pure elemental function real2string_dp(value, format) result (r)
    !< converts a sp real into a string
    real(kind=dp), intent(in) :: value !< input values
    character(len=*), intent(in), optional :: format !< string format
    type(string) :: r !< output
    
    character(len=*), parameter :: DEF_FORMAT = '(f10.4)'
    character(len=50) :: tmp
    character(len=:), allocatable :: curFormat

    curFormat = DEF_FORMAT
    if (present(format)) curFormat = format

    write(tmp, curFormat) value
    r % val = trim(adjustl(tmp))
  end function real2string_dp


  pure elemental function int2string(value, format) result (r)
    !< converts an integer into a string
    integer, intent(in) :: value !< input values
    character(len=*), intent(in), optional :: format !< string format
    type(string) :: r !< output
    
    character(len=*), parameter :: DEF_FORMAT = '(I10)'
    character(len=50) :: tmp
    character(len=:), allocatable :: curFormat

    curFormat = DEF_FORMAT
    if (present(format)) curFormat = format

    write(tmp, curFormat) value
    r % val = trim(adjustl(tmp))
  end function int2string


  pure elemental function sanitize_string(strin) result (strout)
    !< remove front and back spaces from string
    type(string), intent(in) ::strin 
    type(string) ::strOut !< output
 
    strout = trim(adjustl(strin % val)) 
  end function sanitize_string


  pure elemental function sanitize_chars(charin) result (strOut)
    !< remove front and back spaces from char and outputs a string
    character(len=*), intent(in) :: charin
    type(string) :: strOut !< output
    character(len=:), allocatable :: cleanChars

    cleanChars = trim(adjustl(charin))
    strOut = string(cleanChars)
  end function sanitize_chars


  subroutine str2int(str, numint, stats)
    !< converts a str into an integer
    !< if not possible stat has a value different from 0
    !<
    !< Source:
    !< https://stackoverflow.com/questions/24071722/converting-a-string-to-an-integer-in-fortran-90/24077338
    type(string), intent(in) :: str(:)
    integer, allocatable, intent(out) :: numint(:) !< converted value
    integer, allocatable, intent(out) :: stats(:) !< different to 0 if conversion fails

    integer :: i, nelems

    if (allocated(numint)) deallocate(numint)
    if (allocated(stats)) deallocate(stats)

    nelems = size(str)
    allocate(numint(nelems))
    allocate(stats(nelems))

    do i = 1, size(str)
      read(str(i) % val,*,iostat=stats(i))  numint(i)
    enddo
  end subroutine str2int


  function isInteger(str) result (r)
    !< true if the given str can be represented as integer
    type(string), intent(in) :: str(:)
    logical, allocatable :: r(:) !< output

    ! local
    integer, allocatable :: ints(:), stats(:)
    integer :: i, nelems

    nelems = size(str)
    if (allocated(r)) deallocate(r)
    allocate(r(nelems))
    allocate(ints(nelems))
    allocate(stats(nelems))

    call str2int(str, ints, stats)

    do i = 1, size(stats)    
      if (stats(i) == 0) then
        r(i) = .true.
      else
        r(i) = .false.
      endif
    enddo
  end function isInteger

end module SHR_strings_mod
