!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : file_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> file_mod unit tests
!------------------------------------------------------------------------------
module file_test
  use SHR_file_mod, only: file_t, removeIfExists
  use SHR_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuiteFile

  type, extends(testSuite) :: testSuiteFile

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine createTextFile(filename)
    !< create a text file with text
    character(*), intent(in) :: filename
    integer :: fid
    open(fid, file=filename, action='write', position='append')
    write(fid,*) 'Hello'
    write(fid,*) 'This is a test file'
    close(fid)
  end subroutine createTextFile


  subroutine removeTextFile(filename)
    !< it removes 'filename' file
    character(*), intent(in) :: filename
    integer :: fid
    integer :: stat
    open(unit=fid, iostat=stat, file=filename, status='old')
    if (stat == 0) close(fid, status='delete')
  end subroutine removeTextFile


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteFile), intent(inout) :: self
    type(file_t) :: f, ef

    character(*), parameter :: EXISTFILE = "sometext.txt"
    character(*), parameter :: NOFILE = "file.txt"

    ! set up
    call createTextFile(EXISTFILE)

    ! removeIfExists
    !
    ! file_t
    f = file_t(NOFILE)
    ef = file_t(EXISTFILE)

    ! exists
    call self % assert(.not. f % exists(), "file(test.txt) % exists() == F")
    call self % assert(ef % exists(), "file(sometext.txt) % exists() == T")

    ! remove
    call ef % remove()
    call self % assert(.not. ef % exists(), "file(sometext.txt) % exists() == F")

    ! getName
    call self % assert(f % getName() == NOFILE, "file(test.txt) % getName() .eq. 'test.txt' == T")
    call self % assert(ef % getName() == EXISTFILE, "file(sometext.txt) % getName() .eq. 'sometext.txt' == T")

    ! ==
    call self % assert(.not. f == ef, "file(test.txt) .eq. file(sometext.txt) == T")
    call self % assert(f == f, "file(test.txt) .eq. file(test.txt) == T")
    call self % assert(ef == ef, "file(sometext.txt) .eq. file(sometext.txt) == T")

    ! tear down
    call removeTextFile(EXISTFILE)

  end subroutine defineTestCases

end module file_test

