!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : file_mod 
!
!> @author
!> 
!
! DESCRIPTION:
!> 
!------------------------------------------------------------------------------
!
module file_mod

  use error_mod, only: raiseError
  use strings_mod, only: string

  implicit none 

  public :: file_t, removeIfExists

  public :: FILE_NAME_MAX_LEN


  integer, parameter :: FILE_NAME_MAX_LEN = 100


  type file_t
    character(len=:), allocatable :: name
  contains
    procedure :: exists !< true if the file exists 
    procedure :: remove
    procedure :: getName

    procedure :: equiv_file_t
    generic :: operator(==) => equiv_file_t
  end type file_t 

  interface file_t
    module procedure :: file_constructor_char, file_constructor_string
  end interface file_t

contains

  elemental pure type(file_t) function file_constructor_char(fname)
    !< file_t constructor
    character(len=*), intent(in) :: fname !< file name
    file_constructor_char % name = fname
  end function file_constructor_char


  elemental pure type(file_t) function file_constructor_string(fname)
    !< file_t constructor
    type(string), intent(in) :: fname !< file name
    file_constructor_string % name = fname
  end function file_constructor_string


  impure elemental logical function exists(self)
    !< true if the file exists
    class(file_t), intent(in) :: self

    inquire(file=self % name, exist=exists)
  end function exists 


  subroutine remove(self)
    !< remove file. 
    !< If it does not exists nothing happens.
    class(file_t), intent(inout) :: self
    integer, parameter :: fid = 1234
    integer :: stat

    open(unit=fid, iostat=stat, file=self % name, status='old')
    if (stat == 0) close(fid, status='delete')
  end subroutine remove


  subroutine removeIfExists(filename)
    !< remove file if it is found. Nothing happens otherwise
    character(len=*), intent(in) :: filename

    integer, parameter :: fid = 1234
    integer :: stat

    ! remove file if exists
    open(unit=fid, iostat=stat, file=filename, status='old')
    if (stat == 0) close(fid, status='delete')
  end subroutine removeIfExists


  pure elemental logical function equiv_file_t(self, other)
    !< true if self and other have the same characterisics
    class(file_t), intent(in) :: self, other

    logical :: hasSameName

    hasSameName = (self % name == other % name)
    equiv_file_t = hasSameName 
  end function equiv_file_t


  pure elemental function getName(self) result (r)
    !< it returns the filename from the file
    class(file_t), intent(in) :: self
    character(len=FILE_NAME_MAX_LEN) :: r
    r = self % name
  end function getName

end module file_mod
