!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  SHR_logging_mod
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!> Logging  
!>
!> It allows sequential and parallel
!>
!> proc id:: filename:: subroutine name:: message = value
!>
!> Example (mpi):
!>      0 :: logging_mod:: write_char:: found values = hello
!>
!> Example:
!>      logging_mod:: write_char:: found values = hello
!> 
!------------------------------------------------------------------------------
module SHR_logging_mod

#ifdef ENABLE_MPI
  use mpi
#endif
  use SHR_precision_mod, only: sp, dp, eqReal
  use SHR_error_mod, only: raiseError 

  implicit none

  private

  public :: logging 

!  integer, parameter :: LOGGING_LEV_NONE = 0 !< nothing printed
!  integer, parameter :: LOGGING_LEV_MESG = 1 !< it only shows mesg type level
!  integer, parameter :: LOGGING_LEV_DEBUG = 2 !< it show MESG and DEBUG write messages

  type logging
    character(len=:), allocatable :: name !< logging name
    character(len=:), allocatable :: subname !< current subroutine name
    character(len=:), allocatable :: filename !< current file name
    logical :: isEnabled
!    integer :: level !< 0-None, 1-logging , 2-debugging

    integer :: myId !< mpi proc id
    integer :: npes !< total number of mpi procs
    integer :: mpiComm
    integer :: mpiInfo
  contains
    procedure :: setContext
    procedure :: write_char
    procedure :: write_real, write_reals
    procedure :: write_logicals, write_logical
    procedure :: write_ints, write_int
    procedure :: write_mesg
    generic :: write => write_mesg, write_char, write_real, write_reals, &
                         write_ints, write_int, write_logicals, write_logical
  end type logging

  interface logging
    module procedure logging_constructor
  end interface logging
contains


  type(logging) function logging_constructor(name, isEnabled, mpiComm, mpiInfo)
    !< logging constructor
    !< 
    !< To enable mpi mpiComm and mpiInfo must both be defined
    character(len=*), intent(in) :: name !< unique logging name
    logical, intent(in) :: isEnabled
    integer, intent(in), optional :: mpiComm, mpiInfo

    integer :: ierr

    logging_constructor % name = name
    logging_constructor % isEnabled = isEnabled

    logging_constructor % myId = 0 !< default
    logging_constructor % npes = 1 !< default
    logging_constructor % mpiComm = -1 ! default
    logging_constructor % mpiInfo = -1 ! default
    if (present(mpiComm) .and. present(mpiInfo)) then
#ifdef ENABLE_MPI
      call MPI_COMM_RANK(mpiComm, logging_constructor % myId, ierr)
      call MPI_COMM_SIZE(mpiComm, logging_constructor % npes, ierr)
      logging_constructor % mpiComm = mpiComm
      logging_constructor % mpiInfo = mpiInfo
#else
      call raiseError(__FILE__, "logging_constructor", "Mpi not included", &
              "Compile with '-DENABLE_MPI=yes' enabled")
#endif
    endif

  end function logging_constructor


  subroutine setContext(self, filename, subname)
    !< Change filename or sub name contexts
    class(logging), intent(inout) :: self
    character(len=*), intent(in), optional :: filename !< set new filename context
    character(len=*), intent(in), optional :: subname !< set new subroutine context

    if (present(filename)) then
      self % filename = filename
    endif

    if (present(subname)) then
      self % subname = subname
    endif
  end subroutine setContext


  subroutine write_mesg(self, mesg)
    !< write 
    class(logging), intent(inout) :: self
    character(len=*), intent(in) :: mesg !< 

    if (.not. self % isEnabled) return
#ifdef ENABLE_MPI
    write(*,*) self % myId, ":: "//self % filename//" "//self % subname// ":: "//mesg
#else
    write(*,*) self % filename//" "//self % subname// ":: "//mesg
#endif
  end subroutine write_mesg


  subroutine write_char(self, mesg, val)
    !< write 
    class(logging), intent(inout) :: self
    character(len=*), intent(in) :: mesg !< 
    character(len=*), intent(in) :: val

    if (.not. self % isEnabled) return
#ifdef ENABLE_MPI
    write(*,*) self % myId, ":: "//self % filename//" "//self % subname// ":: "//mesg//" = "//val
#else
    write(*,*) self % filename//" "//self % subname// ":: "//mesg//" = "//val
#endif
  end subroutine write_char


  subroutine write_real(self, mesg, val)
    !< write 
    class(logging), intent(inout) :: self
    character(len=*), intent(in) :: mesg !< 
    real(kind=sp), intent(in) :: val

    if (.not. self % isEnabled) return
#ifdef ENABLE_MPI
    write(*,*) self % myId, ":: "//self % filename//" "//self % subname// ":: "//mesg//" = ", val
#else
    write(*,*) self % filename//" "//self % subname// ":: "//mesg//" = ", val
#endif
  end subroutine write_real


  subroutine write_reals(self, mesg, val)
    !< write 
    class(logging), intent(inout) :: self
    character(len=*), intent(in) :: mesg !< 
    real(kind=sp), intent(in) :: val(:)

    if (.not. self % isEnabled) return
#ifdef ENABLE_MPI
    write(*,*) self % myId, ":: "//self % filename//" "//self % subname// ":: "//mesg//" = ", val
#else
    write(*,*) self % filename//" "//self % subname// ":: "//mesg//" = ", val
#endif
  end subroutine write_reals


  subroutine write_ints(self, mesg, val)
    !< write int array
    class(logging), intent(inout) :: self
    character(len=*), intent(in) :: mesg !< 
    integer, intent(in) :: val(:)

    if (.not. self % isEnabled) return
#ifdef ENABLE_MPI
    write(*,*) self % myId, ":: "//self % filename//" "//self % subname// ":: "//mesg//" = ", val
#else
    write(*,*) self % filename//" "//self % subname// ":: "//mesg//" = ", val
#endif
  end subroutine write_ints


  subroutine write_int(self, mesg, val)
    !< write int scalar
    class(logging), intent(inout) :: self
    character(len=*), intent(in) :: mesg !< 
    integer, intent(in) :: val

    if (.not. self % isEnabled) return
#ifdef ENABLE_MPI
    write(*,*) self % myId, ":: "//self % filename//" "//self % subname// ":: "//mesg//" = ", val
#else
    write(*,*) self % filename//" "//self % subname// ":: "//mesg//" = ", val
#endif
  end subroutine write_int


  subroutine write_logical(self, mesg, val)
    !< write logical array 
    class(logging), intent(inout) :: self
    character(len=*), intent(in) :: mesg !< 
    logical, intent(in) :: val

    if (.not. self % isEnabled) return
#ifdef ENABLE_MPI
    write(*,*) self % myId, ":: "//self % filename//" "//self % subname// ":: "//mesg//" = ", val
#else
    write(*,*) self % filename//" "//self % subname// ":: "//mesg//" = ", val
#endif
  end subroutine write_logical


  subroutine write_logicals(self, mesg, val)
    !< write logical array 
    class(logging), intent(inout) :: self
    character(len=*), intent(in) :: mesg !< 
    logical, intent(in) :: val(:)

    if (.not. self % isEnabled) return
#ifdef ENABLE_MPI
    write(*,*) self % myId, ":: "//self % filename//" "//self % subname// ":: "//mesg//" = ", val
#else
    write(*,*) self % filename//" "//self % subname// ":: "//mesg//" = ", val
#endif
  end subroutine write_logicals


end module SHR_logging_mod
