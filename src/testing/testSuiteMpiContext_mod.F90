!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : SHR_testSuiteMpi_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> testSuiteMpiContext holds mpi info
!------------------------------------------------------------------------------
module SHR_testSuiteMpiContext_mod 
#ifdef ENABLE_MPI
  use mpi
#endif

  implicit none

  private
  public :: testSuiteMpiContext, MPI_ROOT_PROC

  logical, parameter :: ISDEBUG = .false.

  integer, parameter :: MPI_ROOT_PROC = 0 !< master mpi processor
#ifndef ENABLE_MPI
  integer, parameter :: MPI_COMM_NULL = -1
  integer, parameter :: MPI_INFO_NULL = -1
#endif


  type testSuiteMpiContext
    integer :: mpiComm = MPI_COMM_NULL
    integer :: mpiInfo = MPI_INFO_NULL
    integer :: myid !< mpi id proc
    integer :: npes !< number of procs

    integer :: rootProc !< root processor
  contains
    procedure :: isActive
    procedure :: isRootProc
    procedure :: getRank
    procedure :: getSize
    procedure :: getCommunicator
    procedure :: getInfo
    procedure :: bcast_logical, bcast_logical_scalar
    generic :: bcast => bcast_logical, bcast_logical_scalar

    procedure :: gather_logical, gather_logical_scalar
    generic :: gather => gather_logical, gather_logical_scalar

    procedure :: createMpiGroup
    procedure :: delMpiGroup
  end type testSuiteMpiContext


  interface testSuiteMpiContext
    module procedure testSuiteMpiContext_constructor, testSuiteMpiContext_constructor_empty
  end interface 


contains


  type(testSuiteMpiContext) function testSuiteMpiContext_constructor_empty()
    !> testSuiteMpiContext empty constructor
    integer :: ierr

    testSuiteMpiContext_constructor_empty % rootProc = -1

    testSuiteMpiContext_constructor_empty % mpiComm = MPI_COMM_NULL
    testSuiteMpiContext_constructor_empty % mpiInfo = MPI_INFO_NULL
  end function testSuiteMpiContext_constructor_empty


  type(testSuiteMpiContext) function testSuiteMpiContext_constructor(comm, info)
    !> testSuiteMpiContext constructor
    integer, intent(in) :: comm
    integer, intent(in) :: info

    integer :: ierr

    testSuiteMpiContext_constructor % rootProc = MPI_ROOT_PROC

    testSuiteMpiContext_constructor % mpiComm = comm
    testSuiteMpiContext_constructor % mpiInfo = info
#ifdef ENABLE_MPI
    call MPI_COMM_RANK(comm, testSuiteMpiContext_constructor % myid, ierr)
    call MPI_COMM_SIZE(comm, testSuiteMpiContext_constructor % npes, ierr)
#endif
  end function testSuiteMpiContext_constructor


  logical function isRootProc(self)
    !< true if current MPI processor is root 
    class(testSuiteMpiContext), intent(in) :: self

    isRootProc = ( self % myId == MPI_ROOT_PROC )
  end function isRootProc


  integer function getSize(self)
    !< get total number of procs from mpiComm
    class(testSuiteMpiContext), intent(in) :: self
    getSize = self % npes
  end function getSize


  integer function getRank(self)
    !< get current mpi proc id
    class(testSuiteMpiContext), intent(in) :: self
    getRank = self % myId
  end function getRank


  subroutine bcast_logical_scalar(self, value)
    !< scalar of logical mpi bcast 
    class(testSuiteMpiContext), intent(in) :: self
    logical, intent(inout) :: value

    integer :: ierr, sz

    sz = 1

#ifdef ENABLE_MPI
    call mpi_bcast(value, sz, MPI_LOGICAL, MPI_ROOT_PROC, self % mpiComm, ierr)
#endif
  end subroutine bcast_logical_scalar

  
  subroutine bcast_logical(self, values)
    !< 
    class(testSuiteMpiContext), intent(in) :: self
    logical, intent(inout) :: values(:)

    integer :: ierr, sz

    sz = size(values)

#ifdef ENABLE_MPI
    call mpi_bcast(values, sz, MPI_LOGICAL, MPI_ROOT_PROC, self % mpiComm, ierr)
#endif
  end subroutine bcast_logical


  subroutine gather_logical_scalar(self, value, outValues)
    !< mpi gather wrapper for MpiContext
    class(testSuiteMpiContext), intent(in) :: self
    logical, intent(in) :: value
    logical, allocatable, intent(out) :: outValues(:)

    integer :: ierr, sz, npes

    sz = 1
    npes = self % getSize()
    
    allocate(outValues(npes * sz))

#ifdef ENABLE_MPI
    call mpi_gather(value, sz, MPI_LOGICAL, outValues, sz, &
                   MPI_LOGICAL, self % rootProc, self % mpiComm, ierr)
#endif

  end subroutine gather_logical_scalar


  subroutine gather_logical(self, values, outValues)
    !< mpi gather wrapper for MpiContext
    class(testSuiteMpiContext), intent(in) :: self
    logical, intent(in) :: values(:)
    logical, allocatable, intent(out) :: outValues(:)

    integer :: ierr, sz, npes

    sz = size(values)
    npes = self % getSize()
    
    allocate(outValues(npes * sz))

#ifdef ENABLE_MPI
    call mpi_gather(values, sz, MPI_LOGICAL, outValues, sz, &
                   MPI_LOGICAL, self % rootProc, self % mpiComm, ierr)
#endif

  end subroutine gather_logical


  type(testSuiteMpiContext) function createMpiGroup(self, npes)
    !< create a new mpi group with all procs from 0 to npes - 1  (in order)
    !< it returns a testSuiteMpiContext
    class(testSuiteMpiContext), intent(in) :: self
    integer, intent(in) :: npes !< number of procs to use

    integer :: masterMpiGroup
    integer :: childMpiGroup
    integer :: childMpiComm
    integer :: ierr
    integer, allocatable :: groupIds(:)
    integer :: i

    if (self % mpiComm == MPI_COMM_NULL) then
      write(*,*) self % myid, ":: testSuiteMpiGroup:: createMpiGroup:: Communicator not initialized"
      stop 1
    endif

    ! generate mpi ids from requested procs (npes)
    groupIds = [ (i, i=0, npes -1) ]

#ifdef ENABLE_MPI
    ! get group id from master comm
    call MPI_Comm_group(self % mpiComm, masterMpiGroup, ierr )

    ! which procs belong to new group
    call MPI_Group_incl(masterMpiGroup, npes, groupIds, childMpiGroup, ierr)
#endif

    if (self % myid < npes) then ! only use grouped processors
#ifdef ENABLE_MPI
      call MPI_Comm_create_group(self % mpiComm, childMpiGroup, 0, childMpiComm, ierr) ! only involved procs
#endif

      createMpiGroup = testSuiteMpiContext(childMpiComm, self % mpiInfo)
    else
      createMpiGroup = testSuiteMpiContext()
    endif
    
  end function createMpiGroup


  subroutine delMpiGroup(self)
    !< delete mpi group
    class(testSuiteMpiContext), intent(inout) :: self
        
    integer :: ierr
    integer :: mpiGroup

#ifdef ENABLE_MPI
    ! get group id
    call MPI_Comm_group(self % mpiComm, mpiGroup, ierr )

    call MPI_Comm_free(self % mpiComm, ierr)
    call MPI_Group_free(mpiGroup, ierr)
#endif
  end subroutine delMpiGroup


  logical function isActive(self)
    !< true is current testSuiteMpiContext is active
    !< different procs from same testSuiteMpiContext can be disabled 
    !< For example: 
    !<  when using createMpiGroup 
    class(testSuiteMpiContext), intent(in) :: self
    isActive = (self % mpiComm .ne. MPI_COMM_NULL )
  end function isActive


  integer function getCommunicator(self)
    !< it returns mpi communicator
    class(testSuiteMpiContext), intent(in) :: self
    getCommunicator = self % mpiComm
  end function getCommunicator


  integer function getInfo(self)
    !< it returns mpi info
    class(testSuiteMpiContext), intent(in) :: self
    getInfo = self % mpiInfo
  end function getInfo


end module SHR_testSuiteMpiContext_mod
