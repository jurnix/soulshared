!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  array_mod
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!> Common Array subroutines
!>
!> array class (real sp, real dp, int)
!> 
!------------------------------------------------------------------------------
module SHR_array_mod

  use SHR_precision_mod, only: sp, dp, eqReal
  use SHR_error_mod, only: raiseError 
  use SHR_strings_mod, only: string

  implicit none

  private

  public :: closestNumber2Index, PREFER_FIRST, PREFER_LAST, unique, initArrayRange

  public :: absArray , allocAbsArray, sp_rpArray !, ipArray, dp_rpReal, sp_rpReal !< pointer array

  public ::            pAbsArray, sp_rArray !, iArray,  dp_rArray, sp_rArray !< allocatable array

  public :: sp_pointer_rArray_constructor_data_r1 ! debug only, todo to remove
  public :: sp_pointer_rArray_constructor_data_r3 ! debug only, todo to remove
  public :: min, max

  interface initArrayRange
    module procedure initArrayRange_r1d
  end interface

  ! overload min built in subroutine to accept absArray
  interface min
    module procedure :: min_sp_AbsAllocArray_vs_sp_AbsPointArray
  end interface min

  ! overload min built in subroutine to accept absArray
  interface max
    module procedure :: max_sp_AbsAllocArray_vs_sp_AbsPointArray
  end interface max


  integer, parameter :: PREFER_FIRST=1
  integer, parameter :: PREFER_LAST=2

  type, abstract :: absArray
    character(len=:), allocatable :: name
    type(string), allocatable :: dimsName(:) !< A name for each position. 
                                             !< The order fo the array matches the array order.
  end type absArray


  ! https://stackoverflow.com/questions/10032693/how-do-i-overload-an-operator-for-a-derived-type-which-extends-an-abstract-type
  ! https://stackoverflow.com/questions/53047689/derived-types-with-allocatable-arrays-and-overload-operators-in-fortran
  type, abstract, extends(absArray) :: allocAbsArray !< allocatable array
  contains
    ! add
    procedure(iface_allocAbsArray_add_with_pAbsArray), deferred :: add_allocAbsArray_with_pAbsArray
    generic, public :: operator(+) => add_allocAbsArray_with_pAbsArray

    ! div(absArray vs int)
    procedure(iface_div_allocAbsArray_with_int), deferred :: div_allocAbsArray_with_int
    generic, public :: operator(/) => div_allocAbsArray_with_int
    ! div(absArray vs sp real)
    procedure(iface_div_allocAbsArray_with_spReal), deferred :: div_allocAbsArray_with_spReal
    generic, public :: operator(/) => div_allocAbsArray_with_spReal

    ! assign(=)
    procedure(iface_assign_allocAbsArray_with_spReal), deferred :: assign_allocAbsArray_with_spReal
    procedure(iface_assign_allocAbsArray_with_spPointerReal), deferred :: assign_allocAbsArray_with_spPointerReal
    generic, public :: assignment(=) => assign_allocAbsArray_with_spReal
    generic, public :: assignment(=) => assign_allocAbsArray_with_spPointerReal
  end type allocAbsArray

  type, abstract, extends(absArray) :: pAbsArray !< pointer to array
  end type pAbsArray


  ! todo - add dp real, add int
  type, extends(pAbsArray) :: sp_rpArray !< real single precision pointer array
    real(kind=sp), pointer, contiguous :: r1(:) => null()
    real(kind=sp), pointer, contiguous :: r2(:,:) => null()
    real(kind=sp), pointer, contiguous :: r3(:,:,:) => null()
    real(kind=sp), pointer, contiguous :: r4(:,:,:,:) => null()
    real(kind=sp), pointer, contiguous :: r5(:,:,:,:,:) => null()
  contains
    procedure :: getArray_sp_pointer_r1
    generic :: getArray => getArray_sp_pointer_r1
  end type sp_rpArray

  interface sp_rpArray
    module procedure :: sp_pointer_rArray_constructor_data_r1, &
           sp_pointer_rArray_constructor_data_r2, sp_pointer_rArray_constructor_data_r3, &
           sp_pointer_rArray_constructor_data_r4, sp_pointer_rArray_constructor_data_r5
  end interface sp_rpArray

  ! data types
  ! todo - add dp real, add int
  type, extends(allocAbsArray) :: sp_rArray !< allocatable single precision array
    real(kind=sp), allocatable :: r1(:)
    real(kind=sp), allocatable :: r2(:,:)
    real(kind=sp), allocatable :: r3(:,:,:)
    real(kind=sp), allocatable :: r4(:,:,:,:)
    real(kind=sp), allocatable :: r5(:,:,:,:,:)
  contains
    procedure :: getArray_sp_alloc_r1
    generic :: getArray => getArray_sp_alloc_r1

    procedure :: add_allocAbsArray_with_pAbsArray => sp_rArray_add_alloc_pointer

    procedure :: div_allocAbsArray_with_int => sp_rArray_div_spInt_i1
    procedure :: div_allocAbsArray_with_spReal => sp_rArray_div_spReal_r1
    
    procedure :: getData_r1_sp_rArray
    generic, public :: getData => getData_r1_sp_rArray

    procedure :: assign_allocAbsArray_with_spReal => assign_sp_rArray_with_spReal
    procedure :: assign_allocAbsArray_with_spPointerReal => assign_sp_rArray_with_spPointerArray
  end type sp_rArray

  interface sp_rArray
    module procedure :: sp_rArray_constructor_def, sp_rArray_constructor_data_r1, &
                sp_rArray_constructor_data_r2, sp_rArray_constructor_data_r3, &
                sp_rArray_constructor_data_r4, sp_rArray_constructor_data_r5
  end interface sp_rArray

  abstract interface
    ! add alloc absArray vs pointer absArray
    function iface_allocAbsArray_add_with_pAbsArray(left,right) result (total)
      import :: allocAbsArray, pAbsArray
      class(allocAbsArray), intent(in) :: left
      class(pAbsArray), intent(in) :: right
      class(allocAbsArray), allocatable :: total
    end function iface_allocAbsArray_add_with_pAbsArray

    ! div absArray vs real
    function iface_div_allocAbsArray_with_spReal(left,right) result (total)
      import :: allocAbsArray, sp
      class(allocAbsArray), intent(in) :: left
      real(kind=sp), intent(in) :: right
      class(allocAbsArray), allocatable :: total
    end function iface_div_allocAbsArray_with_spReal

    ! div absArray vs int
    function iface_div_allocAbsArray_with_int(left,right) result (total)
      import :: allocAbsArray, sp
      class(allocAbsArray), intent(in) :: left
      integer, intent(in) :: right
      class(allocAbsArray), allocatable :: total
    end function iface_div_allocAbsArray_with_int

    ! assign absArray vs real
    subroutine iface_assign_allocAbsArray_with_spReal(this, from) 
      import allocAbsArray, sp
      class(allocAbsArray), intent(inout) :: this
      real(kind=sp), intent(in) :: from
    end subroutine iface_assign_allocAbsArray_with_spReal

    ! assign absArray vs sp_rpArray
    subroutine iface_assign_allocAbsArray_with_spPointerReal(this, from) 
      import allocAbsArray, pAbsArray 
      class(allocAbsArray), intent(inout) :: this
      class(pAbsArray), intent(in) :: from
    end subroutine iface_assign_allocAbsArray_with_spPointerReal
  end interface 

contains

  subroutine getData_r1_sp_rArray(self, data)
    !< return calculated time step data 
    class(sp_rArray), intent(inout) :: self
    real(kind=sp), allocatable, intent(out) :: data(:)

    if (allocated(data)) deallocate(data)
    allocate(data, source=self % r1)
!    write(*,*) "array_mod:: getData_r1_sp_rArray:: data=", data
!    write(*,*) "array_mod:: getData_r1_sp_rArray:: self % r1=", self % r1
  end subroutine getData_r1_sp_rArray

  
  function sp_rArray_div_spInt_i1(left, right) result (total)
    !< sum sp_rArray and sp_rpArray
    class(sp_rArray), intent(in) :: left
    integer, intent(in) :: right
    class(allocAbsArray), allocatable :: total !< output

!    write (*,*) "array_mod:: sp_rArray_div_spInt_i1:: entering..."

    allocate(total, mold=left)
    select type (total)
    type is (sp_rArray)
      total % r1 = left % r1 / right
!      write (*,*) "array_mod:: sp_rArray_div_spInt_i1:: left / right =", left % r1, right
    class default
      ! raise error not implemented
    end select ! total
!    write (*,*) "array_mod:: sp_rArray_div_spInt_i1:: done"

  end function sp_rArray_div_spInt_i1

  
  function sp_rArray_div_spReal_r1(left, right) result (total)
    !< sum sp_rArray and sp_rpArray
    class(sp_rArray), intent(in) :: left
    real(kind=sp), intent(in) :: right
    class(allocAbsArray), allocatable :: total !< output

    allocate(total, mold=left)
    select type (total)
    type is (sp_rArray)
      total % r1 = left % r1 / right
!      write (*,*) "array_mod:: sp_rArray_div_spReal_r1:: left / right =", left % r1, right
    class default
      ! raise error not implemented
    end select ! total

  end function sp_rArray_div_spReal_r1


  function sp_rArray_add_alloc_pointer(left, right) result (total)
    !< sum sp_rArray and sp_rpArray
    class(sp_rArray), intent(in) :: left
    class(pAbsArray), intent(in) :: right
    class(allocAbsArray), allocatable :: total !< output

!    write (*,*) "Array_mod:: sp_rArray_add_alloc_pointer:: entering..."
    if (.not. allocated(total)) allocate(total, mold=left)

    select type (right)
    type is (sp_rpArray)
!      write (*,*) "Array_mod:: sp_rArray_add_alloc_pointer:: (right)sp_rpArray found"
!      write (*,*) "Array_mod:: sp_rArray_add_alloc_pointer:: left alloc=", allocated(left % r1)
!      write (*,*) "Array_mod:: sp_rArray_add_alloc_pointer:: right assoc=", associated(right % r1)
!      write (*,*) "Array_mod:: sp_rArray_add_alloc_pointer:: total alloc=", allocated(total)
      select type (total)
      type is (sp_rArray)
!          write (*,*) "Array_mod:: sp_rArray_add_alloc_pointer:: sp_rArray found"
!          write (*,*) "Array_mod:: sp_rArray_add_alloc_pointer:: total alloc=", allocated(total % r1)
          if (allocated(left % r1)) then
!            write (*,*) "array_mod:: sp_rarray_add_alloc_pointer:: before add"
!            write (*,*) "array_mod:: sp_rarray_add_alloc_pointer:: left=", left % r1
!            write (*,*) "array_mod:: sp_rarray_add_alloc_pointer:: right=", right % r1
            total % r1 = left % r1 + right % r1
!            write (*,*) "array_mod:: sp_rarray_add_alloc_pointer:: total=", total % r1
!            write (*,*) "Array_mod:: sp_rArray_add_alloc_pointer:: after add"
          else if (allocated(left % r2)) then
            total % r2 = left % r2 + right % r2
          else if (allocated(left % r3)) then
            total % r3 = left % r3 + right % r3
          else if (allocated(left % r4)) then
            total % r4 = left % r4 + right % r4
          else if (allocated(left % r5)) then
            total % r5 = left % r5 + right % r5
          else
            ! error here, dimension not implemented
          endif
      class default
        ! error here, type not implemented
        call raiseError(__FILE__, "sp_rArray_add_alloc_pointer", &
                "'total' class type not found")
      end select ! sp_rArray
    class default 
      ! error here, type not implement
    end select ! left
!    write (*,*) "Array_mod:: sp_rArray_add_alloc_pointer:: done"

  end function sp_rArray_add_alloc_pointer


  subroutine getArray_sp_pointer_r1(self, odata)
    ! Return real(sp) data array
    class(sp_rpArray), intent(inout) :: self
    real(kind=sp), pointer, intent(out) :: odata(:)

    odata => self % r1
  end subroutine getArray_sp_pointer_r1


  subroutine getArray_sp_alloc_r1(self, odata)
    ! Return real(sp) data array
    class(sp_rArray), intent(inout) :: self
    real(kind=sp), allocatable, intent(out) :: odata(:)

    if (allocated(odata)) deallocate(odata)
    odata = self % r1

  end subroutine getArray_sp_alloc_r1


  type(sp_rArray) function sp_rArray_constructor_data_r5(name, dimsName, data)
    !< initialize a sp_rArray with a given name, dimensions name and its data
    !< the given data is fully copied into a new place
    !< dimsName and data must have the same number of dimensions
    character(len=*), intent(in) :: name
    type(string), intent(in) :: dimsName(:) !< name of each dimension
    real(kind=sp), intent(in) :: data(:,:,:,:,:)

    sp_rArray_constructor_data_r5 % name = name
    sp_rArray_constructor_data_r5 % dimsName = dimsName
    sp_rArray_constructor_data_r5 % r5 = data

  end function sp_rArray_constructor_data_r5


  type(sp_rArray) function sp_rArray_constructor_data_r4(name, dimsName, data)
    !< initialize a sp_rArray with a given name, dimensions name and its data
    !< the given data is fully copied into a new place
    !< dimsName and data must have the same number of dimensions
    character(len=*), intent(in) :: name
    type(string), intent(in) :: dimsName(:) !< name of each dimension
    real(kind=sp), intent(in) :: data(:,:,:,:)

    sp_rArray_constructor_data_r4 % name = name
    sp_rArray_constructor_data_r4 % dimsName = dimsName
    sp_rArray_constructor_data_r4 % r4 = data

  end function sp_rArray_constructor_data_r4


  type(sp_rArray) function sp_rArray_constructor_data_r3(name, dimsName, data)
    !< initialize a sp_rArray with a given name, dimensions name and its data
    !< the given data is fully copied into a new place
    !< dimsName and data must have the same number of dimensions
    character(len=*), intent(in) :: name
    type(string), intent(in) :: dimsName(:) !< name of each dimension
    real(kind=sp), intent(in) :: data(:,:,:)

    sp_rArray_constructor_data_r3 % name = name
    sp_rArray_constructor_data_r3 % dimsName = dimsName
    sp_rArray_constructor_data_r3 % r3 = data

  end function sp_rArray_constructor_data_r3


  type(sp_rArray) function sp_rArray_constructor_data_r2(name, dimsName, data)
    !< initialize a sp_rArray with a given name, dimensions name and its data
    !< the given data is fully copied into a new place
    !< dimsName and data must have the same number of dimensions
    character(len=*), intent(in) :: name
    type(string), intent(in) :: dimsName(:) !< name of each dimension
    real(kind=sp), intent(in) :: data(:,:)

    sp_rArray_constructor_data_r2 % name = name
    sp_rArray_constructor_data_r2 % dimsName = dimsName
    sp_rArray_constructor_data_r2 % r2 = data

  end function sp_rArray_constructor_data_r2


  type(sp_rArray) function sp_rArray_constructor_data_r1(name, dimsName, data)
    !< initialize a sp_rArray with a given name, dimensions name and its data
    !< the given data is fully copied into a new place
    !< dimsName and data must have the same number of dimensions
    character(len=*), intent(in) :: name
    type(string), intent(in) :: dimsName(:) !< name of each dimension
    real(kind=sp), intent(in) :: data(:)

    sp_rArray_constructor_data_r1 % name = name
    sp_rArray_constructor_data_r1 % dimsName = dimsName
    sp_rArray_constructor_data_r1 % r1 = data
!    write (*,*) "array_mod:: sp_alloc_rArray_constructor_data_r1:: data=", data

  end function sp_rArray_constructor_data_r1


  type (sp_rArray) function sp_rArray_constructor_def(name, dimsName, dimsSize, initValues )
    !< initialize a sp_rArray with a given name, the dimensions info and an initial value
    !< dimsName and dimsSize must have the same size
    character(len=*), intent(in) :: name
    type(string), intent(in) :: dimsName(:) !< name of each dimension
    integer, intent(in) :: dimsSize(:) !< name of each dimension
    real(kind=sp), intent(in) :: initValues !< provide an initialization name

    integer :: ndims
    character(len=15) :: tmp, tmp1
    character(len=*), parameter :: SNAME = "sp_rArray_constructor_def"

    ndims = size(dimsName)

    if (size(dimsName) .ne. size(dimsSize)) then
      write(tmp, *)  size(dimsName)
      write(tmp1, *) size(dimsSize)
      call raiseError(__FILE__, SNAME, &
                      "dimsName and dimsSize must have the same size", &
                      "dimsName size="//trim(adjustl(tmp))//" , dimsSize size="//trim(adjustl(tmp1)) )
    endif

    sp_rArray_constructor_def % name = name
    sp_rArray_constructor_def % dimsName = dimsName

    if (ndims == 1) then
      allocate(sp_rArray_constructor_def % r1(dimsSize(1)))
      sp_rArray_constructor_def % r1 = initValues
    else if (ndims == 2) then
      allocate(sp_rArray_constructor_def % r2(dimsSize(1), dimsSize(2)))
      sp_rArray_constructor_def % r2 = initValues
    else if (ndims == 3) then
      allocate(sp_rArray_constructor_def % r3(dimsSize(1), dimsSize(2), dimsSize(3)))
      sp_rArray_constructor_def % r3 = initValues
    else if (ndims == 4) then
      allocate(sp_rArray_constructor_def % r4(dimsSize(1), dimsSize(2), dimsSize(3), &
                                          dimsSize(4) ))
      sp_rArray_constructor_def % r4 = initValues
    else if (ndims == 5) then
      allocate(sp_rArray_constructor_def % r5(dimsSize(1), dimsSize(2), dimsSize(3), &
                                          dimsSize(4), dimsSize(5)))
      sp_rArray_constructor_def % r5 = initValues
    else 
      ! not supported
      write(tmp, *) ndims
      call raiseError(__FILE__, SNAME, "Dimensions not implemented: "//tmp)
    endif

  end function sp_rArray_constructor_def


  type(sp_rpArray) function sp_pointer_rArray_constructor_data_r5(name, dimsName, data)
    !< initialize a sp_rArray with a given name, dimensions name and its data
    !< the given data is fully copied into a new place
    !< dimsName and data must have the same number of dimensions
    character(len=*), intent(in) :: name
    type(string), intent(in) :: dimsName(:) !< name of each dimension
    real(kind=sp), target, contiguous, intent(in) :: data(:,:,:,:,:)

    sp_pointer_rArray_constructor_data_r5 % name = name
    sp_pointer_rArray_constructor_data_r5 % dimsName = dimsName
    sp_pointer_rArray_constructor_data_r5 % r5 => data

  end function sp_pointer_rArray_constructor_data_r5


  type(sp_rpArray) function sp_pointer_rArray_constructor_data_r4(name, dimsName, data)
    !< initialize a sp_rArray with a given name, dimensions name and its data
    !< the given data is fully copied into a new place
    !< dimsName and data must have the same number of dimensions
    character(len=*), intent(in) :: name
    type(string), intent(in) :: dimsName(:) !< name of each dimension
    real(kind=sp), target, contiguous, intent(in) :: data(:,:,:,:)

    sp_pointer_rArray_constructor_data_r4 % name = name
    sp_pointer_rArray_constructor_data_r4 % dimsName = dimsName
    sp_pointer_rArray_constructor_data_r4 % r4 => data

  end function sp_pointer_rArray_constructor_data_r4


  type(sp_rpArray) function sp_pointer_rArray_constructor_data_r3(name, dimsName, data)
    !< initialize a sp_rArray with a given name, dimensions name and its data
    !< the given data is fully copied into a new place
    !< dimsName and data must have the same number of dimensions
    character(len=*), intent(in) :: name
    type(string), intent(in) :: dimsName(:) !< name of each dimension
    real(kind=sp), target, contiguous, intent(in) :: data(:,:,:)

    sp_pointer_rArray_constructor_data_r3 % name = name
    sp_pointer_rArray_constructor_data_r3 % dimsName = dimsName
    sp_pointer_rArray_constructor_data_r3 % r3 => data
!    write(*,*) "sp_pointer_rArray_constructor_data_r3:: data=", data

  end function sp_pointer_rArray_constructor_data_r3


  type(sp_rpArray) function sp_pointer_rArray_constructor_data_r2(name, dimsName, data)
    !< initialize a sp_rArray with a given name, dimensions name and its data
    !< the given data is fully copied into a new place
    !< dimsName and data must have the same number of dimensions
    character(len=*), intent(in) :: name
    type(string), intent(in) :: dimsName(:) !< name of each dimension
    real(kind=sp), target, contiguous, intent(in) :: data(:,:)

    sp_pointer_rArray_constructor_data_r2 % name = name
    sp_pointer_rArray_constructor_data_r2 % dimsName = dimsName
    sp_pointer_rArray_constructor_data_r2 % r2 => data

  end function sp_pointer_rArray_constructor_data_r2


  type(sp_rpArray) function sp_pointer_rArray_constructor_data_r1(name, dimsName, data)
    !< initialize a sp_rArray with a given name, dimensions name and its data
    !< the given data is fully copied into a new place
    !< dimsName and data must have the same number of dimensions
    character(len=*), intent(in) :: name
    type(string), intent(in) :: dimsName(:) !< name of each dimension
    real(kind=sp), contiguous, target, intent(in) :: data(:)

!    write(*,*) "sp_pointer_rArray_constructor_data_r1:: data=", data
    sp_pointer_rArray_constructor_data_r1 % name = name
    sp_pointer_rArray_constructor_data_r1 % dimsName = dimsName
    sp_pointer_rArray_constructor_data_r1 % r1 => data

  end function sp_pointer_rArray_constructor_data_r1


  function min_sp_AbsAllocArray_vs_sp_AbsPointArray(absAlloc, absPoint) result (minTotal)
    !< find minimum values between absAlloc and absPoint
    class(allocAbsArray), intent(in) :: absAlloc !< 
    class(pAbsArray), intent(in) :: absPoint
    class(allocAbsArray), allocatable :: minTotal !< output

!    write(*,*) "array_mod:: min_sp_AbsAllocArray_vs_sp_AbsPointArray:: enter..."

    select type (absAlloc)
    type is (sp_rArray)
      select type (absPoint)
      type is (sp_rpArray)
        minTotal = min_sp_realAllocArray_vs_sp_realPointArray(absAlloc, absPoint)
      class default
        call raiseError(__FILE__, "min_sp_AbsAllocArray_vs_sp_AbsPointArray", &
                "Unexpected type", "Required sp_rpArray")
      end select ! absPoint
    class default
      call raiseError(__FILE__, "min_sp_AbsAllocArray_vs_sp_AbsPointArray", &
                "Unexpected type", "sp_rArray")
    end select ! absAlloc
!    write(*,*) "array_mod:: min_sp_AbsAllocArray_vs_sp_AbsPointArray:: done"
  end function min_sp_AbsAllocArray_vs_sp_AbsPointArray


  function min_sp_realAllocArray_vs_sp_realPointArray(spRAlloc, spRPoint) result (minTotal)
    class(sp_rArray), intent(in) :: spRAlloc
    class(sp_rpArray), intent(in) :: spRPoint
    class(sp_rArray), allocatable :: minTotal

!    write(*,*) "array_mod:: min_sp_realAllocArray_vs_sp_realPointArray:: entering..."

    if (allocated(minTotal)) deallocate(minTotal)
    ! init and initialize with dimensions (not values)
    allocate(minTotal, mold=spRAlloc)

    if (allocated(spRAlloc % r1)) then
!      write(*,*) "array_mod:: min_sp_realAllocArray_vs_sp_realPointArray:: spRAlloc=", spRAlloc % r1
!      write(*,*) "array_mod:: min_sp_realAllocArray_vs_sp_realPointArray:: spRPoint=", spRPoint % r1
      minTotal % r1 = min(spRAlloc % r1, spRPoint % r1)
    else if (allocated(spRAlloc % r2)) then
      minTotal % r2 = min(spRAlloc % r2, spRPoint % r2)
    else if (allocated(spRAlloc % r3)) then
      minTotal % r3 = min(spRAlloc % r3, spRPoint % r3)
    else if (allocated(spRAlloc % r4)) then
      minTotal % r4 = min(spRAlloc % r4, spRPoint % r4)
    else if (allocated(spRAlloc % r5)) then
      minTotal % r5 = min(spRAlloc % r5, spRPoint % r5)
    else 
      ! raise error, dimension not implemented 
    endif
!    write(*,*) "array_mod:: min_sp_realAllocArray_vs_sp_realPointArray:: done"

  end function min_sp_realAllocArray_vs_sp_realPointArray


  function max_sp_AbsAllocArray_vs_sp_AbsPointArray(absAlloc, absPoint) result (maxTotal)
    !< find minimum values between absAlloc and absPoint
    class(allocAbsArray), intent(in) :: absAlloc !< 
    class(pAbsArray), intent(in) :: absPoint
    class(allocAbsArray), allocatable :: maxTotal !< output

    select type (absAlloc)
    type is (sp_rArray)
      select type (absPoint)
      type is (sp_rpArray)
        maxTotal = max_sp_realAllocArray_vs_sp_realPointArray(absAlloc, absPoint)
      end select ! absPoint
    end select ! absAlloc
  end function max_sp_AbsAllocArray_vs_sp_AbsPointArray


  function max_sp_realAllocArray_vs_sp_realPointArray(spRAlloc, spRPoint) result (maxTotal)
    !< returns an allocatable array with the mininum values between spRAlloc and spRPoint
    class(sp_rArray), intent(in) :: spRAlloc
    class(sp_rpArray), intent(in) :: spRPoint
    class(sp_rArray), allocatable :: maxTotal !< output

    if (allocated(maxTotal)) deallocate(maxTotal)
    ! init and initialize with dimensions (not values)
    allocate(maxTotal, mold=spRAlloc)

    if (allocated(spRAlloc % r1)) then
      maxTotal % r1 = max(spRAlloc % r1, spRPoint % r1)
    else if (allocated(spRAlloc % r2)) then
      maxTotal % r2 = max(spRAlloc % r2, spRPoint % r2)
    else if (allocated(spRAlloc % r3)) then
      maxTotal % r3 = max(spRAlloc % r3, spRPoint % r3)
    else if (allocated(spRAlloc % r4)) then
      maxTotal % r4 = max(spRAlloc % r4, spRPoint % r4)
    else if (allocated(spRAlloc % r5)) then
      maxTotal % r5 = max(spRAlloc % r5, spRPoint % r5)
    else 
      ! raise error, dimension not implemented 
    endif

  end function max_sp_realAllocArray_vs_sp_realPointArray


  subroutine assign_sp_rArray_with_spPointerArray(this, from) 
    !< assign the values of the array 'from' to 'this'
    class(sp_rArray), intent(inout) :: this
    class(pAbsArray), intent(in) :: from
!    write(*,*) "array_mod::assign_sp_rArray_with_spPointerArray:: entering... "

    select type (from)
    type is (sp_rpArray)
      if (allocated(this % r1)) then
        this % r1 = from % r1

      else if (allocated(this % r2)) then
        this % r2 = from % r2

      else if (allocated(this % r3)) then
        this % r3 = from % r3

      else if (allocated(this % r4)) then
        this % r4 = from % r4

      else if (allocated(this % r5)) then
        this % r5 = from % r5

      else
        ! todo dimension not implemented
      endif
    end select ! from 

  end subroutine assign_sp_rArray_with_spPointerArray


  subroutine assign_sp_rArray_with_spReal(this, from)
    !< assign the scalar value 'from' to the allocated array
    !< In case the dimension is not supported an error is raised
    class(sp_rArray), intent(inout) :: this
    real(kind=sp), intent(in) :: from

!    write(*,*) "tsarray_mod:: assign_sp_rArray_with_spReal:: entering...", from 

    if (allocated(this % r1)) then
      this % r1 = from
    else if (allocated(this % r2)) then
      this % r2 = from
    else if (allocated(this % r3)) then
      this % r3 = from
    else if (allocated(this % r4)) then
      this % r4 = from
    else if (allocated(this % r5)) then
      this % r5 = from
    else
      ! dimensions not supported error
    endif

  end subroutine assign_sp_rArray_with_spReal


  ! todo move to array?
  function closestNumber2Index(num, arr, preferred) result (idx)
    !< find the index from the closest number from num inside the array arr
    !< the given array is sort from + to -
    !< preferred argument is used when the num is found inbetween two values.
    !<      it decides which one pick: the first or last last occurence
    !<   4.25 from array(8,6,4,2) = 3
    !<   3 from array(8,6,4,2), upper = 3
    !<   3 from array(8,6,4,2), lower = 4
    real(kind=sp), intent(in) :: num !< number to search position
    real(kind=sp), dimension(:), intent(in) :: arr !< array to inspect
    integer, optional, intent(in) :: preferred !< preferred direction, *up/down in case the closest number is in the middle
    integer :: idx !< output, index of the array found for the closest number

    real(kind=sp) :: diff, mindiff
    integer :: i
    integer :: preferredChoice

    preferredChoice = PREFER_FIRST
    if (present(preferred)) then
      preferredChoice = preferred
    endif

    !if (.not. isSorted(arr, order=ORDER_MAX_TO_MIN)) then
      ! todo raise error
    !endif
    ! only position?
    if (size(arr) == 1) then
      ! let's return the only index
      idx = 1
      return
    endif

    ! define starting condition
    idx = -1
    mindiff = huge(mindiff)
    ! let's iterate all over 
    do i = 1, size(arr)
      ! callculate difference
      diff = abs(num - arr(i))

      ! is the new diff smaller than any other?
      if (diff < mindiff) then
        ! yes
        mindiff = diff
        idx = i
      else if (eqReal(diff, mindiff)) then
        if (preferredChoice == PREFER_LAST) then ! prefer the latest found
           mindiff = diff
           idx = i
        endif
      endif
    enddo


  end function closestNumber2Index 


  function unique(array) result (uniqueArray)
    !< it returns an array with only unique values (no repeated element)
    !< Naive implementation (meant for short arrays)
    !< https://www.rosettacode.org/wiki/Remove_duplicate_elements#Fortran
    integer, intent(in) :: array(:)
    integer, allocatable :: uniqueArray(:)

    integer :: j, k, i

    allocate(uniqueArray(size(array)))

    k = 1
    uniqueArray(1) = array(1)
    outer: do i=2,size(array)
      do j=1,k
        if (uniqueArray(j) == array(i)) then
           ! Found a match so start looking again
           cycle outer
        end if
      end do
      ! No match found so add it to the output
      k = k + 1
      uniqueArray(k) = array(i)
    end do outer

    ! resize
    uniqueArray = uniqueArray(1:k)

  end function unique


  function initArrayRange_r1d(cstart, cend, csteps) result (newArray)
    !< create a new array generating values starting from 'start' to 'end'
    !< in 'steps'. 
    !< input: initRealArrayRange(89.5, -89.5, -0.5) -> 89.5, 89, ... to ...,-89,-89.5
    !< input: initRealArrayRange(-89.5, 89.5, 0.5) -> -89.5,-89, ... to ..., 89, 89.5
    !<  if cstart is bigger than cend, then csteps must be negative
    !<     cstart is smaller than cend, then csteps must be positive
    real(kind=sp), intent(in) :: cstart
    real(kind=sp), intent(in) :: cend
    real(kind=sp), intent(in) :: csteps

    real(kind=sp), allocatable :: newArray(:)

    real(kind=sp) :: span !< 
    real(kind=sp) :: cvalue !< calculated value
    integer :: length !< of the new array  
    integer :: i
    character(len=200) :: tmp
    character(len=*) , parameter :: SNAME = "initArrayRange_r1d"

    if (cstart > cend) then
      if (csteps > 0) then ! is positive
!        write(*,*) "initRealArrayRange:: error(is positive)= ", cstart, cend, csteps
        write(tmp,*) csteps, " start=", cstart, " to ", cend
        call raiseError(__FILE__, SNAME, &
                       "csteps arguments must be negative", &
                       "but found positive value:"//trim(tmp))
      endif
    end if

    if (cstart < cend) then
      if (csteps < 0) then ! is negative
!        write(*,*) "initRealArrayRange:: error(is negative)= ", cstart, cend, csteps
        write(tmp,*) csteps, " start=", cstart, " to ", cend
        call raiseError(__FILE__, SNAME, &
                       "csteps arguments must be positive", &
                       "but found negative value:"//trim(tmp))
      endif
    end if

    span = abs(cend - cstart)
    length = (span / abs(csteps)) + 1

    !write(*,*) "initRealArrayRange:: span=", span
    !write(*,*) "initRealArrayRange:: length=", length

    allocate(newArray(length))
    newArray = 0.

    cvalue = cstart
    do i = 1, length 
      newArray(i) = cvalue
      cvalue = cvalue + csteps
    enddo

  end function initArrayRange_r1d

end module SHR_array_mod
