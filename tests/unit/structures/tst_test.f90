!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : tst_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> Ternary search tree unit tests
!------------------------------------------------------------------------------
module tst_test
  use SHR_tst_mod, only: string, tst, tstNode
  use SHR_strings_mod, only: equalStringsList, printStrings
  use SHR_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuiteTst

  type testt
    character(len=:), allocatable :: name
  end type testt

  type, extends(testSuite) :: testSuiteTst
      type(tst) :: t1
      type(tst) :: t2

    contains
      procedure :: setUp
      procedure :: define => defineTestCases
  end type 

contains

  subroutine setUp(self)
     class(testSuiteTst), intent(inout) :: self 

     type(testt), pointer :: pdata, pmore
     type(testt), allocatable, target :: adata
     class(*), pointer :: wrap

     ! create tst value allocated
     allocate(adata)
     adata%name = "agood"
     wrap => adata
     call self % t1 % insert("apositive", wrap)

     ! create tst value with pointer
     allocate(pdata)
     pdata%name = "pgood"
     wrap => pdata
     call self % t1 % insert("ppositive", wrap)

     call self % t2 % insert("first", wrap)

     allocate(pmore)
     pmore%name = "more"
     wrap => pmore
     call self % t2 % insert("second", wrap)

  end subroutine setUp


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteTst), intent(inout) :: self

    class(*), pointer :: wrapper 
    integer, target :: val(8)
    integer :: valuefound
    type(Tst) :: t
    type(string), dimension(:), allocatable :: allkeys
    type(string), dimension(:), allocatable :: allkeysEmpty
    type(string), allocatable :: expected7(:)
    type(string), allocatable :: expected5(:)
    class(*), pointer :: asNode 
    type(tst) :: tstEmpty

    call self % setUp()

    val(1) = 21
    val(2) = 22
    val(3) = 23
    val(4) = 24
    val(5) = 25
    val(6) = 26
    val(7) = 27
    val(8) = 28
    allocate(expected7(7))
    expected7(1) % val = "us"
    expected7(2) % val = "i"
    expected7(3) % val = "he"
    expected7(4) % val = "cute"
    expected7(5) % val = "cup"
    expected7(6) % val = "as"
    expected7(7) % val = "at"

    allocate(expected5(5))
    expected5(1) % val = "us"
    expected5(2) % val = "he"
    expected5(3) % val = "cute"
    expected5(4) % val = "cup"
    expected5(5) % val = "at"

    write (*,*) "tst_test::defineTC"
    wrapper => val(1)
    call t % insert("cute", wrapper)
    wrapper => val(2)
    call t % insert("cup", wrapper)
    wrapper => val(3)
    call t % insert("at", wrapper)
    wrapper => val(4)
    call t % insert("as", wrapper)
    wrapper => val(5)
    call t % insert("he", wrapper)
    wrapper => val(6)
    call t % insert("as", wrapper)
    wrapper => val(7)
    call t % insert("us", wrapper)
    wrapper => val(8)
    call t % insert("i", wrapper)
    call t % insert("", wrapper) ! nothing to do

    ! getSize
    call self % assert (t % getSize() == 7, "t % getSize() == 7")

    ! equalStringsList
    allkeys =  t % getAllKeys()    
    call self % assert (equalStringsList(allkeys, expected7), &
            "t % getAllKeys() == [us,i,he,cute,cup,as,at]")

    ! search
    call self % assert (.not. t % search("nothing"), "t % search(nothing) == F")

    call self % assert (t % search("at"), "t % search(testkey) == T")

    call self % assert (.not. t % search(""), "t % search('') == F")

    call self % assert (t % search("i"), "t % search(i) == T")

    call self % assert (t % search("he"), "t % search(he) == T")

    ! get
    asNode => t % get ("as")
    select type(asNode)
    type is (integer)
      valuefound = asNode
      call self % assert (asNode == 26, "t % get(as) .eq. 26 == T")
    class default
      call self % assert (.false., "t % get(as) .eq. 26 == T")
    end select

    ! remove
    call t % remove ("as")
    call t % remove ("i")
    call self % assert (.not. t % search("as"), "t % search(as) == F (after 2 removes)")

    call t % remove ("nonexisting") ! nothing should happen, key does not exist

    call self % assert (.not. t % search("as"), "t % search(as) == F (after 3 removes)")

    call self % assert (t % getSize() == 5, "t % getSize() == 5")

    if (allocated(allkeys)) deallocate(allkeys)

    ! equalStringsList
    allkeys =  t % getAllKeys()    
    call self % assert (equalStringsList(allkeys, expected5) , &
            "t % getAllKeys() == [us,he,cute,cup,at]")


    allocate(allKeysEmpty(0))
    allkeys = tstEmpty % getAllKeys()
    call self % assert (equalStringsList(allkeys, allKeysEmpty) , &
            "tstEmpty % getAllKeys() == []")


    ! allocatable insert (it must fail do to allocatable array)
    ! allocatables are automatically deallocated when the subroutine finishes.
    ! So any allocated local array would be lost.
    asNode => self % t1 % get("apositive")
    select type (asNode)
    type is (testt)
      ! Theoretically it should never reach this point. In some compilers 
      ! accessing asNode it raises a memory error (such as gfortran 7.5)
      ! If if fails at this point it could indicated the compiler is old
      call self % assert (.true., "allocatable t1 % get(apositive) % name .eq. 'agood (unexpected)' == F")
    class default 
      call self % assert (.true., "allocatable t1 % get(apositive) % name .eq. 'agood' == F")
    end select

    ! pointer insert (it works)
    asNode => self % t1 % get("ppositive")
    select type (asNode)
    type is (testt)
      call self % assert (asNode % name .eq. "pgood", "pointer t1 % get(ppositive) % name .eq. 'pgood' == T")
    class default
      call self % assert (.false., "pointer t1 % get(ppositive) % name .eq. 'good' == T")
    end select

    call self % t2 % traverse(updateVar)

  contains
    subroutine updateVar(var)
        !< feed all history variable with new data
        type(tstNode), pointer, intent(inout)  :: var

        select type(obj => var % value)
        type is (testt)
          call self % assert(.true., "t2 % traverse() value assoc? .eq. True == T")
        class default
          !write(*,*) "tst_test::updateVar:: associated(var%value)?", associated(var % value)
          call self % assert(.false., "t2 % traverse() value assoc? .eq. True == F")
        end select ! var % value 
    end subroutine updateVar

  end subroutine defineTestCases

end module tst_test

