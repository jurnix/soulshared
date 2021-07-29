!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : linkedList_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> Linked list unit tests
!> 
!> https://github.com/thomasms/fortsraw/blob/master/examples/linked_list/main.F90
!------------------------------------------------------------------------------
module linkedList_test
  use SHR_precision_mod, only: sp, dp
  use SHR_linkedList_mod, only: linkedList, linkedListNode
  use SHR_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuiteLinkedList

  logical, parameter :: ISLOG = .false.

  type, extends(testSuite) :: testSuiteLinkedList

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteLinkedList), intent(inout) :: self

    type(linkedList) :: list
    class(*), pointer :: general_pointer
    
    type(LinkedListNode), pointer :: indexnode

    if (ISLOG) write(*, "(A)") "Appending integer value..."
    allocate(general_pointer, source=4)
    call list%append(general_pointer)

    ! atindex, extract int
    indexnode => list % atindex(1)
    select type (getter => indexnode % value)
      type is (integer)
        call self % assert(getter == 4, "list % atindex(1) .eq. 4 = T")
      class default
        call self % assert(.false., "list % atindex(1) .eq. 4 = T")
    end select

    if (ISLOG) write(*, "(A)") "Appending real value..."
    allocate(general_pointer, source=89.3_dp)
    call list%append(general_pointer)

    ! atindex, extract real
    indexnode => list % atindex(2)
    select type (getter => indexnode % value)
      type is (real(dp))
        call self % assert(getter == 89.3_dp, "list % atindex(2) .eq. 89.3 = T")
      class default
        call self % assert(.false., "list % atindex(2) .eq. 89.3 = T")
    end select

    if (ISLOG) write(*, "(A)") "Appending string..."
    allocate(general_pointer, source="A string")
    call list%append(general_pointer)

    ! atindex, extract char
    indexnode => list % atindex(3)
    select type (getter => indexnode % value)
      type is (character(*))
        call self % assert(getter == "A string", "list % atindex(3) .eq. 'A string' = T")
      class default
        call self % assert(.false., "list % atindex(3) .eq. 'A string' = T")
    end select

    ! first (extract int)
    indexnode => list % first()
    select type (getter => indexnode % value)
      type is (integer)
        call self % assert(getter == 4, "list % first() .eq. 4 = T")
      class default
        call self % assert(.false., "list % first() .eq. 4 = T")
    end select

    ! last (extract char)
    indexnode => list % last()
    select type (getter => indexnode % value)
      type is (character(*))
        call self % assert(getter == "A string", "list % last() .eq. 'A string' = T")
      class default
        call self % assert(.false., "list % last() .eq. 'A string' = T")
    end select

    if (ISLOG) write(*, "(A, I2.1)") "Size: ", list%length()
    call self % assert(list%length() == 3, "list % length() .eq. 3 = T")

    call list%traverse(printvalues)

    call list%reset()
    if (ISLOG) write(*, "(A, I2.1)") "Size after reset: ", list%length()
    call self % assert(list%length() == 0, "list % length() .eq. 0 (after reset) = T")
    
  end subroutine defineTestCases


  subroutine printvalues(node)
    type(LinkedListNode), pointer, intent(inout)  :: node

    select type(p => node%value)
    type is(integer)
      if (ISLOG) write(*, "(A, I2)") "Got integer =", p
    type is(real(dp))
      if (ISLOG) write(*, "(A, F5.1)") "Got real =", p
    type is(character(*))
      if (ISLOG) write(*, "(A, A)") "Got string = ", p
    class default
      write(*, "(A)") "ERROR!"
    end select

  end subroutine printvalues

end module linkedList_test

