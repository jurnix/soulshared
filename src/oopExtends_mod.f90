!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  SHR_oopExtends_mod
!
!> @author
!> Albert Jornet Puig 
!
! DESCRIPTION:
!> Include new functionalities to oop 
!> 
!------------------------------------------------------------------------------
module SHR_oopExtends_mod

  implicit none

  private

  public :: all_same_type_as 


contains


  logical function all_same_type_as(elements, baseType)
    !< true if all elements are the same type as baseType
    !< false if elements is empty or any element is different from baseType
    class(*), intent(in) :: elements(:) !< list of elements to check
    class(*), intent(in) :: baseType !< type to compare will all elements

    integer :: i

    if (size(elements) == 0) then
      all_same_type_as = .false.
      return
    endif

    all_same_type_as = .true.
    do i = 1, size(elements)
      if (.not. same_type_as(elements(i), baseType)) then
        all_same_type_as = .false.
        exit 
      endif
    enddo
  end function all_same_type_as


end module SHR_oopExtends_mod
