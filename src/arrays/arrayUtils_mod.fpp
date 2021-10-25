!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  arrayUtils_mod
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

#:include "../common.fpp"


module SHR_arrayUtils_mod

  use SHR_precision_mod, only: sp, dp, eqReal
  use SHR_error_mod, only: raiseError 
  use SHR_strings_mod, only: string

  implicit none

  private

  public :: trimArrayIndex

  public :: closestNumber2Index, PREFER_FIRST, PREFER_LAST, unique, initArrayRange


  integer, parameter :: PREFER_FIRST = 1
  integer, parameter :: PREFER_LAST = 2


  interface initArrayRange
#:for IKIND, ITYPE, IHEADER in ALL_KINDS_TYPES
    module procedure initArrayRange_${IHEADER}$
#:endfor
  end interface


contains


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


#:for IKIND, ITYPE, IHEADER in ALL_KINDS_TYPES

  function initArrayRange_${IHEADER}$(cstart, cend, csteps) result (newArray)
    !< create a new array generating values starting from 'start' to 'end'
    !< in 'steps'. 
    !< input: initRealArrayRange(89.5, -89.5, -0.5) -> 89.5, 89, ... to ...,-89,-89.5
    !< input: initRealArrayRange(-89.5, 89.5, 0.5) -> -89.5,-89, ... to ..., 89, 89.5
    !<  if cstart is bigger than cend, then csteps must be negative
    !<     cstart is smaller than cend, then csteps must be positive
    ${ITYPE}$, intent(in) :: cstart
    ${ITYPE}$, intent(in) :: cend
    ${ITYPE}$, intent(in) :: csteps

    ${ITYPE}$, allocatable :: newArray(:)

    real(kind=sp) :: span !< 
    real(kind=sp) :: cvalue !< calculated value
    integer :: length !< of the new array  
    integer :: i
    character(len=200) :: tmp
    character(len=*) , parameter :: SNAME = "initArrayRange_${IHEADER}$"

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

  end function initArrayRange_${IHEADER}$

#:endfor


  integer function trimArrayIndex(array, direction)
    !< it returns the 1st index found as True in 'array'
    !< direction ("start", "end"): from which position start searching
    logical, intent(in) :: array(:)
    character(*), intent(in), optional :: direction !< start, end

    integer :: startidx, endidx, stepidx
    integer :: ipos

    startidx = 1
    endidx = size(array) 
    stepidx = 1
    trimArrayIndex = 1
    if (present(direction)) then
      if (direction == "start") then
        ! nothing
      else if (direction == "end") then
        startidx = size(array)
        trimArrayIndex = size(array)
        endidx = 1
        stepidx = -1
      else
        ! error
        call raiseError(__FILE__, "trimArrayIndex", &
                "Unexpected direction found ('start' and 'end' allowed)", &
                "direction: "//direction)
      endif
    endif

    do ipos = startidx, endidx, stepidx
      if (.not. array(ipos)) then
        trimArrayIndex = trimArrayIndex + stepidx
      else
        exit ! found
      endif
    enddo
  end function trimArrayIndex

end module SHR_arrayUtils_mod
