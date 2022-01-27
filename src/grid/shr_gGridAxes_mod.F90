!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridAxes_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gGridAxes defines a grid axes.
!> 
!------------------------------------------------------------------------------
module shr_gGridAxes_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_arrayDim_mod, only: shr_arrayRspDim
  use shr_strings_mod, only: string
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_gGridAxesCell_mod, only: shr_gGridAxesCell
  use shr_strings_mod, only: string, real2string
  
  implicit none

  public :: shr_gGridAxes, shr_igGridAxes

  logical, parameter :: ISDEBUG = .false.


  type, abstract :: shr_igGridAxes
  contains
    procedure(iface_getName), deferred :: getName
    procedure(iface_getBounds), deferred :: getBounds
    procedure(iface_getResolution), deferred :: getResolution
    procedure(iface_getCells), deferred :: getCells
    procedure(iface_getSize), deferred :: getSize
    procedure(iface_toString), deferred :: toString
    procedure(iface_equal), deferred :: equal
    generic :: operator(==) => equal
  end type shr_igGridAxes


  abstract interface
    elemental type(string) function iface_getName(self)
      import :: shr_igGridAxes, string
      !< it returns the name attribute
      class(shr_igGridAxes), intent(in) :: self
    end function iface_getName


    elemental type(shr_gGridAxesBounds) function iface_getBounds(self)
      import :: shr_igGridAxes,  shr_gGridAxesBounds
      !< it returns the bounds attribute
      class(shr_igGridAxes), intent(in) :: self
    end function iface_getBounds


    elemental real(kind=sp) function iface_getResolution(self)
      import :: sp, shr_igGridAxes
      !< it returns the resolution attribute
      class(shr_igGridAxes), intent(in) :: self
    end function iface_getResolution

    function iface_getCells(self, axisCoord) result (gcells)
      import :: shr_igGridAxes, sp, shr_gGridAxesCell
      !< given an coordinate from the current axis, it returns itss shr_gGridAxesCell(s)
      class(shr_igGridAxes), intent(in) :: self
      real(kind=sp), intent(in) :: axisCoord
      type(shr_gGridAxesCell), allocatable :: gcells(:)
    end function iface_getCells

    integer function iface_getSize(self)
      import :: shr_igGridAxes
      !< it returns how many grid axes cells has
      class(shr_igGridAxes), intent(in) :: self
    end function iface_getSize

    type(string) function iface_toString(self)
      import :: shr_igGridAxes, string
      !< string representation of gGridAxes
      class(shr_igGridAxes), intent(in) :: self
    end function iface_toString

    elemental impure logical function iface_equal(self, other)
      import :: shr_igGridAxes
      !< true if 'self' and 'other' have the same attributes
      class(shr_igGridAxes), intent(in) :: self
      class(shr_igGridAxes), intent(in) :: other
    end function iface_equal
  end interface


  type, extends(shr_igGridAxes) :: shr_gGridAxes
    type(string), allocatable :: name
    real(kind=sp) :: resolution
    type(shr_gGridAxesBounds), allocatable :: bounds
    type(shr_gGridAxesCell), allocatable :: cells(:)
  contains
    procedure :: init => gGridAxes_initialize 

    procedure :: getName => gGridAxes_getName
    procedure :: getBounds => gGridAxes_getBounds
    procedure :: getResolution => gGridAxes_getResolution
    procedure :: getSize => gGridAxes_getSize

    procedure :: expand !< array2d
    procedure :: getCells => gGridAxes_getCells

    procedure :: hasGridCoord 
    procedure :: hasGridAxisCell

    procedure :: equal => gGridAxes_equal

    procedure :: toString => gGridAxes_toString
  end type shr_gGridAxes

contains

  subroutine gGridAxes_initialize(self, name, resolution, bounds)
    !< gGridAxes initialization
    class(shr_gGridAxes), intent(inout) :: self
    type(string), intent(in) :: name !< grid axes name
    real(kind=sp), intent(in) :: resolution !< axes cell width ( always +)
    type(shr_gGridAxesBounds), intent(in) :: bounds !< axes boundaries

    type(shr_arrayRspDim) :: arrayDim !< used to generate axes cells
    integer :: icell !< iterator
    type(shr_gGridAxesBounds), allocatable :: cellBounds
    real(kind=sp) :: center, cellStart, cellEnd
    integer :: nCells !< total number of axes cells

    if (resolution <= 0) then
      call raiseError(__FILE__, "gGridAxes_initialize", &
              "'resolution cannot be 0 or negative'")
    endif

    allocate(self % name, source = name)
    self % resolution = resolution
    allocate(self % bounds, source = bounds)

    !< generate grid axes boundaries
    call arrayDim % init(name % toString(), bounds % getStart(), &
            bounds % getEnd(), -resolution)

    nCells = arraydim % getSize() - 1
    allocate(self % cells(nCells))

    !< create grid axes cells
    do icell = 1, nCells !- 1
      ! find axes cell boundary
      cellStart = arrayDim % getValue(icell)
      cellEnd = arrayDim % getValue(icell+1)

      allocate(cellBounds)
      call cellBounds % init(cellStart, cellEnd)
      ! calculate its axes cell center
      center = (cellStart + cellEnd) / 2.0

      ! add
      call self % cells(icell) % init(center, cellBounds)
      deallocate(cellBounds) !< reuse
    enddo
  end subroutine gGridAxes_initialize


  elemental type(string) function gGridAxes_getName(self)
    !< it returns the name attribute
    class(shr_gGridAxes), intent(in) :: self
    gGridAxes_getName = self %  name
  end function gGridAxes_getName


  elemental type(shr_gGridAxesBounds) function gGridAxes_getBounds(self)
    !< it returns the bounds attribute
    class(shr_gGridAxes), intent(in) :: self
    gGridAxes_getBounds = self % bounds
  end function gGridAxes_getBounds


  elemental real(kind=sp) function gGridAxes_getResolution(self)
    !< it returns the resolution attribute
    class(shr_gGridAxes), intent(in) :: self
    gGridAxes_getResolution = self % resolution
  end function gGridAxes_getResolution


  integer function gGridAxes_getSize(self)
    !< it returns how many grid axes cells has
    class(shr_gGridAxes), intent(in) :: self
    gGridAxes_getSize = size(self % cells)
  end function gGridAxes_getSize


  function expand(self, otherAxes) result (gridcells)
    !< expands 'self' and 'other' to create a 2d array of shr_gridcells
    !< its expansion provides each possible combination between each
    !< grid axes.
    !<
    !< example: gridcells(:,:) = latsAxes(1d) % expand( lonsAxes(1d) )
    !<
    use shr_gridcell_mod, only: shr_gridcell
    class(shr_gGridAxes), intent(in) :: self
    type(shr_gGridAxes), intent(in) :: otherAxes
    type(shr_gridcell), allocatable :: gridcells(:,:) !< output

    integer :: ilat, ilon
    integer :: nlats, nlons

    nlats = self % getSize()
    nlons = otherAxes % getSize()
    allocate(gridcells(nlats, nlons))

    do ilat = 1, nlats
      do ilon = 1, nlons
        gridcells(ilat, ilon) = self % cells(ilat) * otherAxes % cells(ilon)
      enddo
    enddo 

  end function expand


  logical function hasGridCoord(self, axisCoord)
    !< true if gridCoord is found inside gridAxes bounds
    class(shr_gGridAxes), intent(in) :: self
    real(kind=sp), intent(in):: axisCoord
    hasGridCoord = self % bounds % isIn(axisCoord) 
  end function hasGridCoord


  logical function hasGridAxisCell(self, gridAxisCell)
    !< true if gridAxisCell is found 
    class(shr_gGridAxes), intent(in) :: self
    type(shr_gGridAxesCell), intent(in):: gridAxisCell
    integer :: icell
    hasGridAxisCell = .false.
    do icell = 1, self % getSize()
      if (gridAxisCell == self % cells(icell)) then
        hasGridAxisCell = .true.
        exit
      endif
    enddo
  end function hasGridAxisCell


  elemental impure logical function gGridAxes_equal(self, other)
    !< true if 'self' and 'other' have the same attributes
    class(shr_gGridAxes), intent(in) :: self
    class(shr_igGridAxes), intent(in) :: other
    logical :: hasSameName, hasSameRes, hasSameBounds
    !type(string) :: otherStr

    if (.not. same_type_as(self, other)) then
      gGridAxes_equal = .false.
      return
    end if

    hasSameName = (self % name == other % getName())
    !otherStr = other % getName()
    hasSameRes = (self % resolution == other % getResolution())
    hasSameBounds = (self % bounds == other % getBounds())

    !write(*,*) "shr_gGridAxes:: eq_gGridAxes:: hasSameName=", &
    !        hasSameName, self % name % toString(), otherStr % toString()
    !write(*,*) "shr_gGridAxes:: eq_gGridAxes:: hasSameRes=", hasSameRes
    !write(*,*) "shr_gGridAxes:: eq_gGridAxes:: hasSameBounds=", hasSameBounds
    !write(*,*) "shr_gGridAxes:: eq_gGridAxes:: hasSameCells=", hasSameCells

    gGridAxes_equal = (hasSameName .and. hasSameRes .and. &
                    hasSameBounds )
  end function gGridAxes_equal


  type(string) function gGridAxes_toString(self)
    !< string representation of gGridAxes
    class(shr_gGridAxes), intent(in) :: self
    type(string) :: strResolution
    strResolution = real2string(self % resolution)
    gGridAxes_toString = self % name + ", resolution=" + strResolution + ", bounds=" + &
          self % bounds % toString()
  end function gGridAxes_toString


  function gGridAxes_getCells(self, axisCoord) result (gcells)
    !< given an coordinate from the current axis,
    !< itreturns an array of  shr_gGridAxesCell(s)
    class(shr_gGridAxes), intent(in) :: self
    real(kind=sp), intent(in) :: axisCoord
    type(shr_gGridAxesCell), allocatable :: gcells(:)

    integer :: ngcs, igc
    type(shr_gGridAxesCell) :: currentGAcell


    ngcs = self % getSize()
    allocate(gcells(0))

    do igc = 1, ngcs
      currentGAcell = self % cells(igc)
      if (currentGAcell % isIn(axisCoord)) then
        gcells = [gcells, currentGAcell]
      end if
    end do
  end function gGridAxes_getCells

end module shr_gGridAxes_mod 

