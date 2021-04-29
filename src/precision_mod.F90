!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : name of the module or program
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> Define default real and integer precision 
!------------------------------------------------------------------------------

module precision_mod

  use iso_fortran_env, only: int16,int32,int64
  use iso_fortran_env, only: real32,real64,real128

  implicit none

  private

  public :: real_kind, int_kind, sp, dp, qp, eqReal

  interface eqReal
    module procedure :: eqReal_sp, eqReal_dp, eqReal_qp
  end interface

  ! real type tolerance
  ! http://jules-lsm.github.io/coding_standards/guidelines/fp_arithmetic.html
  real(kind=real32),  parameter :: PREC_SP_TOL = epsilon(0.1)
  real(kind=real64),  parameter :: PREC_DP_TOL = epsilon(real(0.1,kind=real64))
  real(kind=real128), parameter :: PREC_QP_TOL = epsilon(real(0.1,kind=real128))

  integer,parameter :: sp = real32
  integer,parameter :: dp = real64
  integer,parameter :: qp = real128

#ifdef REAL64
  integer,parameter :: real_kind = real64 ! * double precision
#elif REAL128
  integer,parameter :: real_kind = real128 ! quadruple precision
#else
  integer,parameter :: real_kind = real32 ! single precision
#endif

#ifdef INT16
  integer,parameter :: int_kind = int16
#elif INT64
  integer,parameter :: int_kind = int64
#else
  integer,parameter :: int_kind = int32 ! preferred
#endif

contains

  elemental logical function eqReal_qp(r1, r2, tolerance) 
    real(kind=qp), intent(in) :: r1
    real(kind=qp), intent(in) :: r2
    real(kind=qp), optional, intent(in) :: tolerance

    real(kind=qp) :: selTolerance

    selTolerance = PREC_QP_TOL
    if (present(tolerance)) then
      selTolerance = tolerance
    endif

    eqReal_qp =  (abs(r1 - r2) < selTolerance) 

  end function eqReal_qp


  elemental logical function eqReal_dp(r1, r2, tolerance) 
    real(kind=dp), intent(in) :: r1
    real(kind=dp), intent(in) :: r2
    real(kind=dp), optional, intent(in) :: tolerance

    real(kind=dp) :: selTolerance

    selTolerance = PREC_DP_TOL
    if (present(tolerance)) then
      selTolerance = tolerance
    endif

    eqReal_dp =  (abs(r1 - r2) < selTolerance) 

  end function eqReal_dp


  elemental logical function eqReal_sp(r1, r2, tolerance) 
    real(kind=sp), intent(in) :: r1
    real(kind=sp), intent(in) :: r2
    real(kind=sp), optional, intent(in) :: tolerance

    real(kind=sp) :: selTolerance

    selTolerance = PREC_SP_TOL
    if (present(tolerance)) then
      selTolerance = tolerance
    endif

    eqReal_sp =  (abs(r1 - r2) < selTolerance) 

  end function eqReal_sp

endmodule precision_mod
