!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : common.fypp 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> 
!>
!------------------------------------------------------------------------------

#:set MAXRANK = 4

#:set NEXTRANK = int(MAXRANK) + 1

#:set REAL_KINDS = ['sp', 'dp']

#:set REAL_HEADERS = ['rsp', 'rdp']

#:set REAL_TYPES = ["real(kind={})".format(k) for k in REAL_KINDS]

#! Collected (kind, type) tuples for real types
#:set REAL_KINDS_TYPES = list(zip(REAL_KINDS, REAL_TYPES, REAL_HEADERS))


#:set INT_HEADERS = ['int']

#:set INT_KINDS = ['']

#:set INT_TYPES = ['integer']

#! Collected (kind, type) tuples for real types
#:set INT_KINDS_TYPES = list(zip(INT_KINDS, INT_TYPES, INT_HEADERS))

! function_rspX
! 
! function_iX


#:set ALL_KINDS_TYPES = REAL_KINDS_TYPES + INT_KINDS_TYPES


#:set RANKS = range(1, MAXRANK+1)

#:set PLUSRANKS = range(1, MAXRANK+2)

#:set RSHIFTRANKS = range(2, MAXRANK+2)

#:def ranksuffix(RANK)
$:'' if RANK == 0 else '(' + ':' + ',:' * (RANK - 1) + ')'
#:enddef ranksuffix

#:def rankimplicit(RANK)
$:'' if RANK == 0 else ',:' * (RANK) 
#:enddef rankimplicit

#:def rankexplicit(RANK,dimsize)
$:'' if RANK == 0 else (',' + str(dimsize)) * (RANK) 
#:enddef rankexplicit

!
! Prints the allocation array dimensions from an array which contains its dimensions 
!
! Args:
!   vname (str): array name which contains dimensions
!   nrank (int): how many rank generate
!
! Returns:
!   ( vname(1), vname(2), vname(3), ... ) 
!                
#:def array_allocation(vname, nrank)
${ '(' + (', '.join([vname+'('+str(irank)+ ')' for irank in range(1, nrank+1)])) + ')' }$
#:enddef

