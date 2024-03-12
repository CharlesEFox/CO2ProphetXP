C STRUCTURED BY FOR_STRUCT, V2.0.1, ON 04/21/94 AT 16:17:44
C  OPTIONS SET: DUP=R FMT=ACHN I=50,3 LABF=10000,10 LABX=50,50 OFF=AI
C               S=9 SP=DE V
      SUBROUTINE arpoly(nvertx, xvertx, yvertx, parea)
*
************************************************************************
*
*     SUBROUTINE TO DETERMINE THE AREA OF A POLYGON.
*
*     CALLING VARIABLES:
*
*     NVERTX - INPUT        = NUMBER OF SIDES OR VERTICIES OF THE
*                             POLYGON
*     XVERTX - INPUT        = ARRAY OF X VERTICIES OF THE POLYGON
*     YVERTX - INPUT        = ARRAY OF Y VERTICIES OF THE POLYGON
*     PAREA  -       OUTPUT = AREA OF THE POLYGON IN X/YVERTX UNITS
*
*     LOCAL VARIABLES:
*
*     I      - INTEGER      = DO LOOP INDEX
*     NLOCAL - INTEGER      = LOCAL NUMBER OF POLYGON VERTICIES
*     NPOLY  - INTEGER      = MAXIMUM NUMBER OF VERTICIES A POLYGON CAN
*                             HAVE
*     XLOEND - REAL         = LOCAL CLOSING POLYGON X VERTEX
*     YLOEND - REAL         = LOCAL CLOSING POLYGON Y VERTEX
*
*     WRITTEN BY              JAMES W. MUELLER           OCTOBER 9, 1991
*                             SOLUTIONS SYSTEMS          HOUSTON, TEXAS
*
************************************************************************
*
*     CALLING VARIABLE DEFINITIONS:
*
      INTEGER nvertx
*
      DOUBLE PRECISION parea
      DOUBLE PRECISION xvertx(*)
      DOUBLE PRECISION yvertx(*)
*
************************************************************************
*
*     LOCAL VARIABLE DEFINITIONS:
*
      INTEGER npoly
      PARAMETER (npoly = 128)
*
      INTEGER i
*
      DOUBLE PRECISION sx1ty2
      DOUBLE PRECISION sy1tx2
      DOUBLE PRECISION xloend
      DOUBLE PRECISION yloend
*
************************************************************************
*     BEGINNING OF SUBROUTINE.
************************************************************************
*
      nlocal = nvertx
      IF (nlocal .LE. 0 .OR. nlocal .GT. npoly) THEN
         WRITE (*, '('' NUMBER OF POLYGON VERTICIES OUT OF RANGE'')')
         STOP ' ARPOLY ERROR 1'
      ELSE
*
************************************************************************
*     COMPLETE THE POLYGON IF IT IS NOT CLOSED.
***********************************************************************
*                                                                                      XVERTX(NLOCAL) = XVERTX(1)
         IF (xvertx(1) .NE. xvertx(nvertx) .AND. yvertx(1) .NE. yvertx
     1    (nvertx)) THEN
            nlocal = nvertx
         ELSE
            nlocal = nvertx - 1
         END IF
         xloend = xvertx(1)
         yloend = yvertx(1)
*
************************************************************************
*     COMPUTE THE AREA (PLUS OR MINUS) OF THE POLYGON DEPENDING ON THE
*     ORDER OF THE POLYGON VERTICIES.
***********************************************************************
*
         sx1ty2 = 0.0d0
         sy1tx2 = 0.0d0
         DO i = 1, nlocal - 1
            sx1ty2 = sx1ty2 + xvertx(i)*yvertx(i + 1)
            sy1tx2 = sy1tx2 + yvertx(i)*xvertx(i + 1)
         END DO
         sx1ty2 = sx1ty2 + xloend*yvertx(1)
         sy1tx2 = sy1tx2 + yloend*xvertx(1)
         parea = abs(0.5d0*(sx1ty2 - sy1tx2))
*
************************************************************************
*     RETURN ANSWER TO CALLER.
************************************************************************
*
         RETURN
      END IF
      END
