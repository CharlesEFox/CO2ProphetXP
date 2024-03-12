C STRUCTURED BY FOR_STRUCT, V2.0.1, ON 04/21/94 AT 16:17:44
C  OPTIONS SET: DUP=R FMT=ACHN I=50,3 LABF=10000,10 LABX=50,50 OFF=AI
C               S=9 SP=DE V
      SUBROUTINE inpoly(xpoint, ypoint, nvertx, xvertx, yvertx, pontin)
*
************************************************************************
*
*     SUBROUTINE TO DETERMINE WHETHER A POINT IS "INSIDE OF" OR "OUTSIDE
*     OF" A POLYGON.
*
*     CALLING VARIABLES:
*
*     XPOINT - INPUT        = X VALUE OF THE POINT TO DETERMINE IF IT IS
*                             IN THE POLYGON OR NOT
*     YPOINT - INPUT        = Y VALUE OF THE POINT TO DETERMINE IF IT IS
*                             IN THE POLYGON OR NOT
*     NVERTX - INPUT        = NUMBER OF SIDES OR VERTICIES OF THE
*                             POLYGON
*     XVERTX - INPUT        = ARRAY OF X VERTICIES OF THE POLYGON
*     YVERTX - INPUT        = ARRAY OF Y VERTICIES OF THE POLYGON
*     PONTIN -       OUTPUT = LOGICAL VARIABLE TO RETURN "IN" OR "OUT"
*                             ANSWER AS .TRUE. OR .FALSE. RESPECTIVELY
*
*     LOCAL VARIABLES:
*
*     COUNT  - INTEGER      = COUNT OF TIMES THE +Y LINE CROSSES THE
*                             POLYGON BOUNDARY
*     DELTAX - REAL         = PRESENT POLYGON VECTOR DELTA X VALUE
*     EPSLON - REAL         = VALUE FOR SIGNIFICANCE CHECK ON EQUALS
*     I      - INTEGER      = DO LOOP INDEX
*     INTRCP - REAL         = INTERCEPT OF PRESENT POLYGON VECTOR WITH
*                             THE Y AXIS
*     NLOCAL - INTEGER      = LOCAL NUMBER OF POLYGON VERTICIES
*     NPOLY  - INTEGER      = MAXIMUM NUMBER OF VERTICIES A POLYGON CAN
*                             HAVE
*     SLOPE  - REAL         = SLOPE OF THE PRESENT POLYGON VECTOR
*     PASS1  - LOGICAL      = LOGICAL FIRST PASS ONLY SETUP SWITCH
*     XCHECK - REAL         = LOCAL X VALUE OF POINT TO BE CHECKED
*     XLI    - REAL         = PRESENT ITH VALUE OF LOCAL X VERTICIES
*                             ARRAY
*     XLIP1  - REAL         = PRESENT (I+1)TH VALUE OF LOCAL X VERTICIES
*                             ARRAY
*     XLOCAL - REAL         = ARRAY OF LOCAL X POLYGON VERTICIES VALUES
*     YCHECK - REAL         = LOCAL Y VALUE OF POINT OF BE CHECKED
*     YINTER - REAL         = PRESENT VECTOR Y INTERCEPT WITH THE
*                             PERPENDICULAR POINT VECTOR
*     YLI    - REAL         = PRESENT ITH VALUE OF LOCAL Y VERTICIES
*                             ARRAY
*     YLIP1  - REAL         = PRESENT (I+1)TH VALUE OF LOCAL Y VERTICIES
*                             ARRAY
*     YLOCAL - REAL         = ARRAY OF LOCAL Y POLYGON VERTICIES VALUES
*
*     WRITTEN BY              JAMES W. MUELLER        SEPTEMBER 26, 1991
*                             SOLUTIONS SYSTEMS       HOUSTON, TEXAS
*
************************************************************************
*
*     CALLING VARIABLE DEFINITIONS:
*
      INTEGER nvertx
*
      LOGICAL pontin
*
      DOUBLE PRECISION xpoint
      DOUBLE PRECISION xvertx(*)
      DOUBLE PRECISION ypoint
      DOUBLE PRECISION yvertx(*)
*
************************************************************************
*
*     LOCAL VARIABLE DEFINITIONS:
*
      INTEGER npoly
      PARAMETER (npoly = 128)
*
      INTEGER count
      INTEGER i
      INTEGER nlocal
*
      LOGICAL pass1
*
      REAL deltax
      REAL epslon
      REAL intrcp
      REAL slope
      REAL xcheck
      REAL xli
      REAL xlip1
      REAL xlocal(npoly)
      REAL ycheck
      REAL yinter
      REAL yli
      REAL ylip1
      REAL ylocal(npoly)
*
      DATA epslon/1.0e-6/
      DATA pass1/.TRUE./
*
************************************************************************
*     BEGINNING OF SUBROUTINE - ONE TIME INITIALIZATION.
************************************************************************
*
      IF (pass1) THEN
         nlocal = nvertx
         IF (nlocal .GT. npoly - 1) THEN
            WRITE (*, '('' NUMBER OF POLYGON VERTICIES OUT OF RANGE'')'
     1       )
            STOP ' INPOLY ERROR 1'
         ELSE
            DO i = 1, nlocal
               xlocal(i) = xvertx(i)
               ylocal(i) = yvertx(i)
            END DO
            IF (xvertx(1) .NE. xvertx(nlocal) .AND. yvertx(1) .NE. 
     1       yvertx(nlocal)) THEN
               nlocal = nlocal + 1
               xlocal(nlocal) = xlocal(1)
               ylocal(nlocal) = ylocal(1)
            END IF
            pass1 = .FALSE.
         END IF
      END IF
*
************************************************************************
*     SET UP LOCAL VARIABLES FOR THIS PASS THROUGH SUBROUTINE.
***********************************************************************
*
      xcheck = xpoint
      ycheck = ypoint
      count = 0
*
************************************************************************
*     TO CHECK IF THE POINT IS CONTAINED WITHIN THE POLYGON, DRAW AN
*     IMAGINARY LINE STRAIGHT UP (+Y DIRECTION) FROM THE POINT AND
*     THEN COMPARE IT WITH ALL POLYGON VECTORS.  TO DO THIS, FIRST
*     DETERMINE THE LINEAR EQUATIONS FOR EVERY POLYGON VECTOR.  SECOND,
*     COMPUTE THE INTERSECTIONS OF EVERY VECTOR WITH THE STRAIGHT LINE.
*     FINALLY, FOR EVERY VECTOR THAT INTERSECTS THE PERPENDICULAR LINE
*     WITHIN ITS RANGE LIMITS, ADD ONE TO THE VECTORS CROSSED COUNT.
*     ONCE ALL OF THE COMPARISIONS ARE MADE; AND IF THE NUMBER OF
*     CROSSINGS IS "ODD", THE POINT IS "IN" THE POLYGON, AND IF THE
*     NUMBER OF CROSSINGS IS "EVEN", THE POINT IS "OUT" OF THE POLYGON.
*     NOTE: A POINT LYING ON A VECTOR OR ON A VERTEX IS CONSIDERED TO BE
*     "IN" THE POLYGON.
************************************************************************
*
      DO i = 1, nlocal - 1
         xli = xlocal(i)
         xlip1 = xlocal(i + 1)
         yli = ylocal(i)
         ylip1 = ylocal(i + 1)
         deltax = xli - xlip1
         IF (abs(deltax) .GT. epslon) THEN
            slope = (yli - ylip1)/deltax
            intrcp = yli - slope*xli
            yinter = slope*xcheck + intrcp
            IF (((xli .LE. xcheck .AND. xlip1 .GE. xcheck) .OR. (xli 
     1       .GE. xcheck .AND. xlip1 .LE. xcheck)) .AND. (yinter .GE. 
     2       ycheck)) count = count + 1
*        ELSEIF( ABS ( DELTAX ) .LE. EPSLON ) THEN
         ELSE IF (((yli .LE. xcheck .AND. ylip1 .GE. ycheck) .OR. (yli 
     1    .GE. ycheck .AND. ylip1 .LE. ycheck)) .AND. (abs(xli - 
     2    xcheck) .LT. epslon)) THEN
            count = count + 1
         END IF
      END DO
*
************************************************************************
*     SET ANSWER "IN" OR "OUT" DEPENDING ON THE NUMBER OF POLYGON EDGE
*     CROSSING FOUND.
************************************************************************
*
      IF (count/2*2 .EQ. count) THEN
         pontin = .FALSE.
*
************************************************************************
*     RETURN ANSWER TO CALLER.
************************************************************************
*
      ELSE
         pontin = .TRUE.
      END IF
      RETURN
      END
