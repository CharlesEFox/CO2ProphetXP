C STRUCTURED BY FOR_STRUCT, V2.0.1, ON 04/21/94 AT 16:17:44
C  OPTIONS SET: DUP=R FMT=ACHN I=50,3 LABF=10000,10 LABX=50,50 OFF=AI
C               S=9 SP=DE V
      SUBROUTINE slpang(dy, dx, angle)
*
************************************************************************
*
*     SUBROUTINE TO CALCULATE THE ANGLE OF THE SLOPE BETWEEN TWO POINTS.
*
*     CALLING VARIABLES:
*
*     DY     - INPUT        = DELTA Y VALUE BETWEEN THE TWO POINTS
*     DX     - INPUT        = DELTA X VALUE BETWEEN THE TWO POINTS
*     ANGLE  -       OUTPUT = ANGLE IN DEGREES OF THE SLOPE BETWEEN THE
*                             TWO POINTS - RANGE = 0.0 TO 359.99 DEGREES
*
*     LOCAL VARIABLES:
*
*     EPSLON - REAL         = VALUE FOR SIGNIFICANCE CHECK ON EQUALS
*     RAD2DG - REAL         = RADIANS TO DEGREES CONVERSION CONSTANT
*
*     WRITTEN BY              JAMES W. MUELLER           OCTOBER 5, 1991
*                             SOLUTIONS SYSTEMS          HOUSTON, TEXAS
*
************************************************************************
*
*     CALLING VARIABLE DEFINITIONS
*
      DOUBLE PRECISION angle
      DOUBLE PRECISION dx
      DOUBLE PRECISION dy
*
************************************************************************
*
*     LOCAL VARIABLE DEFINITIONS
*
      DOUBLE PRECISION rad2dg
      PARAMETER (rad2dg = 180.0d0/3.141592653589793238d0)
*
      DOUBLE PRECISION epslon
*
      DATA epslon/1.0e-6/
*
************************************************************************
*     BEGINNING OF SUBROUTINE - COMPUTE ANGLE.
************************************************************************
*
      IF (abs(dy) .LE. epslon .AND. dx .GT. 0.0d0) THEN
         angle = 0.0d0
      ELSE IF (abs(dx) .LE. epslon .AND. dy .GT. 0.0d0) THEN
         angle = 90.0d0
      ELSE IF (abs(dy) .LE. epslon .AND. dx .LT. 0.0d0) THEN
         angle = 180.0d0
      ELSE IF (abs(dx) .LE. epslon .AND. dy .LT. 0.0d0) THEN
         angle = 270.0d0
      ELSE IF (dy .GT. 0.0d0 .AND. dx .GT. 0.0d0) THEN
         angle = atan(dy/dx)*rad2dg
      ELSE IF (dy .GT. 0.0d0 .AND. dx .LT. 0.0d0) THEN
         angle = atan(dy/dx)*rad2dg + 180.0d0
      ELSE IF (dy .LT. 0.0d0 .AND. dx .LT. 0.0d0) THEN
         angle = atan(dy/dx)*rad2dg + 180.0d0
      ELSE IF (dy .LT. 0.0d0 .AND. dx .GT. 0.0d0) THEN
         angle = atan(dy/dx)*rad2dg + 360.0d0
*
************************************************************************
*     END OF SUBROUTINE - RETURN ANGLE.
************************************************************************
*
      END IF
      RETURN
      END
