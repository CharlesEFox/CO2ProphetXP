C STRUCTURED BY FOR_STRUCT, V2.0.1, ON 04/21/94 AT 16:17:44
C  OPTIONS SET: DUP=R FMT=ACHN I=50,3 LABF=10000,10 LABX=50,50 OFF=AI
C               S=9 SP=DE V
      DOUBLE PRECISION FUNCTION potent(x, y, nowels, wellx, welly, 
     1 wellq)
*
************************************************************************
*
*     FUNCTION TO COMPUTE THE POTENT(IAL) AT THE GIVEN X/Y POINT
*
*     CALLING VARIABLES:
*
*     X      - REAL         = X LOCATION OF TEST IMAGE WELL
*     Y      - REAL         = Y LOCATION OF TEST IMAGE WELL
*     NOWELS - INTEGER      = NUMBER OF WELLS SO FAR
*     WELLX  - REAL         = ARRAY OF X WELL LOCATIONS
*     WELLY  - REAL         = ARRAY OF Y WELL LOCATIONS
*     WELLQ  - REAL         = ARRAY OF WELL INPUT OR OUTPUT RATES
*
*     LOCAL VARIABLES:
*
*     I      - INTEGER      = DO LOOP INDEX
*     TEMP   - REAL         = TEMPORARY VARIALB TO SUM UP POTENTIALS
*
*     WRITTEN BY                  JAMES F. STEVENS
*                                 TEXACO, INC.         BRIARPARK CENTER
*     CONVERTED TO FORTRAN BY     JAMES W. MUELLER     SEPTEMBER 4, 1991
*                                 SOLUTIONS SYSTEMS    HOUSTON, TEXAS
*
************************************************************************
*
*     CALLING VARIABLE DEFINITIONS
*
      INTEGER nowels
*
      DOUBLE PRECISION wellq(*)
      DOUBLE PRECISION wellx(*)
      DOUBLE PRECISION welly(*)
      DOUBLE PRECISION x
      DOUBLE PRECISION y
*
************************************************************************
*
*     LOCAL VARIABLE DEFINITIONS
*
      INTEGER i
*
      DOUBLE PRECISION temp
*
************************************************************************
*     BEGINNING OF REAL FUNTION - SUM UP INDIVIDUAL POTENTIALS.
************************************************************************
*
      temp = 0.0
      DO i = 1, nowels
         temp = temp + wellq(i)*log((x - wellx(i))**2 + (y - welly(i))*
     1    *2)
      END DO
      potent = temp
*
************************************************************************
*     RETURN ANSWER TO CALLER.
************************************************************************
*
      RETURN
      END

