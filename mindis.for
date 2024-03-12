C STRUCTURED BY FOR_STRUCT, V2.0.1, ON 04/21/94 AT 16:17:44
C  OPTIONS SET: DUP=R FMT=ACHN I=50,3 LABF=10000,10 LABX=50,50 OFF=AI
C               S=9 SP=DE V
      SUBROUTINE mindis
*
************************************************************************
*
*     SUBROUTINE TO CALCULATE THE MINIMUM DISTANCE BETWEEN EACH
*     INJECTION WELL AND ITS CLOSEST PRODUCTION WELL.
*
*     CALLING VARIABLES:
*
*     NONE
*
*     LOCAL VARIABLES:
*
*     DISI2P - REAL         = TEMPORARY ARRAY OF DISTANCES
*     I      - INTEGER      = DO LOOP INDEX
*     J      - INTEGER      = DO LOOP INDEX
*     K      - INTEGER      = ARRAY POINTER
*     NPOINT - INTEGER      = TEMPORARY ARRAY OF POINTERS
*
*     WRITTEN BY                 JAMES W. MUELLER     SEPTEMBER 18, 1991
*                                SOLUTIONS SYSTEMS    HOUSTON, TEXAS
*
************************************************************************
*
*     SUBROUTINE INCLUDE STATEMENTS
*
      INCLUDE 'PARAM.INC'
      INCLUDE 'FIELD.INC'
*
************************************************************************
*
*     LOCAL VARIABLE DEFINITIONS
*
      INTEGER i
      INTEGER j
      INTEGER k
      INTEGER npoint(maxwel)
*
      DOUBLE PRECISION disi2p(maxwel)
*
************************************************************************
*     BEGINNING OF SUBROUTINE - INITIALIZE.
***********************************************************************
*
      k = 0
*
************************************************************************
*     COMPUTE AND SAVE THE MINIMUM DISTANCE FROM EACH INJECTION WELL TO
*     ITS NEAREST PRODUCTION WELL.
************************************************************************
*
      DO i = 1, nwells
*
************************************************************************
*     DETERMINE WHICH WELLS ARE INJECTION WELLS.
************************************************************************
*
         IF (wellq(i) .GT. 0.0d0) THEN
            k = k + 1
*
************************************************************************
*     DETERMINE WHICH WELLS ARE PRODUCTRION WELLS AND COMPUTE DISTANCES.
************************************************************************
*
            DO j = 1, nwells
               IF (j .NE. i .OR. wellq(j) .LT. 0.0d0) THEN
                  disi2p(j) = sqrt((wellx(j) - wellx(i))**2 + (welly(j) 
     1             - welly(i))**2)
                  npoint(j) = j
               ELSE
                  npoint(j) = 32767
               END IF
            END DO
*
************************************************************************
*     DETERMINE THE MINIMUM DISTANCE FOR THIS INJECTION WELL TO ITS
*     NEAREST NEIGHBOR AND SAVE ITS X/Y ARRAY POSITION.
************************************************************************
*
            dismin(k) =  + 1.0d30
            linjp(k) = i
            DO j = 1, nwells
               IF (npoint(j) .NE. 32767) THEN
                  IF (dismin(k) .GT. disi2p(j)) THEN
                     dismin(k) = disi2p(j)
                     lprodp(k) = j
                  END IF
               END IF
            END DO
         END IF
      END DO
      noinjw = k
      noprow = nwells - k
*
************************************************************************
*     RETURN ANSWERS TO CALLER.
************************************************************************
*
      RETURN
      END

