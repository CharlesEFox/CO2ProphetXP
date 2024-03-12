C STRUCTURED BY FOR_STRUCT, V2.0.1, ON 04/21/94 AT 16:17:44
C  OPTIONS SET: DUP=R FMT=ACHN I=50,3 LABF=10000,10 LABX=50,50 OFF=AI
C               S=9 SP=DE V
      SUBROUTINE prmint(nlayrs, dpc)
*
************************************************************************
*
*     SUBROUTINE TO COMPUTE THE ABSOLUTE PREMEABILITIES FOR EACH LAYER
*     IN THE FLOOD.
*
*     CALLING VARIABLES:
*
*     NLAYRS - INPUT        = NUMBER OF LAYERS INTO WHICH THE PRODUCTION
*                             ZONE IS TO BE DIVIDED
*     DPC    - INPUT        = DYKSTRA-PARSONS COEFFICIENT
*
*     LOCAL VARIABLES:
*
*     E      - REAL         = THE VALUE OF "E" - (NATURAL LOG)
*     I      - INTEGER      = DO LOOP INDEX
*     INTRCP - REAL         = VALUE OF THE INTERCEPT ON THE DYKSTRA-
*                             PARSONS LOG/PROBABILITY CURVE
*     MAXLAY - INTEGER      = MAXIMUM NUMBER OF LAYERS ALLOWED IN
*                             THE PRODUCTION ZONE TO BE EVALUATED
*     PERM50 - REAL         = PERMEABILY AT THE 50% VALUE ON DYKSTRA-
*                             PARSONS CUMMULATIVE PROBABILITY CURVE
*     PERM84 - REAL         = PERMEABILY AT THE 84.1% VALUE ON DYKSTRA-
*                             PARSONS CUMMULATIVE PROBABILITY CURVE AND
*                             IS EQUIVALENT TO ONE STANDARD DEVIATION
*     PLAYER - REAL         = INCREMENTAL PROBABILTY BETWEEN THE CENTER
*                             POINTS OF EACH SUCCESSIVE LAYER
*     PROBAB - REAL         = ARRAY OF CUMMULATIVE PROBABILITIES FOR EACH
*                             LAYER
*     PROBI  - REAL         = THE ITH LAYER'S PROBABILITY
*     PROBIC - REAL         = THE ITH LAYER'S PROBABILITY CUBED
*     PROBIQ - REAL         = THE ITH LAYER'S PROBABILITY TO THE FOURTH
*     PROBIS - REAL         = THE ITH LAYER'S PROBABILITY SQUARED
*     PROBIT - REAL         = ARRAY OF PROBITS FOR EACH LAYER
*     SLOPE  - REAL         = VALUE OF THE SLOPE ON THE DYKSTRA-PARSONS
*                             LOG/PROBABILITY CURVE
*     TEMP   - REAL         = TEMPORARY VARAIBLE TO SAVE SUBSCRIPT TIME
*
*     STRUCTURED BY           ROBERT B. ALSTON          OCTOBER 18, 1991
*                             TEXACO, INC.              HOUSTON, TEXAS
*
*     WRITTEN BY              JAMES W. MUELLER          OCTOBER 21, 1991
*                             SOLUTIONS SYSTEMS         HOUSTON, TEXAS
*
***********************************************************************
*
*     SUBROUTINE INCLUDE STATEMENTS
*
      INCLUDE 'PARAM.INC'
      INCLUDE 'ROCKAT.INC'
*
************************************************************************
*
*     CALLING VARIABLE DEFINITIONS:
*
      INTEGER nlayrs
*
************************************************************************
*
*     LOCAL VARIABLE DEFINITIONS:
*
      INTEGER i
*
      DOUBLE PRECISION e
      PARAMETER (e = 2.3025850930d0)
*
      DOUBLE PRECISION dpc
      DOUBLE PRECISION intrcp
      DOUBLE PRECISION perm50
      DOUBLE PRECISION perm84
      DOUBLE PRECISION player
      DOUBLE PRECISION probab(0:maxlay)
      DOUBLE PRECISION probi
      DOUBLE PRECISION probic
      DOUBLE PRECISION probiq
      DOUBLE PRECISION probis
      DOUBLE PRECISION probit(maxlay)
      DOUBLE PRECISION slope
      DOUBLE PRECISION temp
*
************************************************************************
*     BEGINNING OF SUBROUTINE - ONE TIME PASS THROUGH INITIALIZATION.
************************************************************************
*
      perm50 = permav*(9.7442d-1 + 3.8406d-1*dpc - 1.51497d0*dpc*dpc)
      perm84 = perm50*(1.0d0 - dpc)
      intrcp = log(perm50)
C     SLOPE = (INTRCP - LOG ( PERM84 ))/(0.0D0 - (-0.99919D0))
      slope = (intrcp - log(perm84))/9.99919d-1
      player = 100.0d0/real(nlayrs)
      probab(0) =  - 0.5d0*player
      DO i = 1, nlayrs
         probab(i) = probab(i - 1) + player
      END DO
      permax =  - 1.0d30
      sumpab = 0.0d0
      DO i = 1, nlayrs
         probi = probab(i)
         probis = probi*probi
         probic = probi*probis
         probiq = probis*probis
         probit(i) = (2.694938d0 + 4.09207d-1*probi - 1.480d-2*probis + 
     1    1.190d-4*probic - 1.600d-7*probiq)/(1.0d0 + 3.39026d-1*probi 
     2    - 9.700d-4*probis - 5.700d-5*probic + 3.200d-7*probiq)
         temp = e**(slope*probit(i) + intrcp)
         IF (temp .GT. permax) permax = temp
         sumpab = sumpab + temp
         prmabs(i) = temp
C         WRITE (12,*)'  LAYER    PERM    CUM PERM'
C         WRITE (12,'('' '',I5,2F10.4)') I,TEMP,SUMPAB
*
************************************************************************
*     RETURN ANSWER TO CALLER.
************************************************************************
*
      END DO
      RETURN
      END
