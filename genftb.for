C STRUCTURED BY FOR_STRUCT, V2.0.1, ON 04/21/94 AT 16:17:44
C  OPTIONS SET: DUP=R FMT=ACHN I=50,3 LABF=10000,10 LABX=50,50 OFF=AI
C               S=9 SP=DE V
      SUBROUTINE genftb
*
************************************************************************
*
*     SUBROUTINE TO CALCULATE ALL OF THE ADMISSIBLE FRACIONAL FLOW
*     COEFFICIENTS FOR ALL OF THE POSSIBLE SATURATION COMBINATIONS
*     FOR A FIELD HAVING THE GIVEN ROCK ATTRIBUTES.  THE ANSWERS TAKE
*     THE FORM OF TWO ONE HUNDRED ONE BY ONE HUNDRED ONE TABLES WITH
*     THE VALUES CALCULATED EVERY 0.01 SATURATION UNITS APART.  THE
*     X AXIS IS WATER SATURATION, THE Y AXIS IS OIL SATURATION, AND
*     THE TABLE VALUE IS EITHER THE WATER OR OIL FRATIONAL FLOW RATE.
*     THE VALUES ARE LATER OBTAINED BY SIMPLY MULTIPLYING THE SATURATION
*     VALUES BY ONE HUNDRED, ROUNDING THEM TO THE NEAREST EVEN 0.01
*     SATURATION MULTIPLE IN THE TABLES, AND THEN BY USING THESE VALUES
*     AS THE INDICIES INTO THE TABLES.
*
*     DIRECTED BY             JAMES F. STEVENS         NOVEMBER 1, 1991
*                             TEXACO, INC.             HOUSTON, TEXAS
*
*     STRUCTURED BY           JOHN PRIEDITIS           OCTOBER 21, 1991
*                             TEXACO, INC.             HOUSTON, TEXAS
*
*     WRITTEN BY              JAMES W. MUELLER         NOVEMBER 20, 1991
*                             SOLUTIONS SYSTEMS        HOUSTON, TEXAS
*
*     MODIFIED BY             JOHN PRIEDITIS           JUN 3, 1993
*                             TEXACO, INC.             HOUSTON, TEXAS
************************************************************************
*
*     CALLING VARIABLES:
*
*     NONE
*
*     LOCAL VARIABLES:
*
*     FLOW,FLOW2 - REAL     = TEMPORARY VARIABLES TO BE USED TO CHECK
*                             FINAL PARTIAL FLOW VALUES
*     KRGMIS - REAL         = KRG VALUE FOR CALCULATION OF KRM USING
*                             THE AVERAGE OF KROW AND KRG WHERE KRG IS
*                             CALCULATED AT SG = 1-SW
*     I      - INTEGER      = OIL SATURATION DO LOOP INDEX
*     J      - INTEGER      = WATER SATURATION DO LOOP INDEX
*     SG     - REAL         = GAS SOLVENT SATURATION
*     SO     - REAL         = OIL SATURATION
*     SRM    - REAL         = SORM OR 0.0001
*     SW     - REAL         = WATER SATURATION
*     SNW    - REAL         = NON-WATER SATURATION. EQUAL TO 1.0-SW
*     SGEFF  - REAL         = EFFECTIVE GAS SATURATION USED IN REL
*                             PERM CALCULATIONS
*     TEMP1  - REAL         = TEMPORARY VARIABLE TO BE USED IN STATE
*                             CALCULATIONS
*
************************************************************************
*
*     SUBROUTINE INCLUDE STATEMENTS
*
      INCLUDE 'PARAM.INC'
      INCLUDE 'ROCKAT.INC'
      INCLUDE 'SATFLW.INC'
*
***********************************************************************
*
*     LOCAL VARIABLE DEFINITIONS
*
      INTEGER i
      INTEGER j
*
      DOUBLE PRECISION flow, flow2, flow3
      DOUBLE PRECISION krgmis
      DOUBLE PRECISION sg
      DOUBLE PRECISION snw
      DOUBLE PRECISION so
      DOUBLE PRECISION srm
      DOUBLE PRECISION sw
      DOUBLE PRECISION sgeff
      DOUBLE PRECISION temp
*
************************************************************************
*     INITIALIZE
************************************************************************
*
      somin = sorm
      somax = 1.0d0 - swc
      swmin = swc
      swmax = 1.0d0 - sorm
      temp = 1.0d0 - sorm - sgr
      IF (temp .GT. swmax) swmax = temp
      temp = 1.0d0 - sorm - ssr
      IF (temp .GT. swmax) swmax = temp
*
************************************************************************
*     FOR ALL SATURATION VALUES,COMPUTE THE FRACTIONAL FLOW AND MOBILITY
*     VALUES
************************************************************************
*
      DO i = 0, 100
         so = real(i)*0.01d0
         DO j = 0, 100
            sw = real(j)*0.01d0
            sg = 1.0d0 - so - sw
            snw = 1.0d0 - sw
            IF (sg .LT. 0.0d0) sg = 0.0d0
            IF (sg .LT. 0.05d0) THEN
               srm = 0.0001d0
            ELSE
               srm = sorm
            END IF
*
************************************************************************
*     COMPUTE THE PERMEABILITIES
************************************************************************
*
            temp = 1.0d0 - swc
            IF (sg .LE. sgr) THEN
               krg = 0.0d0
            ELSE IF (sg .GE. temp) THEN
               krg = krgcw
            ELSE
               krg = krgcw*((sg - sgr)/(temp - sgr))**expg
            END IF
            IF (snw .LE. sgr) THEN
               krgmis = 0.0d0
            ELSE IF (snw .GE. temp) THEN
               krgmis = krgcw
            ELSE
               krgmis = krgcw*((snw - sgr)/(temp - sgr))**expg
            END IF
************************************************************************
            IF (1.0d0 - sg .LE. swc + sorg) THEN
               krog = 0.0d0
            ELSE IF (sg .LE. 0.0d0) THEN
               krog = krocw
            ELSE
               krog = krocw*((temp - sorg - sg)/(temp - sorg))**expog
            END IF
************************************************************************
            temp = 1.0d0 - sorw
            IF (sw .LE. swr) THEN
               krw = 0.0d0
            ELSE IF (sw .GE. temp) THEN
               krw = kwro
            ELSE
               krw = kwro*((sw - swr)/(temp - swr))**expw
               IF (sg .LT. 0.05d0) krw = kwro*((sw - swc)/(temp - swc))
     1          **expw
            END IF
************************************************************************
            IF (sw .GE. 1.0d0 - sorw) THEN
               krow = 0.0d0
            ELSE IF (sw .LE. swc) THEN
               krow = krocw
            ELSE
               krow = kromax*((temp - sw)/(temp - swc))**expo
               IF (sg .LT. 0.05d0) krow = krocw*((temp - sw)/(temp - 
     1          swc))**expow
            END IF
            IF (sw .GT. swc .AND. sw .LT. swir) krow = krocw*((temp - 
     1       sw)/(temp - swc))**expow
            IF (krow .GT. krocw) krow = krocw
************************************************************************
            IF (krmsel .EQ. 4) THEN
               sgeff = snw
            ELSE
               sgeff = snw - s0rm
            END IF
C                SGEFF = SG
            IF (sgeff .LE. ssr) THEN
               krs = 0.0d0
            ELSE IF (sgeff .GE. 1.0d0 - swir - s0rm) THEN
               krs = krsmax
            ELSE
               krs = krsmax*((sgeff - ssr)/(1.0d0 - swir - ssr - sorm))
     1          **exps
            END IF
************************************************************************
            temp = 1.0d0/krocw
            kro = temp*((temp*krow + krw)*(temp*krog + krg) - krg - 
     1       krw)
            IF (kro .LT. 0.0d0) kro = 0.0d0
************************************************************************
            temp = 1.0d0/(1.0d0 - sw - srm)
            IF (krmsel .EQ. 0) krm = (so - srm)*temp*krow + sg*temp*krs
            IF (krmsel .EQ. 1) THEN
               krm = 0.5d0*(krow + krgmis)
               IF (sg .LT. 0.02d0) krm = krow
            END IF
            IF (krmsel .EQ. 2) krm = krow
            IF (krmsel .EQ. 3) THEN
               IF (sg .GE. so) krm = krgmis
               IF (sg .LT. so) krm = krow
            END IF
            IF (krmsel .EQ. 4) THEN
               IF (sg .GE. so) krm = krs
               IF (sg .LT. so) krm = krow
            END IF
************************************************************************
            IF (so .LE. srm) THEN
               kroeff = onemal*kro
            ELSE
               kroeff = onemal*kro + alpha*(so - srm)*temp*krm
            END IF
            krgeff = onemal*krg + alpha*sg*temp*krm
*
************************************************************************
*     COMPUTE THE VISCOSITIES
************************************************************************
*
            IF (sw .EQ. 1.0d0) THEN
               sw = 0.9999d0
               sg = 0.0001d0
            END IF
            vism = ((so/(viso**0.25d0) + sg/(visg**0.25d0))/(1.0d0 - 
     1       sw))**( - 4.0d0)
            IF (sw .EQ. 0.9999d0) THEN
               sw = 1.0d0
               sg = 0.0d0
            END IF
            visgm = visg**onemw*vism**w
            visom = viso**onemw*vism**w
            visge = onemal*visg + alpha*visgm
            visoe = onemal*viso + alpha*visom
*
************************************************************************
*     COMPUTE THE FRACTIONAL FLOWS AND THE TOTAL MOBILITY COEFFICIENT
************************************************************************
*
            temp = 1.0d0/(krw/visw + krgeff/visge + kroeff/visoe)
            flow = kroeff*temp/visoe
            IF (so .LE. sorm) THEN
               flow = 0.0d0
            ELSE IF (sg .LE. ssr .AND. sw .LE. swr) THEN
               flow = 1.0d0
            ELSE IF (flow .GT. 1.0d0) THEN
               flow = 1.0d0
            ELSE IF (flow .LT. 0.0d0) THEN
               flow = 0.0d0
            END IF
            flow2 = krw*temp/visw
            IF (sw .LE. swr) THEN
               flow2 = 0.0d0
            ELSE IF (sw .GT. 1.0d0 - sorw) THEN
               flow2 = 1.0d0
            ELSE IF (flow2 .GT. 1.0d0) THEN
               flow2 = 1.0d0
            ELSE IF (flow2 .LT. 0.0d0) THEN
               flow2 = 0.0d0
            END IF
            flow3 = krgeff*temp/visge
            IF ((flow + flow2 + flow3) .GT. 1.0d0) THEN
               flow = flow/(flow + flow2 + flow3)
               flow2 = flow2/(flow + flow2 + flow3)
               flow3 = flow3/(flow + flow2 + flow3)
            END IF
            IF (1.0d0 - flow - flow2 .LT. 1.0d-6) THEN
               flow = flow/(flow + flow2)
               flow2 = flow2/(flow + flow2)
            END IF
            fotabl(i, j) = flow
            fwtabl(i, j) = flow2
            tmtabl(i, j) = temp
         END DO
*
************************************************************************
*     RETURN ANSWER TO CALLER
************************************************************************
*
      END DO
      RETURN
      END
