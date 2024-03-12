C STRUCTURED BY FOR_STRUCT, V2.0.1, ON 04/21/94 AT 16:28:49
C  OPTIONS SET: DUP=R FMT=ACHN I=50,3 LABF=10000,10 LABX=50,50 OFF=AI
C               S=9 SP=DE V
      SUBROUTINE strmtb(plotit, ntimes, nlayrs, nlines, nsegms, nsteps)
*
************************************************************************
*
*     SUBROUTINE TO CALCULATE FIRST ALL OF THE STREAMLINES.  THEN TO
*     CALCULATE ALL OF THE STREAMTUBES FROM ALL OF THE INJECTION WELLS
*     TO ALL OF THE ADMISSIBLE PRODUCTION WELLS.  FINALLY TO CALCULATE
*     ALL OF THE FLOW FROM ALL OF THE INJECTION WELLS THROUGH ALL OF THE
*     STREAMTUBES TO ALL OF THE PRODUCTION WELLS.  THIS IS DONE IN A
*     STEPWISE FORWARD DIFFERENCE MODEL.
*
*     POTENTIAL FIELD STARTS ON A CIRCLE WITH ITS CENTER AT AN
*     INJECTION WELL AND ITS RADIUS SET TO 0.01 TIMES THE MINIMUM
*     DISTANCE FROM THE INJECTION WELL TO THE NEAREST PRODUCTING WELL.
*
*     DIRECTED BY             JAMES F. STEVENS        SEPTEMBER 2, 1991
*                             TEXACO, INC.            HOUSTON, TEXAS
*
*     STRUCTURED BY           JOHN PRIEDITIS          OCTOBER 21, 1991
*                             TEXACO, INC.            HOUSTON, TEXAS
*
*     WRITTEN BY              JAMES W. MUELLER        SEPTEMBER 18, 1991
*                             SOLUTIONS SYSTEMS       HOUSTON, TEXAS
*
*     MODIFIED BY             JOHN PRIEDITIS          MAY 27, 1993
*                             TEXACO, INC             HOUSTON, TEXAS
************************************************************************
*
*     CALLING VARIABLES:
*
*     PLOTIT - INPUT        = PLOT OR DO NOT PLOT STREAMLINE INDICATOR
*     NLAYRS - INPUT        = NUMBER OF EQUALLY SPACED LAYERS INTO WHICH
*                             TO DIVIDE THE FIELD'S PRODUCTION ZONE
*     NLINES - INPUT        = NUMBER OF EQUALLY SPACED STREAMLINES TO
*                             START FROM EACH INJECTION WELL
*     NSEGMS - INPUT        = NUMBER OF SEGMENTS INTO WHICH TO DIVIDE
*                             THE STREAMTUBES
*     NSTEPS - INPUT        = MAXIMUM NUMBER OF STEPS IN ANY STREAMLINE
*     NTIMES - INPUT        = NUMBER OF DIFFERENT TIMED FLOOD PROGRAMS
*                             IN THE FIELD PRODUCTION STUDY
*
*     LOCAL VARIABLES:
*
*     A      - REAL         = "A" TERM IN THE QUADRATIC EQUATION
*     ATRBTS - REAL         = PRODUCT OF SEVERAL FIELD ATTRIBUTES
*     B      - REAL         = "B" TERM IN THE QUADRATIC EQUATION
*     C      - REAL         = "C" TERM IN THE QUADRATIC EQUATION
*     C1D2PI - REAL         = CONSTANT OF ONE DIVIDED BY TWO PI
*     CORARA - REAL         = CORRECTION FACTOR:(TUBE AREA)/(TOT AREA)
*     DELSQR - REAL         = SUM OF THE DELTA X AND Y DISTANCES SQUARED
*     DELTAL - REAL         = LINE LENGTH OF LAST STEP TO CIRCLE AROUND
*                             PRODUCTION WELL
*     DELTAX - REAL         = X DISTANCE FROM STREAMLINE TO OBJECT WELL
*     DELTAY - REAL         = Y DISTANCE FROM STREAMLINE TO OBJECT WELL
*     DELTIM - REAL         = DELTA TIME USED TO MOVE A STREAMLINE STEP
*     DPOTDX - REAL         = PARTIAL DIFFERENTIAL OF POTENTIAL WITH
*                             RESPECT TO X
*     DPOTDY - REAL         = PARTIAL DIFFENENTIAL OF POTENTIAL WITH
*                             RESPECT TO Y
*     FGIN   - REAL         = FRACTIONAL FLOW OF GAS SOLVENT INTO
*                             SEGMENT
*     FGOUT  - REAL         = ARRAY OF FRACTIONAL FLOWS OF GAS SOLVENT
*                             OUT OF SEGMENTS
*     FLWTIM - REAL         = FLOW TIME THAT THE MODLE HAS BEEN RUNNING
*     FOIN   - REAL         = FRACTIONAL FLOW OF OIL INTO SEGMENT
*     FOOUT  - REAL         = ARRAY OF FRACTIONAL FLOWS OF OIL OUT OF
*                             SEGMENTS
*     FWIN   - REAL         = FRACTIONAL FLOW OF WATER INTO SEGMENT
*     FWOUT  - REAL         = ARRAY OF FRACTIONAL FLOWS OF WATER OUT OF
*                             SEGMENTS
*     PVFACT - REAL         = FACTOR USED IN OUTPUT TO MAKE IT IN TERMS
*                             OF TOTAL PORE VOLUME OR HYDROCARBON PV
*     I      - INTEGER      = WELL NUMBER FOR THE WELL LOOP
*     II     - INTEGER      = WELL NUMBER FOR THE WELL LOOP
*     INTRCP - REAL         = INTERCEPT OF LINE EQUATION JOINING LAST
*                             TWO COMPUTED POINTS IN STREAMLINE
*     IPRINT - INTEGER      = COUNTER FOR PRINTING
*     ISTEP  - INTEGER      = OUTPUT STEP INCREMENT NUMBER
*     ISO    - INTEGER      = OIL SATURATION INDEX - "SO" VALUE ROUNDED
*                             TO THE NEAREST INTEGER TO BE USED IN
*                             OBTAINING VALUES FROM THE FRACTIONAL FLOW
*                             AND TOTAL MOBILITY TABLES
*     ITEMP  - INTEGER      = TEMPORARY INTEGER VALUE
*     J      - INTEGER      = STREAMLINE OR TUBE NUMBER FOR THE TUBE
*                             LOOP
*     JSW    - INTEGER      = WATER SATURATION INDEX - "SW" VALUE
*                             ROUNDED TO THE NEAREST INTEGER TO BE USED
*                             IN OBTAINING VALUES FROM THE FRACTIONAL
*                             FLOW AND TOTAL MOBILITY TABLES
*     K      - INTEGER      = PRESENT STREAMLINE STEP FOR THE STREAMLINE
*                             LOOP OR SEGMENT NUMBER FOR THE SEGMENT
*                             LOOP
*     KSTEP  - INTEGER      = PRESENT STREAMLINE STEP PLUS ONE
*     L      - INTEGER      = PRESENT OBJECT WELL OR LAYER NUMBER FOR
*                             THE LAYER LOOP INDEX
*     N      - INTEGER      = PRODUCTION WELL NUMBER POINTER FOR THE
*                             WELL LOOP
*     NOTUBE - INTEGER      = NUMBER OF STREAM TUBES ON PRESENT LAYER
*     LCM    - REAL         = LENGTH OF LINE FROM LAST POINT OUTSIDE OF
*                             CIRCLE AROUND A PRODUCER TO MINUS SOLUTION
*                             OF THE CIRCLE AND LINE INTERSECTION
*     LCP    - REAL         = LENGTH OF LINE FROM LAST POINT OUTSIDE OF
*                             CIRCLE AROUND A PRODUCER TO PLUS SOLUTION
*                             OF THE CIRCLE AND LINE INTERSECTION
*     LENGTH - REAL         = LENGTH TO REACH PRESENT STREAMLINE STEP
*     LWFILE - CHARACTER    = LENGTH-WIDTH FILE
*     M      - INTEGER      = INJECTION WELL NUMBER POINTER FOR THE WELL
*                             LOOP
*     MINSTV - REAL         = MINIMUM OF ALL STVMIN VALUES
*     MINTUB - INTEGER      = MINIMUM NUMBER OF TUBES USED FOR ALL
*                             INJECTION WELLS
*     NOTUB  - INTEGER      = ARRAY OF TUBES USED IN INJECTION WELLS
*     NN     - INTEGER      = INDEX FOR DO LOOPS
*     ONEMTR - REAL         = ONE MINUS ISO TABLE PROPORTION RATIO
*     ONEMUR - REAL         = ONE MINUS JSW TABLE PROPORTION RATIO
*     ORNLAY - REAL         = ORIGINAL NUMBER OF LAYERS WHEN STREAM
*                             TUBE PARAMETERS ARE GENERATED
*     PASS1  - LOGOCAL      = FIRST PASS THROUGH MAJOR LOOP INDICATOR
*     PENDWA - INTEGER      = PLOT VARIABLE FOR "PEN DOWN AFTER MOVE"
*     PENDWB - INTEGER      = PLOT VARIABLE FOR "PEN DOWN BEFORE MOVE"
*     PENUPA - INTEGER      = PLOT VARIABLE FOR "PEN UP AFTER MOVE"
*     PENUPB - INTEGER      = PLOT VARIABLE FOR "PEN UP BEFORE MOVE"
*     PLOTR  - REAL         = REAL*4 TEMPORARY VARIABLE FOR CIRCLE CALL
*     PLOTX  - REAL         = REAL*4 TEMPORARY VARIABLE FOR PLOT CALL
*     PLOTY  - REAL         = REAL*4 TEMPORARY VARIABLE FOR PLOT CALL
*     PONTIN - LOGICAL      = LOGICAL TO TELL IF A POINT IS WITHIN THE
*                             BOUNDING POLYGON OR NOT
*     PORVOL - REAL         = ONE PORE VOLUME
*     PVINJT - REAL         = PORE VOLUME INJECTED
*     QINBAS - REAL         = NORMALIZING RATE
*     QINMAX - REAL         = MAXIMUM INJECTION RATE FOR ANY
*                             TIME PERIOD OR INJECTION WELL
*     QINTOT - REAL         = TOTAL INJECTION RATE
*     RADIUS - REAL         = RADIUS OF POTENTIAL CIRCLE
*     RADSQR - REAL         = POTENTIAL CIRCLE RADIUS SQUARED FOR SEARCH
*     RANCON - REAL         = CONSTANT NUMBER OF RADIANS IN CIRCLE STEP
*     RANGL0 - REAL         = STARTING ANGLE FOR LOOP CALCULATIONS
*     RANGLE - REAL         = CURRENT STEP ANGLE IN LOOP CALCULATIONS
*     RNSTEP - REAL         = ARRAY TO CONTROL PROPORTIONING OF SEGMENTS
*     SAI2PW - REAL         = SLOPE ANGLE OF LINE JOINING PRESENT
*                             INJECTION WELL TO SELECTED PRODUCTION WELL
*     SAI2J1 - REAL         = SLOPE ANGLE OF LINE JOINING PRESENT
*                             INJECTION WELL TO JTH STREAMLINE ON THE
*                             MINIMUM DISTANCE CIRCLE
*     SAI2J2 - REAL         = SLOPE ANGLE OF LINE JOINING PRESENT
*                             INJECTION WELL TO (J+1)TH STREAMLINE ON
*                             THE MINIMUM DISTANCE CIRCLE
*     SCALE  - REAL         = SCALE FACTOR TO FILL SCREEN WITH PLOT
*     SEGDIS - REAL         = ARRAY OF TEMPORARYILY SAVED STEP LENGTHS
*     SG     - REAL         = GAS SOLVENT SATURATION
*     SLOPE  - REAL         = SLOPE OF LINE EQUATION JOINING LAST TWO
*                             COMPUTED POINTS IN STREAMLINE
*     SO     - REAL         = OIL SATURATION
*     SOMAX  - REAL         = MAXIMUM OIL SATURATION ALLOWED AFTER OIL
*                             ENTERS MAX/MIN RANGE
*     SOMIM  - REAL         = MINIMUM OIL SATURATION ALLOWED AFTER OIL
*                             ENTERS MAX/MIN RANGE
*     STVMIN - REAL         = MINIMUM VOLUME VALUE OF ALL SEGMENTS IN
*                             ALL OF THE STREAMTUBES OF ONE WELL
*     STVMN  - REAL         = ARRAY OF STVMIN
*     SUMALL - REAL         = SUM OF ALL OF THE LAYER, TUBE, AND SEGMENT
*                             FLOWS
*     SUMLAY - REAL         = SUM OF ALL OF THE TUBE AND SEGMENT FLOWS
*     SUMLWT - REAL         = SUM OF ALL OF THE SEGMENT FLOWS
*     SUMTUB - REAL         = SUM OR RATIO OF THE FLOW INTO A GIVEN
*                             LAYER VERSUS THE TOTAL FLOW INTO THE WELL
*     SW     - REAL         = WATER SATURATION
*     SWMAX  - REAL         = MAXIMUM WATER SATURATION ALLOWED AFTER
*                             WATER ENTERS MAX/MIN RANGE
*     SWMIM  - REAL         = MINIMUM WATER SATURATION ALLOWED AFTER
*                             WATER ENTERS MAX/MIN RANGE
*     T      - INTEGER      = TIME STEP INDEX FOR TIME LOOP
*     TT     - INTEGER      = TIME STEP INDEX FOR TIME LOOP
*     TAREA  - REAL         = TEMPORARY VARIABLE TO HOLD X/Y AREA OF A
*                             SEGMENT
*     TEMP   - REAL         = TEMPORARY VARIABLE TO BE USED IN
*                             CALCULATIONS TO SAVE REDUNDENCY
*     TEMPFG - REAL         = TEMPORARY SOLVENT INCREMENTAL FLOW VALUE
*     TEMPTM - REAL         = TEMPORARY NEXT TOTAL MOBILITY VALUE
*     TIME   - REAL         = TIME TO REACH PRESENT STREAMLINE STEP
*     TLONG  - REAL         = TEMPORARY VARIABLE TO HOLD MEAN LENGTH OF
*                             A SEGMENT
*     TR     - REAL         = ISO TABLE PROPORTION RATIO
*     TSTEP  - REAL         = TEMPORARY VARIABLE TO HOLD SUM OF LINE
*                             SEGMENTS ALONG STREAMLINE AS IT IS DIVIDED
*     TUBINF - CHARACTER    = TUBE INFORMATION FILE
*     TVOLM  - REAL         = TEMPORARY VARIABLE TO HOLD MEAN VOLUME OF
*                             A SEGMENT
*     TWIDE  - REAL         = TEMPORARY VARIABLE TO HOLD MEAN WIDTH OF A
*                             SEGMENT
*     UR     - REAL         = JSW TABLE PROPORTION RATIO
*     VELCTX - REAL         = X VELOCITY OF PRESENT STREAMLINE STEP
*     VELCTY - REAL         = Y VELOCITY OF PRESENT STREAMLINE STEP
*     VSTEP  - REAL         = LENTH OF EQUAL LINE SEGMENTS INTO WHICH
*                             STREAMLINE IS DIVIDED
*     VOLARA - CHARACTER    = VOLUME - AREA FILE
*     WELLCH - CHARACTER    = CHARACTER VARIABLE FOR WELL NUMBER
*     WPRODG - REAL         = WELL GAS PRODUCTION
*     WPRODO - REAL         = WELL OIL PRODUCTION
*     WPRODW - REAL         = WELL WATER PRODUCTION
*     WTRCHS - CHARACTER    = VARIABLE TO DETERMINE WHETHER SIMULATION IS
*                             IN A WATER CHASE PERIOD
*     XBACK  - REAL         = X VALUES OF BACKGROUND SQUARE FOR GRAPHICS
*     XCM    - REAL         = X COMPONENT OF LINE FROM LAST POINT
*                             OUTSIDE OF CIRCLE AROUND A PRODUCER TO
*                             MINUS SOLUTION OF THE CIRCLE AND LINE
*                             INTERSECTION
*     XCP    - REAL         = X COMPONENT OF LINE FROM LAST POINT
*                             OUTSIDE OF CIRCLE AROUND A PRODUCER TO
*                             PLUS SOLUTION OF THE CIRCLE AND LINE
*                             INTERSECTION
*     XMAX   - REAL         = MAXIMUM X VALUE TO PLOT
*     XMIN   - REAL         = MINUMUM X VALUE TO PLOT
*     XR     - REAL         = SELECTED X COMPONENT BECAUSE SOLUTION
*                             IS CLOSED TO SECOND FROM THE LAST POINT
*     XSTEP  - REAL         = ARRAY X STEPS IN PRESENT STREAMLINE
*     YBACK  - REAL         = Y VALUES OF BACKGROUND SQUARE FOR GRAPHICS
*     YCM    - REAL         = Y COMPONENT OF LINE FROM LAST POINT
*                             OUTSIDE OF CIRCLE AROUND A PRODUCER TO
*                             MINUS SOLUTION OF THE CIRCLE AND LINE
*                             INTERSECTION
*     YCP    - REAL         = Y COMPONENT OF LINE FROM LAST POINT
*                             OUTSIDE OF CIRCLE AROUND A PRODUCER TO
*                             PLUS SOLUTION OF THE CIRCLE AND LINE
*                             INTERSECTION
*     YMAX   - REAL         = MAXIMUM Y VALUE TO PLOT
*     YMIN   - REAL         = MINIMUM Y VALUE TO PLOT
*     YORN   - CHARACTER    = SWITCH TO RELEASE PROGRAM FROM GRAPHICS
*     YR     - REAL         = SELECTED Y COMPONENT BECAUSE SOLUTION
*                             IS CLOSED TO SECOND FROM THE LAST POINT
*     YSTEP  - REAL         = ARRAY Y STEPS IN PRESENT STREAMLINE
*
************************************************************************
*
*     SUBROUTINE INCLUDE STATEMENTS
*
      INCLUDE 'PARAM.INC'
      INCLUDE 'FIELD.INC'
      INCLUDE 'ROCKAT.INC'
      INCLUDE 'SATFLW.INC'
      INCLUDE 'STREAM.INC'
*
***********************************************************************
*
*     CALLING VARIABLE DEFINITIONS
*
      INTEGER nlayrs
      INTEGER nlines
      INTEGER nsegms
      INTEGER nsteps
      INTEGER ntimes
*
      LOGICAL plotit
*
************************************************************************
*
*     LOCAL VARIABLE DEFINITIONS
*
      DOUBLE PRECISION c1d2pi
      PARAMETER (c1d2pi = 1.0d0/(2.0d0*3.141592653589793238d0))
*
      CHARACTER*1 yorn
      CHARACTER*9 lenwid(maxinj)
      CHARACTER*12 lwfile
      CHARACTER*12 volara
      CHARACTER*12 tubinf
      CHARACTER*3 wellch(maxinj)
      CHARACTER*1 wtrchs
*
      INTEGER i
      INTEGER ii
      INTEGER iprint
      INTEGER istep
      INTEGER itemp
      INTEGER iso
      INTEGER j
      INTEGER jsw
      INTEGER k
      INTEGER kstep
      INTEGER l
      INTEGER m
      INTEGER mintub
      INTEGER n, nn
      INTEGER notube
      INTEGER notub(maxinj)
      INTEGER penupb
      INTEGER pendwb
      INTEGER penupa
      INTEGER pendwa
      INTEGER t
      INTEGER tt
      INTEGER ornlay
*
      LOGICAL pass1
      LOGICAL pontin
*
      DOUBLE PRECISION a
      DOUBLE PRECISION atrbts
      DOUBLE PRECISION b
      DOUBLE PRECISION c
      DOUBLE PRECISION corara
      DOUBLE PRECISION delsqr
      DOUBLE PRECISION deltal
      DOUBLE PRECISION deltax
      DOUBLE PRECISION deltay
      DOUBLE PRECISION deltim
      DOUBLE PRECISION dpotdx
      DOUBLE PRECISION dpotdy
      DOUBLE PRECISION flwtim
      DOUBLE PRECISION foin
      DOUBLE PRECISION foout(maxlay, maxlin, maxseg)
      DOUBLE PRECISION fwin
      DOUBLE PRECISION fwout(maxlay, maxlin, maxseg)
      DOUBLE PRECISION hpor
      DOUBLE PRECISION intrcp
      DOUBLE PRECISION lcp
      DOUBLE PRECISION lcm
      DOUBLE PRECISION length
      DOUBLE PRECISION minstv
      DOUBLE PRECISION newtem
      DOUBLE PRECISION onemtr
      DOUBLE PRECISION onemur
      DOUBLE PRECISION otemp
      DOUBLE PRECISION wtemp
      DOUBLE PRECISION gtemp
      DOUBLE PRECISION porvol
      DOUBLE PRECISION pvfact
      DOUBLE PRECISION pvinjt
      REAL plotr
      REAL plotx
      REAL ploty
      DOUBLE PRECISION qinbas
      DOUBLE PRECISION qinmax
      DOUBLE PRECISION qintot
      DOUBLE PRECISION radius
      DOUBLE PRECISION radsqr
      DOUBLE PRECISION rancon
      DOUBLE PRECISION rangl0
      DOUBLE PRECISION rangle
      DOUBLE PRECISION rnstep(maxseg)
      DOUBLE PRECISION sai2j1
      DOUBLE PRECISION sai2j2
      DOUBLE PRECISION sai2pw
      DOUBLE PRECISION scale
      DOUBLE PRECISION segdis(maxstp)
      DOUBLE PRECISION slope
      DOUBLE PRECISION so
      DOUBLE PRECISION stvmin
      DOUBLE PRECISION stvmn(maxinj)
      DOUBLE PRECISION sumall
      DOUBLE PRECISION sumlay(maxlay)
      DOUBLE PRECISION sumlwt(maxlay, maxlin)
      DOUBLE PRECISION sumtub
      DOUBLE PRECISION sw
      DOUBLE PRECISION tarea
      DOUBLE PRECISION tr
      DOUBLE PRECISION temp
      DOUBLE PRECISION tempfg
      DOUBLE PRECISION temptm
      DOUBLE PRECISION time
      DOUBLE PRECISION tlong
      DOUBLE PRECISION tstep
      DOUBLE PRECISION tvolm
      DOUBLE PRECISION twide
      DOUBLE PRECISION ur
      DOUBLE PRECISION velctx
      DOUBLE PRECISION velcty
      DOUBLE PRECISION vstep
      REAL xback(4)
      DOUBLE PRECISION wprodg(maxwel)
      DOUBLE PRECISION wprodo(maxwel)
      DOUBLE PRECISION wprodw(maxwel)
      DOUBLE PRECISION xstep(maxstp + 1)
      DOUBLE PRECISION xmax
      DOUBLE PRECISION xmin
      DOUBLE PRECISION xcm
      DOUBLE PRECISION xcp
      DOUBLE PRECISION xr
      REAL yback(4)
      DOUBLE PRECISION ystep(maxstp + 1)
      DOUBLE PRECISION ymax
      DOUBLE PRECISION ymin
      DOUBLE PRECISION ycm
      DOUBLE PRECISION ycp
      DOUBLE PRECISION yr
*
      DATA pass1/.TRUE./
*
      DATA pendwa, pendwb, penupa, penupb/3, 2, 2, 3/
*
      DATA xback/0.0, 11.0, 11.0, 0.0/
      DATA yback/0.0, 0.0, 8.5, 8.5/
*
      DATA wellch/'.1', '.2', '.3', '.4', '.5', '.6', '.7', '.8', '.9', 
     1     '.10'/
***********************************************************************
*     COMMUTE STREAMTUBE SEGMENT SIZE MODIFICATIONS TO INCREASE
*     SIZE OF INLET AND OUTLET SEGMENTS
***********************************************************************
*
      rnstep(1) = 5.75d0
      rnstep(2) = 3.25d0
      rnstep(3) = 2.25d0
      rnstep(4) = 1.25d0
      rnstep(nsegms) = 5.75d0
      rnstep(nsegms - 1) = 3.25d0
      rnstep(nsegms - 2) = 2.25d0
      rnstep(nsegms - 3) = 1.25d0
      DO i = 5, (nsegms - 4)
         rnstep(i) = 1.0d0
      END DO
      IF (nsegms .LT. 50) THEN
         DO i = nsegms + 1, 50
            rnstep(i) = 1.0d0
         END DO
      END IF
*
************************************************************************
*     INITIALIZE
************************************************************************
*
      rangl0 = 0.0d0
      iprint = 0
      rancon = 0.017453292d0*360.0d0/real(nlines)
      radius = 0.0d0
      DO i = 1, noinjw
         radius = radius + 0.01d0*dismin(i)
      END DO
      radius = radius/real(noinjw)
      radsqr = radius*radius
*
************************************************************************
*     COMPUTE MAXIMUM INJECTION FOR ANY WELL AND ANY TIME PERIOD.
*     QINMAX IS THIS INJECTION RATE.
*     NORMALIZE THE INJECTION RATES BASED ON THE FIRST WELL.
*     STORE THE ACTUAL (ORIGINAL RATES).
************************************************************************
*
      temp = 0.0d0
      qinbas = qinjct(1, 1)
      DO t = 1, ntimes
         qinmax = 0.0d0
         DO i = 1, noinjw
            qinorg(t, i) = qinjct(t, i)
            qinjct(t, i) = qinjct(t, i)/qinbas
            qinmax = qinmax + qinjct(t, i)
         END DO
         IF (qinmax .GT. temp) temp = qinmax
      END DO
      qinmax = temp
************************************************************************
*     NAME SEGMENT LENGTH AND WIDTH OUTPUT FILES AND MIN VOL FILE
************************************************************************
*
      lenwid(1) = 'ZZLNWD01.'
      lenwid(2) = 'ZZLNWD02.'
      lenwid(3) = 'ZZLNDW03.'
      lenwid(4) = 'ZZLNWD04.'
      lenwid(5) = 'ZZLNWD05.'
      lenwid(6) = 'ZZLNWD06.'
      lenwid(7) = 'ZZLNWD07.'
      lenwid(8) = 'ZZLNWD08.'
      lenwid(9) = 'ZZLNWD09.'
      lenwid(10) = 'ZZLNWD10.'
*
************************************************************************
*     INITIALIZE VARIABLES AND GRAPHICS FOR PLOT.
************************************************************************
*
      IF (plotit) THEN
         xmax =  - 1.0d30
         xmin = 1.0d30
         ymax =  - 1.0d30
         ymin = 1.0d30
         DO i = 1, nwells
            IF (wellx(i) .GT. xmax) THEN
               xmax = wellx(i)
            ELSE IF (wellx(i) .LT. xmin) THEN
               xmin = wellx(i)
            END IF
            IF (welly(i) .GT. ymax) THEN
               ymax = welly(i)
            ELSE IF (welly(i) .LT. ymin) THEN
               ymin = welly(i)
            END IF
         END DO
         IF (nbndpt .GT. 0) THEN
            DO i = 1, nbndpt
               IF (boundx(i) .GT. xmax) THEN
                  xmax = boundx(i)
               ELSE IF (boundx(i) .LT. xmin) THEN
                  xmin = boundx(i)
               END IF
               IF (boundy(i) .GT. ymax) THEN
                  ymax = boundy(i)
               ELSE IF (boundy(i) .LT. ymin) THEN
                  ymin = boundy(i)
               END IF
            END DO
         END IF
         IF ((ymax - ymin)/(xmax - xmin) .GT. 8.5d0/11.0d0) THEN
            scale = 8.5d0/(ymax - ymin)
         ELSE
            scale = 11.0d0/(xmax - xmin)
         END IF
	 CALL plots(0, 1, 0)
         CALL factor(1.0)
         CALL plot(0.0, 0.0, -999)
         CALL newpen(15)
         CALL fill(4, xback, yback)
         CALL newpen(0)
         plotx = (boundx(1) - xmin)*scale
         ploty = (boundy(1) - ymin)*scale
         CALL plot(plotx, ploty, penupb)
         DO i = 2, nbndpt - 1
            plotx = (boundx(i) - xmin)*scale
            ploty = (boundy(i) - ymin)*scale
            CALL plot(plotx, ploty, pendwb)
         END DO
         plotx = (boundx(nbndpt) - xmin)*scale
         ploty = (boundy(nbndpt) - ymin)*scale
         CALL plot(plotx, ploty, penupa)
      END IF
*
************************************************************************
*    INITIALIZE PRODUCTION AND ACCUMULATION ARRAYS
************************************************************************
*
      DO nn = 1, nwells
         wprodg(nn) = 0.0d0
         wprodo(nn) = 0.0d0
         wprodw(nn) = 0.0d0
      END DO
      DO nn = 1, maxout
         prodg(nn) = 0.0d0
         prodo(nn) = 0.0d0
         prodw(nn) = 0.0d0
      END DO
*
************************************************************************
*     IF THE STREAM TUBE INFORMATION FILES ARE NOT TO BE
*     CREATED (THAT IS, IF LWGEN = 'N'), THEN THE EXISTING
*     STREAMTUBE INFORMATION FILES
*     ARE READ IN
************************************************************************
*
      t = 1
      stvmin = 1.0d30
      IF (lwgen .NE. 'Y') THEN
         volara = 'ZZAREA.'//pattrn
         OPEN (11, FILE = volara)
         REWIND (11)
         READ (11, *) minstv, fldara, effara, ornlay, mintub
         DO ii = 1, noinjw
            READ (11, '(1X,I12)') linjp(ii)
         END DO
         CLOSE (11)
         minstv = minstv*real(ornlay)/real(nlayrs)
         tubinf = 'ZZTUBE.'//pattrn
         OPEN (13, FILE = tubinf)
         REWIND (13)
         DO ii = 1, noinjw
            mm = linjp(ii)
            DO j = 1, nlines
               READ (13, '(1X,2I12)') stpath(mm, j), ntubes(mm, j)
            END DO
         END DO
         CLOSE (13)
      END IF
      porvol = fldara*porsty*height*real(nlayrs)
*
************************************************************************
*     CALCULATE NPRINT.  NPRINT IS THE NUMBER OF TIME STEPS FOR
*     EACH PRINT STEP.  MINSTV HAS ALREADY BEEN ADJUSTED FOR
*     THE NUMBER OF LAYERS
************************************************************************
*
      IF (lwgen .EQ. 'N') nprint = nint(0.05/minstv + 5.0)
*
************************************************************************
*     CALCULATE THE STREAMLINES FROM EACH INJECTION WELL TO EACH
*     ADMISSIBLE PRODUCTION WELL IN NOLINE EVEN ANGLULAR INCREMENTS.
*     "I" IS THE PRESENT INJECTION WELL INDEX, AND "M" IS THE ACTUAL
*     INJECTION WELL ARRAY LOCATION.
************************************************************************
*
      istep = 0
      flwtim = 0.0d0
      pvinjt = 0.0d0
      CALL prmint(nlayrs, dpcoef)
      WRITE (12, *) '   LAYER    PERM'
      DO i = 1, nlayrs
         WRITE (12, '(''  '',I5, F10.4)') i, prmabs(i)
      END DO
*
************************************************************************
*  IF CO2 MOBILITY IS LESS THAN OR EQUAL TO WATER MOBILITY,
*  THE RELATIVE INJECTIVITIES INTO DIFFERENT STREAMTUBES ARE PERMITTED
*  TO CHANGE DURING CHASE WATER INJECTION.  IF THE CO2 MOBILITY IS
*  GREATER, THEN THE INJECTIVITY RATIOS FOR ALL THE STREAMTUBES REMAIN
*  THE SAME THAT THEY WERE AT THE END OF THE CO2 OR WAG INJECTION.
************************************************************************
*
      DO i = 1, ntimes
         IF (krmsel .EQ. 0 .OR. krmsel .EQ. 4) THEN
            IF ((krsmax/visg) .LE. (kwro/visw)) chswtr(i) = 'N'
         END IF
         IF (krmsel .EQ. 1 .OR. krmsel .EQ. 3) THEN
            IF ((krgcw/visg) .LE. (kwro/visw)) chswtr(i) = 'N'
         END IF
         IF (krmsel .EQ. 2) THEN
            IF ((krocw/visg) .LE. (kwro/visw)) chswtr(i) = 'N'
         END IF
      END DO
*
************************************************************************
*
*
*     THE MAJOR INJECTION WELL LOOP BEGINS HERE.
*
*     THE DO LOOP (USING 'I') IS AROUND ALL OF THE INJECTION WELLS.
*     THE STREAMTUBES MAY OR MAY NOT BE GENERATED.
*     THE VARIABLE FOR GENERATING STREAMTUBES IS LWGEN.
*         IF LWGEN .EQ. 'N', THEN STREAMTUBES ARE NOT GENERATED
*         IF LWGEN .EQ. 'Y', THEN STREAMTUBES ARE GENERATED
*     IF THE STREAMTUBES ARE TO BE GENERATED, THEN
*     CALCULATE THE STREAMLINES FROM EACH INJECTION WELL TO EACH
*     ADMISSIBLE PRODUCTION WELL IN EVEN ANGLULAR INCREMENTS.
*     "I" IS THE PRESENT INJECTION WELL INDEX, AND "M" IS THE ACTUAL
*     INJECTION WELL ARRAY LOCATION.
*
*
************************************************************************
*
*
*
      DO i = 1, noinjw  !..MAJOR INJECTION WELL LOOP
         wtrchs = 'N'
         IF (.NOT. plotit) WRITE (*, *) 'INJECTION WELL ', i
	 m = linjp(i)
*
************************************************************************
*    SET PARAMETERS
*    CALL GENFTB TO GENERATE FRACTIONAL FLOW TABLE
************************************************************************
*
         kromax = krocw
         expo = expow
         swr = swc
	 CALL genftb()
*
***********************************************************************
*     STREAMTUBE SEGMENT LENGTH AND WIDTH GENERATION BEGINS HERE IF
*     LWGEN EQUALS 'Y'.  IF LWGEN EQUALS 'N', THE FOLLOWING LARGE
*     "IF BLOCK" IS BYPASSED.  STREAMTUBES ARE GENERATED WITHIN THIS
*     STREAMTUBE GENERATION "IF BLOCK".  (A COMMENT IS GIVEN LATER IN
*     THE PROGRAM INDICATING WHERE THIS BLOCK ENDS.)
***********************************************************************
*
	 IF (lwgen .EQ. 'Y') THEN !..STREAMTUBE GENERATION "IF BLOCK"
*
***********************************************************************
*     CALCULATE THE STREAMLINES FROM EACH INJECTION WELL "M".
*     "J" IS THE PRESENT STREAMLINE INDEX.
************************************************************************
*
            DO j = 1, nlines
               rangle = real(j - 1)*rancon + rangl0
               xstep(1) = wellx(m) + radius*cos(rangle)
               ystep(1) = welly(m) + radius*sin(rangle)
               length = 0.0d0
               time = 0.0d0
*
************************************************************************
*     CALCULATE THE STEPS ALONG THE STREAMLINE ONE STEP AT A TIME.
*     "K" IS THE PRESENT STEP INDEX.
************************************************************************
*
               DO k = 1, nsteps
                  kstep = k + 1
                  dpotdx = 0.0d0
                  dpotdy = 0.0d0
*
************************************************************************
*     CALCULATE THIS STEP TAKING INTO ACCOUNT THE EFFECT OF ALL OF THE
*     OTHER WELLS IN THE FIELD.
*     "L" IS THE PRESENT OBJECT WELL INDEX.
************************************************************************
*
                  DO l = 1, nwells + nimgwl
                     deltax = wellx(l) - xstep(k)
                     deltay = welly(l) - ystep(k)
                     delsqr = deltax*deltax + deltay*deltay
                     dpotdx = dpotdx - wellq(l)*deltax/delsqr
                     dpotdy = dpotdy - wellq(l)*deltay/delsqr
                  END DO
                  atrbts = c1d2pi*porsty*height
                  velctx = dpotdx*atrbts
                  velcty = dpotdy*atrbts
                  deltim = radius/sqrt(velctx*velctx + velcty*velcty)
                  deltax = velctx*deltim
                  deltay = velcty*deltim
                  xstep(kstep) = xstep(k) + deltax
                  ystep(kstep) = ystep(k) + deltay
*
************************************************************************
*     CHECK IF STREAMLINE IS STILL WITHIN THE FIELD BOUNDARIES.
************************************************************************
*
                  CALL inpoly(xstep(kstep), ystep(kstep), nbndpt, 
     1             boundx, boundy, pontin)
*
************************************************************************
*     THE VECTOR CAN GO OUT OF THE BOUNDARIES AND ALSO BE IN THE RADIUS
*     OF THE RECEIVING PRODUCTION WELL.  THIS CAN HAPPEN WHEN THE
*     BOUNDARY VALUE(S) GO RIGHT THROUGH THE PRODUCTION WELL'S CENTROID.
*     THEREFORE, DO NOT CHECK FOR THE VECTOR BEING OUT OF THE POLYGON
*     UNTIL IT IS FIRST CHECKED FOR BEING WITHIN THE RECEIVING WELL'S
*     RADIUS.  WHEN THE PROGRAM COMPUTES THE INTERSECTION OF THE VECTOR
*     AND THE RECEIVING RADIUS, THE END POINT WILL BE BACK WITHIN THE
*     POLYGON BOUNDARY LIMITS.
************************************************************************
*
                  tstep = sqrt(deltax*deltax + deltay*deltay)
                  length = length + tstep
                  segdis(k) = tstep
                  time = time + deltim
*
************************************************************************
*     CHECK IF THE PRESENT STREAMLINE HAS REACHED (WITHIN ONE RADIUS OF)
*     ONE OF THE PRODUCTION WELLS.
*     "L" IS THE PRESENT OBJECT WELL INDEX.
************************************************************************
*
                  DO l = 1, nwells
                     IF (wellq(l) .LT. 0.0d0) THEN
                        deltax = wellx(l) - xstep(kstep)
                        deltay = welly(l) - ystep(kstep)
                        delsqr = deltax*deltax + deltay*deltay
                        IF (delsqr .LE. radsqr) GOTO 100
                     END IF
                  END DO
                  IF (.NOT. pontin) GOTO 50
               END DO
               stlong(m, j) = 1.0d30
               sttime(m, j) = 1.0d30
               stpath(m, j) = 32767
               GOTO 150
   50          stlong(m, j) = 1.0d30
               sttime(m, j) = 1.0d30
               stpath(m, j) = 32767
               GOTO 150
  100          slope = (ystep(kstep) - ystep(k))/(xstep(kstep) - xstep
     1          (k))
               intrcp = ystep(kstep) - welly(l) - slope*(xstep(kstep) - 
     1          wellx(l))
               a = 1.0d0 + slope*slope
               b = 2.0d0*slope*intrcp
               c = intrcp*intrcp - radsqr
               xcp = ( - b + sqrt(b*b - 4.0d0*a*c))/(2.0d0*a)
               xcm = ( - b - sqrt(b*b - 4.0d0*a*c))/(2.0d0*a)
               ycp = slope*xcp + intrcp
               ycm = slope*xcm + intrcp
               lcp = sqrt((xcp - xstep(k) + wellx(l))**2 + (ycp - ystep
     1          (k) + welly(l))**2)
               lcm = sqrt((xcm - xstep(k) + wellx(l))**2 + (ycm - ystep
     1          (k) + welly(l))**2)
               IF (lcp .LT. lcm) THEN
                  xr = xcp + wellx(l)
                  yr = ycp + welly(l)
               ELSE
                  xr = xcm + wellx(l)
                  yr = ycm + welly(l)
               END IF
               deltal = sqrt((xr - xstep(k))**2 + (yr - ystep(k))**2)
               segdis(k) = deltal
               stlong(m, j) = length - tstep + deltal
               sttime(m, j) = time
               stpath(m, j) = l
               xstep(kstep) = xr
               ystep(kstep) = yr
*
************************************************************************
*     DIVIDE STREAMLINE INTO ITS "NSEGMS + 36" NONEVENLY SPACED
*     DIVIDE STREAMLINE INTO ITS "NSEGMS + 17" NONEVENLY SPACED
*     INCREMENTS.  THE FIRST AND LAST 4 SEGMENT LENGTHS WILL BE LARGER
*     THAN THE REST SO THAT THEIR MINIMUM VOLUMES ARE NOT AS SMALL ON
*     THESE PARTS OF THE STREAMTUBES AS THEY WOULD BE OTHERWISE.  THIS
*     HELPS SPEED UP THE RUN TIME AS THE RUN TIME IS A FUNCTION OF THE
*     MINIMUM VOLUME.  (SEE THE "DATA RNSTEP" STATEMENT TO SEE FROM
*     WHERE THE 36 COMES.)
************************************************************************
*
               vstep = stlong(m, j)/real(nsegms + 17)
               tstep = 0.0d0
               l = 1
               segxpt(j, 1) = xstep(1)
               segypt(j, 1) = ystep(1)
               DO k = 1, kstep - 2
                  tstep = tstep + segdis(k)
                  temp = vstep*rnstep(l)
                  IF (tstep .GT. temp) THEN
                     deltal = temp - tstep + segdis(k)
                     deltax = deltal*(xstep(k + 1) - xstep(k))/segdis
     1                (k)
                     deltay = deltal*(ystep(k + 1) - ystep(k))/segdis
     1                (k)
                     l = l + 1
                     segxpt(j, l) = xstep(k) + deltax
                     segypt(j, l) = ystep(k) + deltay
                     tstep = segdis(k) - deltal
                  END IF
               END DO
               l = l + 1
               segxpt(j, l) = xstep(kstep)
               segypt(j, l) = ystep(kstep)
*
************************************************************************
*     IF PLOT OF THE STREAMLINES IS DESIRED, PLOT SAME.
************************************************************************
*
  150          IF (plotit .AND. pass1 .AND. stpath(m, j) .NE. 32767) 
     1          THEN
                  IF (j .EQ. 1) THEN
                     plotr = radius*scale
                     plotx = (wellx(m) - xmin)*scale
                     ploty = (welly(m) - ymin)*scale
                     CALL plot(plotx, ploty, penupb)
                     CALL circle(plotx, ploty, plotr)
                  END IF
                  plotx = (xstep(1) - xmin)*scale
                  ploty = (ystep(1) - ymin)*scale
                  CALL plot(plotx, ploty, penupb)
                  DO l = 2, kstep - 1
                     plotx = (xstep(l) - xmin)*scale
                     ploty = (ystep(l) - ymin)*scale
                     CALL plot(plotx, ploty, pendwb)
                  END DO
                  plotx = (xstep(kstep) - xmin)*scale
                  ploty = (ystep(kstep) - ymin)*scale
                  CALL plot(plotx, ploty, penupa)
                  l = stpath(m, j)
                  plotx = (wellx(l) - xmin)*scale
                  ploty = (welly(l) - ymin)*scale
                  CALL plot(plotx, ploty, penupb)
                  CALL circle(plotx, ploty, plotr)
               END IF
            END DO
*
************************************************************************
*     DELTERMINE WHICH ADJACENT STREAMLINES FORM ADMISSIBLE STREAMTUBES.
************************************************************************
*
            DO j = 1, nlines
               IF (j .EQ. nlines) THEN
                  k = 1
               ELSE
                  k = j + 1
               END IF
               tubara(m, j) = 0.0d0
               IF (stpath(m, j) .EQ. 32767 .OR. stpath(m, j) .NE. 
     1          stpath(m, k)) THEN
                  ntubes(m, j) = 32767
               ELSE
                  IF (j .EQ. 1 .OR. stpath(m, j) .NE. stpath(m, j - 1)) 
     1             THEN
                     CALL slpang(welly(stpath(m, j)) - welly(m), wellx
     1                (stpath(m, j)) - wellx(m), sai2pw)
                     sai2pw = sai2pw + 180.0d0
                     CALL slpang(segypt(j, 1) - welly(m), segxpt(j, 1) 
     1                - wellx(m), sai2j1)
                     CALL slpang(segypt(k, 1) - welly(m), segxpt(k, 1) 
     1                - wellx(m), sai2j2)
                  ELSE
                     sai2j1 = sai2j2
                     CALL slpang(segypt(k, 1) - welly(m), segxpt(k, 1) 
     1                - wellx(m), sai2j2)
                  END IF
                  IF (sai2j2 .LT. 90.0d0 .AND. sai2j1 .GT. 270.0d0) 
     1             sai2j1 = sai2j1 - 360.0d0
                  IF (sai2j1 .LE. sai2pw .AND. sai2pw .LE. sai2j2) THEN
                     ntubes(m, j) = 32767
                  ELSE
                     ntubes(m, j) = j
                  END IF
               END IF
            END DO
*
************************************************************************
*     DIVIDE STREAMLINES INTO "NLINES" OR LESS STREAMTUBES WITH "NSEGMS"
*     EVENLY SPACED AREAS.
************************************************************************
*
            notube = 0
            DO j = 1, nlines
               IF (ntubes(m, j) .NE. 32767) THEN
                  IF (j .EQ. nlines) THEN
                     n = 1
                  ELSE
                     n = j + 1
                  END IF
                  DO k = 1, nsegms
                     tarea = 0.5d0*(segxpt(j, k)*segypt(j, k + 1) + 
     1                segxpt(j, k + 1)*segypt(n, k + 1) + segxpt(n, k + 
     2                1)*segypt(n, k) + segxpt(n, k)*segypt(j, k) - 
     3                segypt(j, k)*segxpt(j, k + 1) - segypt(j, k + 1)
     4                *segxpt(n, k + 1) - segypt(n, k + 1)*segxpt(n, k) 
     5                - segypt(n, k)*segxpt(j, k))
                     tlong = sqrt(((segxpt(n, k) + segxpt(n, k + 1) - 
     1                segxpt(j, k) - segxpt(j, k + 1))*0.5d0)**2 + (
     2                (segypt(n, k) + segypt(n, k + 1) - segypt(j, k) - 
     3                segypt(j, k + 1))*0.5d0)**2)
                     twide = sqrt(((segxpt(j, k) + segxpt(n, k) - 
     1                segxpt(j, k + 1) - segxpt(n, k + 1))*0.5d0)**2 + 
     2                ((segypt(j, k) + segypt(n, k) - segypt(j, k + 1) 
     3                - segypt(n, k + 1))*0.5d0)**2)
                     seglen(j, k) = tlong
                     segwid(j, k) = twide
                     segldw(j, k) = tlong/twide
                     tubara(m, j) = tubara(m, j) + tarea
                     tvolm = twide*tlong*height
                     IF (tvolm .LT. stvmin .AND. i .EQ. 1) stvmin = 
     1                tvolm
                  END DO
                  notube = notube + 1
               END IF
            END DO
            stvmn(i) = stvmin
*
************************************************************************
*     WRITE TO STREAM TUBE INFORMATION FILES
************************************************************************
*
            IF (t .EQ. 1) THEN
               lwfile = lenwid(i)//pattrn
               OPEN (10, FILE = lwfile)
               REWIND (10)
               WRITE (10, '(1X,I5)') notube
               notub(i) = notube
               DO j = 1, nlines
                  DO k = 1, nsegms
                     IF (ntubes(m, j) .NE. 32767) WRITE (10, 
     1                '(1X,3F15.8)') seglen(j, k), segwid(j, k)
                  END DO
               END DO
               CLOSE (10)
               IF (.NOT. plotit) WRITE (*, *) ' DONE WRITING'
            END IF
*
************************************************************************
*     IF STREAMTUBES ARE BEING GENERATED, SKIP THE CALCULATION
*     OF FRACTIONAL FLOWS AND GO TO THE END OF THE MAJOR INJECTION
*     WELL DO LOOP
************************************************************************
*
	 IF (lwgen .EQ. 'Y') GOTO 200
*
************************************************************************
*     THE FOLLOWING ENDIF IS FOR THE STREAMTUBE GENERATION "IF BLOCK"
************************************************************************
*
	 END IF  !..END OF STREAMTUBE GENERATION "IF BLOCK"
*
************************************************************************
*
*
*     IF STREAMTUBES ARE NOT GENERATED, ENTRY IS HERE AFTER THE
*     STREAMTUBE GENERATION IF BLOCK
*
*
************************************************************************
*     CALCULATE THE FLOWS FROM EACH INJECTION WELL TO EACH OF THE
*     ADMISSIBLE PRODUCTION WELLS.  NO PRODUCTION WELL IS SUPPLIED WITH
*     INJECTANT FROM MORE THAN ONE INJECTION WELL AT ANY GIVEN POINT. IN
*     OTHER WORDS, ALL STREAMTUBES ARE MUTUALLY EXCLUSIVE.
*     "I" IS THE PRESENT INJECTION WELL INDEX, AND "M" IS THE ACTUAL
*     INJECTION WELL ARRAY LOCATION.
************************************************************************
*
	 flwtim = 0.0d0
*
************************************************************************
*     READ STREAMTUBE INFORMATION FILES
************************************************************************
*
         IF (t .EQ. 1 .AND. lwgen .NE. 'Y') THEN
            WRITE (*, *) 'READ STREAMTUBE FILES'
            lwfile = lenwid(i)//pattrn
            OPEN (10, FILE = lwfile)
            REWIND (10)
            READ (10, '(1X,I5)') notube
            DO j = 1, nlines
               DO k = 1, nsegms
                  IF (ntubes(m, j) .NE. 32767) READ (10, '(1X,3F15.8)') 
     1             seglen(j, k), segwid(j, k)
               END DO
            END DO
            CLOSE (10)
            WRITE (*, *) 'DONE READING'
            WRITE (*, *)
         END IF
*
************************************************************************
*     COMPUTE THE BASIC TIME STEP SO THAT THE SMALLEST CELL OR SEGMENT
*     WILL NOT BE FLOODED OUT BY HAVING TOO MUCH FLUID
*     FORCED THROUGH IT DURING ANY GIVEN FLOW INCREMENT.  THE "VOLADJ"
*     FACTOR CONTROLS THE AMOUNT OF FLUID INJECTED.
************************************************************************
*
         timstp = voladj*minstv*porsty*real(mintub)*sumpab/(permax
     1    *qinmax)
         IF (stcont .EQ. 'W') THEN
            OPEN (13, FILE = restar//'.TS')
            REWIND (13)
            WRITE (13, '(1X,F20.16)') timstp
         END IF
         IF (stcont .EQ. 'R') THEN
            OPEN (13, FILE = restar//'.TS')
            REWIND (13)
            READ (13, '(1X,F20.16)') timstp
         END IF
*
************************************************************************
*     THIS SECTION CALCULATES INJECTION PERIOD TIMES FROM THE PORE
*     VOLUMES SPECIFIED IN THE INPUT TO THE PROGRAM FOR EACH OF THE
*     INJECTION PERIODS.  BY INJECTION PERIOD IS MEANT A PERIOD OF
*     INJECTION OF WATER, CO2, OR WAG.  FOUR SUCH PERIODS ARE PERMITTED.
*     THE VARIABLE "TIMQIN(T)" GIVES THE LENGTH OF EACH "T" PERIOD.
*     THERE IS A DIFFERENT VALUE OF "TIMQIN" FOR EACH "T" PERIOD.
*     "T" VARIES BETWEEN 1 AND "NTIMES", WHERE "NTIMES" IS THE MAXIMUM
*     NUMBER OF INJECTION PERIODS.
************************************************************************
*
         IF (lwgen .EQ. 'Y') effara = fldara
         IF (ynhcpv .EQ. 'Y') THEN
            pvfact = 1.0d0 - swc
         ELSE
            pvfact = 1.0d0
         END IF
         DO tt = 1, ntimes
            temp = 0.0d0
            DO ii = 1, noinjw
               temp = temp + qinjct(tt, ii)
            END DO
            IF (tt .EQ. 1) THEN
               timqin(tt) = (pv(tt)*effara*height*real(nlayrs)*porsty
     1          *pvfact)/temp
            ELSE IF (tt .GE. 2) THEN
               timqin(tt) = timqin(tt - 1) + ((pv(tt) - pv(tt - 1))
     1          *effara*height*real(nlayrs)*porsty*pvfact)/temp
            END IF
	 END DO
	 timqin(ntimes) = timqin(ntimes)*1.025d0
*
************************************************************************
*     THE FOLLOWING "DO WHILE (.TRUE.) LOOP" CONTROLS HOW LONG FLOW
*     CALCULATIONS ARE PERMITTED TO TAKE PLACE.
*     CALCULATIONS ARE MADE FOR JUST ONE INJECTION WELL AT A TIME.
*     "FLWTIM" IS THE  INJECTION TIME VARIABLE. IT IS INCREMENTED EACH
*     ITERATION BY "TIMSTP" (THE LENGTH OF TIME FOR EACH TIME STEP).
*          I.E., FLWTIM = FLWTIM + TIMSTP
*     THE CALCULATIONS PASS FROM ONE INJECTION PERIOD TO THE NEXT
*     WHEN "FLWTIM" EXCEEDS THE "TIMQIN(T)" FOR EACH PERIOD. (SEE
*     PREVIOUS COMMENT FOR THE DEFINITION OF "TIMQIN(T)".)
*     THE CALCULATIONS STAY WITHIN THIS 'DO WHILE LOOP' AS TIME IS BEING
*     INCREMENTED AND AS PROGRESS IS MADE FROM INJECTION PERIOD TO
*     INJECTION PERIOD.  INJECTION PERIODS ARE DENOTED BY 'T'.
*     THERE IS AN EXIT MADE FROM THE 'DO WHILE TRUE' LOOP WHEN 'T'
*     IS MADE GREATER THAN 'NTIMES'. 'NTIMES' IS THE NUMBER OF INJECTION
*     PERIODS.  WHEN THIS 'DO WHILE TRUE' LOOP IS EXITED, CALCULATIONS
*     START FOR THE NEXT INJECTION WELL.
*         THE VARIABLE 'FLWTIM' IS INCREMETED BY 'TIMSTP'
*         THE VARIABLE 'T' GOES FROM 1 TO 'NTIMES + 1'.  EXIT IS
*         MADE WHEN T IS GREATER THAN NTIMES.
*     THE END OF THE "DO WHILE TRUE LOOP" IS INDICATED LATER IN THE
*     PROGRAM.
************************************************************************
*
*
	 DO WHILE (.TRUE.)
*
*
            qintot = 0.0d0
            DO ii = 1, noinjw
               qintot = qintot + qinjct(t, ii)
            END DO
            IF (wtrchs .EQ. 'Y') THEN
               DO l = 1, nlayrs
                  DO j = 1, nlines
                     n = stpath(m, j)
                     IF (ntubes(m, j) .NE. 32767 .AND. n .NE. 32767) 
     1                THEN
                        DO k = 1, nsegms
                           so = segso(l, j, k)
                           sw = segsw(l, j, k)
                           iso = nint(so*100.0d0)
                           jsw = nint(sw*100.0d0)
                           foout(l, j, k) = fotabl(iso, jsw)
                           fwout(l, j, k) = fwtabl(iso, jsw)
                           temptm = tmtabl(iso, jsw)
                        END DO
                     END IF
                  END DO
               END DO
            END IF
            IF (wtrchs .EQ. 'N') THEN
               sumall = 0.0d0
               DO l = 1, nlayrs
                  sumlay(l) = 0.0d0
                  DO j = 1, nlines
                     n = stpath(m, j)
                     IF (ntubes(m, j) .NE. 32767 .AND. n .NE. 32767) 
     1                THEN
                        sumlwt(l, j) = 0.0d0
                        DO k = 1, nsegms
                           so = segso(l, j, k)
                           sw = segsw(l, j, k)
                           iso = nint(so*100.0d0)
                           jsw = nint(sw*100.0d0)
                           foout(l, j, k) = fotabl(iso, jsw)
                           fwout(l, j, k) = fwtabl(iso, jsw)
                           temptm = tmtabl(iso, jsw)
                           sumlwt(l, j) = sumlwt(l, j) + temptm*seglen
     1                      (j, k)/segwid(j, k)
                        END DO
                        IF (sumlwt(l, j) .LT. 1.e-8) THEN
                           sumtmp = 0.0
                        ELSE
                           sumtmp = prmabs(l)/sumlwt(l, j)
                        END IF
                        sumlay(l) = sumlay(l) + sumtmp
                     END IF
                  END DO
                  sumall = sumall + sumlay(l)
               END DO
            END IF
*
************************************************************************
*     COMPUTE NEW VALUES OF FLOW PARAMETERS AFTER EACH TIME STEP
************************************************************************
*
            hpor = height*porsty
	    DO l = 1, nlayrs
               DO j = 1, nlines
                  n = stpath(m, j)
                  IF (ntubes(m, j) .NE. 32767 .AND. n .NE. 32767) THEN
		     sumtub = prmabs(l)/sumlwt(l, j)
		     newtem = qinjct(t, i)*sumtub/sumall*timstp/hpor
                     DO k = 1, nsegms
                        IF (k .EQ. 1) THEN
                           foin = foinit(t, i)
                           fwin = fwinit(t, i)
                        ELSE
                           foin = foout(l, j, k - 1)
                           fwin = fwout(l, j, k - 1)
                        END IF
*
************************************************************************
*     COMPUTE NEW VALUES OF PARAMETERS AFTER TIME STEP AND SAVE SAME
************************************************************************
*
			temp = newtem/(seglen(j, k)*segwid(j, k))
                        tmpsoo = segso(l, j, k) - temp*(foout(l, j, k) 
     1                   - foin)
                        IF (tmpsoo .LT. somin) THEN
                           tmpsoo = somin
                        ELSE IF (tmpsoo .GT. somax) THEN
                           tmpsoo = somax
                        END IF
			segso(l, j, k) = tmpsoo
                        tmpsww = segsw(l, j, k) - temp*(fwout(l, j, k) 
     1                   - fwin)
                        IF (tmpsww .LT. swmin) THEN
                           tmpsww = swmin
                        ELSE IF (tmpsww .GT. swmax) THEN
                           tmpsww = swmax
                        END IF
                        segsw(l, j, k) = tmpsww
*
************************************************************************
*     COMPUTE PRODUCTION
************************************************************************
                        IF (k .EQ. nsegms) THEN
                           temp = qinjct(t, i)*sumtub/sumall*timstp
                           tempfg = (1.0d0 - foout(l, j, k) - fwout(l, 
     1                      j, k))*temp
			   IF (tempfg .LT. 1.0d-10) tempfg = 0.0d0
                           wprodg(n) = wprodg(n) + tempfg
                           wprodo(n) = wprodo(n) + foout(l, j, k)*temp
                           wprodw(n) = wprodw(n) + fwout(l, j, k)*temp
                        END IF
                     END DO
                  END IF
               END DO
            END DO
            flwtim = flwtim + timstp
            pvinjt = pvinjt + timstp*qintot/porvol
            iprint = iprint + 1
            IF (iprint .EQ. nprint) THEN
               otemp = 0.0d0
               wtemp = 0.0d0
               gtemp = 0.0d0
               istep = istep + 1
               WRITE (*, 10000) istep
               DO nn = 1, nwells
                  otemp = otemp + wprodo(nn)
                  wtemp = wtemp + wprodw(nn)
                  gtemp = gtemp + wprodg(nn)
               END DO
               iprint = 0
               ftime(istep) = pvinjt
               prodo(istep) = otemp + prodo(istep)
               prodw(istep) = wtemp + prodw(istep)
               prodg(istep) = gtemp + prodg(istep)
	    END IF
************************************************************************
*     THE FOLLOWING "PERIOD CHANGE IF BLOCK" DETERMINES WHETHER THE NEXT
*     INJECTION PERIOD IS ENTERED.  THE PERIOD IS CHANGED BY
*     INCREMENTING "T".  A NUMBER OF PROGRAM SETTING CHANGES ARE MADE
*     IF THE NEXT INJECTION PERIOD IS ENTERED.
*
*     IF 'FLWTIM'IS LESS THAN 'TIMQIN(T)', THEN THE FOLLOWING "IF BLOCK"
*     IS BYPASSED AND 'FLWTIM' CONTINUES TO BE INCREMENTED BY TIMSTP.
*     THE ARRAY 'TIMQIN(T)' GIVES THE LENGTH OF TIME FOR EACH OF THE
*     INJECTION PERIODS.
*     IF THE FOLLOWING 'PERIOD CHANGE IF BLOCK' IS NOT BYPASSED, THEN
*     THE NEXT INJECTION PERIOD IS ENTERED.
************************************************************************
*
	    IF (flwtim .GE. timqin(t)) THEN !..PERIOD CHANGE IF BLOCK
               t = t + 1
               wtrchs = chswtr(t)
************************************************************************
*      SET ALTERNATIVES FOR LESS THAN, EQUAL TO, OR GREATER
*      THAN 'NTIMES'.
************************************************************************
*              IF (t .LT. ntimes) OR
*              IF (t .EQ. ntimes .AND. stcont .NE. 'W') THEN
*                 BOTH OF THE NEXT TWO IF BLOCKS ARE BYPASSED
************************************************************************
*
	       IF (t .EQ. ntimes .AND. stcont .EQ. 'W') THEN
*              *********************************************************
*                WRITE RESTART FILE IF DESIRED. STCONT DETERMINES
*                WHETHER A RESTART FILE IS WRITTEN
*              *********************************************************
                     wellid = i
                     satfil = restar//wellch(wellid)
                     OPEN (13, FILE = satfil)
                     REWIND (13)
                     DO l = 1, nlayrs
                        DO j = 1, nlines
                           DO k = 1, nsegms
                              WRITE (13, '(1X,2D16.9)') segso(l, j, k), 
     1                         segsw(l, j, k)
                           END DO
                        END DO
                     END DO
                     CLOSE (13)
                     WRITE (*, *) 'WRITE RESTART FILE  ', satfil
		     WRITE (*, *) '   '
	       END IF
************************************************************************
*
	       IF (t .GT. ntimes)  EXIT
*
************************************************************************
*     THE ABOVE STATEMENT GIVES THE EXIT FROM THE "DO WHILE (.TRUE.)
*     LOOP".
*
*     THE ENDIF JUST BELOW IS FOR THE "FLWTIM IF BLOCK".
************************************************************************
*
	    END IF !..END OF PERIOD CHANGE IF BLOCK
*
************************************************************************
*     THE FOLLOWING END DO IS FOR THE 'DO WHILE (.TRUE.) LOOP.  THIS
*     LOOP IS EXITED WHEN T IS GREATER THAN NTIMES.
************************************************************************
*
*
	 END DO !..END OF DO WHILE (.TRUE.) LOOP
*
*
************************************************************************
*     THE FOLLOWING RESETS VARIABLES BEFORE PROCEEDING TO THE NEXT
*     INJECTION WELL.
************************************************************************
*
	 IF (t .GT. ntimes) THEN
            t = 1
            ismax = istep
            istep = 0
            iprint = 0
            flwtim = 0.0d0
            pvinjt = 0.0d0
            DO nn = 1, nwells
               wprodg(nn) = 0.0d0
               wprodo(nn) = 0.0d0
               wprodw(nn) = 0.0d0
            END DO
            IF (i .LT. noinjw) THEN
               wellid = i + 1
               CALL flwint(ntimes, nlayrs, nlines, nsegms)
            END IF
         END IF
************************************************************************
  200 CONTINUE
************************************************************************
*
*
*
      END DO !..END OF MAJOR INJECTION WELL LOOP
*
*
*
************************************************************************
*
*     THE ABOVE END DO IS FOR THE MAJOR INJECTION WELL LOOP.  THIS
*     IS THE MAJOR LOOP OF THE PROGRAM.
*
************************************************************************
*     CREATE STREAMTUBE DATA FILES
************************************************************************
      pass1 = .FALSE.
      IF (lwgen .EQ. 'Y') THEN
         effara = 0.0d0
         mintub = maxlin
         minstv = fldara
         temp = 0.0d0
         itemp = 0
         OPEN (9, FILE = 'TUBAREA.'//pattrn)
         REWIND (9)
         DO ii = 1, noinjw
            mm = linjp(ii)
            temp = stvmn(ii)
            itemp = notub(ii)
            IF (temp .LT. minstv) minstv = temp
            IF (itemp .LT. mintub) mintub = itemp
            DO j = 1, nlines
               effara = effara + tubara(mm, j)
               WRITE (9, '(1X,2I6,F15.10)') ii, j, tubara(mm, j)
            END DO
         END DO
         volara = 'ZZAREA.'//pattrn
         OPEN (11, FILE = volara)
         REWIND (11)
         WRITE (11, '(1X,3F25.8,2I3)') minstv, fldara, effara, nlayrs, 
     1    mintub
         DO ii = 1, noinjw
            WRITE (11, '(1X,I12)') linjp(ii)
         END DO
         CLOSE (11)
         tubinf = 'ZZTUBE.'//pattrn
         OPEN (13, FILE = tubinf)
         REWIND (13)
         DO ii = 1, noinjw
            mm = linjp(ii)
            DO j = 1, nlines
               WRITE (13, '(1X,2I12)') stpath(mm, j), ntubes(mm, j)
            END DO
         END DO
         CLOSE (13)
      END IF
*
************************************************************************
*     WRITE OUTPUT FILES
************************************************************************
*
      pendwa = pendwa
      IF (.NOT. (lwgen .EQ. 'Y' .AND. .NOT. plotit)) THEN
         IF (.NOT. plotit) WRITE (12, *) 'NUMBER OF STEPS =', int
     1    (timqin(ntimes)/timstp)
         corara = effara/fldara
         IF (.NOT. plotit) THEN
            WRITE (12, *) 'EFFECTIVE STREAMTUBE AREA = ', effara
            WRITE (12, *) 'POLYGON AREA              = ', fldara
         END IF
         DO istep = 1, ismax
            ftime(istep) = ftime(istep)/(corara*pvfact)
            prodo(istep) = prodo(istep)/(porvol*corara*pvfact)
            prodg(istep) = prodg(istep)/(porvol*corara*pvfact)
	    prodw(istep) = prodw(istep)/(porvol*corara*pvfact)
         END DO
         OPEN (15, FILE = 'OUTPUT')
         REWIND (15)
         DO istep = 1, ismax
            WRITE (15, '(1X,4F15.7)') ftime(istep), prodo(istep), prodg
     1       (istep), prodw(istep)
         END DO
         CLOSE (15)
************************************************************************
*     IF CALLER IS PLOTTING STREAMLINES, LEAVE PICTURE ON THE SCREEN
*     UNTIL CALLER WISHES TO CONTINUE.  THERE IS A DUMMY TIMING LOOP
*     TO ALLOW FOR "GRABBING" OG GRAPHICS ON SCREEN.
************************************************************************
*
	 IF (plotit) THEN
            DO i = 1, 32767
               DO j = 1, 255
                  k = i + j
               END DO
            END DO
            DO WHILE (.TRUE.)
               WRITE (*, '('' CONTINUE - Y/N? '')')
               READ (*, '(A)') yorn
               IF (yorn .EQ. 'Y' .OR. yorn .EQ. 'y') EXIT
            END DO
            CALL plot(0.0, 0.0, 999)
            pendwa = pendwa
         END IF
*
************************************************************************
*     RETURN ANSWER TO CALLER
************************************************************************
*
10000    FORMAT ('+', i4)
      END IF
      RETURN
      END
