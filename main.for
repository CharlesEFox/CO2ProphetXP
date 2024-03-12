      PROGRAM co2
C**********************************************************************
C     Designed and written by:
C
C     Texaco Exploration & Production Technology Department
C     Houston, Texas
C     May 6, 1994
C
C     for the Department of Energy as part of 
C     Contract No. DE-FC22-93BC14960
C**********************************************************************
C
C     DRIVER FOR CO2
C***********************************************************************
C     LOCAL VARIABLES
C***********************************************************************
C     DENSTY - REAL        = CO2 DENSITY
C     DUMMY  - CHARACTER   = DUMMY VARIABLE FOR READING UNUSED
C                            STATEMENTS
C     I      - INTEGER     = WELL NUMBER INDEX
C     NLAYRS - INTEGER     = NUMBER OF LAYERS
C     NLINES - INTEGER     = NUMBER OF STREAMLINES
C     NSEGMS - INTEGER     = NUMBER OF SEGMENTS IN THE STREAMTUBES
C     NSTEPS - INTEGER     = NUMBER OF STEPS IN STREAMLINE GENERATION
C                            ROUTINE
C     PLOTIT - LOGICAL     = 'TRUE' OR 'FALSE' IF STREAMLINE SCREEN
C                            PLOT IS DESIRED
C     QTR5   - CHARACTER   = 'Y' OR 'N' WHETHER QUARTER 5 SPOT
C     STFILE - CHARACTER   = INPUT VARIABLE FOR STREAMTUBE GENERATION
C                            INFORMATION.  PROVIDES THREE ITEMS:
C                                  SWITCH FOR GENERATING STREAMTUBES
C                                  SWITCH FOR READING OR WRITING SAT'S
C                                  FILE NAME FOR STORED SATURATIONS
C     T      - INTEGER     = INJECTION PERIOD INDEX
C     TEMP   - REAL        = TEMPORARY VARIABLE
C     TEMP2  - REAL        = TEMPORARY VARIABLE
C     TIMDA1 - CHARACTER   = TIME OF DAY
C     TIMDA2 - CHARACTER   = TIME OF DAY
C     TIMDA3 - CHARACTER   = TIME OF DAY
C     TRES   - REAL        = RESERVOIR TEMPERATURE
C     TYPE   - CHARACTER   = INPUT VARIABLE FOR PATTERN INFORMATION
C                            AND FOR FOAM AND HYSTERESIS INFORMATION
C
C***********************************************************************
C
      DOUBLE PRECISION densty
      DOUBLE PRECISION temp, temp2
      DOUBLE PRECISION tres
      CHARACTER timda1*11, timda2*11, timda3*11
      CHARACTER qtr5*1
      CHARACTER dummy*72
      CHARACTER stfile*12
      CHARACTER type*4
      INTEGER i, nlayrs, nlines, nsteps, nsegms, ntimes, t
      LOGICAL plotit
C
C***********************************************************************
C     INCLUDE STATEMENTS
C***********************************************************************
C
      INCLUDE 'PARAM.INC'
      INCLUDE 'FIELD.INC'
      INCLUDE 'STREAM.INC'
      INCLUDE 'ROCKAT.INC'
      INCLUDE 'CNVRT.INC'
C
C***********************************************************************
C
      OPEN (12, FILE = 'INFO.RUN')
      REWIND (12)
      WRITE (*, '('' START PROPHET'')')
C The following line is a Lahey FORTRAN extension.  It can be commented
C out losing only the ability to determine run time.
      CALL time(timda1)
      DO i = 1, maxwel
         wellx(i) = 0.0d0
         welly(i) = 0.0d0
      END DO
C
C***********************************************************************
C     READ DATA FILE
C***********************************************************************
C
      OPEN (16, FILE = 'INDATA')
      REWIND (16)
      READ (16, *) title
      READ (16, *) dummy
      READ (16, *) dummy
      READ (16, *) type
      type = type//'Z'
      pattrn = type(1:2)
      modif = type(3:3)
      READ (16, *) dummy
      READ (16, *) nwells, noinjw
      READ (16, *) dummy
      DO i = 1, nwells
         READ (16, *) wellx(i), welly(i), wellq(i)
      END DO
      READ (16, *) dummy
      READ (16, *) nbndpt
      READ (16, *) dummy
      DO i = 1, nbndpt
         READ (16, *) boundx(i), boundy(i)
      END DO
      READ (16, *) dummy
      READ (16, *) dummy
      READ (16, *) stfile, outtim
      stfile = stfile//'ZZZZZZ'
      stcont = stfile(1:1)
      IF (stcont .EQ. 'N' .OR. stcont .EQ. 'W' .OR. stcont .EQ. 'R')
     1 lwgen = 'N'
      IF (stcont .EQ. 'P') lwgen = 'P'
      IF (stcont .EQ. 'Y') lwgen = 'Y'
      restar = stfile(2:7)
      READ (16, *) dummy
      READ (16, *) dummy
      READ (16, *) sorw, sorg, sorm
      READ (16, *) dummy
      READ (16, *) sgr, ssr
      READ (16, *) dummy
      READ (16, *) swc, swir
      READ (16, *) dummy
      READ (16, *) krocw, kwro, krsmax, krgcw
      READ (16, *) dummy
      READ (16, *) expow, expw, exps, expg, expog
      READ (16, *) dummy
      READ (16, *) krmsel, w
      READ (16, *) dummy
      READ (16, *) dummy
      READ (16, *) viso, visw
      READ (16, *) dummy
      READ (16, *) bo, rs, api, saln, gsg
      READ (16, *) dummy
      READ (16, *) dummy
      READ (16, *) tres, p, mmp
      READ (16, *) dummy
      READ (16, *) dpcoef, permav, thick, poros, nlayrs
      READ (16, *) dummy
      READ (16, *) soinit, sginit, swinit
      READ (16, *) dummy
      READ (16, *) area, xkvh
      READ (16, *) dummy
      READ (16, *) dummy
      READ (16, *) ntimes, wagtag
      READ (16, *) dummy
      DO i = 1, noinjw
         DO t = 1, ntimes
            READ (16, *) hcpvi(t), wtrrat(t, i), solrat(t, i), tmorvl
     1       (t, i)
         END DO
      END DO
      CLOSE (16)
C
C***********************************************************************
C     SET AND MODIFY INPUT DATA
C***********************************************************************
C
C
C     NOTE: THE POROSTY AND HEIGHT ARE MADE CONSTANT FOR
C           CALCULATION PURPOSES
C
      voladj = 0.2d0
      height = 100.0d0/real(nlayrs)
      porsty = 0.10d0
      nsteps = 600
      sormis = sorm
      ynhcpv = 'Y'
      phigh = mmp
      plow = 0.75d0*phigh
      IF (pattrn .NE. 'CS') lwgen = 'N'
      IF (lwgen .EQ. 'P') THEN
         plotit = .TRUE.
         lwgen = 'Y'
      ELSE
         plotit = .FALSE.
      END IF
C***********************************************************************
C     CALCULATE TOTAL PORE VOLUMES INJECTED.
C     DETERMINE IF THERE IS A CHASE WATER SLUG
C***********************************************************************
C
      temp = 0.0d0
      DO t = 1, ntimes
         IF (abs(tmorvl(t, 1) - 1.0d0) .LT. 1.0d-5 .OR. abs(tmorvl(t,
     1    1) - 0.0d0) .LT. 1.0d-5) THEN
            temp2 = 1.0d0
         ELSE
            temp2 = 1.0d0 - tmorvl(t, 1)
         END IF
         temp = temp + hcpvi(t)/temp2
         IF (t .GT. 1 .AND. solrat((t - 1), 1) .GT. 1.e-3 .AND. solrat
     1    (t, 1) .LT. 1.e-4) THEN
            chswtr(t) = 'Y'
         ELSE
            chswtr(t) = 'N'
         END IF
      END DO
C
C***********************************************************************
C     ADJUST NSEGMS, AND NLINES
C***********************************************************************
C
      IF (pattrn .EQ. '5S' .OR. pattrn .EQ. 'LN') THEN
         nsegms = 50
      ELSE
         nsegms = 20
      END IF
      IF (pattrn .EQ. '5S' .OR. pattrn .EQ. '7S' .OR. pattrn .EQ. 'LD')
     1 THEN
         nlines = 18
      ELSE
         nlines = 36
      END IF
C
C***********************************************************************
C
      IF (pattrn .EQ. '5S') THEN
         qtr5 = 'Y'
      ELSE
         qtr5 = 'N'
      END IF
      dummy = dummy//'A'
C
C***********************************************************************
C
      CALL cnvrti(tres, ntimes, nlayrs)
C
C***********************************************************************
C
      CALL co2pac(tres, p, densty, visg)
      WRITE (12, *) 'CO2 DENSTY = ', densty
      WRITE (12, *) 'CO2 VISCOSITY =', visg
C
C***********************************************************************
C     THE FOLLOWING " STREAM TUBE GENERATION IF BLOCK" PERMITS BYPASSING
C     OF SOME SUBROUTINE CALLS WHEN STREAMTUBES ARE NOT BEING GENERATED
C***********************************************************************
C
C
C
      IF (lwgen .EQ. 'Y') THEN  !..STREAMTUBE GENERATION IF BLOCK
C
C***********************************************************************
C
	 CALL setiwl()
C
C***********************************************************************
C
         IF (qtr5 .EQ. 'Y') THEN
            DO i = 4, 34
               j = 70 - i
               wellq(i) = wellq(j)
            END DO
         END IF
C
C***********************************************************************
C        NORMALIZE WELL AND BOUNDARY LOCATIONS
C***********************************************************************
C
            temp = 0.0d0
            DO i = 1, nbndpt
               IF (boundx(i) .GT. temp) temp = boundx(i)
               IF (boundy(i) .GT. temp) temp = boundy(i)
            END DO
            DO i = 1, nbndpt
               boundx(i) = boundx(i)/temp
               boundy(i) = boundy(i)/temp
            END DO
            DO i = 1, nwells
               wellx(i) = wellx(i)/temp
               welly(i) = welly(i)/temp
	    END DO
C
C***********************************************************************
C
	 CALL mindis()
C
C***********************************************************************
C
         CALL arpoly(nbndpt, boundx, boundy, fldara)
C
	 WRITE (*, '('' BOUNDARY POLYGON AREA = '',F10.3,/)') fldara
C
C***********************************************************************
C
      END IF  !..END OF STREAMTUBE GENERATION IF BLOCK
C
C
C
C***********************************************************************
C     THE ABOVE END IF IS FOR THE STREAMTUBE GENERATION IF BLOCK.
C
C     IF STREAMTUBES ARE NOT BEING GENERATED, THEN RE-ENRY INTO THE
C     THE PROGRAM IS AT THIS POINT.
C************************************************************************
C
C     SET WELL NUMBER TO 1
C
      wellid = 1
C
C     INITIALIZE VAIABLES
C
      CALL flwint(ntimes, nlayrs, nlines, nsegms)
C
C***********************************************************************
C
      WRITE (*, 10000)
10000 FORMAT (' ENTER MAIN PROGRAM')
C The following line is a Lahey FORTRAN extension.  It can be commented
C out losing only the ability to determine run time.
      CALL time(timda2)
C
C***********************************************************************
C
      CALL strmtb(plotit, ntimes, nlayrs, nlines, nsegms, nsteps)
C
C***********************************************************************
C
C The following line is a Lahey FORTRAN extension.  It can be commented
C out losing only the ability to determine run time.
      CALL time(timda3)
      WRITE (*, 10010)
10010 FORMAT (' LEAVE MAIN PROGRAM')
C
C***********************************************************************
      WRITE (12, *) 'START PROGRAM ', timda1
      WRITE (12, *) 'START STRMTB ', timda2
      WRITE (12, *) 'LEAVE STRMTB ', timda3
      CLOSE (12)
C***********************************************************************
C
      CALL cnvout(nlayrs, ntimes, tres)
C
C***********************************************************************
      STOP 'PROPHT'
      END
