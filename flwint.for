C STRUCTURED BY FOR_STRUCT, V2.0.1, ON 04/21/94 AT 16:17:44
C  OPTIONS SET: DUP=R FMT=ACHN I=50,3 LABF=10000,10 LABX=50,50 OFF=AI
C               S=9 SP=DE V
      SUBROUTINE flwint(ntimes, nlayrs, nlines, nsegms)
*
************************************************************************
*
*     SUBROUTINE TO CHECK RANGES OF AND TO INITIALIZE SOME OF THE FLOW
*     ATTRIBUTES.
*
*     CALLING VARIABLES:
*
*     NLAYRS - INPUT        = NUMBER OF LAYERS INTO WHICH THE PRODUCTION
*                             ZONE IS TO BE DIVIDED
*     NLINES - INPUT        = NUMBER OF EQUALLY SPACED STREAMLINES TO
*                             START FROM EACH INJECTION WELL
*     NSEGMS - INPUT        = NUMBER OF SEGMENTS INTO WHICH TO DIVIDE
*                             THE STREAMTUBES
*     NTIMES - INPUT        = NUMBER OF DIFFERENT TIMED FLOOD PROGRAMS
*                             IN THE FIELD PRODUCTION STUDY
*
*     LOCAL VARIABLES:
*
*     I      - INTEGER      = DO LOOP INDEX
*     J      - INTEGER      = DO LOOP INDEX
*     K      - INTEGER      = DO LOOP INDEX
*     L      - INTEGER      = DO LOOP INDEX
*     TEMP   - REAL         = TEMPORARY VARIABLE FOR CALCULATIONS
*     WELLCH - CHARACTER    = CHARACTER VARIABLE FOR WELL NUMBER
*
*     WRITTEN BY              JAMES W. MUELLER          OCTOBER 21, 1991
*                             SOLUTIONS SYSTEMS         HOUSTON, TEXAS
*
************************************************************************
*
*     SUBROUTINE INCLUDE STATEMENTS
*
      INCLUDE 'PARAM.INC'
      INCLUDE 'FIELD.INC'
      INCLUDE 'ROCKAT.INC'
*
************************************************************************
*
*     CALLING VARIABLE DEFINITIONS:
*
      INTEGER nlayrs
      INTEGER nlines
      INTEGER nsegms
      INTEGER ntimes
*
************************************************************************
*
*     LOCAL VARIABLE DEFINITIONS:
*
      INTEGER i
      INTEGER j
      INTEGER k
      INTEGER l
*
      DOUBLE PRECISION temp
*
      CHARACTER*3 wellch(maxinj)
*
      DATA wellch/'.1', '.2', '.3', '.4', '.5', '.6', '.7', '.8', '.9', 
     1     '.10'/
*
************************************************************************
*     RANGE CHECK SOME OF THE INPUT FOR OBVIOUS ERRORS
************************************************************************
*
      IF (krgcw .LT. 0.0d0 .OR. krgcw .GT. 1.0d0) THEN
         WRITE (*, '('' KRGCW OUT OF 0.0 TO 1.0 RANGE'')')
         STOP 'FLWINT ERROR 1'
      ELSE IF (krocw .LT. 0.0d0 .OR. krocw .GT. 1.0d0) THEN
         WRITE (*, '('' KROCW OUT OF 0.0 TO 1.0 RANGE'')')
         STOP 'FLWINT ERROR 2'
      ELSE IF (krsmax .LT. 0.0d0 .OR. krsmax .GT. 1.0d0) THEN
         WRITE (*, '('' KRSMAX OUT OF 0.0 TO 1.0 RANGE'')')
         STOP 'FLWINT ERROR 3'
      ELSE IF (kwro .LT. 0.0d0 .OR. kwro .GT. 1.0d0) THEN
         WRITE (*, '('' KWRO OUT OF 0.0 TO 1.0 RANGE'')')
         STOP 'FLWINT ERROR 4'
      ELSE IF (sgr .LT. 0.0d0 .OR. sgr .GT. 1.0d0) THEN
         WRITE (*, '('' SGR OUT OF 0.0 TO 1.0 RANGE'')')
         STOP 'FLWINT ERROR 5'
      ELSE IF (sorg .LT. 0.0d0 .OR. sorg .GT. 1.0d0) THEN
         WRITE (*, '('' SORG OUT OF 0.0 TO 1.0 RANGE'')')
         STOP 'FLWINT ERROR 6'
C      WRITE(*,*)'SORM =',SORM
      ELSE IF (sorm .LT. 0.0d0 .OR. sorm .GT. 1.0d0) THEN
         WRITE (*, '('' SORM OUT OF 0.0 TO 1.0 RANGE'')')
         STOP 'FLWINT ERROR 7'
      ELSE IF (sorw .LT. 0.0d0 .OR. sorw .GT. 1.0d0) THEN
         WRITE (*, '('' SORW OUT OF 0.0 TO 1.0 RANGE'')')
         STOP 'FLWINT ERROR 8'
      ELSE IF (ssr .LT. 0.0d0 .OR. ssr .GT. 1.0d0) THEN
         WRITE (*, '('' SSR OUT OF 0.0 TO 1.0 RANGE'')')
         STOP 'FLWINT ERROR 9'
      ELSE IF (swc .LT. 0.0d0 .OR. swc .GT. 1.0d0) THEN
         WRITE (*, '('' SWC OUT OF 0.0 TO 1.0 RANGE'')')
         STOP 'FLWINT ERROR 10'
      ELSE IF (swir .LT. 0.0d0 .OR. swir .GT. 1.0d0) THEN
         WRITE (*, '('' SWIR OUT OF 0.0 TO 1.0 RANGE'')')
         STOP 'FLWINT ERROR 11'
      ELSE IF (w .LE. 0.0d0 .OR. w .GE. 1.0d0) THEN
         WRITE (*, '('' W OUT OF 0.0 TO 1.0 RANGE'')')
         STOP 'FLWINT ERROR 12'
      ELSE IF (phigh .LE. plow .OR. p .LE. 0.0d0 .OR. plow .LE. 0.0d0 
     1 .OR. phigh .LE. 0.0d0) THEN
         WRITE (*, '('' PRESSURE(S) OUT OF LOGICAL RANGE'')')
         STOP 'FLWINT ERROR 13'
      ELSE IF (krmsel .LT. 0 .OR. krmsel .GT. 4) THEN
         WRITE (*, '('' KRMSEL OUT OF 0 TO 4 RANGE'')')
         STOP 'FLWINT ERROR 14'
C      WRITE(*,*)'NTIMES =',NTIMES
      ELSE IF (ntimes .LE. 0 .OR. ntimes .GT. maxtim) THEN
         WRITE (*, 
     1    '('' NUMBER OF FLOOD PROGRAMS OUT OF 1 TO '',I3,              
     2      '' RANGE'')'
     3    ) maxtim
         STOP 'FLWINT ERROR 15'
      ELSE IF (noinjw .LE. 0 .OR. noinjw .GT. maxinj) THEN
         WRITE (*, 
     1    '('' NUMBER OF INJECTION WELLS OUT OF 1 TO '',I3,             
     2      '' RANGE'')'
     3    ) maxinj
         WRITE (*, *) 'NOINJW =', noinjw
         STOP 'FLWINT ERROR 16'
      ELSE IF (nlayrs .LE. 0 .OR. nlayrs .GT. maxlay) THEN
         WRITE (*, '('' LAYERS OUT OF 1 TO '',I3,'' RANGE'')') maxlay
         STOP 'FLWINT ERROR 17'
      ELSE IF (nlines .LE. 0 .OR. nlines .GT. maxlin) THEN
         WRITE (*, 
     1    '('' NUMBER OF STREAMTUBES OUT OF 1 TO '',I3,                 
     2      '' RANGE'')'
     3    ) maxlin
         STOP 'FLWINT ERROR 18'
      ELSE
         DO i = 1, ntimes
            DO j = 1, noinjw
               temp = fginit(i, j) + foinit(i, j) + fwinit(i, j)
               IF (0.9999d0 .GT. temp .OR. temp .GT. 1.0001d0) GOTO 50
            END DO
         END DO
         temp = sginit + soinit + swinit
         IF (0.9999d0 .GT. temp .OR. temp .GT. 1.0001d0) THEN
            WRITE (*, 
     1       '('' SUM OF INITIAL SATURATION VALUES NOT EQUAL TO'',      
     2         '' 1.0'')'
     3       )
            STOP 'FLWINT ERROR 20'
         ELSE
*
            IF (p .LE. plow) THEN
               alpha = 0.0d0
            ELSE IF (p .GE. phigh) THEN
               alpha = 1.0d0
            ELSE
               alpha = (p - plow)/(phigh - plow)
            END IF
            onemal = 1.0d0 - alpha
            onemw = 1.0d0 - w
*
************************************************************************
*     INITIALIZE ALL OF THE INDIVIDUAL SATURATION VALUES IN ALL OF THE
*     DIFFERENT SEGMENTS.
************************************************************************
*
            IF (stcont .EQ. 'R') THEN
               satfil = restar//wellch(wellid)
               OPEN (13, FILE = satfil)
               REWIND (13)
               DO l = 1, nlayrs
                  DO j = 1, nlines
                     DO k = 1, nsegms
                        READ (13, '(1X,2D16.9)') segso(l, j, k), segsw
     1                   (l, j, k)
                     END DO
                  END DO
               END DO
               WRITE (*, *) 'READ RESTART FILE ', satfil
            ELSE
               DO l = 1, nlayrs
                  DO j = 1, nlines
                     DO k = 1, nsegms
CJWM               SEGSG(L,J,K) = SGINIT
                        segso(l, j, k) = soinit
                        segsw(l, j, k) = swinit
                     END DO
                  END DO
               END DO
            END IF
*
************************************************************************
*     INITIALIZE ALL OF THE INDIVIDUAL PRODUCTION VALUES FOR ALL OF THE
*     DIFFERENT POSSIBLE PRODUCTION WELLS.
************************************************************************
*
            DO j = 1, maxinj + maxpro
               qprodg(j) = 0.0d0
               qprodo(j) = 0.0d0
               qprodw(j) = 0.0d0
*
************************************************************************
*     RETURN TO CALLER
************************************************************************
*
            END DO
            RETURN
         END IF
   50    WRITE (*, 
     1    '('' SUM OF INITIAL FRACTIONAL FLOWS NOT'',                   
     2      '' EQUAL TO 1.0'')'
     3    )
         STOP 'FLWINT ERROR 19'
      END IF
      END
