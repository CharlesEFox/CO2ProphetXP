C STRUCTURED BY FOR_STRUCT, V2.0.1, ON 04/21/94 AT 16:17:44
C  OPTIONS SET: DUP=R FMT=ACHN I=50,3 LABF=10000,10 LABX=50,50 OFF=AI
C               S=9 SP=DE V
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      SUBROUTINE co2pac(tin, pin, dout, vout)
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
*
*     PURPOSE --- THIS SUBROUTNE VERSION OF CO2PAC CALCULATES THE
*                 PROPERTIES OF PURE CARBON DIOXIDE USING WAGNER'S
*                 EQUATION OF STATE
*
*     VERSION 1.3  -- 1/11/86
*
*     WRITTEN BY JAMES F. ELY             THERMOPHYSICS DIVISION 774.03
*     NATIONAL BUREAU OF STANDARDS        BOULDER, COLORADO       80303
*                                         (303) 497-5467
*
*     CONVERTED TO A SUBROUTINE -- 10/15/91
*
*     REWRITTEN BY:
*
*     JAMES W. MUELLER -- SOLUTION SYSTEMS -- HOUSTON, TEXAS  77080
*                         (713) 468-6084
*
*     ROBERT B. ALSTON -- TEXACO, INC. -- HOUSTON, TEXAS  77042
*                         (713) 954-6114
*
*
************************************************************************
*
*     CALLING VARIABLES:
*
*     TIN    - INPUT        = INPUT TEMPERATURE OF CO2
*     PIN    - INPUT        = INPUT PRESSURE OF CO2
*     DOUT   -       OUTPUT = OUTPUT DENSITY OF CO2 AT TIN AND PIN
*     VOUT   -       OUTPUT = OUTPUT VISCOSITY OF CO2 AT TIN AND PIN
*                             (PRESENTLY SET USING JIM STEVEN'S
*                             WONDERFUL AND WACKY FORMULA)
*
************************************************************************
*
      IMPLICIT DOUBLE PRECISION(a-h, o-z)
*
      PARAMETER (nu = 6, ns = 4, np = 9, nopt = 6, nsum = nu + ns)
*
      CHARACTER*22 lbl
      CHARACTER*10 unitl
*
      LOGICAL iout
*
      DIMENSION prps(9, 2)
      REAL cvn
      REAL pcvn
*
      COMMON /convrt/ks(nu), kl(np), cvn(2, nopt, nu), pcvn(np)
      COMMON /units/unitl(nopt, nu), lbl(np)
*
*     INITIALIZE PROGRAM PARAMETERS
*
      CALL eoset()
      CALL inunit()
      iout = .FALSE.
*
*     CONVERT TO INTERNAL UNITS AND CHECK BOUNDS ON VARIABLES
*
*     CONVERT TEMPERATURE TO DEGREES FAHRENHEIT FOR CHECKING.
*
      tx = (tin - pcvn(2))/pcvn(1)
C
CAAAA
C     WRITE (*,121) TIN,TX
C 121 FORMAT (1X,' TIN=',F5.1,' F',3X,' TX=',F5.1,' K')
CAAAA
C
      IF (tx .LT. 215.0d0 .OR. tx .GT. 1000.0d0) THEN
         WRITE (6, 10000)
10000    FORMAT (' TEMPERATURE IS OUTSIDE THE PROGRAM LIMITS OF -72 TO'
     1    , ' +1340 DEGREES F.')
         STOP 'CO2PAC ERROR 1'
      ELSE
*     CONVERT PRESSURE TO BARS FOR CHECKING.
         px = pin/pcvn(3)
C
CAAAA
C     WRITE (*,122) PIN,PX
C 122 FORMAT (1X,' PIN=',F8.2,' PSIA',3X,' PX=',F7.1,' BARS')
CAAAA
C
         IF (px .LE. 0.0d0 .OR. px .GT. 3000.0d0) THEN
            WRITE (6, 10010)
10010       FORMAT (' PRESSURE IS OUTSIDE THE PROGRAM LIMITS OF >0 TO', 
     1       ' 43511 PSIA.')
            STOP 'CO2PAC ERROR 2'
         ELSE
            dx = rhof(px, tx)
C
CAAAA
C     WRITE (*,123) DX
C 123 FORMAT (1X,'DX=',F10.5)
CAAAA
C    
*
*     CALCULATE SINGLE PHASE PROPERTIES AND OUTPUT
*
            CALL props(dx, tx, prps)
            IF (iout) THEN
               WRITE (6, 10020)
10020          FORMAT ('     T        P       D         H       S', 9x, 
     1          'CP    CP/CV     WS     F/P')
               WRITE (6, 10030) (prps(k, 1), k = 1, 9)
10030          FORMAT (1x, f8.3, f9.3, f9.4, f9.3, f9.4, f9.3, f7.3, 
     1          f8.1, f9.5)
            END IF
*
            dout = prps(3, 1)
            volume = 44.01d0/dout
            viscos = 31.02d0/(0.00167d0*(volume - 31.02d0))
            viscos = viscos + 33.3d0*((44.01d0*(tin + 460.0d0)*5.0d0
     1       /9.0d0)**0.5d0/(60.69d0**0.6666d0) - 3.0d0)*exp( - 60.69d0
     2       /volume)
            vout = viscos/1.0d4
CAAAA
C     WRITE (*,124) DOUT,VOUT
C 124 FORMAT (' DENSITY(DOUT)=',F7.5,3X,' VISCOSITY(VOUT)=',F7.5)
CAAAA
            RETURN
         END IF
      END IF
      END
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      SUBROUTINE sunits
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
************************************************************************
*
*      PURPOSE --- THIS ROUTINE SELECTS THE INPUT AND OUTPUT
*                  UNITS FOR DDMIX.  THE PROGRAM IS BASICALLY
*                  A MENU DRIVER FOR VARIOUS COMMON UNIT COMBINATIONS
*
*                  NU   = NUMBER OF BASIC UNITS (T,P,V,ETC)
*                  NS   = NUMBER OF DEFAULT SETS (S.I.,ENG.,ETC)
*                  NP   = NUMBER OF PROPERTIES (DENSITY, ENTHALPY, ETC)
*                  NOPT = NUMBER OF POSSIBLE CHOICES FOR A UNIT
*                  NSUM = NU + NS
*
*      CODED BY -- J. F. ELY
*                  THERMOPHYSICS DIVISION 774.03
*                  NATIONAL BUREAU OF STANDARDS
*                  BOULDER, CO  80303
*
*      VERSION 1.0 -- 3/4/84
*
*      COPYRIGHT (C) OFFICE OF STANDARD REFERENCE DATA, 1988
*
************************************************************************
*
      IMPLICIT DOUBLE PRECISION(a-h, o-z)
      PARAMETER (nu = 6, ns = 4, np = 9, nopt = 6, nsum = nu + ns)
C     CHARACTER PROP(NSUM)*11, UNITL*10, IUNIT*10, LBL*22, DOT*1
C     CHARACTER BUFFER*22, KOPT*1, SLASH*1, LINE*40, STACK*40
      CHARACTER*1 dot
      CHARACTER*22 lbl
      CHARACTER*1 slash
      CHARACTER*10 unitl
*
      REAL cvn
      REAL pcvn
C     DIMENSION KDS(NU,NS), KOPT(NS)
*
      COMMON /convrt/ks(nu), kl(np), cvn(2, nopt, nu), pcvn(np)
      COMMON /units/unitl(nopt, nu), lbl(np)
*
C      DATA KDS / 1, 1, 1, 4, 1, 1, 1, 3, 2, 5, 3, 1,
C     *           4, 5, 5, 3, 5, 3, 1, 2, 1, 5, 1, 1 /
*
C     DATA DOT, SLASH, KOPT / '.', '/', '1', '2', '3', '4' /
      DATA dot/'.'/
      DATA slash/'/'/
*
C     DATA PROP/'TEMPERATURE','PRESSURE   ','VOLUME    ','ENERGY     ',
C    *          'MASS       ','VELOCITY   ','SCIENTIFIC','S.I.       ',
C    *          'ENGINEERING','ELY        '/
*
C  10 WRITE ( 6, 20 )
C  20 FORMAT(///28X,26(1H*)/28X,'CO2PAC PROPERTY UNIT MENU '/28X,26(1H*)
C    *///' PROPERTY    ',24(1H*),'OPTIONS',23(1H*),'  CURRENT'/ )
C     DO 40 J = 1, NU
C         WRITE ( 6, 30 ) PROP(J), (UNITL(K,J),K = 1, NOPT),
C    *                    UNITL(KS(J),J)
C  30    FORMAT(' ',A11,' (',A6,',',3(A10,','),A7,',',A4,') - ',A10)
C  40 CONTINUE
C     WRITE ( 6, 50 )
C  50 FORMAT(//' DEFAULT SET OPTIONS'/)
C     DO 70 J = NU + 1, NSUM
C        L = J - NU
C        WRITE ( 6, 60 ) KOPT(L), PROP(J),
C    *                    (UNITL(KDS(I,L),I),I = 1, NU)
C  60    FORMAT(' (',A1,')  ',A11,' (',A2,',',4(A8,','),A8,')')
C  70 CONTINUE
CC
CC     GET NEW INDIVIDUAL UNIT OR LIST
CC
C     WRITE ( 6, 80 )
C  80 FORMAT(/' ENTER THE NEW UNIT OR DEFAULT OPTION (X TO EXIT)? ')
C     READ (5, '(A)', END = 150 ) LINE
C     CALL STRIP ( NCH, LINE )
C      NCH = NCH + 1
C     LINE(NCH:NCH) = ','
C     L1 = 1
C     DO 140 L = 1, 40
C        STACK = LINE(L1:NCH)
C        L2 = INDEX ( STACK, ',' ) - 1
C        IF( L2 .EQ. 0 ) GO TO 130
C        IUNIT = STACK(1:L2)
C        IF( IUNIT .EQ. 'X' .OR. IUNIT .EQ. ' ' ) GO TO 150
C        DO 110 J = 1, NU
C           IF( J .GT. 4 ) GO TO 100
C           IF( IUNIT .EQ. KOPT(J) ) THEN
CC
CC     NEW DEFAULT SET REQUESTED
CC
C     DO 90 I = 1, NU
C        KS(I) = KDS(I,J)
C  90 CONTINUE
C     GO TO 130
C        ENDIF
CC
CC     SEARCH FOR THE INPUT UNIT
CC
C 100 DO 110 K = 1, NOPT
C       IF( IUNIT .NE. UNITL(K,J) ) GO TO 110
C       KS(J) = K
C       GO TO 130
C 110 CONTINUE
C     WRITE ( 6, 120 ) IUNIT
C 120 FORMAT(' ',A,' IS AN UNRECOGNIZED OPTION.  TRY AGAIN')
C 130 L1 = L1 + L2 + 1
C     IF( L1 .GE. NCH ) GO TO 10
C 140 CONTINUE
      RETURN
*
*     LOAD THE CONVERSION FACTORS INTO THE PCVN ARRAY AND BUILD THE
*     APPROPRIATE LABELS
*     NOTE: FOR MAKING THIS A SUBROUTINE, THE KS ARRAY INITIALIZED IN
*           BLOCK DATA UNITD MUST BE SET AS FOLLOWS -
*           KS(1) = 4 FOR TEMPERATURE IN DEGREES FAHRENHEIT,
*           KS(2) = 5 FOR PRESSURE IN POUNDS PER SQUARE INCH ABSOLUTE,
*           KS(3) = 3 FOR VOLUME IN CUBIC CENTIMETERS,
*           KS(4) = 3 FOR ENERGY IN BRITISH THERMAL UNITS,
*           KS(5) = 4 FOR MASS IN GRAMS, AND
*           KS(6) = 3 FOR VELOCITY IN FEET PER SECOND.
*
      ENTRY inunit
*
*     TEMPERATURE
*
      pcvn(1) = cvn(1, ks(1), 1)
      pcvn(2) = cvn(2, ks(1), 1)
*
*     PRESSURE
*
      pcvn(3) = cvn(1, ks(2), 2)
      pcvn(4) = cvn(2, ks(2), 2)
*
*     DENSITY
*
      cvnden = cvn(1, ks(5), 5)
      IF (ks(5) .GT. 2) cvnden = cvnden*44.009d0
      pcvn(4) = cvnden/cvn(1, ks(3), 3)
      lbl(4) = unitl(ks(5), 5)//slash//unitl(ks(3), 3)
*
*     ENTHALPY
*
      pcvn(5) = cvn(1, ks(4), 4)/cvnden
      lbl(5) = unitl(ks(4), 4)//slash//unitl(ks(5), 5)
*
*     ENTROPY
*
      ks4 = ks(4)
      IF (ks4 .GT. 3 .AND. ks(5) .NE. 3) ks4 = ks4 - 3
      pcvn(6) = cvn(1, ks4, 4)/cvnden/cvn(1, ks(1), 1)
      lbl(6) = unitl(ks4, 4)//slash//unitl(ks(5), 5)
      LBL(6)(21:22) = dot//unitl(ks(1), 1)(1:1)
*
*     HEAT CAPACITY
*
      pcvn(7) = pcvn(6)
      lbl(7) = lbl(6)
*
*     HEAT CAPACITY RATIO
*
      pcvn(8) = 1.0d0
      lbl(8) = ' '
*
*     SOUND VELOCITY
*
      pcvn(9) = cvn(1, ks(6), 6)
      lbl(9) = unitl(ks(6), 6)
*
*     EXIT TO CALLING PROGRAM
*
C      CALL FILRST
      RETURN
      END
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      SUBROUTINE props(d, t, prps)
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      IMPLICIT DOUBLE PRECISION(a-h, o-z)
      PARAMETER (nu = 6, ns = 4, np = 9, nopt = 6, nsum = nu + ns)
      CHARACTER*22 lbl
      CHARACTER*10 unitl
      DIMENSION prps(9)
      REAL cvn
      REAL pcvn
*
      COMMON /convrt/ks(nu), kl(np), cvn(2, nopt, nu), pcvn(np)
      COMMON /refdat/r, pc, dc, tc, g(32), zc, ptrp, dtrp, ttrp, cmw
      COMMON /units/unitl(nopt, nu), lbl(np)
*
      rj = 8.31434d0
      CALL press(p, d, t, 1)
      CALL dpdt(dpt, d, t, 1)
      CALL dpdd(dpd, d, t, 1)
      CALL ar(ares, d, t, 1)
      CALL sr(sres, d, t, 1)
      CALL cvr(cvres, d, t, 1)
      CALL ideal(t, h0, s0, cv0)
      rjt = rj*t
      z = p/(r*t*d)
      ur = ares + t*sres
      hr = ur + rjt*(z - 1.0d0)
      h = hr + h0
      s = sres + s0 + rj*log(z/p*1.01325d0)
      cv = cvres + cv0
      cp = cv + 100.0d0*t*(dpt/d)**2/dpd
      IF (t .EQ. tc .AND. d .EQ. dc) dpd = 0.0d0
      gamma = cp/cv
      ws = sqrt(1.0d5*gamma*dpd/cmw)
      fop = ares/rjt + z - 1.0d0 - log(z)
      fop = exp(fop)
*
      prps(1) = pcvn(1)*t + pcvn(2)
      prps(2) = pcvn(3)*p
      prps(3) = pcvn(4)*d
      prps(4) = pcvn(5)*h
      prps(5) = pcvn(6)*s
      prps(6) = pcvn(7)*cp
      prps(7) = gamma
      prps(8) = pcvn(9)*ws
      prps(9) = fop
      RETURN
      END
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      SUBROUTINE ideal(tk, hz, sz, cvz)
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
*
*      PURPOSE --- THIS ROUTINE CALCULATES THE IDEAL GAS
*                  FUNCTIONS FOR CO2.  THE FUNCTIONAL FORMULATION
*                  IS DUE TO GOODWIN.
*
*      VERSION 1.0 --- 5/20/81
*
************************************************************************
*
      IMPLICIT DOUBLE PRECISION(a-h, o-z)
      DIMENSION a(5)
*
      DATA a/2.3878698195d-2, 4.3507944279d0, -10.33404847d0, 
     1     7.9815905196d0, -1.9405584200d0/
      DATA ep/1.83500d0/
      DATA hi/14.3070d0/
      DATA r/8.31434d0/
      DATA si/25.7280d0/
*
      xi = 0.01d0*tk
      xp = exp( - ep/xi)
      cp = 0.0d0
      DO k = 1, 5
         cp = cp + a(k)*xi**(2 - k)
      END DO
      cp = 3.5d0 + cp*xp
*
*     NUMERICAL INTERATION FOR ENTROPY AND ENTHALPY
*
      h = 0.0d0
      s = 0.0d0
      n = abs(tk - 300.0)*0.25d0 + 2.0d0
      dx = (xi - 3.0d0)/real(n)
      DO j = 1, n
         x = 3.0d0 + (real(j) - 0.5d0)*dx
         xp = exp( - ep/x)
         cpx = 3.5d0
         DO k = 1, 5
            cpx = cpx + a(k)*xp*x**(2 - k)
         END DO
         h = h + cpx*dx
         s = s + cpx*dx/x
      END DO
      hz = 100.0d0*r*(h + 3.0d0*hi)
      sz = r*(s + si)
      cpz = r*cp
      cvz = cpz - r
*
      RETURN
      END
*FFFF FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FUNCTION rhof(p1, p2)
*FFFF FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
************************************************************************
*
*     PURPOSE --- THIS ROUTINE CALCULATES THE DENSITY OF A FLUID AT
*                 T AND P GIVEN AN INITIAL GUESS IN FOP.  ON EXIT,
*                 IT RETURNS THE FUGACITY COEFFICIENT IN FOP.  IT
*                 REQUIRES A ROUTINE 'PVTF' WHICH CALCULATES P,
*                 DPDD, AND GR = G(T,P)-G*(T,1)
*
*     CODED BY -- J. F. ELY
*                 THERMOPHYSICAL PROPERTIES DIVISION
*                 NATIONAL ENGINEERING LABORATORY
*                 NATIONAL BUREAU OF STANDARDS
*                 BOULDER, COLORADO 80302
*
*     VERSION 2.0 -- 5/23/82
*
*     ORIGINAL VERSION OF SUBROUTINE RHO FROM NBS PROGRAM -- CO2PAC
*
************************************************************************
*
      IMPLICIT DOUBLE PRECISION(a-h, o-z)
      COMMON /refdat/r, pc, dc, tc, g(32), zc, ptrp, dtrp, ttrp, cmw
      DATA tolerd, tolerp/1.0d-8, 1.0d-8/
*
*     ESTABLISH BOUNDS AND START NEWTON-RAPHSON
*
      p = p1
      t = p2
C
CAAAA
C     WRITE (*,125) P,PC,T,TC
C 125 FORMAT (1X,' P=',F8.2,2X,' PC=',F8.2,2X,
C    * ' T=',F5.1,2X,' TC=',F5.1)
CAAAA
C
      IF (p .EQ. pc .AND. t .EQ. tc) THEN
         rhof = dc
      ELSE
         d = min(p/(r*t), dtrp)
         IF (t .LE. tc) THEN
            CALL satf(t, ps, dsl, dsv)
            d = dsv
            IF (p .GE. ps) d = (2.0d0*dsl + dtrp)/3.0
         END IF
*
*     ESTABLISH BOUNDS AND START NEWTON RAPHSON
*
         dlo = 0.0d0
         dhi = 1.25d0*dtrp
         dmax = dhi
         DO lap = 1, 20
            CALL pvtf(px, d, t, dpdd, d2pdd2, gr)
*
*     IF DPDD IS ZERO OR NEGATIVE, TRY BISECTION
*
            IF (dpdd .LE. 1.0d-3) EXIT
            dp = p - px
            dd = dp/dpdd
*
*     SAVE DENSITY FOR POSSIBLE BISECTION
*
            IF (dp .EQ. 0) GOTO 50
            IF (dp .GT. 0) THEN
               dlo = d
            ELSE
               dhi = d
            END IF
            dn = d + dd
*
*     KEEP D WITHIN BOUNDS OR GO TO BISECTION
*
            IF (dn .LT. 0.0d0 .OR. dn .GT. dmax) EXIT
            d = dn
            IF (lap .NE. 1) THEN
               IF (abs(dp/p) .LE. tolerp) GOTO 50
               IF (abs(dd/d) .LE. tolerd) GOTO 50
            END IF
         END DO
*
*     NEWTON-RAPHSON FAILURE.  TRY BISECTION
*
         IF (t .LT. tc) THEN
*
*     SUB-CRITICAL.  MAKE SURE THAT WE HAVE THE PROPER 
*     BOUNDS ON THE DENSITY.
*
C
CAAAA
C     WRITE (*,126)
C 126 FORMAT (1X,' THIS IS THE SUB-CRITICAL TEMPERATURE
C    * TEST REGION')
CAAAA
C
            IF (p .LT. ps) THEN
C
               IF (dlo .GE. dsv) dlo = 0.0d0
               dhi = dsv
            ELSE
               dlo = dsl
               IF (dhi .LE. dsl) dhi = dmax
C
CAAAA
C     WRITE(*,127) DMAX
C 127 FORMAT (1X,' DHI=DMAX=',F7.5)
CAAAA
C
            END IF
         END IF
C
CAAAA
C     WRITE (*,128) DHI
C 128 FORMAT (1X,' DHI=DSV=',F7.5)
CAAAA
C
*
*        START THE BISECTION
*
         DO WHILE (.TRUE.)
            d = 0.50d0*(dlo + dhi)
            CALL pvtf(px, d, t, dpdd, d2pdd2, gr)
            dp = px - p
            IF (dp .EQ. 0) GOTO 50
            IF (dp .GT. 0) THEN
               dhi = d
            ELSE
               dlo = d
            END IF
            IF (abs(dp/p) .LE. tolerp) GOTO 50
            IF (abs(dlo/dhi - 1.0d0) .LE. tolerd) EXIT
         END DO
*
*     BISECTION FAILED.  GIVE UP
*
         IF (abs(dp) .GE. 5.0d-5) WRITE (6, 10000) t, p
*
*     CONVERGENCE
*
   50    rhof = d
      END IF
      RETURN
10000 FORMAT (' RHOF FAILED AT T =', f9.3, ' P =', g14.7)
      END
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      SUBROUTINE satf(ts, ps, dsl, dsv)
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
***********************************************************************
*
*      PURPOSE -- THIS ROUTINE CALCULATES THE SATURATION
*                 PRESSURE AND COEXISTING DENSITIES FROM
*                 AN EQUATION OF STATE.
*
*      VERSION 2.0 -- 5/20/82
*
*      COPYRIGHT (C) OFFICE OF STANDARD REFERENCE DATA, 1988
*
*      CODED BY -- J. F. ELY
*                  THERMOPHYSICAL PROPERTIES DIVISION
*                  NATIONAL BUREAU OF STANDARDS
*                  BOULDER, COLORADO  80303
*
************************************************************************
*
      IMPLICIT DOUBLE PRECISION(a-h, o-z)
      PARAMETER (nc = 32, nf = nc + 1, nb = 5)
      LOGICAL enter
      COMMON /refdat/r, pc, dc, tc, g(nc), zc, ptrp, dtrp, ttrp, cmw
      DATA enter/.FALSE./
      DATA ftol/1.0d-6/
      DATA tol/1.0d-5/
*
      IF (ts .GE. tc) THEN
         ps = pc
         dsl = dc
         dsv = dc
      ELSE
         IF (.NOT. enter) THEN
            enter = .TRUE.
            bv = log(ptrp/pc)/(1.0d0/ttrp - 1.0d0/tc)
            av = log(pc) - bv/tc
         END IF
*
*     INITIAL GUESS AT THE VAPOR PRESSURE
*
         ps = exp(av + bv/ts)
*
*     INITAL GUESS AT THE VAPOR DENSITY
*
         dv = ps/(r*ts)
*
*     INITIAL GUESS AT LIQUID DENSITY
*
         tr = ts/tc
         eps = (1.0d0 - tr)**(2.0d0/7.0d0)
         dl = dc/zc**eps
         IF (dl .GT. dtrp) dl = dtrp
*
*     IMPROVE VAPOR GUESS NEAR CRITICAL
*
         IF (tr .GE. 0.85d0) dv = dl - 3.75d0*dc*(1.0d0 - tr)*
     1    *0.333333333d0
*
*     NEWTON-RAPHSON ITERATION FOR DENSITIES
*
         DO i = 1, 25
            DO WHILE (.TRUE.)
               CALL pvtf(pl, dl, ts, dpdl, d2pdd2, gl)
               IF (dpdl .GT. 0.0d0 .AND. pl .GT. 0.0d0) EXIT
               dl = 1.02d0*dl
            END DO
            DO WHILE (.TRUE.)
               CALL pvtf(pv, dv, ts, dpdv, d2pdd2, gv)
               IF (dpdv .GT. 0.0d0) EXIT
               dv = 0.98d0*dv
            END DO
            f1 = gl - gv
            f2 = pl - pv
            f2l = dpdl
            f2v =  - dpdv
            f1l = f2l/dl
            f1v = f2v/dv
            denom = f1l*f2v - f2l*f1v
            IF (abs(denom) .LE. 1.0d-10) GOTO 50
            ddl =  - (f1*f2v - f2*f1v)/denom
            ddv =  - (f1 + ddl*f1l)/f1v
            dl = dl + ddl
            IF (dl .LT. dc) dl = dc
            dvs = dv
            dv = dv + ddv
            IF (dv .GT. dc) dv = dc
            IF (dv .LE. 0.0d0) dv = dvs*0.5d0
            IF (abs(ddl/dl) .LT. tol .AND. abs(ddv/dv) .LE. tol) EXIT
            fnorm = f1*f1 + f2*f2
            IF (tr .LT. 0.99d0 .AND. fnorm .LE. ftol) EXIT
         END DO
         GOTO 100
   50    WRITE (6, 10000) denom
10000    FORMAT (' DENOM IS TOO SMALL = ', g13.6)
  100    ps = pv
         dsl = dl
         dsv = dv
      END IF
      RETURN
*
      END
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      SUBROUTINE eosprp(pp, dd, tt, ival)
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
***********************************************************************
*
*      PURPOSE --- THIS ROUTINE GENERATES THE APPROPRIATE FITTING
*                  FUNCTIONS FOR A WAGNER TYPE EOS
*
*      VERSION 1.0 - 4/2/85
*
*      CODED BY -- J. F. ELY
*                  NATIONAL BUREAU OF STANDARDS
*                  325 BROADWAY
*                  BOULDER, CO  80303
*
************************************************************************
*
      IMPLICIT DOUBLE PRECISION(a-h, o-z)
      PARAMETER (nc = 32, nf = nc + 1, nb = 5)
      REAL rjx
      DIMENSION d(0:16), b(nf), b1(nc), b2(nc), b3(nc), f(3)
*
      COMMON /eosp/in(nc), jn(nc), kb(nb), rjx(nc)
      COMMON /refdat/r, pc, dc, tc, g(nc), zc, ptrp, dtrp, ttrp, cmw
*
      DATA d(0)/0.0d0/
      DATA dlast/-1.0d0/
      DATA rj/8.31434d0/
      DATA tlast/-1.0d0/
      GOTO 50
*
      ENTRY press(pp, dd, tt, ival)
*
      label = 60
      GOTO 50
*
      ENTRY dpdd(pp, dd, tt, ival)
*
      label = 80
      GOTO 50
*
      ENTRY dp2d2(pp, dd, tt, ival)
*
      label = 100
      GOTO 50
*
      ENTRY dpdt(pp, dd, tt, ival)
*
      label = 120
      GOTO 50
*
      ENTRY dp2dt2(pp, dd, tt, ival)
*
      label = 140
      GOTO 50
*
      ENTRY cvr(pp, dd, tt, ival)
*
      label = 160
      GOTO 50
*
      ENTRY sr(pp, dd, tt, ival)
*
      label = 180
*
*
   50 IF (dd .NE. dlast) THEN
         dlast = dd
         delta = dd/dc
         d(1) = delta
         DO i = 2, 16
            d(i) = d(i - 1)*delta
         END DO
         f(1) = 1.0d0
         f(2) = exp( - d(2))
         f(3) = exp( - d(4))
      END IF
C
      IF (tt .NE. tlast) THEN
         tlast = tt
         tau = tc/tt
C
C     GENERATE BASE TEMPERATURE TERMS
C
         DO l = 1, nc
            b1(l) = tau**rjx(l)
            b2(l) = b1(l)*rjx(l)
            b3(l) = (rjx(l) - 1.0d0)*b2(l)
         END DO
      END IF
C
      rt = r*tt
      rjt = rj*tt
      drt = dd*rt
      dr = dd*r
      rtd = rt/dd
      drtt = dr/tt
      IF (ival .EQ. 1) pp = 0.0d0
      SELECT CASE (label)
         CASE (40)
            GOTO 100
         CASE (60)
            GOTO 150
         CASE (80)
            GOTO 200
         CASE (100)
            GOTO 250
         CASE (120)
            GOTO 300
         CASE (140)
            GOTO 350
         CASE (160)
            GOTO 400
         CASE (180)
            GOTO 450
      END SELECT
*
      ENTRY ar(pp, dd, tt, ival)
*
      label = 40
      GOTO 50
  100 CONTINUE
      DO l = 1, nc
         b(l) = b1(l)*rjt*d(in(l))*f(jn(l))
      END DO
      b(nf) = pp
      padd = 0.0d0
      GOTO 500
  150 CONTINUE
      DO l = 1, nc
         i = in(l)
         j = jn(l)
         j2 = 2*(j - 1)
         trm1 = real(i) - real(j2)*d(j2)
         b(l) = b1(l)*d(i)*trm1*drt*f(j)
      END DO
      b(nf) = pp - drt
      padd = drt
      GOTO 500
  200 CONTINUE
      DO l = 1, nc
         i = in(l)
         j = jn(l)
         j2 = 2*(j - 1)
         trm1 = real(j2)*d(j2)
         trm2 = real(i*(i + 1)) - trm1*(real(2*i + j2 + 1) - trm1)
         b(l) = b1(l)*rt*d(i)*trm2*f(j)
      END DO
      b(nf) = pp - rt
      padd = rt
      GOTO 500
  250 CONTINUE
      DO l = 1, nc
         i = in(l)
         i2 = 2*i
         j = jn(l)
         j2 = 2*(j - 1)
         trm0 = real(i*i*(i + 1))
         trm1 = real(i*(i - 1) + (i2 + j2 - 1)*(i + j2 + 2) + 2)
         trm2 = real(3*(i + j2) + 1)
         x = real(j2)*d(j2)
         trmx = trm0 - x*(trm1 - x*(trm2 - x))
         b(l) = b1(l)*d(i)*rtd*trmx*f(j)
      END DO
      b(nf) = pp
      padd = 0.0d0
      GOTO 500
  300 CONTINUE
      DO l = 1, nc
         i = in(l)
         j = jn(l)
         j2 = 2*(j - 1)
         trm1 = real(i) - real(j2)*d(j2)
         b(l) = (b1(l) - b2(l))*d(i)*trm1*dr*f(j)
      END DO
      b(nf) = pp - dr
      padd = dr
      GOTO 500
  350 CONTINUE
      DO l = 1, nc
         i = in(l)
         j = jn(l)
         j2 = 2*(j - 1)
         trm1 = real(i) - real(j2)*d(j2)
         b(l) = b3(l)*d(i)*trm1*drtt*f(j)
      END DO
      b(nf) = pp
      padd = 0.0d0
      GOTO 500
  400 CONTINUE
      DO l = 1, nc
         b(l) =  - b3(l)*rj*d(in(l))*f(jn(l))
      END DO
      b(nf) = pp
      padd = 0.0d0
      GOTO 500
  450 CONTINUE
      DO l = 1, nc
         i = in(l)
         j = jn(l)
         b(l) = (b2(l) - b1(l))*d(i)*rj*f(j)
      END DO
      b(nf) = pp
      padd = 0.0d0
C     GO TO 200
*
  500 IF (ival .GT. 0) THEN
         psum = 0.0d0
         DO l = 1, nc
            psum = psum + g(l)*b(l)
         END DO
         pp = psum + padd
*
      END IF
      RETURN
      END
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      SUBROUTINE pvtf(p0, d0, t0, dpdd, d2pd2, g0)
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
***********************************************************************
*
*     PURPOSE --- THIS ROUTINE CALCULATES THE  PRESSURE, ITS FIRST
*                 TWO DENSITY DERIVATIVES AND THE GIBBS ENERGY
*                 RELATIVE TO THE IDEAL GAS AT 1 BAR
*                 THIS ROUTINE IS FOR THE WAGNER EQUATION OF STATE
*
*     VERSION 1.0 - 2/18/85
*
*     CODED BY -- J. F. ELY
*                 CHEMICAL ENGINEERING SCIENCE DIVISION
*                 NATIONAL BUREAU OF STANDARDS
*                 BOULDER, COLORADO  80303
*
************************************************************************
*
      IMPLICIT DOUBLE PRECISION(a-h, o-z)
      PARAMETER (nc = 32, nf = nc + 1, nb = 5)
      DIMENSION s(4), p(10, 3), q(3), dqdd(3), d2qdd2(3), d3qdd3(3)
      REAL rjx
*
      COMMON /eosp/in(nc), jn(nc), kb(nb), rjx(nc)
      COMMON /refdat/rg, pc, dc, tc, g(nc), zc, ptrp, dtrp, ttrp, cmw
*
      DATA p/30*0.0d0/
      DATA tlast/-1.000d0/
      tlast =  - 1.0d0
*
      t = tc/t0
      d = d0/dc
      IF (t .NE. tlast) THEN
         tlast = t
         rt = rg*t0
         DO j = 1, 3
            DO i = 1, 10
               p(i, j) = 0.0d0
            END DO
         END DO
         DO k = 1, nc
            j = jn(k)
            i = in(k)
            p(i, j) = p(i, j) + g(k)*t**rjx(k)
         END DO
      END IF
*
      DO j = 1, 3
         DO i = 1, 4
            s(i) = 0.0d0
         END DO
         DO i = 1, 10
            s(1) = s(1)*d + p(11 - i, j)
            s(2) = s(2)*d + s(1)
            IF (i .NE. 10) THEN
               s(3) = s(3)*d + s(2)
               IF (i .NE. 9) s(4) = s(4)*d + s(3)
            END IF
         END DO
         q(j) = s(1)*d
         dqdd(j) = s(2)
         d2qdd2(j) = s(3) + s(3)
         d3qdd3(j) = s(4)*6.0d0
      END DO
*
      d2 = d*d
      d3 = d*d2
      f2 = exp( - d2)
      f3 = exp( - d*d3)
      twod = d + d
      ford = 4.0d0*d3
      drt = d0*rt
*
      phi = q(1) + q(2)*f2 + q(3)*f3
*
      trm2 = dqdd(2) - twod*q(2)
      trm3 = dqdd(3) - ford*q(3)
      dphdd = dqdd(1) + trm2*f2 + trm3*f3
      p0 = drt*(1.0d0 + d*dphdd)
*
      trm2p = d2qdd2(2) - 2.0d0*q(2) - twod*dqdd(2)
      trm3p = d2qdd2(3) - 12.0d0*d2*q(3) - ford*dqdd(3)
      trm22 = trm2p - twod*trm2
      trm33 = trm3p - ford*trm3
      d2phd2 = d2qdd2(1) + trm22*f2 + trm33*f3
      dpdd = rt*(1.0d0 + twod*dphdd + d2*d2phd2)
*
      trm22p = d3qdd3(2) - 4.0d0*dqdd(2) - twod*d2qdd2(2) - 2.0d0*trm2 
     1 - twod*trm2p
      trm33p = d3qdd3(3) - 24.0d0*d*(q(3) + d*dqdd(3)) - ford*(d2qdd2
     1 (3) + trm3p) - 12.0d0*d2*trm3
      d3phd3 = d3qdd3(1) + (trm22p - twod*trm22)*f2 + (trm33p - ford
     1 *trm33)*f3
      d2pd2 = rt*(2.0d0*dphdd + 4.0d0*d*d2phd2 + d2*d3phd3)/dc
*
      z0 = p0/drt
      g0 = rt*(phi + z0 - 1.0d0 + log(drt))
*
      RETURN
      END
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      SUBROUTINE eoset
*SSSS SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      IMPLICIT DOUBLE PRECISION(a-h, o-z)
      PARAMETER (nc = 32, nf = nc + 1, nb = 5)
      DIMENSION mask(5, 10, 3), xpj(5, 10, 3)
      REAL xpj
      REAL rjx
      COMMON /eosp/in(nc), jn(nc), kb(nb), rjx(nc)
*
      DATA((xpj(k, i, 1), k = 1, 5), i = 1, 10)/0.0, 1.5, 2.5, 0.0, 
     1 0.0, -0.5, 1.5, 2.0, 0.0, 0.0, 0.0, 1.0, 2.5, 0.0, 0.0, 0.0, 
     2 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
     3 0.0, 2.0, 5.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
     4 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/
*
      DATA((xpj(k, i, 2), k = 1, 5), i = 1, 10)/5.0, 6.0, 0.0, 0.0, 
     1 0.0, 3.5, 5.5, 0.0, 0.0, 0.0, 3.0, 7.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
     2 0.0, 0.0, 0.0, 6.0, 0.0, 0.0, 0.0, 0.0, 8.5, 0.0, 0.0, 0.0, 0.0, 
     3 4.0, 0.0, 0.0, 0.0, 0.0, 6.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
     4 0.0, 0.0, 5.5, 0.0, 0.0, 0.0, 0.0/

      DATA((xpj(k, i, 3), k = 1, 5), i = 1, 10)/0.0, 0.0, 0.0, 0.0, 
     1 0.0, 22.0, 0.0, 0.0, 0.0, 0.0, 11.0, 18.0, 0.0, 0.0, 0.0, 11.0, 
     2 23.0, 0.0, 0.0, 0.0, 17.0, 18.0, 23.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
     3 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
     4 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/
*
      DATA((mask(k, i, 1), k = 1, 5), i = 1, 10)/1, 1, 1, 0, 0, 1, 1, 
     1 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 
     2 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
     3 /
*
      DATA((mask(k, i, 2), k = 1, 5), i = 1, 10)/1, 1, 0, 0, 0, 1, 1, 
     1 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 
     2 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0
     3 /
*
      DATA((mask(k, i, 3), k = 1, 5), i = 1, 10)/0, 0, 0, 0, 0, 1, 0, 
     1 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 
     2 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
     3 /
*
      l = 0
      m = 0
      DO j = 1, 3
         DO i = 1, 10
            DO k = 1, 5
               IF (mask(k, i, j) .NE. 0) THEN
                  l = l + 1
                  in(l) = i
                  jn(l) = j
                  rjx(l) = xpj(k, i, j)
                  IF (i .EQ. 1) THEN
                     m = m + 1
                     kb(m) = l
                  END IF
               END IF
            END DO
         END DO
      END DO
      END
*DBDB BDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDB
      BLOCK DATA unitd
*DBDB BDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDB
      IMPLICIT DOUBLE PRECISION(a-h, o-z)
      PARAMETER (nu = 6, ns = 4, np = 9, nopt = 6, nsum = nu + ns)
      CHARACTER*22 lbl
      CHARACTER*10 unitl
      REAL cvn
      REAL pcvn
*
      COMMON /convrt/ks(nu), kl(np), cvn(2, nopt, nu), pcvn(np)
      COMMON /units/unitl(nopt, nu), lbl(np)
*
C      DATA KS / 1, 2, 1, 5, 1, 1 /, KL / NP*1 /
*
*     INITIALIZE FOR SUBROUTINE VERSION AS FOLLOWS.  (SEE SUBROUTINE
*     SUNITS FOR MORE DETAILS.)
*
      DATA ks/4, 5, 3, 3, 4, 3/
*
      DATA lbl/np*' '/
      DATA kl/np*1/
*
      DATA unitl/'K      ', 'R      ', 'C      ', 'F      ', 2*'    ', 
     1     'ATM    ', 'BAR    ', 'MPA    ', 'KPF    ', 'PSIA   ', 
     2     'MMHG  ', 'LITER  ', 'M**3   ', 'CM**3  ', 'IN**3  ', 
     3     'FT**3  ', '      ', 'CAL    ', 'J      ', 'BTU    ', 
     4     'KCAL   ', 'KJ     ', '      ', 'G-MOL  ', 'LB-MOL ', 
     5     'KG     ', 'G      ', 'LB     ', '      ', 'M/S    ', 
     6     'CM/S   ', 'FT/S   ', 'IN/S   ', '       ', '      '/
*
      DATA((cvn(i, j, 1), i = 1, 2), j = 1, nopt)/1.00000e00, 
     1 0.00000e00, 1.80000e00, 0.00000e00, 1.00000e00, -2.73150e02, 
     2 1.80000e00, -4.59670e02, 0.00000e00, 0.00000e00, 0.00000e00, 
     3 0.00000e00/
*
      DATA((cvn(i, j, 2), i = 1, 2), j = 1, nopt)/9.86923e-1, 
     1 0.00000e00, 1.00000e00, 0.00000e00, 1.00000e-1, 0.00000e00, 
     2 1.01927e00, 0.00000e00, 1.450377e1, 0.00000e00, 7.50062e02, 
     3 0.00000e00/
*
      DATA((cvn(i, j, 3), i = 1, 2), j = 1, nopt)/1.00000e00, 
     1 0.00000e00, 1.00000e-3, 0.00000e00, 1.00000e03, 0.00000e00, 
     2 6.102376e1, 0.00000e00, 3.53147e-2, 0.00000e00, 0.00000e00, 
     3 0.00000e00/
*
      DATA((cvn(i, j, 4), i = 1, 2), j = 1, nopt)/2.39006e-1, 
     1 0.00000e00, 1.00000e00, 0.00000e00, 9.48451e-4, 0.00000e00, 
     2 2.39006e-1, 0.00000e00, 1.00000e-3, 0.00000e00, 0.00000e00, 
     3 0.00000e00/
*
      DATA((cvn(i, j, 5), i = 1, 2), j = 1, nopt)/1.00000e00, 
     1 0.00000e00, 2.20462e-3, 0.00000e00, 1.00000e-3, 0.00000e00, 
     2 1.00000e00, 0.00000e00, 2.20462e-3, 0.00000e00, 0.00000e00, 
     3 0.00000e00/
*
      DATA((cvn(i, j, 6), i = 1, 2), j = 1, nopt)/1.00000e00, 
     1 0.00000e00, 1.00000e02, 0.00000e00, 3.28084e00, 0.00000e00, 
     2 3.93708e01, 0.00000e00, 0.00000e00, 0.00000e00, 0.00000e00, 
     3 0.00000e00/
*
      END
*DBDB BDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDB
      BLOCK DATA wagner
*DBDB BDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDB
      IMPLICIT DOUBLE PRECISION(a-h, o-z)
      PARAMETER (nc = 32)
      COMMON /refdat/r, pc, dc, tc, g(nc), zc, ptrp, dtrp, ttrp, cmw
*
*     WAGNER TYPE CARBON DIOXIDE EOS - 5/23/85 (AHLQABKF)
*
      DATA g, pc, dc, tc, r, ptrp, dtrp, ttrp, zc, cmw
     1     /0.485270895829d00, -0.191835554052d01, 0.452678296738d00, 
     2     0.871399014900d-02, 0.306077592832d00, -0.176172077242d00, 
     3     0.451617030635d-01, -0.373343036900d-01, -0.174991397189d
     4     -01, 0.817323498996d-03, 0.318521233571d-03, 
     5     -0.671472847264d-05, -0.465414461899d-04, 
     6     -0.392146573287d00, 0.135843407224d00, 0.106253850720d00, 
     7     -0.276114808295d-01, -0.103648242782d00, -0.173959632974d
     8     -01, -0.269413926673d-01, -0.649763728286d-02, 
     9     -0.230640203011d-01, 0.152662831281d-01, -0.342538620134d
     1     -02, 0.380395688183d-03, -0.174811272097d-01, 
     2     -0.261526006332d-01, 0.376496293521d-01, 0.140541502444d-01, 
     3     -0.514869731875d00, 0.588742419462d00, -0.944739176053d-01, 
     4     0.737521000000d02, 0.106300000000d02, 0.304130000000d03, 
     5     0.831434000000d-01, 0.517750000000d01, 0.267800000000d02, 
     6     0.216568000000d03, 0.274381031000d00, 0.440098000000d02/
*
      END

