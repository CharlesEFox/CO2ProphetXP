C STRUCTURED BY FOR_STRUCT, V2.0.1, ON 04/21/94 AT 16:17:44
C  OPTIONS SET: DUP=R FMT=ACHN I=50,3 LABF=10000,10 LABX=50,50 OFF=AI
C               S=9 SP=DE V
C
************************************************************************
      SUBROUTINE cnvout(nlayrs, ntimes, tres)
************************************************************************
C
C
C     THIS SUBROUTINE CONVERTS THE OUTPUT UNITS OF HYDROCARBON PORE
C     VOLUMES TO SURFACE UNITS AND DETERMINES THESE VALUES AT THE
C     SPECIFIED TIME INTERVALS.  A CHRONOLOGICAL TABLE IS PREPARED
C     FOR BOTH THE INPUT AND THE OUTPUT DATA.  INJECTION/PRODUCTION
C     RATES (RESERVOIR UNITS) ARE ASSUMED TO BE BALANCED.
C
C     MODIFIED BY JOHN DOBITZ, 1/18/93
C
      INCLUDE 'PARAM.INC'
      INCLUDE 'FIELD.INC'
      INCLUDE 'ROCKAT.INC'
      INCLUDE 'CNVRT.INC'
C
C
      DOUBLE PRECISION acres, bco2rb, bwrb, cnvoil, cnvsol
      DOUBLE PRECISION cnvwtr, curoil, cursol, curtot, curwtr
      DOUBLE PRECISION delo, deloil, dels, delsol, deltot, deltpv
      DOUBLE PRECISION delw, delwtr, dgor, dhcgas, dlyrat, doil, dooip
      DOUBLE PRECISION dwor, dwtr, gor
      DOUBLE PRECISION hcgas, hcpvrb, hcpvmm
      DOUBLE PRECISION hcpvyr(0:maxtim), lstoil, lstsol, lsttot, lsttim
      DOUBLE PRECISION lstvol, lstwtr, masoil, maswtr, oil, ooip
      DOUBLE PRECISION pctsol, pctwtr, propvt(0:maxout)
      DOUBLE PRECISION propvo(0:maxout), propvs(0:maxout)
      DOUBLE PRECISION propvw(0:maxout), prttim(0:maxout)
      DOUBLE PRECISION pvproo(maxout), pvpros(maxout)
      DOUBLE PRECISION pvprot(maxout), pvprow(maxout), rate
      DOUBLE PRECISION ratsol(0:maxtim + 1), ratwtr(0:maxtim + 1), sol
      DOUBLE PRECISION solscf, solvol, tres, volsol(maxout)
      DOUBLE PRECISION volwtr(maxout), water, wor
      DOUBLE PRECISION wtrstb, wtrvol, yrlyrt
C
C
C
      INTEGER outstp, prostp, t
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     **** CALCULATION OF OUTPUT CONVERSION FACTORS AND DATA           C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      bco2rb = bco2/5.6146d0
      bwrb = bw/5.6146d0
      hcpvrb = hcpv/5.6145d0
      hcpvmm = hcpvrb/1000000d0
      cnvoil = hcpvrb/bo
      cnvsol = hcpvrb/bco2rb
      cnvwtr = hcpvrb/bwrb
      acres = area/43560d0
      masoil = (141.5d0/(api + 131.5d0))*0.99795d0*350.5068d0 + (rs/
     1 (378.62d0*(1.00308d0 - 0.0094d0*gsg))*28.964d0*gsg)
      doil = (masoil*453.59d0)/(bo*158986.8d0)
      maswtr = 62.291d0 + (0.438059d-4)*saln + (0.1598753d-10)*saln
     1 *saln
      dwtr = (maswtr*5.6145d0*453.59)/(bwrb*158986.8d0)
C
C
      OPEN (110, FILE = 'OUTPUT')
      REWIND (110)
C
      OPEN (21, FILE = 'LABELOUT')
      REWIND (21)
C
      OPEN (30, FILE = 'DIGITOUT')
      REWIND (30)
C
C
      WRITE (21, 10000) title
10000 FORMAT (13x, a50, //)
C
      prostp = ismax
      DO t = 1, ntimes
         ratwtr(t) = 0.0
         ratsol(t) = 0.0
      END DO
      DO i = 1, noinjw
         DO t = 1, ntimes
            ratwtr(t) = ratwtr(t) + wtrrat(t, i)
            ratsol(t) = ratsol(t) + solrat(t, i)
         END DO
      END DO
C
C
      DO i = 1, prostp
         READ (110, 10010) pvprot(i), pvproo(i), pvpros(i), pvprow(i)
10010    FORMAT (1x, 4(f15.7))
      END DO
      outstp = int(sumtim(ntimes)/outtim) + ntimes + 1
      lsttim = 0.0
      sumtim(ntimes + 1) = 100.0
      t = 1
      DO i = 1, outstp
         IF (lsttim .GT. sumtim(t)) THEN
            prttim(i) = sumtim(t)
            t = t + 1
         ELSE
            prttim(i) = lsttim
            lsttim = lsttim + outtim
         END IF
      END DO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      j = 0
      t = 1
      hcpvyr(t) = totrat(t)*365.25d0/hcpv
      lstvol = 0.0
      prttim(0) = 0.0
C
      DO i = 1, outstp
         propvt(i) = lstvol + (prttim(i) - prttim(i - 1))*hcpvyr(t)
         lstvol = propvt(i)
         IF (prttim(i) .EQ. sumtim(t)) THEN
            t = t + 1
            hcpvyr(t) = totrat(t)*365.25d0/hcpv
         END IF
      END DO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      t = 1
      j = 1
      hcpvyr(t) = totrat(t)*365.25d0/hcpv
C
      DO i = 1, outstp
         IF (j .EQ. 6) j = 0
C     PAUSE
         j = j + 1
         IF (prttim(i) .EQ. sumtim(t)) THEN
            t = t + 1
            hcpvyr(t) = totrat(t)*365.25d0/hcpv
         END IF
      END DO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                     C
C                  WRITING TO OUTPUT FILE                             C
C                                                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      WRITE (21, 10020)
10020 FORMAT (/, 15x, '************** RESERVOIR  DATA **************', 
     1 /)
      WRITE (21, 10030)
10030 FORMAT (13x, 'PRESSURE', 34x, 'PATTERN')
      WRITE (21, 10040)
10040 FORMAT (3x, 'TEMP', 5x, 'OPERATING', 3x, 'MMP', 8x, 'POROSITY', 
     1 2x, 'THICKNESS', 2x, 'AREA')
      WRITE (21, 10050)
10050 FORMAT (5x, 'F', 8x, 'PSIA', 5x, 'PSIA', 8x, 'FRACTION', 4x, 
     1 'FEET', 5x, 'ACRES')
      WRITE (21, 10060) tres, p, mmp, poros, thick, acres
10060 FORMAT (3x, f5.1, 5x, f6.1, 2x, f7.1, 8x, f6.4, 3x, f6.1, 2x, 
     1 f8.2, /)
      WRITE (21, 10070)
10070 FORMAT (35x, 'INITITAL HC')
      WRITE (21, 10080)
10080 FORMAT (4x, 'FLOOD START SATURATIONS', 8x, 'PORE VOLUME', 4x, 
     1 'DYKSTRA-')
      WRITE (21, 10090)
10090 FORMAT (5x, 'OIL', 6x, 'WATER', 6x, 'GAS', 7x, 'HCPV - OOIP', 4x, 
     1 'PARSONS', 3x, 'HORIZONTAL')
      WRITE (21, 10100)
10100 FORMAT (3x, 'SOINIT', 4x, 'SWINIT', 4x, 'SGINIT', 9x, 'MMRB', 8x, 
     1 'FACTOR', 6x, 'LAYERS')
      WRITE (21, 10110) soinit, swinit, sginit, hcpvmm, dpcoef, nlayrs
10110 FORMAT (2x, f6.4, 2(4x, f6.4), 8x, f7.4, 7x, f6.4, 6x, i3//)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      WRITE (21, 10120)
10120 FORMAT (14x, '*************** FLUID DATA ***************', /)
      WRITE (21, 10130)
10130 FORMAT (5x, 'STOCK', 3x, 'SOLUTION', 2x, 'SPECIFIC', 7x, 
     1 'FORMATION VOLUME FACTOR')
      WRITE (21, 10140)
10140 FORMAT (3x, 'TANK OIL', 5x, 'GOR'5x, 'GRAVITY', 5x, 'OIL', 7x, 
     1 'WATER', 5x, 'SOLVENT')
      WRITE (21, 10150)
10150 FORMAT (4x, 'GRAVITY', 5x, '(Rs)', 6x, 'SG', 9x, 'Bo', 8x, 'Bw', 
     1 8x, 'Bco2')
      WRITE (21, 10160)
10160 FORMAT (6x, 'API', 5x, 'ft3/STB', 2x, '(Air=1.0)', 3x, 'RB/STB', 
     1 4x, 'RB/STB', 4x, 'RB/MMSCF')
      WRITE (21, 10170) api, rs, gsg, bo, bwrb, bco2rb
10170 FORMAT (5x, f4.1, 5x, f6.1, 4x, f6.4, 5x, f6.4, 4x, f6.4, 3x, 
     1 f8.3, /)
C
C
      WRITE (21, 10180)
10180 FORMAT (3x, 'FLUID DENSITIES AT RES T&P', 6x, 
     1 'FLUID VISCOSITIES  AT RES T&P', 4x, 'WATER')
      WRITE (21, 10190)
10190 FORMAT (3x, 'RES OIL', 3x, ' WATER ', 3x, 'SOLVENT', 6x, 'OIL', 
     1 7x, 'WATER', 5x, 'SOLVENT', 3x, 'SALINITY')
      WRITE (21, 10200)
10200 FORMAT (3x, 'GMS/CC', 4x, 'GMS/CC', 4x, 'GMS/CC', 8x, 'cp', 8x, 
     1 'cp', 9x, 'cp', 9x, 'ppm')
      WRITE (21, 10210) doil, dwtr, dco2, viso, visw, visg, saln
10210 FORMAT (3x, 3(f6.4, 4x), f7.3, 5x, f5.3, 6x, f5.3, 5x, f7.0, //)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      WRITE (21, 10220)
10220 FORMAT (10x, 
     1 '********** RELATIVE PERMEABILITY PARAMETERS **********', /)
      WRITE (21, 10230)
10230 FORMAT (4x, 'WTR FLD', 2x, 'GAS FLD', 4x, 'MISC FLD')
      WRITE (21, 10240)
10240 FORMAT (5x, 'SORW', 5x, 'SORG', 8x, 'SORM')
      WRITE (21, 10250) sorw, sorg, sorm
10250 FORMAT (3x, f6.4, 3x, f6.4, 6x, f6.4, /)
C
C
      WRITE (21, 10260)
10260 FORMAT (5x, 'SGR', 6x, 'SSR', 19x, 'SWC', 7x, 'SWIR')
      WRITE (21, 10270) sgr, ssr, swc, swir
10270 FORMAT (3x, f6.4, 3x, f6.4, 17x, f6.4, 4x, f6.4, //)
C
C
      WRITE (21, 10280)
10280 FORMAT (4x, 'KROCW', 4x, 'KWRO', 8x, 'KRSMAX', 5x, 'KRGCW')
      WRITE (21, 10290) krocw, kwro, krsmax, krgcw
10290 FORMAT (3x, f6.4, 3x, f6.4, 6x, f6.4, 5x, f6.4, /)
C
C
      WRITE (21, 10300)
10300 FORMAT (4x, 'EXPOW', 4x, 'EXPW', 8x, 'EXPS', 7x, 'EXPG', 6x, 
     1 'EXPOG')
      WRITE (21, 10310) expow, expw, exps, expg, expog
10310 FORMAT (2x, f6.3, 3x, f6.3, 6x, f6.3, 5x, f6.3, 4x, f6.3, //)
C
C
      WRITE (21, 10320)
10320 FORMAT (3x, 'MIX PARAMETER')
      WRITE (21, 10330)
10330 FORMAT (5x, 'OMEGA', 8x, 'MISCIBLE RELATIVE PERMEABILITY')
      IF (krmsel .EQ. 1) THEN
         WRITE (21, 10340) w
10340    FORMAT (4x, f6.4, 8x, 'AVERAGE OF Krow AND Krg', //)
      ELSE IF (krmsel .EQ. 2) THEN
         WRITE (21, 10350) w
10350    FORMAT (4x, f6.4, 8x, 'EQUAL TO Krow', //)
      ELSE
         WRITE (21, 10360) w
10360    FORMAT (4x, f6.4, 8x, 'SATURATION WEIGHTED Krow AND Krs', //)
      END IF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      WRITE (21, 10370)
10370 FORMAT (/, 5x, 
     1 '*********** INJECTION/PRODUCTION PARAMETERS *********', /)
      WRITE (21, 10380)
10380 FORMAT (52x, 'OUTPUT')
      WRITE (21, 10390)
10390 FORMAT (3x, 'INJECTION', 2x, 'WAG EXPRESSED', 3x, 'PATTERN', 2x, 
     1 'INJECTION', 2x, 'TIME STEPS')
      WRITE (21, 10400)
10400 FORMAT (3x, 'SEQUENCES', 2x, 'AS TIME OR VOL', 3x, 'TYPE', 6x, 
     1 'WELLS', 6x, 'YEARS')
      WRITE (21, 10410) ntimes, wagtag, pattrn, noinjw, outtim
10410 FORMAT (7x, i1, 12x, a1, 11x, a2, 9x, i1, 8x, f5.3, /)
C
C
      WRITE (21, 10420)
10420 FORMAT (39x, 'AVGE (TOTAL WELLS)')
      WRITE (21, 10430)
10430 FORMAT (5x, 'CUM.', 4x, 'INCRE', 3x, 'CUM.', 3x, 'SEQUENCE', 4x, 
     1 'INJECTION FLUID')
      WRITE (21, 10440)
10440 FORMAT (4x, 'INJECT', 3x, 'TIME', 4x, 'TIME', 4x, 'RATE', 6x, 
     1 'FRACTIONAL CONTENT', 7x, 'WAG')
      WRITE (21, 10450)
10450 FORMAT (5x, 'HCPV', 4x, 'YEARS', 3x, 'YEARS', 3x, 'RB/D', 7x, 
     1 ' WATER', 4x, 'SOLVENT', 5x, 'WTR:GAS')
C
      DO t = 1, ntimes
         rate = totrat(t)/5.6145d0
         WRITE (21, 10460) pv(t), seqtim(t), sumtim(t), rate, fwinit(t, 
     1    1), fginit(t, 1), wag(t, 1)
10460    FORMAT (4x, f6.4, 2x, f6.3, 2x, f6.3, 2x, f7.2, 5x, f6.4, 4x, 
     1    f6.4, 5x, e9.4)
      END DO
C
C
      WRITE (21, 10470)
10470 FORMAT (//, 54x, 'TOTAL PATTERN')
      WRITE (21, 10480)
10480 FORMAT (4x, 'INCRE', 4x, 'CUM.', 5x, 'AVGE RATE FOR PATTERN', 
     1 11x, 'SURFACE RATES')
      WRITE (21, 10490)
10490 FORMAT (4x, 'TIME', 4x, 'TIME', 5x, '****** SEQUENTIAL ******', 
     1 8x, 'WATER', 3x, 'SOLVENT')
      WRITE (21, 10500)
10500 FORMAT (4x, 'YEARS', 3x, 'YEARS', 3x, 'RB/D', 7x, 'HCPV/D', 5x, 
     1 'HCPV/YR', 4x, 'STB/D', 3x, 'MMSCF/D')
C
      DO t = 1, ntimes
         rate = totrat(t)/5.6145d0
         dlyrat = totrat(t)/hcpv
         yrlyrt = dlyrat*365.25d0
         WRITE (21, 10510) seqtim(t), sumtim(t), rate, dlyrat, yrlyrt, 
     1    ratwtr(t), ratsol(t)
10510    FORMAT (3x, f6.3, 2x, f6.3, 2x, f7.2, 2x, e11.5, 2x, f7.5, 4x, 
     1    f7.1, 2x, f5.2)
      END DO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C            **** SUMMARY OF FLUID INJECTION ****
C
      WRITE (21, 10520)
10520 FORMAT (////, 3x, 
     1 '**** INJECTION **** INJECTION **** INJECTION **** INJECTION ****
     2 INJECTION'
     3 )
      WRITE (21, 10530)
10530 FORMAT (//, 13x, 'SUMMARY OF FLUID INJECTION')
      WRITE (21, 10540)
10540 FORMAT (19x, 'CUMULATIVE DATA', //)
      WRITE (21, 10550)
10550 FORMAT (3x, 'TIME', 4x, '***** HCPV INPUT *****', 4x, 'WATER', 
     1 4x, 'SOLVENT')
      WRITE (21, 10560)
10560 FORMAT (4x, 'YRS', 4x, 'TOTAL', 3x, 'WATER', 2x, 'SOLVENT', 4x, 
     1 'MSTB', 5x, 'MMSCF')
C
      wtrvol = 0.0
      solvol = 0.0
      propvt(0) = 0.0
      t = 1
      DO i = 1, outstp
         IF (propvt(i) .LE. pvprot(prostp)) THEN
            IF (sumtim(t) .LE. (prttim(i) - 0.003)) t = t + 1
            deltpv = (propvt(i) - propvt(i - 1))
            wtrvol = wtrvol + deltpv*fwinit(t, 1)
            volwtr(i) = wtrvol
            wtrstb = (wtrvol*cnvwtr)/1000d0
            solvol = solvol + deltpv*fginit(t, 1)
            volsol(i) = solvol
            solscf = solvol*cnvsol
            WRITE (21, 10570) prttim(i), propvt(i), wtrvol, solvol, 
     1       wtrstb, solscf
10570       FORMAT (2x, f6.3, 3(1x, f7.4), 2(2x, f8.1))
         END IF
      END DO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C            ***** SUMMARY OF FLUID PRODUCTION *****
C
      WRITE (21, 10580)
10580 FORMAT (//, 3x, 
     1 '*** PRODUCTION ***** PRODUCTION ***** PRODUCTION ***** PRODUCTIO
     2N ***'
     3 )
      WRITE (21, 10590)
10590 FORMAT (//, 13x, 'SUMMARY OF FLUID PRODUCTION')
      WRITE (21, 10600)
10600 FORMAT (19x, 'CUMULATIVE DATA', //)
      WRITE (21, 10610)
10610 FORMAT (11x, '********** HCPV OUTPUT **********', 5x, 'OIL', 9x, 
     1 'RECOVERY')
      WRITE (21, 10620)
10620 FORMAT (3x, 'TIME', 6x, 'HYDROCARBON PORE VOLUMES OUTPUT', 3x, 
     1 'RECOVERY', 3x, '% OF INJECTANT')
      WRITE (21, 10630)
10630 FORMAT (4x, 'YRS', 5x, 'TOTAL', 5x, 'OIL', 5x, 'WATER', 3x, 
     1 'SOLVENT', 3x, '%OOIP', 5x, 'WATER', 2x, 'SOLVENT')
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     **** CALCULATE THE CUMULATIVE PRODUCED HCPV OF OIL, SOLVENT,     C
C          WATER AND THE OIL RECOVERY EFFICIENCY (%OOIP) AT THE        C
C          DESIGNATED OUTPUT TIME-STEP (PRTTIM(I)) ****                C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      curtot = 0.0
      curoil = 0.0
      cursol = 0.0
      curwtr = 0.0
      pctwtr = 0.0
      i = 1
C
      DO j = 1, prostp
         lsttot = curtot
         curtot = pvprot(j)
         deltot = curtot - lsttot
         lstoil = curoil
         curoil = pvproo(j)
         deloil = curoil - lstoil
         lstsol = cursol
         cursol = pvpros(j)
         delsol = cursol - lstsol
         lstwtr = curwtr
         curwtr = pvprow(j)
         delwtr = curwtr - lstwtr
C
         DO WHILE (i .LE. outstp)
            IF (propvt(i) .GT. curtot) EXIT
            frctot = (propvt(i) - lsttot)/deltot
            propvo(i) = lstoil + frctot*deloil
            propvs(i) = lstsol + frctot*delsol
            propvw(i) = lstwtr + frctot*delwtr
            i = i + 1
         END DO
      END DO
C
C
      pctsol = 0.0
      DO i = 1, outstp
         IF (propvt(i) .LE. pvprot(prostp)) THEN
            IF (volsol(i) .NE. 0.0) pctsol = (propvs(i)/volsol(i))*100.
            IF (volwtr(i) .NE. 0.0) pctwtr = (propvw(i)/volwtr(i))*100.
            ooip = propvo(i)*100.
            WRITE (21, 10640) prttim(i), propvt(i), propvo(i), propvw
     1       (i), propvs(i), ooip, pctwtr, pctsol
*     WRITE (30, 990) PRTTIM(I), PROPVT(I), PROPVO(I), PROPVW(I),
*    * PROPVS(I), OOIP, PCTWTR, PCTSOL
10640       FORMAT (2x, f6.3, 4(2x, f7.4), 2x, f7.2, 4x, f6.2, 2x, 
     1       f6.2)
         END IF
      END DO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     ***** CALCULATE THE CUMULATIVE PRODUCED OIL, WATER, HC GAS,      C
C           SOLVENT, GOR AND WOR AT STANDARD SURFACE UNITS *****       C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      WRITE (21, 10650)
10650 FORMAT (///, 54x, 'CUMULATIVE')
      WRITE (21, 10660)
10660 FORMAT (10x, 'ER OIL', 5x, 'OIL', 4x, 'WATER', 3x, 'HC GAS', 3x, 
     1 'SOLVENT', 4x, 'GOR', 8x, 'WOR')
      WRITE (21, 10670)
10670 FORMAT (4x, 'YRS', 3x, '%OOIP', 5x, 'MSTB', 5x, 'MSTB', 3x, 
     1 'MMSCF', 5x, 'MMSCF', 2x, 'MSCF/STB', 4x, 'STB/STB')
C     
      DO i = 1, outstp
         IF (propvt(i) .LE. pvprot(prostp)) THEN
            ooip = propvo(i)*100d0
            oil = (propvo(i)*cnvoil)/1000d0
            sol = propvs(i)*cnvsol
            water = propvw(i)*cnvwtr/1000d0
            hcgas = oil*rs/1000d0
            IF (oil .GT. 0.0) THEN
               gor = (hcgas + sol)/oil
               wor = water/oil
               IF (gor .GT. 999999) gor = 999999
               IF (wor .GT. 999999) wor = 999999
            ELSE
               gor = 0.0
               wor = 999999
               IF (water .EQ. 0.0) wor = 0.0
               IF (sol .GT. 0.0) gor = 998888
            END IF
            WRITE (21, 10680) prttim(i), ooip, oil, water, hcgas, sol, 
     1       gor, wor
10680       FORMAT (2x, f6.3, 1x, f6.2, 2(2x, f7.1), 1x, f7.1, 3x, 
     1       f7.1, 1x, e9.3, 2x, e9.3)
         END IF
      END DO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     **** CALCULATE THE INCREMENTAL PRODUCED HCPV OF OIL, SOLVENT,    C
C           WATER AND THE OIL RECOVERY EFFICIENCY (%OOIP) AT THE       C
C           DESIGNATED OUTPUT TIME-STEP (PRTTIM(I)) ****               C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      WRITE (21, 10690)
10690 FORMAT (//, 5x, '***********************************************'
     1 )
      WRITE (21, 10700)
10700 FORMAT (/, 13x, 'SUMMARY OF FLUID PRODUCTION')
      WRITE (21, 10710)
10710 FORMAT (18x, 'INCREMENTAL DATA', //)
      WRITE (21, 10720)
10720 FORMAT (11x, '********** HCPV OUTPUT **********', 5x, 'OIL')
      WRITE (21, 10730)
10730 FORMAT (3x, 'TIME', 6x, 'HYDROCARBON PORE VOLUMES OUTPUT', 3x, 
     1 'RECOVERY')
      WRITE (21, 10740)
10740 FORMAT (4x, 'YRS', 5x, 'TOTAL', 5x, 'OIL', 5x, 'WATER', 3x, 
     1 'SOLVENT', 3x, '%OOIP')
C
      propvt(0) = 0.0
      propvo(0) = 0.0
      propvw(0) = 0.0
      propvs(0) = 0.0
C
      DO i = 1, outstp
         IF (propvt(i) .LE. pvprot(prostp)) THEN
            deltot = propvt(i) - propvt(i - 1)
            deloil = propvo(i) - propvo(i - 1)
            delwtr = propvw(i) - propvw(i - 1)
            delsol = propvs(i) - propvs(i - 1)
            ooip = deloil*100d0
            WRITE (21, 10750) prttim(i), deltot, deloil, delwtr, 
     1       delsol, ooip
10750       FORMAT (2x, f6.3, 4(2x, f7.4), 2x, f7.2)
         END IF
      END DO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     ***** CALCULATE THE INCREMENTAL PRODUCED OIL, WATER, HC GAS,     C
C           SOLVENT, GOR AND WOR AT STANDARD SURFACE UNITS *****       C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      WRITE (21, 10760)
10760 FORMAT (///, 53x, 'INCREMENTAL')
      WRITE (21, 10770)
10770 FORMAT (10x, 'ER OIL', 4x, 'OIL', 3x, 'WATER', 2x, 'HC GAS', 2x, 
     1 'SOLVENT', 4x, 'GOR', 8x, 'WOR')
      WRITE (21, 10780)
10780 FORMAT (4x, 'YRS', 3x, '%OOIP', 4x, 'MSTB', 4x, 'MSTB', 2x, 
     1 'MMSCF', 4x, 'MMSCF', 2x, 'MSCF/STB', 4x, 'STB/STB')
C
      propvt(0) = 0.0
      propvo(0) = 0.0
      propvw(0) = 0.0
      propvs(0) = 0.0
C
      DO i = 1, outstp
         IF (propvt(i) .LE. pvprot(prostp)) THEN
            delo = ((propvo(i) - propvo(i - 1))*cnvoil)/1000d0
            delw = ((propvw(i) - propvw(i - 1))*cnvwtr)/1000d0
            dels = (propvs(i) - propvs(i - 1))*cnvsol
            dooip = (propvo(i) - propvo(i - 1))*100d0
            dhcgas = (delo*rs)/1000d0
            IF (delo .GT. 0.0) THEN
               dgor = (dhcgas + dels)/delo
               dwor = delw/delo
               IF (dgor .GT. 999999d0) dgor = 999999d0
               IF (dwor .GT. 999999d0) dwor = 999999d0
            ELSE
               dgor = 0.0
               dwor = 998888d0
               IF (delw .EQ. 0.0) dwor = 0.0
               IF (dels .GT. 0.0) dgor = 999999d0
            END IF
            WRITE (21, 10790) prttim(i), dooip, delo, delw, dhcgas, 
     1       dels, dgor, dwor
10790       FORMAT (2x, f6.3, 1x, f6.2, 2(2x, f6.1), 1x, f6.1, 3x, 
     1       f6.1, 1x, e9.3, 2x, e9.3)
         END IF
      END DO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS OUTPUT FILE -'DIGITOUT'- CONTAINS ALL THE OUTPUT PARAMETERS C
C          AS A FUNCTION OF THE PRINT OR OUTPUT TIME                   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      propvt(0) = 0.0
      propvo(0) = 0.0
      propvw(0) = 0.0
      propvs(0) = 0.0
C
      WRITE (30, 10800)
10800 FORMAT (5x, 'TIME', 1x, 'RECOVERY', 5x, 'CUM', 2x, 'PRODUCED', 
     1 2x, 'FLUIDS', 4x, 'HCPV', 7x, 'CUM INJECTED', 8x, 'OIL', 5x, 
     2 'WATER', 4x, 'HC_GAS', 3x, 'SOLVENT', 2x, 'CUM_GOR', 2x, 
     3 'CUM_WOR', 3x, 'INC_ER', 6x, 'INC', 1x, 'PRODUCED', 1x, 'FLUIDS'
     4 , 5x, 'HCPV', 8x, 'OIL', 4x, 'WATER', 3x, 'HC_GAS', 2x, 
     5 'SOLVENT', 2x, 'INC_GOR', 2x, 'INC_WOR')
C
      WRITE (30, 10810)
10810 FORMAT (5x, 'YRS', 5x, '%OOIP', 6x, 'OIL', 4x, 'WATER', 2x, 
     1 'SOLVENT', 4x, 'TOTAL', 2x, 'WATER_M', 2x, 'SOLVENT_MM', 3x, 
     2 'MSTB'6x, 'MSTB', 5x, 'MMSCF', 5x, 'MMSCF', 1x, 'MSCF/STB', 2x, 
     3 'STB/STB', 4x, '%OOIP', 6x, 'OIL', 4x, 'WATER', 2x, 'SOLVENT', 
     4 4x, 'TOTAL', 5x, 'MSTB', 5x, 'MSTB', 4x, 'MMSCF', 4x, 'MMSCF', 
     5 1x, 'MSCF/STB', 2x, 'STB/STB')
C
      DO i = 1, outstp
         IF (propvt(i) .LE. pvprot(prostp)) THEN
            IF (volsol(i) .EQ. 0.0) THEN
               solscf = 0.0d0
            ELSE
               solscf = volsol(i)*cnvsol
            END IF
            IF (volwtr(i) .EQ. 0.0) THEN
               wtrstb = 0.0
            ELSE
               wtrstb = volwtr(i)*cnvwtr/1000
            END IF
            ooip = propvo(i)*100d0
*
            oil = (propvo(i)*cnvoil)/1000d0
            sol = propvs(i)*cnvsol
            water = propvw(i)*cnvwtr/1000d0
            hcgas = oil*rs/1000d0
            IF (oil .GT. 0.0) THEN
               gor = (hcgas + sol)/oil
               wor = water/oil
               IF (gor .GT. 999999d0) gor = 999999d0
               IF (wor .GT. 999999d0) wor = 999999d0
            ELSE
               gor = 0.0
               wor = 999999d0
               IF (water .EQ. 0.0) wor = 0.0
               IF (sol .GT. 0.0) gor = 998888d0
            END IF
C
            deltot = propvt(i) - propvt(i - 1)
            deloil = propvo(i) - propvo(i - 1)
            delwtr = propvw(i) - propvw(i - 1)
            delsol = propvs(i) - propvs(i - 1)
            dooip = deloil*100d0
C
            delo = deloil*cnvoil/1000d0
            delw = delwtr*cnvwtr/1000d0
            dels = delsol*cnvsol
            dhcgas = (delo*rs)/1000d0
            IF (delo .GT. 0.0) THEN
               dgor = (dhcgas + dels)/delo
               dwor = delw/delo
               IF (dgor .GT. 999999d0) dgor = 999999d0
               IF (dwor .GT. 999999d0) dwor = 999999d0
            ELSE
               dgor = 0.0
               dwor = 998888d0
               IF (delw .EQ. 0.0) dwor = 0.0
               IF (dels .GT. 0.0) dgor = 999999d0
            END IF
C
            WRITE (30, 10820) prttim(i), ooip, propvo(i), propvw(i), 
     1       propvs(i), propvt(i), wtrstb, solscf, oil, water, hcgas, 
     2       sol, gor, wor, dooip, deloil, delwtr, delsol, deltot, 
     3       delo, delw, dhcgas, dels, dgor, dwor
10820       FORMAT (2x, f7.3, 2x, f7.2, 4(2x, f7.4), 2x, f7.1, 2x, 
     1       f7.1, 4(3x, f7.1), 2(1x, e8.3), 2x, f7.2, 4(2x, f7.4), 4
     2       (3x, f6.1), 2(1x, e8.3))
         END IF
      END DO
C
      CLOSE (110)
      CLOSE (21)
      CLOSE (30)
      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     END OF SUBROUTINE CNVOUT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
