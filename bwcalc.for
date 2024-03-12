C STRUCTURED BY FOR_STRUCT, V2.0.1, ON 04/21/94 AT 16:17:44
C  OPTIONS SET: DUP=R FMT=ACHN I=50,3 LABF=10000,10 LABX=50,50 OFF=AI
C               S=9 SP=DE V
C
*************************************************************************
      SUBROUTINE bwcalc(tin, pin, bwrb)
*************************************************************************
C
C     PURPOSE --- THIS SUBROUTINE CALCULATES THE WATER FORMATION VOLUME
C                 FACTOR, BWRB, IN RB/STB SUING THE PROCEDURE DESCRIBED
C                 BY MCCAIN (SECOND EDITION) USING DATA FROM DOTSON AND
C                 STANDING AND THE INTERNATIONAL CRITICAL TABLES.
C
C                 VERSION 1.0
C
C
C
      DOUBLE PRECISION tin, pin, bwrb, delvwt, delvwp
C
      delvwt = ( - 0.01d0) + 1.33391d-4*tin + 5.50654d-7*tin**2
      delvwp =  - 1.95301d-9*pin*tin - ((1.72834d-13)*pin**2)*tin - 
     1 (3.58922d-7)*pin - (2.25341d-10)*pin**2
      bwrb = (1.0d0 + delvwt)*(1.0d0 + delvwp)
C
      RETURN
      END
