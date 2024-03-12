*
************************************************************************
*     START OF ROCKAT COMMON.
************************************************************************
*
*     NAMED COMMON TO HOLD AND PASS INFORMATION PERTAINING TO THE ROCK
*     ATTRIBUTES AND FLUID PROPERTIES OF THE OIL FIELD UNDER STUDY.
*
*     WRITTEN BY              JAMES W. MUELLER           OCTOBER 9, 1991
*                             SOLUTIONS SYSTEMS          HOUSTON, TEXAS
*
************************************************************************
*
*     ROCKAT VARIABLES:
*
*     ALPHA  - REAL         = WEIGHT FACTOR INDICATING DEGREE OF
*                             MISCIBILITY AND VARYING BETWEEN 0.0 AND
*                             1.0
*     API    - REAL         = API GRAVITY OF OIL, DEG API
*     BO     - REAL         = OIL FORMATION VOLUME FACTOR, RB/STB
*     DPCOEF - REAL         = DYKSTRA-PARSONS COEFFICIENT FOR THE
*                             PRODUCTION ZONE
*     DPCOF2 - REAL         = DYKSTRA-PARSONS COEFFICIENT FOR THE
*                             PRODUCTION ZONE AFTER FOAM TREATMENT
*     EXPG   - REAL         = EXPONENT FOR GAS CURVE FROM GAS-OIL
*                             RELATIVE PERMEABILITIES
*     EXPOG  - REAL         = EXPONENT FOR OIL CURVE FROM GAS-OIL
*                             RELATIVE PERMEABILITIES
*     EXPS   - REAL         = EXPONENT FOR SOLVENT FROM RELATIVE
*                             PERMEABILITY CURVE
*     EXPW   - REAL         = EXPONENT FOR WATER CURVE FROM WATER-OIL
*                             RELATIVE PERMEABILITIES
*     EXPO   - REAL         = EXPONENT FOR OIL CURVE FROM WATER-OIL
*                             REL PERM.  ACTUAL VARIABLE USED IN
*                             EQUATION.  IS EQUAL TO EXPOW OR EXPOWH
*     EXPOW  - REAL         = EXPONENT FOR OIL CURVE FROM WATER-OIL
*                             RELATIVE PERMEABILITIES
*     EXPOWH - REAL         = HYSTERESIS EXPONENT FOR OIL CURVE FROM
*                             WATER-OIL RELATIVE PERMEABILITIES
*     GASVIS - REAL         = ALTERNATE GAS VISCOSITY: USED FOR GAS
*                             OTHER THAN CO2 OR GAS WITH FOAM
*     GSG    - REAL         = GAS SPECIFIC GRAVITY (WRT AIR)
*     HEIGHT - REAL         = HEIGHT OF STREAMLINE SLICE IN THE FIELD
*     KRG    - REAL         = RELATIVE PERMEABILITY OF GAS IN TWO PHASE
*                             GAS-OIL FLOW
*     KRGCW  - REAL         = RELATIVE PERMEABILITY OF GAS AT CONNATE
*                             WATER SATURATION
*     KRGEFF - REAL         = EFFECTIVE RELATIVE PERMEABILITY OF GAS
*                             SOLVENT
*     KRM    - REAL         = RELATIVE PERMEABILITY OF MISCIBLE OLEIC
*                             PHASE
*     KRMSEL - INTEGER      = OPTIONS FOR MISCIBLE OLEIC PHASE
*                             PERMEABILITY:
*                               0 = USE LINEAR VARIATION BETWEEN SOLVENT
*                                   AND OIL RELATIVE PERMEABILITY,
*                               1 = USE AVERAGE OF OIL AND GAS RELATIVE
*                                   PERMEABILITY, AND
*                               2 = USE ONLY OIL RELATIVE PERMEABILITY.
*     KRO    - REAL         = RELATIVE PERMEABILITY OF OIL IN THREE
*                             PHASE FLOW USING MODIFIED STONE METHOD
*     KROEFF - REAL         = EFFECTIVE RELATIVE PERMEABILITY OF OIL
*     KROCW  - REAL         = RELATIVE PERMEABILITY OF OIL AT CONNATE
*                             WATER SATURATION
*     KROCWH - REAL         = HYSTERESIS RELATIVE PERMEABILITY OF
*                             OIL AT CONNATE WATER SATURATION.  THIS
*                             IS A CALCULATED VALUE AND NOT AN INPUT
*     KROSWR - REAL         = OIL RELATIVE PERMEABILITY AT SWIR
*                             AFTER HYSTERESIS
*     KROMAX - REAL         = ENDPOINT REL PERM FOR OIL IN WATER-OIL
*                             CURVE.  EQUALS KROCW OR KROCWH
*     KROG   - REAL         = RELATIVE PERMEABILITY OF OIL IN TWO PHASE
*                             GAS-OIL FLOW
*     KRS    - REAL         = RELATIVE PERMEABILITY OF SOLVENT
*     KRSMAX - REAL         = ENDPOINT RELATIVE PERMEABILITY OF SOLVENT
*                             AT A SOLVENT SATURATION OF 1 - SWIR - SORM
*     KROW   - REAL         = RELATIVE PERMEABILITY OF OIL IN TWO PHASE
*                             OIL-WATER FLOW
*     KRW    - REAL         = RELATIVE PERMEABILITY OF WATER IN TWO
*                             PHASE OIL-WATER FLOW
*     KWRO   - REAL         = RELATIVE PERMEABILITY OF WATER AT RESIDUAL
*                             OIL - NO GAS SATURATION
*     MMP    - REAL         = OIL MINIMUM (MULTI-CONTACT) MISCIBILITY
*                             PRESSURE, PSIA
*     ONEMAL - REAL         = ONE MINUS ALPHA - THE DEGREE OF
*                             MISCIBILITY WEIGHT FACTOR
*     ONEMW  - REAL         = ONE MINUS W - THE DEGREE OF MIXING
*                             PARAMETER
*     P      - REAL         = RESERVIOR PRESSURE
*     PERMAV - REAL         = AVERAGE PERMEABILITY OF THE PRODUCTION
*                             ZONE
*     PERMAX - REAL         = MAXIMUM PERMEABILITY IN ANY GIVEN LAYER
*     PHIGH  - REAL         = PRESSURE ABOVE WHICH COMPLETE MISCIBILITY
*                             EXISTS
*     PLOW   - REAL         = PRESSURE BELOW WHICH COMPLETE
*                             IMMISCIBILTY EXISTS
*     PORSTY - REAL         = POROSITY USED JUST FOR CALCULATION
*     POROS  - REAL         = ACTUAL POROSITY OF FIELD
*     PRMABS - REAL         = ARRAY OF ABSOLUTE PERMEABILITIES FOR EACH
*                             LAYER
CJWM*     SEGFG  - REAL         = ARRAY OF FRACTIONAL FLOW VALUES OF GAS
CJWM*                             SOLVENT IN EACH SEGMENT
*     RS     - REAL         = SOLUTION GAS-OIL RATIO, SCF/STB
*     SALN   - REAL         = WATER SALINITY, PPM
*     SEGFO  - REAL         = ARRAY OF FRACTIONAL FLOW VALUES OF OIL IN
*                             EACH SEGMENT
*     SEGFW  - REAL         = ARRAY OF FRACTIONAL FLOW VALUES OF WATER
*                             IN EACH SEGMENT
CJWM*     SEGSG  - REAL         = ARRAY OF GAS SOLVENT SATURATION IN EACH
CJWM*                             SEGMENT
*     SEGSO  - REAL         = ARRAY OF OIL SATURATION IN EACH SEGMENT
*     SEGSW  - REAL         = ARRAY OF WATER SATURATION IN EACH SEGMENT
*     SGINIT - REAL         = INITIAL GAS SOLVENT SATURATION VALUE IN
*                             ALL SEGMENTS
*     SGR    - REAL         = RESIDUAL GAS SATURATION
*     SOINIT - REAL         = INITIAL OIL SATURATION VALUE IN ALL
*                             SEGMENTS
*     SORG   - REAL         = RESIDUAL OIL SATURATION TO GAS
*     SORM   - REAL         = RESIDUAL OIL SATURATION TO MISCIBLE GAS
*                             SOLVENT FLOOD
*     SORMIS - REAL         = RESIDUAL OIL SATURATION TO MISCIBLE GAS
*                             SOLVENT FLOOD
*     SORW   - REAL         = RESIDUAL OIL SATURATION TO WATER
*     SSR    - REAL         = RESIDUAL SOLVENT SATURATION
*     SUMPAB - REAL         = SUM OF THE INDIVIDUAL LAYER
*                             PERMEABILITIES
*     SWINIT - REAL         = INITIAL WATER SATURATION VALUE IN ALL
*                             SEGMENTS
*     SWC    - REAL         = CONNATE WATER SATURATION
*     SWIR   - REAL         = IRREDUCIBLE WATER SATURATION AS WATER
*     SWR    - REAL         = EQUAL TO SWC OR SWIR.  VALUE USED IN
*                             EQUATION
*                             WATER SATURATION IS DECREASING
CJMW*     TMOBIL - REAL         = ARRAY OF TOTAL MOBILITY VALUES - EQUAL TO
CJWM*                             THE SUMS OF ALL OF THE INDIVIDUAL
CJWM*                             PERMEABILITIES DIVIDED THE INDIVIDUAL
CJWM*                             VISCOSITIES
*     THICK  - REAL         = NET PAY OF RESERVOIR
*     VISG   - REAL         = VISCOSITY OF GAS SOLVENT
*     VISGE  - REAL         = EFFECTIVE VISCOSITY OF MISCIBLE MIXED GAS
*                             SOLVENT
*     VISGM  - REAL         = VISCOSITY OF MOBILE GAS SOLVENT
*     VISM   - REAL         = VISCOSITY OF OIL-SOLVENT MIXTURE
*     VISO   - REAL         = VISCOSITY OF OIL
*     VISOE  - REAL         = EFFECTIVE VISCOSITY OF MISCIBLE MIXED OIL
*     VISOM  - REAL         = VISCOSITY OF MISCIBLE MIXED OIL
*     VISW   - REAL         = VISCOSITY OF WATER
*     W      - REAL         = DEGREEE OF MIXING PARAMETER - RANGES FROM
*                             0.0 TO 1.0
*     XKVH   - REAL         = RATIO OF VERTICAL TO HORIZONTAL PERM
*     VOLADJ - REAL         = FACTOR TO CONTROL THE MAXIMUM SIZE OF THE
*                             TIMESTEP: RANGES FROM GREATER THAN 0.0 TO
*                             1.0
*
************************************************************************
*
*     ROCKAT VARIABLE DEFINITIONS
*
      INTEGER KRMSEL
*
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION API
      DOUBLE PRECISION BO
      DOUBLE PRECISION DPCOEF
      DOUBLE PRECISION DPCOF2
      DOUBLE PRECISION EXPG
      DOUBLE PRECISION EXPOG
      DOUBLE PRECISION EXPS
      DOUBLE PRECISION EXPW
      DOUBLE PRECISION EXPOW
      DOUBLE PRECISION EXPOWH
      DOUBLE PRECISION EXPO
      DOUBLE PRECISION GASVIS
      DOUBLE PRECISION GSG
      DOUBLE PRECISION HEIGHT
      DOUBLE PRECISION KRG
      DOUBLE PRECISION KRGCW
      DOUBLE PRECISION KRGEFF
      DOUBLE PRECISION KRM
      DOUBLE PRECISION KRO
      DOUBLE PRECISION KROEFF
      DOUBLE PRECISION KROCW
      DOUBLE PRECISION KROCWH
      DOUBLE PRECISION KROSWR
      DOUBLE PRECISION KROMAX
      DOUBLE PRECISION KROG
      DOUBLE PRECISION KRS
      DOUBLE PRECISION KRSMAX
      DOUBLE PRECISION KROW
      DOUBLE PRECISION KRW
      DOUBLE PRECISION KWRO
      DOUBLE PRECISION MMP
      DOUBLE PRECISION ONEMAL
      DOUBLE PRECISION ONEMW
      DOUBLE PRECISION P
      DOUBLE PRECISION PERMAV
      DOUBLE PRECISION PERMAX
      DOUBLE PRECISION PHIGH
      DOUBLE PRECISION PLOW
      DOUBLE PRECISION POROS
      DOUBLE PRECISION PORSTY
      DOUBLE PRECISION PRMABS(MAXLAY)
      DOUBLE PRECISION RS
      DOUBLE PRECISION SALN
CJWM      DOUBLE PRECISION SEGFG(MAXLAY,MAXLIN,MAXSEG)
CJP      DOUBLE PRECISION SEGFO(MAXLAY,MAXLIN,MAXSEG)
CJP      DOUBLE PRECISION SEGFW(MAXLAY,MAXLIN,MAXSEG)
CJWM      DOUBLE PRECISION SEGSG(MAXLAY,MAXLIN,MAXSEG)
      DOUBLE PRECISION SEGSO(MAXLAY,MAXLIN,MAXSEG)
      DOUBLE PRECISION SEGSW(MAXLAY,MAXLIN,MAXSEG)
      DOUBLE PRECISION SGINIT
      DOUBLE PRECISION SGR
      DOUBLE PRECISION SOINIT
      DOUBLE PRECISION SORG
      DOUBLE PRECISION SORM
      DOUBLE PRECISION SORMIS
      DOUBLE PRECISION SORW
      DOUBLE PRECISION SSR
      DOUBLE PRECISION SUMPAB
      DOUBLE PRECISION SWC
      DOUBLE PRECISION SWINIT
      DOUBLE PRECISION SWIR
      DOUBLE PRECISION SWR
CJWM      DOUBLE PRECISION TMOBIL(MAXLAY,MAXLIN,MAXSEG)
      DOUBLE PRECISION THICK
      DOUBLE PRECISION VISG
      DOUBLE PRECISION VISGE
      DOUBLE PRECISION VISGM
      DOUBLE PRECISION VISM
      DOUBLE PRECISION VISO
      DOUBLE PRECISION VISOE
      DOUBLE PRECISION VISOM
      DOUBLE PRECISION VISW
      DOUBLE PRECISION XKVH
      DOUBLE PRECISION W
      DOUBLE PRECISION VOLADJ
*
************************************************************************
*
*     ROCKAT COMMON DEFINITION
*
      COMMON / ROCKAT / KRMSEL,  ALPHA, DPCOEF, DPCOF2,  EXPOG,   EXPS,
     *                    EXPW,  EXPOW, EXPOWH,   EXPO,  EXPG,
     *                  GASVIS, HEIGHT,    KRG,  KRGCW, KRGEFF,
     *                     KRM,    KRO, KROEFF,  KROCW,  KROCWH,  KROG,
     *                  KROMAX, KROSWR,    KRS,
     *                  KRSMAX,   KROW,    KRW,   KWRO, ONEMAL,  ONEMW,
     *                       P, PERMAV, PERMAX,  PHIGH,   PLOW, PORSTY,
     *                   POROS, PRMABS,  SEGSO,  SEGSW, SGINIT,    SWR,
     *                     SGR, SOINIT,   SORG,   SORM,   SORW,    SSR,
     *                  SUMPAB,    SWC, SWINIT,   SWIR,   VISG,  VISGE,
     *                   VISGM,   VISM,   VISO,  VISOE,  VISOM,   VISW,
     *                    XKVH,      W, VOLADJ, SORMIS,  THICK,
     *                     API,     BO,    GSG,    MMP,     RS,   SALN
CJWM     *                  PRMABS,  SEGFG,  SEGFO,  SEGFW,  SEGSG,  SEGSO,
CJWM     *                   SEGSW, SGINIT,    SGR, SOINIT,   SORG,   SORM,
CJMW     *                    SORW,    SSR, SUMPAB,    SWC, SWINIT,   SWIR,
CJWM     *                  TMOBIL,   VISG,  VISGE,  VISGM,   VISM,   VISO,
CJWM     *                   VISOE,  VISOM,   VISW,      W
*
************************************************************************
*     END OF ROCKAT COMMON.
************************************************************************
*
