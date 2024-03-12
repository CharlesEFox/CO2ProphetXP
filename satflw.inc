*
************************************************************************
*     START OF SATFLW COMMON.
************************************************************************
*
*     NAMED COMMON TO HOLD AND PASS INFORMATION PERTAINING TO THE OIL
*     AND WATER SATURATIONS, THE FRACTIONAL FLOWS, AND THE TOTAL
*     MOBILITIES OF THE GAS FIELD UNDER STUDY.
*
*     WRITTEN BY             JAMES W. MUELLER          NOVEMBER 20, 1991
*                            SOLUTIONS SYSTEMS         HOUSTON, TEXAS
*
************************************************************************
*
*     SATFLW VARIABLES:
*
*     FOTABL - REAL         = ARRAY OR TABLE OF THE FRACTIONAL OIL FLOW
*                             VALUES FOR ALL VALUES OF OIL AND WATER
*                             SATURATIONS
*     FWTABL - REAL         = ARRAY OR TABLE OF THE FRACTIONAL WATER
*                             FLOW VALUES FOR ALL VALUES OF OIL AND
*                             WATER SATURATIONS
*     SOMAX  - REAL         = MAXIMUM OIL SATURATION ALLOWED AFTER OIL
*                             ENTERS MAX/MIN RANGE
*     SOMIN  - REAL         = MINIMUM OIL SATURATION ALLOWED AFTER OIL
*                             ENTERS MAX/MIN RANGE
*     SWMAX  - REAL         = MAXIMUM WATER SATURATION ALLOWED AFTER
*                             WATER ENTERS MAX/MIN RANGE
*     SWMIN  - REAL         = MIMIMUM WATER SATURATION ALLOWED AFTER
*                             WATER ENTERS MAX/MIN RANGE
*     TMTABL - REAL         = ARRAY OR TABLE OF THE TOTAL MOBILITY
*                             VALUES FOR ALL VALUES OF OIL AND WATER
*                             SATURATIONS
*
************************************************************************
*
*     SATFLW VARIABLE DEFINITIONS
*
      DOUBLE PRECISION FOTABL(0:100,0:100)
      DOUBLE PRECISION FWTABL(0:100,0:100)
      DOUBLE PRECISION SOMAX
      DOUBLE PRECISION SOMIN
      DOUBLE PRECISION SWMAX
      DOUBLE PRECISION SWMIN
      DOUBLE PRECISION TMTABL(0:100,0:100)
*
************************************************************************
*
*     SATFLW COMMON DEFINITION
*
      COMMON / SATFLW / FOTABL,FWTABL,SOMAX,SOMIN,SWMAX,SWMIN,TMTABL
*
************************************************************************
*     END OF SATFLW COMMON.
************************************************************************
*
