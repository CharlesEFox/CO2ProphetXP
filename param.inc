*
************************************************************************
*     START OF PARAMETER DEFINITIONS.
************************************************************************
*
*     NAMED PARAMETERS ARE DEFINED TO LIMIT THE MAXIMUM NUMBER OF
*     ELEMENTS THE DIFFERENT COMMON VARIABLES ARRAYS CAN HOLD.
*
*     WRITTEN BY              JAMES W. MUELLER           OCTOBER 9, 1991
*                             SOLUTIONS SYSTEMS          HOUSTON, TEXAS
*
************************************************************************
*
*     PARAMETER VARIABLES:
*
*     MAXBND - INTEGER      = MAXIMUM NUMBER OF VERTICIES ALLOWED IN THE
*                             FIELD BOUNDARY
*     MAXIMG - INTEGER      = MAXIMUM NUMBER OF IMAGE WELLS ALLOWED
*                             AROUND THE FIELD
*     MAXINJ - INTEGER      = MAXIMUM NUMBER OF INJECTION WELLS ALLOWED
*                             IN THE FIELD
*     MAXLAY - INTEGER      = MAXIMUM NUMBER OF LAYERS ALLOWED IN
*                             THE PRODUCTION ZONE TO BE EVALUATED
*     MAXLIN - INTEGER      = MAXIMUM NUMBER OF STREAMLINES ALLOWED IN
*                             THE FIELD
*     MAXPRO - INTEGER      = MAXIMUM NUMBER OF PRODUCTION WELLS ALLOWED
*                             IN THE FIELD
*     MAXSEG - INTEGER      = MAXIMUM NUMBER OF SEGMENTS ALLOWED IN A
*                             STREAMLINE
*     MAXSTP - INTEGER      = MAXIMUM NUMBER OF STEPS ALLOWED FOR A
*                             STREAMLINE TO REACH A PRODUCTION WELL
*     MAXTIM - INTEGER      = MAXIMUM NUMBER OF DIFFERENT SEQUENTIAL
*                             TIMED FLOOD PROGRAMS ALLOWED IN THE STUDY
*     MAXTST - INTEGER      = MAXIMUM NUMBER OF TEST WELLS ALLOWED ALONG
*                             THE FIELD BOUNDARY
*     MAXWEL - INTEGER      = MAXIMUM NUMBER OF INJECTION AND PRODUCTION
*                             WELLS ALLOWED IN A FIELD
*                             NOTE: MAXWEL = MAXIMG + MAXINJ + MAXPRO
*     MAXOUT - INTEGER      = MAXIMUM NUMBER OF OUTPUT INCREMENTS
************************************************************************
*
*     PARAMETER DEFINITIONS
*
      INTEGER MAXBND
      INTEGER MAXIMG
      INTEGER MAXINJ
      INTEGER MAXLAY
      INTEGER MAXLIN
      INTEGER MAXPRO
      INTEGER MAXSEG
      INTEGER MAXSTP
      INTEGER MAXTIM
      INTEGER MAXTST
      INTEGER MAXWEL
      INTEGER MAXOUT
*
*     PARAMETER SETTINGS
*
      PARAMETER ( MAXBND = 20 )
      PARAMETER ( MAXIMG = 100 )
      PARAMETER ( MAXINJ = 10 )
      PARAMETER ( MAXLAY = 10 )
      PARAMETER ( MAXLIN = 36 )
      PARAMETER ( MAXPRO = 10 )
      PARAMETER ( MAXSEG = 50 )
      PARAMETER ( MAXSTP = 600 )
      PARAMETER ( MAXTIM = 4)
      PARAMETER ( MAXTST = 200 )
      PARAMETER ( MAXWEL = 150 )
      PARAMETER ( MAXOUT = 1000)
*
************************************************************************
*     END OF PARAMETER DEFINITIONS.
************************************************************************
*
