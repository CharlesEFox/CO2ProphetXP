*
************************************************************************
*     START OF STREAM COMMON.
************************************************************************
*
*     NAMED COMMON TO HOLD AND PASS INFORMATION PERTAINING TO THE OIL
*     AND/OR GAS FIELD UNDER STUDY.
*
*     WRITTEN BY              JAMES W. MUELLER           OCTOBER 9, 1991
*                             SOLUTIONS SYSTEMS          HOUSTON, TEXAS
*
************************************************************************
*
*     STREAM VARIABLES:
*
*     NTUBES - INTEGER      = ARRAY OF INDICATORS STATING WHETHER A
*                             STREAMTUBE EXISTS OR DOES NOT EXIT BETWEEN
*                             ADJACENT STREAMLINES
*     SEGLDW - REAL         = ARRAY OF THE MEAN LENGHTS OF SEGMENTS
*                             DIVIDED BY THEIR CORRESPONDING MEAN WIDTHS
*                             ALONG THE STREAMTUBES
*     SEGDIS - REAL         = ARRAY OF THE DISTANCES OF SEGMENTS ALONG
*                             THE STREAMLINES
*     SEGLEN - REAL         = ARRAY OF THE MIDPOINT LENGTHS OF THE AREAS
*                             OF SEGMENTS ALONG STREAMTUBES
*     SEGWID - REAL         = ARRAY OF THE MIDPOINT WIDTHS OF THE AREAS
*                             OF SEGMENTS ALONG STREAMTUBES
*     SEGXPT - REAL         = ARRAY OF THE X POINTS OF SEGMENTS ALONG
*                             THE STREAMLINES
*     SEGYPT - REAL         = ARRAY OF THE Y POINTS OF SEGMENTS ALONG
*                             THE STREAMLINES
*     STLONG - REAL         = ARRAY OF LENGTHS FOR THE STREAMLINES TO GO
*                             FROM THE INJECTION WELLS TO THE PRODUCTION
*                             WELLS
*     STPATH - INTEGER      = ARRAY OF THE POINTERS INDICATING WHICH OF
*                             THE INJECTION WELLS AND THE PRODUCTION
*                             WELLS ARE JOINED BY EACH OF THE
*                             STREAMLINES
*     STTIME - REAL         = ARRAY OF THE TIMES FOR STREAMLINES TO GO
*                             FROM THE INJECTION WELLS TO THE PRODUCTION
*                             WELLS
*     TUBARA - REAL         = ARRAY OF STREAMTUBE AREAS
*     LWGEN  - CHARACTER    = 'Y' (YES) OR 'N' (NO) TO SELECT WHETHER
*                             STREAMTUBES ARE GENERATED
*
************************************************************************
*
*     STREAM VARIABLE DEFINITIONS
*
      INTEGER NTUBES(MAXINJ+MAXPRO,MAXLIN)
      INTEGER STPATH(MAXINJ+MAXPRO,MAXLIN)
*
      DOUBLE PRECISION SEGLDW(MAXLIN,MAXSEG)
      DOUBLE PRECISION SEGLEN(MAXLIN,MAXSEG)
      DOUBLE PRECISION SEGWID(MAXLIN,MAXSEG)
      DOUBLE PRECISION SEGXPT(MAXLIN,MAXSEG+1)
      DOUBLE PRECISION SEGYPT(MAXLIN,MAXSEG+1)
      DOUBLE PRECISION STLONG(MAXINJ+MAXPRO,MAXLIN)
      DOUBLE PRECISION STTIME(MAXINJ+MAXPRO,MAXLIN)
      DOUBLE PRECISION TUBARA(MAXINJ+MAXPRO,MAXLIN)
      CHARACTER *1 LWGEN
*
************************************************************************
*
*     STREAM COMMON DEFINITION
*
      COMMON / STREAM / NTUBES, STPATH, SEGLDW, SEGLEN, SEGWID, SEGXPT,
     *                  SEGYPT, STLONG, STTIME, TUBARA, LWGEN
*
************************************************************************
*     END OF STREAM COMMON.
************************************************************************
*
