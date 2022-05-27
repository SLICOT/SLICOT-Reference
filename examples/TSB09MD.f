*     SB09MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, NCMAX, NBMAX
      PARAMETER        ( NMAX = 20, NCMAX = 20, NBMAX = 20 )
      INTEGER          LDH1, LDH2, LDSS, LDSE, LDPRE
      PARAMETER        ( LDH1 = NCMAX, LDH2 = NCMAX, LDSS = NCMAX,
     $                   LDSE = NCMAX, LDPRE = NCMAX )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO, J, N, NB, NC
*     .. Local Arrays ..
      DOUBLE PRECISION H1(LDH1,NMAX*NBMAX), H2(LDH2,NMAX*NBMAX),
     $                 PRE(LDPRE,NBMAX), SE(LDSE,NBMAX), SS(LDSS,NBMAX)
*     .. External Subroutines ..
      EXTERNAL         SB09MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, NC, NB, TOL
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE IF ( NB.LT.0 .OR. NB.GT.NBMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) NB
      ELSE IF ( NC.LT.0 .OR. NC.GT.NCMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) NC
      ELSE
         READ ( NIN, FMT = * ) ( ( H1(I,J), I = 1,NC ), J = 1,N*NB )
         READ ( NIN, FMT = * ) ( ( H2(I,J), I = 1,NC ), J = 1,N*NB )
*        Compare the given sequences and evaluate their closeness.
         CALL SB09MD( N, NC, NB, H1, LDH1, H2, LDH2, SS, LDSS, SE, LDSE,
     $                PRE, LDPRE, TOL, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 20 I = 1, NC
               WRITE ( NOUT, FMT = 99996 ) ( SS(I,J), J = 1,NB )
   20       CONTINUE
            WRITE ( NOUT, FMT = 99995 )
            DO 40 I = 1, NC
               WRITE ( NOUT, FMT = 99996 ) ( SE(I,J), J = 1,NB )
   40       CONTINUE
            WRITE ( NOUT, FMT = 99994 )
            DO 60 I = 1, NC
               WRITE ( NOUT, FMT = 99996 ) ( PRE(I,J), J = 1,NB )
   60       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB09MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB09MD = ',I2)
99997 FORMAT (' The sum-of-squares matrix SS is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The quadratic error matrix SE is ')
99994 FORMAT (/' The percentage relative error matrix PRE is ')
99993 FORMAT (/' N is out of range.',/' N = ',I5)
99992 FORMAT (/' NB is out of range.',/' NB = ',I5)
99991 FORMAT (/' NC is out of range.',/' NC = ',I5)
      END
