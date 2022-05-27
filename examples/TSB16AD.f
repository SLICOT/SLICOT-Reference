*     SB16AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX, NCMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20,
     $                   NCMAX = 20 )
      INTEGER          MPMAX, NNCMAX
      PARAMETER        ( MPMAX  = MMAX + PMAX, NNCMAX = NMAX + NCMAX )
      INTEGER          LDA, LDB, LDC, LDD, LDAC, LDBC, LDCC, LDDC
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDD = PMAX, LDAC = NCMAX, LDBC = NCMAX,
     $                   LDCC = PMAX, LDDC = PMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = 2*MAX( NCMAX, MPMAX ) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 2*NCMAX*NCMAX +
     $                            NNCMAX*( NNCMAX + 2*MPMAX ) +
     $                            MAX( NNCMAX*( NNCMAX +
     $                                 MAX( NNCMAX, MMAX, PMAX ) + 7 ),
     $                                 MPMAX*( MPMAX + 4 ) ) )
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, TOL1, TOL2
      INTEGER          I, INFO, IWARN, J, M, N, NCR, NCS, NC, P
      CHARACTER*1      DICO, EQUIL, JOBC, JOBO, JOBMR, ORDSEL, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 D(LDD,MMAX), DWORK(LDWORK), HSVC(NMAX),
     $                 AC(LDAC,NCMAX), BC(LDBC,PMAX), CC(LDCC,NMAX),
     $                 DC(LDDC,PMAX)
      INTEGER          IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         SB16AD
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, NC, NCR, ALPHA, TOL1, TOL2, DICO,
     $                      JOBC, JOBO, JOBMR, WEIGHT, EQUIL, ORDSEL
      IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99989 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1, N )
            IF( P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99988 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
               IF( NC.LT.0 .OR. NC.GT.NCMAX ) THEN
                  WRITE ( NOUT, FMT = 99986 ) NC
               ELSE
                  IF( NC.GT.0 ) THEN
                     READ ( NIN, FMT = * )
     $                 ( ( AC(I,J), J = 1,NC ), I = 1,NC )
                     READ ( NIN, FMT = * )
     $                 ( ( BC(I,J), J = 1,P ), I = 1, NC )
                     READ ( NIN, FMT = * )
     $                 ( ( CC(I,J), J = 1,NC ), I = 1,M )
                  END IF
                  READ ( NIN, FMT = * )
     $                 ( ( DC(I,J), J = 1,P ), I = 1,M )
               END IF
*              Find a reduced ssr for (AC,BC,CC,DC).
               CALL SB16AD( DICO, JOBC, JOBO, JOBMR, WEIGHT, EQUIL,
     $                      ORDSEL, N, M, P, NC, NCR, ALPHA, A, LDA,
     $                      B, LDB, C, LDC, D, LDD, AC, LDAC, BC, LDBC,
     $                      CC, LDCC, DC, LDDC, NCS, HSVC, TOL1, TOL2,
     $                      IWORK, DWORK, LDWORK, IWARN, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  IF( IWARN.NE.0) WRITE ( NOUT, FMT = 99984 ) IWARN
                  WRITE ( NOUT, FMT = 99997 ) NCR
                  WRITE ( NOUT, FMT = 99987 )
                  WRITE ( NOUT, FMT = 99995 ) ( HSVC(J), J = 1, NCS )
                  IF( NCR.GT.0 ) WRITE ( NOUT, FMT = 99996 )
                  DO 20 I = 1, NCR
                     WRITE ( NOUT, FMT = 99995 ) ( AC(I,J), J = 1,NCR )
   20             CONTINUE
                  IF( NCR.GT.0 ) WRITE ( NOUT, FMT = 99993 )
                  DO 40 I = 1, NCR
                     WRITE ( NOUT, FMT = 99995 ) ( BC(I,J), J = 1,P )
   40             CONTINUE
                  IF( NCR.GT.0 ) WRITE ( NOUT, FMT = 99992 )
                  DO 60 I = 1, M
                     WRITE ( NOUT, FMT = 99995 ) ( CC(I,J), J = 1,NCR )
   60             CONTINUE
                  WRITE ( NOUT, FMT = 99991 )
                  DO 70 I = 1, M
                     WRITE ( NOUT, FMT = 99995 ) ( DC(I,J), J = 1,P )
   70             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB16AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB16AD = ',I2)
99997 FORMAT (/' The order of reduced controller = ',I2)
99996 FORMAT (/' The reduced controller state dynamics matrix Ac is ')
99995 FORMAT (20(1X,F8.4))
99993 FORMAT (/' The reduced controller input/state matrix Bc is ')
99992 FORMAT (/' The reduced controller state/output matrix Cc is ')
99991 FORMAT (/' The reduced controller input/output matrix Dc is ')
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' P is out of range.',/' P = ',I5)
99987 FORMAT (/' The Hankel singular values of weighted ALPHA-stable',
     $         ' part are')
99986 FORMAT (/' NC is out of range.',/' NC = ',I5)
99984 FORMAT (' IWARN on exit from SB16AD = ',I2)
      END
