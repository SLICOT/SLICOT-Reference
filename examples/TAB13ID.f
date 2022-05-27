*     AB13ID EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDE
      PARAMETER        ( LDA = NMAX, LDB = NMAX,
     $                   LDC = MAX( MMAX, PMAX ), LDE = NMAX )
      INTEGER          LDWORK, LIWORK
      PARAMETER        ( LDWORK = 2*NMAX*NMAX + 
     $                            MAX( 2*( NMAX*( NMAX + MMAX + PMAX ) +
     $                                 MAX( MMAX, PMAX ) + NMAX - 1 ),
     $                                 10*NMAX + MAX( NMAX, 23 ) ),
     $                   LIWORK = 2*NMAX + MAX( MMAX, PMAX ) + 7 )
*     .. Local Scalars ..
      LOGICAL          LISPRP
      CHARACTER        CKSING, EQUIL, JOBEIG, JOBSYS, RESTOR, UPDATE
      INTEGER          I, INFO, IWARN, J, M, N, NO, NR, P, RANKE
*     .. Local Arrays ..
      INTEGER          IWORK(LIWORK)
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MAX(MMAX,PMAX)), C(LDC,NMAX),
     $                 DWORK(LDWORK), E(LDE,NMAX), TOL(3)

*     .. External Functions ..
      LOGICAL          AB13ID, LSAME
      EXTERNAL         AB13ID, LSAME
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, TOL(1), TOL(2), TOL(3), JOBSYS,
     $                      JOBEIG, EQUIL, CKSING, RESTOR, UPDATE
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99988 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99987 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99986 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
*              Check whether the transfer function of the descriptor
*              system is proper.
               LISPRP = AB13ID( JOBSYS, JOBEIG, EQUIL, CKSING, RESTOR,
     $                          UPDATE, N, M, P, A, LDA, E, LDE, B, LDB,
     $                          C, LDC, NR, RANKE, TOL, IWORK, DWORK,
     $                          LDWORK, IWARN, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  IF ( LISPRP ) THEN
                     WRITE ( NOUT, FMT = 99991 )
                  ELSE
                     WRITE ( NOUT, FMT = 99990 )
                  END IF
                  WRITE ( NOUT, FMT = 99994 ) NR
                  WRITE ( NOUT, FMT = 99989 ) RANKE
                  IF ( LSAME( JOBSYS, 'N' ).AND.( LSAME( JOBEIG, 'A' )
     $             .OR.LSAME( UPDATE, 'U' ) ) ) THEN
                     NO = N
                  ELSE
                     NO = NR
                  END IF
                  WRITE ( NOUT, FMT = 99997 )
                  DO 10 I = 1, NO
                     WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,NO )
   10             CONTINUE
                  WRITE ( NOUT, FMT = 99996 )
                  DO 20 I = 1, NR
                     WRITE ( NOUT, FMT = 99995 ) ( E(I,J), J = 1,NR )
   20             CONTINUE
                  IF ( LSAME( JOBSYS, 'N' ).AND.LSAME( JOBEIG, 'I' )
     $                                     .AND.LSAME( EQUIL,  'S' )
     $                                     .AND.LSAME( UPDATE, 'N' ) )
     $               NO = N
                  WRITE ( NOUT, FMT = 99993 )
                  DO 30 I = 1, NR
                     WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   30             CONTINUE
                  WRITE ( NOUT, FMT = 99992 )
                  DO 40 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,NR )
   40             CONTINUE
                  IF ( IWARN.NE.0 )
     $               WRITE ( NOUT, FMT = 99998 ) IWARN
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB13ID EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB13ID = ',I2)
99997 FORMAT (/' The reduced state dynamics matrix Ar is ')
99996 FORMAT (/' The reduced descriptor matrix Er is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' Order of reduced system =', I5 )
99993 FORMAT (/' The reduced input/state matrix Br is ')
99992 FORMAT (/' The reduced state/output matrix Cr is ')
99991 FORMAT ( ' The system is proper')
99990 FORMAT ( ' The system is improper')
99989 FORMAT (' Rank of matrix E =', I5 )
99988 FORMAT (/' N is out of range.',/' N = ',I5)
99987 FORMAT (/' M is out of range.',/' M = ',I5)
99986 FORMAT (/' P is out of range.',/' P = ',I5)
      END
