*     AB13DD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 10, MMAX = 10, PMAX = 10 )
      INTEGER          LDA, LDB, LDC, LDD, LDE
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDD = PMAX, LDE = NMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX )
      INTEGER          LCWORK
      PARAMETER        ( LCWORK = ( NMAX + MMAX )*( NMAX + PMAX ) +
     $                             2*MIN( PMAX, MMAX ) +
     $                             MAX( PMAX, MMAX ) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 15*NMAX*NMAX + PMAX*PMAX + MMAX*MMAX +
     $                            ( 6*NMAX + 3 )*( PMAX + MMAX ) +
     $                            4*PMAX*MMAX + NMAX*MMAX + 22*NMAX +
     $                            7*MIN( PMAX, MMAX ) )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )

*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO, J, M, N, P
      CHARACTER        DICO, EQUIL, JOBD, JOBE
*     .. Local Arrays ..
      INTEGER          IWORK( LIWORK )
      DOUBLE PRECISION A( LDA, NMAX ), B( LDB, MMAX ),  C( LDC, NMAX ),
     $                 D( LDD, MMAX ), DWORK( LDWORK ), E( LDE, NMAX ),
     $                 FPEAK( 2 ), GPEAK( 2 )
      COMPLEX*16       CWORK( LCWORK )
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         AB13DD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, FPEAK, TOL, DICO, JOBE, EQUIL, JOBD
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) M
      ELSE IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) P
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( LSAME( JOBE, 'G' ) )
     $      READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
         IF ( LSAME( JOBD, 'D' ) )
     $      READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
*        Computing the Linf norm.
         CALL AB13DD( DICO, JOBE, EQUIL, JOBD, N, M, P, FPEAK, A, LDA,
     $                E, LDE, B, LDB, C, LDC, D, LDD, GPEAK, TOL, IWORK,
     $                DWORK, LDWORK, CWORK, LCWORK, INFO )
*
         IF ( INFO.EQ.0 ) THEN
            IF ( GPEAK( 2 ).EQ.ZERO ) THEN
               WRITE ( NOUT, FMT = 99991 )
            ELSE
               WRITE ( NOUT, FMT = 99997 )
               WRITE ( NOUT, FMT = 99995 ) GPEAK( 1 )
            END IF
            IF ( FPEAK( 2 ).EQ.ZERO ) THEN
               WRITE ( NOUT, FMT = 99990 )
            ELSE
               WRITE ( NOUT, FMT = 99996 )
               WRITE ( NOUT, FMT = 99995 ) FPEAK( 1 )
            END IF
         ELSE
            WRITE( NOUT, FMT = 99998 ) INFO
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB13DD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (/' INFO on exit from AB13DD =',I2)
99997 FORMAT (/' The L_infty norm of the system is'/)
99996 FORMAT (/' The peak frequency is'/)
99995 FORMAT (D17.10)
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' M is out of range.',/' M = ',I5)
99992 FORMAT (/' P is out of range.',/' P = ',I5)
99991 FORMAT (/' The L_infty norm of the system is infinite')
99990 FORMAT (/' The peak frequency is infinite'/)
      END
