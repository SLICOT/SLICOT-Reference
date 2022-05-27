*     SB10FD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX, N2MAX
      PARAMETER        ( NMAX = 10, MMAX = 10, PMAX = 10, N2MAX = 20 )
      INTEGER          LDA, LDB, LDC, LDD, LDAK, LDBK, LDCK, LDDK,
     $                 LDAC, LDBC, LDCC, LDDC
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX, LDD = PMAX,
     $                   LDAK = NMAX, LDBK = NMAX, LDCK = MMAX,
     $                   LDDK = MMAX, LDAC = 2*NMAX, LDBC = 2*NMAX,
     $                   LDCC = PMAX, LDDC = PMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = MAX( 2*MAX( NMAX, MMAX, PMAX ),
     $                            NMAX*NMAX ) )
      INTEGER          MPMX
      PARAMETER        ( MPMX = MAX( MMAX, PMAX ) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 2*MPMX*( 3*MPMX + 2*NMAX ) +
     $                   MAX( ( NMAX + MPMX )*( NMAX + MPMX + 6 ),
     $                   MPMX*( MPMX + MAX( NMAX, MPMX, 5 ) + 1 ),
     $                   2*NMAX*( NMAX + 2*MPMX ) +
     $                   MAX( 4*MPMX*MPMX + MAX( 2*MPMX, 3*NMAX*NMAX +
     $                   MAX( 2*NMAX*MPMX, 10*NMAX*NMAX+12*NMAX+5 ) ),
     $                   MPMX*( 3*NMAX + 3*MPMX +
     $                          MAX( 2*NMAX, 4*MPMX +
     $                               MAX( NMAX, MPMX ) ) ) ) ) )
*     .. Local Scalars ..
      INTEGER SDIM
      LOGICAL SELECT
      DOUBLE PRECISION GAMMA, TOL
      INTEGER          I, INFO1, INFO2, INFO3, J, M, N, NCON, NMEAS, NP
*     .. Local Arrays ..
      LOGICAL          BWORK(N2MAX)
      INTEGER          IWORK(LIWORK)
      DOUBLE PRECISION A(LDA,NMAX), AK(LDAK,NMAX), AC(LDAC,N2MAX),
     $                 B(LDB,MMAX), BK(LDBK,PMAX), BC(LDBC,MMAX),
     $                 C(LDC,NMAX), CK(LDCK,NMAX), CC(LDCC,N2MAX),
     $                 D(LDD,MMAX), DK(LDDK,PMAX), DC(LDDC,MMAX),
     $                 DWORK(LDWORK), RCOND( 4 )
*     .. External Subroutines ..
      EXTERNAL         SB10FD, SB10LD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, NP, NCON, NMEAS
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99987 ) N
      ELSE IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99986 ) M
      ELSE IF ( NP.LT.0 .OR. NP.GT.PMAX ) THEN
         WRITE ( NOUT, FMT = 99985 ) NP
      ELSE IF ( NCON.LT.0 .OR. NCON.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99984 ) NCON
      ELSE IF ( NMEAS.LT.0 .OR. NMEAS.GT.PMAX ) THEN
         WRITE ( NOUT, FMT = 99983 ) NMEAS
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,NP )
         READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,NP )
         READ ( NIN, FMT = * ) GAMMA, TOL
*        Compute the suboptimal controller
         CALL SB10FD( N, M, NP, NCON, NMEAS, GAMMA, A, LDA, B, LDB,
     $                C, LDC, D, LDD, AK, LDAK, BK, LDBK, CK, LDCK,
     $                DK, LDDK, RCOND, TOL, IWORK, DWORK, LDWORK,
     $                BWORK, INFO1 )
*
         IF ( INFO1.EQ.0 ) THEN
            WRITE ( NOUT, FMT = 99996 )
            DO 10 I = 1, N
               WRITE ( NOUT, FMT = 99989 ) ( AK(I,J), J = 1,N )
   10       CONTINUE
            WRITE ( NOUT, FMT = 99995 )
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99989 ) ( BK(I,J), J = 1,NMEAS )
   20       CONTINUE
            WRITE ( NOUT, FMT = 99994 )
            DO 30 I = 1, NCON
               WRITE ( NOUT, FMT = 99989 ) ( CK(I,J), J = 1,N )
   30       CONTINUE
            WRITE ( NOUT, FMT = 99993 )
            DO 40 I = 1, NCON
               WRITE ( NOUT, FMT = 99989 ) ( DK(I,J), J = 1,NMEAS )
   40       CONTINUE
            WRITE( NOUT, FMT = 99992 )
            WRITE( NOUT, FMT = 99988 ) ( RCOND(I), I = 1, 4 )
*           Compute the closed-loop matrices
            CALL SB10LD(N, M, NP, NCON, NMEAS, A, LDA, B, LDB, C, LDC,
     $                  D, LDD, AK, LDAK, BK, LDBK, CK, LDCK, DK, LDDK,
     $                  AC, LDAC, BC, LDBC, CC, LDCC, DC, LDDC, IWORK,
     $                  DWORK, LDWORK, INFO2 )
*
            IF ( INFO2.EQ.0 ) THEN
*              Compute the closed-loop poles
               CALL DGEES( 'N','N', SELECT, 2*N, AC, LDAC, SDIM,
     $                     DWORK(1), DWORK(2*N+1), DWORK, 2*N,
     $                     DWORK(4*N+1), LDWORK-4*N, BWORK, INFO3)
*
               IF( INFO3.EQ.0 ) THEN
                  WRITE( NOUT, FMT = 99991 )
                  WRITE( NOUT, FMT = 99988 ) (DWORK(I), I =1, 2*N)
                  WRITE( NOUT, FMT = 99990 )
                  WRITE( NOUT, FMT = 99988 ) (DWORK(2*N+I), I =1, 2*N)
               ELSE
                  WRITE( NOUT, FMT = 99996 ) INFO3
               END IF
            ELSE
               WRITE( NOUT, FMT = 99997 ) INFO2
            END IF
         ELSE
            WRITE( NOUT, FMT = 99998 ) INFO1
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB10FD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (/' INFO on exit from SB10FD =',I2)
99997 FORMAT (/' INFO on exit from SB10LD =',I2)
99996 FORMAT (' The controller state matrix AK is'/)
99995 FORMAT (/' The controller input matrix BK is'/)
99994 FORMAT (/' The controller output matrix CK is'/)
99993 FORMAT (/' The controller matrix DK is'/)
99992 FORMAT (/' The estimated condition numbers are'/)
99991 FORMAT (/' The real parts of the closed-loop system poles are'/)
99990 FORMAT (/' The imaginary parts of the closed-loop system',
     $           ' poles are'/)
99989 FORMAT (10(1X,F8.4))
99988 FORMAT ( 5(1X,D12.5))
99987 FORMAT (/' N is out of range.',/' N = ',I5)
99986 FORMAT (/' M is out of range.',/' M = ',I5)
99985 FORMAT (/' N is out of range.',/' N = ',I5)
99984 FORMAT (/' NCON is out of range.',/' NCON = ',I5)
99983 FORMAT (/' NMEAS is out of range.',/' NMEAS = ',I5)
      END
