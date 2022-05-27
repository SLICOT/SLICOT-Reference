*     MB02MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX, LMAX
      PARAMETER        ( MMAX = 20, NMAX = 20, LMAX = 20 )
      INTEGER          LDC, LDX
      PARAMETER        ( LDC = MAX( MMAX,NMAX+LMAX ), LDX = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MMAX*(NMAX+LMAX) +
     $                            MAX( 3*MIN(MMAX,NMAX+LMAX) +
     $                                   MAX(MMAX,NMAX+LMAX),
     $                                 5*MIN(MMAX,NMAX+LMAX),
     $                                 3*LMAX ) )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = LMAX )
      INTEGER          LENGS
      PARAMETER        ( LENGS = MIN( MMAX, NMAX+LMAX ) )
*     .. Local Scalars ..
      DOUBLE PRECISION SDEV, TOL
      INTEGER          I, INFO, IWARN, J, L, M, N, RANK
      CHARACTER*1      JOB
*     .. Local Arrays ..
      DOUBLE PRECISION C(LDC,NMAX+LMAX), DWORK(LDWORK), S(LENGS),
     $                 X(LDX,LMAX)
      INTEGER          IWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB02MD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N, L, JOB
*
      IF ( LSAME( JOB, 'R' ) ) THEN
         READ ( NIN, FMT = * ) TOL
      ELSE IF ( LSAME( JOB, 'T' ) ) THEN
         READ ( NIN, FMT = * ) RANK, SDEV
         TOL = SDEV
      ELSE IF ( LSAME( JOB, 'N' ) ) THEN
         READ ( NIN, FMT = * ) RANK, TOL
      ELSE
         READ ( NIN, FMT = * ) SDEV
         TOL = SDEV
      END IF
*
      IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) M
      ELSE IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) N
      ELSE IF ( L.LT.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99989 ) L
      ELSE
         READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N+L ), I = 1,M )
*        Compute the solution to the TLS problem Ax = b.
         CALL MB02MD( JOB, M, N, L, RANK, C, LDC, S, X, LDX, TOL, IWORK,
     $                DWORK, LDWORK, IWARN, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( IWARN.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99997 ) IWARN
               WRITE ( NOUT, FMT = 99996 ) RANK
            ELSE
               IF ( ( LSAME( JOB, 'R' ) ) .OR. ( LSAME( JOB, 'B' ) ) )
     $            WRITE ( NOUT, FMT = 99996 ) RANK
            END IF
            WRITE ( NOUT, FMT = 99995 )
            DO 40 J = 1, L
               DO 20 I = 1, N
                  WRITE ( NOUT, FMT = 99994 ) X(I,J)
   20          CONTINUE
               IF ( J.LT.L ) WRITE ( NOUT, FMT = 99993 )
   40       CONTINUE
            WRITE ( NOUT, FMT = 99992 ) ( S(J),J = 1, MIN( M, N+L ) )
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB02MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02MD = ',I2)
99997 FORMAT (' IWARN on exit from MB02MD = ',I2,/)
99996 FORMAT (' The computed rank of the TLS approximation = ',I3,/)
99995 FORMAT (' The solution X to the TLS problem is ',/)
99994 FORMAT (1X,F8.4)
99993 FORMAT (' ')
99992 FORMAT (/' The singular values of C are ',//(1X,F8.4))
99991 FORMAT (/' N is out of range.',/' N = ',I5)
99990 FORMAT (/' M is out of range.',/' M = ',I5)
99989 FORMAT (/' L is out of range.',/' L = ',I5)
      END
