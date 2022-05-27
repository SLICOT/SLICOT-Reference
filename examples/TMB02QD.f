*     MB02QD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, NRHSMX
      PARAMETER        ( NMAX = 20, MMAX = 20, NRHSMX = 20 )
      INTEGER          LDA, LDB
      PARAMETER        ( LDA = MMAX, LDB = MAX( MMAX, NMAX ) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX(   MIN( MMAX, NMAX) + 3*NMAX + 1,
     $                                 2*MIN( MMAX, NMAX) + NRHSMX ) )
*     .. Local Scalars ..
      DOUBLE PRECISION RCOND, SVLMAX
      INTEGER          I, INFO, J, M, N, NRHS, RANK
      CHARACTER*1      INIPER, JOB
*     .. Local Arrays ..
      INTEGER          JPVT(NMAX)
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,NRHSMX), DWORK(LDWORK),
     $                 SVAL(3), Y(NMAX*NRHSMX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB02QD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N, NRHS, RCOND, SVLMAX, JOB, INIPER
      IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) M
      ELSE
         IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
            WRITE ( NOUT, FMT = 99993 ) N
         ELSE
            IF ( NRHS.LT.0 .OR. NRHS.GT.NRHSMX ) THEN
               WRITE ( NOUT, FMT = 99992 ) NRHS
            ELSE
               READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,M )
               READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,NRHS ), I = 1,M )
               IF ( LSAME( JOB, 'F' ) )
     $            READ ( NIN, FMT = * ) ( Y(I),  I = 1,N*NRHS )
               IF ( LSAME( INIPER, 'P' ) )
     $            READ ( NIN, FMT = * ) ( JPVT(I),  I = 1,N )
*              Find the least squares solution.
               CALL MB02QD( JOB, INIPER, M, N, NRHS, RCOND, SVLMAX, A,
     $                      LDA, B, LDB, Y, JPVT, RANK, SVAL, DWORK,
     $                      LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 ) RANK, SVAL
                  WRITE ( NOUT, FMT = 99996 )
                  DO 10 I = 1, N
                     WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,NRHS )
   10             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB02QD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02QD =',I2)
99997 FORMAT (' The effective rank of A =',I2,/
     $        ' Estimates of the singular values SVAL = '/3(1X,F8.4))
99996 FORMAT (' The least squares solution is')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' M is out of range.',/' M = ',I5)
99993 FORMAT (/' N is out of range.',/' N = ',I5)
99992 FORMAT (/' NRHS is out of range.',/' NRHS = ',I5)
      END
