*     AB01ND EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX
      PARAMETER        ( NMAX = 20, MMAX = 20 )
      INTEGER          LDA, LDB, LDZ
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDZ = NMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = MMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( NMAX, 3*MMAX ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO, INDCON, J, M, N, NCONT
      CHARACTER*1      JOBZ
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), DWORK(LDWORK),
     $                 TAU(NMAX), Z(LDZ,NMAX)
      INTEGER          IWORK(LIWORK), NBLK(NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         AB01ND, DORGQR
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, TOL, JOBZ
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99989 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), I = 1,N ), J = 1,M )
*           Find a controllable ssr for the given system.
            CALL AB01ND( JOBZ, N, M, A, LDA, B, LDB, NCONT, INDCON,
     $                   NBLK, Z, LDZ, TAU, TOL, IWORK, DWORK, LDWORK,
     $                   INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 ) NCONT
               WRITE ( NOUT, FMT = 99996 )
               DO 20 I = 1, NCONT
                  WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,NCONT )
   20          CONTINUE
               WRITE ( NOUT, FMT = 99994 ) ( NBLK(I), I = 1,INDCON )
               WRITE ( NOUT, FMT = 99993 )
               DO 40 I = 1, NCONT
                  WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   40          CONTINUE
               WRITE ( NOUT, FMT = 99992 ) INDCON
               IF ( LSAME( JOBZ, 'F' ) )
     $            CALL DORGQR( N, N, N, Z, LDZ, TAU, DWORK, LDWORK,
     $                         INFO )
               IF ( LSAME( JOBZ, 'F' ).OR.LSAME( JOBZ, 'I' ) ) THEN
                  WRITE ( NOUT, FMT = 99991 )
                  DO 60 I = 1, N
                     WRITE ( NOUT, FMT = 99995 ) ( Z(I,J), J = 1,N )
   60             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB01ND EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB01ND = ',I2)
99997 FORMAT (' The order of the controllable state-space representati',
     $       'on = ',I2)
99996 FORMAT (/' The transformed state dynamics matrix of a controllab',
     $       'le realization is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' and the dimensions of its diagonal blocks are ',
     $       /20(1X,I2))
99993 FORMAT (/' The transformed input/state matrix B of a controllabl',
     $       'e realization is ')
99992 FORMAT (/' The controllability index of the transformed system r',
     $       'epresentation = ',I2)
99991 FORMAT (/' The similarity transformation matrix Z is ')
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
      END
