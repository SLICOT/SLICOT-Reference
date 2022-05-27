*     MB04DL EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 10 )
      INTEGER          LDA, LDB
      PARAMETER        ( LDA = NMAX, LDB = NMAX )
*     .. Local Scalars ..
      CHARACTER*1      JOB
      INTEGER          I, ILO, INFO, IWARN, J, N
      DOUBLE PRECISION THRESH
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA, NMAX), B(LDB, NMAX), DWORK(8*NMAX),
     $                 LSCALE(NMAX), RSCALE(NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB04DL
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * )  N, JOB, THRESH
      IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99985 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,N ), I = 1,N )
         CALL MB04DL( JOB, N, THRESH, A, LDA, B, LDB, ILO, IHI, LSCALE,
     $                RSCALE, DWORK, IWARN, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 10  I = 1, N
               WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,N )
10          CONTINUE
            WRITE ( NOUT, FMT = 99996 )
            DO 40  I = 1, N
               WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,N )
40          CONTINUE
            WRITE ( NOUT, FMT = 99994 )  ILO
            WRITE ( NOUT, FMT = 99993 )  IHI
            WRITE ( NOUT, FMT = 99991 )
            WRITE ( NOUT, FMT = 99995 ) ( LSCALE(I), I = 1,N )
            WRITE ( NOUT, FMT = 99990 )
            WRITE ( NOUT, FMT = 99995 ) ( RSCALE(I), I = 1,N )
            IF ( LSAME( JOB, 'S' ) .OR. LSAME( JOB, 'B' ) ) THEN
               IF ( .NOT.( THRESH.EQ.-2 .OR. THRESH.EQ.-4 ) ) THEN
                  WRITE ( NOUT, FMT = 99989 )
                  WRITE ( NOUT, FMT = 99995 ) ( DWORK(I), I = 1,2 )
                  WRITE ( NOUT, FMT = 99988 )
                  WRITE ( NOUT, FMT = 99995 ) ( DWORK(I), I = 3,4 )
                  WRITE ( NOUT, FMT = 99987 )
                  WRITE ( NOUT, FMT = 99995 ) ( DWORK(5) )
               ELSE
                  WRITE ( NOUT, FMT = 99986 ) IWARN
               END IF
            END IF
         END IF
      END IF
*
99999 FORMAT (' MB04DL EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB04DL = ',I2)
99997 FORMAT (' The balanced matrix A is ')
99996 FORMAT (/' The balanced matrix B is ')
99995 FORMAT (20(1X,G12.4))
99994 FORMAT (/' ILO = ',I4)
99993 FORMAT (/' IHI = ',I4)
99991 FORMAT (/' The permutations and left scaling factors are ')
99990 FORMAT (/' The permutations and right scaling factors are ')
99989 FORMAT (/' The initial 1-norms of the (sub)matrices are ')
99988 FORMAT (/' The final 1-norms of the (sub)matrices are ')
99987 FORMAT (/' The threshold value finally used is ')
99986 FORMAT (/' IWARN on exit from MB04DL = ',I2)
99985 FORMAT (/' N is out of range.',/' N = ',I5)
      END
