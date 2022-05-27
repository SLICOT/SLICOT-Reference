*     MB04DZ EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 100 )
      INTEGER          LDA, LDQG
      PARAMETER        ( LDA = NMAX, LDQG = NMAX )
*     .. Local Scalars ..
      CHARACTER*1      JOB
      INTEGER          I, ILO, INFO, J, N
*     .. Local Arrays ..
      COMPLEX*16       A(LDA, NMAX), QG(LDQG, NMAX+1)
      DOUBLE PRECISION DUMMY(1), SCALE(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION DLAPY2, ZLANTR
      EXTERNAL         DLAPY2, ZLANTR
*     .. External Subroutines ..
      EXTERNAL         MB04DZ
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * )  N, JOB
      IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( QG(I,J), J = 1,N+1 ), I = 1,N )
         CALL MB04DZ( JOB, N, A, LDA, QG, LDQG, ILO, SCALE, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 10  I = 1, N
               WRITE (NOUT, FMT = 99995) ( A(I,J), J = 1,N )
   10       CONTINUE
            WRITE ( NOUT, FMT = 99996 )
            DO 20  I = 1, N
               WRITE (NOUT, FMT = 99995) ( QG(I,J), J = 1,N+1 )
   20       CONTINUE
            WRITE (NOUT, FMT = 99993)  ILO
            IF ( ILO.GT.1 ) THEN
                WRITE (NOUT, FMT = 99992) DLAPY2( ZLANTR( 'Frobenius',
     $                 'Lower', 'No Unit', N-1, ILO-1, A(2,1), LDA,
     $                 DUMMY ), ZLANTR( 'Frobenius', 'Lower', 'No Unit',
     $                 N, ILO-1, QG(1,1), LDQG, DUMMY ) )
            END IF
         END IF
      END IF
*
99999 FORMAT (' MB04DZ EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB04DZ = ',I2)
99997 FORMAT (' The balanced matrix A is ')
99996 FORMAT (/' The balanced matrix QG is ')
99995 FORMAT (20(1X,F9.4,SP,F9.4,S,'i '))
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' ILO = ',I4)
99992 FORMAT (/' Norm of subdiagonal blocks: ',G7.2)
      END
