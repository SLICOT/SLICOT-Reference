*     TB01ID EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX )
*     .. Local Scalars ..
      CHARACTER*1      JOB
      INTEGER          I, INFO, J, M, N, P
      DOUBLE PRECISION MAXRED
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 SCALE(NMAX)
*     .. External Subroutines ..
      EXTERNAL         TB01ID, UD01MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, JOB, MAXRED
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99992 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            IF ( P.LT.0 .OR. P.GT.MMAX ) THEN
               WRITE ( NOUT, FMT = 99991 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
*              Balance system matrix S.
               CALL TB01ID( JOB, N, M, P, MAXRED, A, LDA, B, LDB, C,
     $                      LDC, SCALE, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  CALL UD01MD( N, N, 5, NOUT, A, LDA,
     $                        'The balanced matrix A', INFO )
                  IF ( M.GT.0 )
     $               CALL UD01MD( N, M, 5, NOUT, B, LDB,
     $                            'The balanced matrix B', INFO )
                  IF ( P.GT.0 )
     $               CALL UD01MD( P, N, 5, NOUT, C, LDC,
     $                            'The balanced matrix C', INFO )
                  CALL UD01MD( 1, N, 5, NOUT, SCALE, 1,
     $                        'The scaling vector SCALE', INFO )
                  WRITE ( NOUT, FMT = 99994 ) MAXRED
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TB01ID EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TB01ID = ',I2)
99994 FORMAT (/' MAXRED is ',E13.4)
99993 FORMAT (/' N is out of range.',/' N = ',I5)
99992 FORMAT (/' M is out of range.',/' M = ',I5)
99991 FORMAT (/' P is out of range.',/' P = ',I5)
      END
