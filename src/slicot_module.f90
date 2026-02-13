!
! SPDX-License-Identifier: BSD-3-Clause
!

!
! This file is part of SLICOT. It enables the module 
! and type checking functionality of Fortran 90 and 
! newer for the old f77 style SLICOT interface.
!
module slicot 
    implicit none 
    
    interface 
        subroutine ab01md(jobz, n, a, lda, b, ncont, z, ldz, &
                       tau, tol, dwork, ldwork, info)
            character, intent(inout)          :: jobz
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(*)
            integer, intent(inout)            :: ncont
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab01md
    end interface
    public :: ab01md
    
    interface 
        subroutine ab01nd(jobz, n, m, a, lda, b, ldb, ncont, &
                       indcon, nblk, z, ldz, tau, tol, iwork, dwork, &
                       ldwork, info)
            character, intent(inout)          :: jobz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            integer, intent(inout)            :: ncont
            integer, intent(inout)            :: indcon
            integer, intent(inout)            :: nblk(*)
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab01nd
    end interface
    public :: ab01nd
    
    interface 
        subroutine ab01od(stages, jobu, jobv, n, m, a, lda, b, &
                       ldb, u, ldu, v, ldv, ncont, indcon, kstair, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: stages
            character, intent(inout)          :: jobu
            character, intent(inout)          :: jobv
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            integer, intent(inout)            :: ncont
            integer, intent(inout)            :: indcon
            integer, intent(inout)            :: kstair(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab01od
    end interface
    public :: ab01od
    
    interface 
        subroutine ab04md(type, n, m, p, alpha, beta, a, lda, &
                       b, ldb, c, ldc, d, ldd, iwork, dwork, &
                       ldwork, info)
            character, intent(inout)          :: type
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab04md
    end interface
    public :: ab04md
    
    interface 
        subroutine ab05md(uplo, over, n1, m1, p1, n2, p2, a1, &
                       lda1, b1, ldb1, c1, ldc1, d1, ldd1, a2, &
                       lda2, b2, ldb2, c2, ldc2, d2, ldd2, n, &
                       a, lda, b, ldb, c, ldc, d, ldd, &
                       dwork, ldwork, info)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: over
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: m1
            integer, intent(inout)            :: p1
            integer, intent(inout)            :: n2
            integer, intent(inout)            :: p2
            double precision, intent(inout)   :: a1(lda1, *)
            integer, intent(inout)            :: lda1
            double precision, intent(inout)   :: b1(ldb1, *)
            integer, intent(inout)            :: ldb1
            double precision, intent(inout)   :: c1(ldc1, *)
            integer, intent(inout)            :: ldc1
            double precision, intent(inout)   :: d1(ldd1, *)
            integer, intent(inout)            :: ldd1
            double precision, intent(inout)   :: a2(lda2, *)
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: b2(ldb2, *)
            integer, intent(inout)            :: ldb2
            double precision, intent(inout)   :: c2(ldc2, *)
            integer, intent(inout)            :: ldc2
            double precision, intent(inout)   :: d2(ldd2, *)
            integer, intent(inout)            :: ldd2
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab05md
    end interface
    public :: ab05md
    
    interface 
        subroutine ab05nd(over, n1, m1, p1, n2, alpha, a1, lda1, &
                       b1, ldb1, c1, ldc1, d1, ldd1, a2, lda2, &
                       b2, ldb2, c2, ldc2, d2, ldd2, n, a, &
                       lda, b, ldb, c, ldc, d, ldd, iwork, &
                       dwork, ldwork, info)
            character, intent(inout)          :: over
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: m1
            integer, intent(inout)            :: p1
            integer, intent(inout)            :: n2
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a1(lda1, *)
            integer, intent(inout)            :: lda1
            double precision, intent(inout)   :: b1(ldb1, *)
            integer, intent(inout)            :: ldb1
            double precision, intent(inout)   :: c1(ldc1, *)
            integer, intent(inout)            :: ldc1
            double precision, intent(inout)   :: d1(ldd1, *)
            integer, intent(inout)            :: ldd1
            double precision, intent(inout)   :: a2(lda2, *)
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: b2(ldb2, *)
            integer, intent(inout)            :: ldb2
            double precision, intent(inout)   :: c2(ldc2, *)
            integer, intent(inout)            :: ldc2
            double precision, intent(inout)   :: d2(ldd2, *)
            integer, intent(inout)            :: ldd2
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab05nd
    end interface
    public :: ab05nd
    
    interface 
        subroutine ab05od(over, n1, m1, p1, n2, m2, alpha, a1, &
                       lda1, b1, ldb1, c1, ldc1, d1, ldd1, a2, &
                       lda2, b2, ldb2, c2, ldc2, d2, ldd2, n, &
                       m, a, lda, b, ldb, c, ldc, d, &
                       ldd, info)
            character, intent(inout)          :: over
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: m1
            integer, intent(inout)            :: p1
            integer, intent(inout)            :: n2
            integer, intent(inout)            :: m2
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a1(lda1, *)
            integer, intent(inout)            :: lda1
            double precision, intent(inout)   :: b1(ldb1, *)
            integer, intent(inout)            :: ldb1
            double precision, intent(inout)   :: c1(ldc1, *)
            integer, intent(inout)            :: ldc1
            double precision, intent(inout)   :: d1(ldd1, *)
            integer, intent(inout)            :: ldd1
            double precision, intent(inout)   :: a2(lda2, *)
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: b2(ldb2, *)
            integer, intent(inout)            :: ldb2
            double precision, intent(inout)   :: c2(ldc2, *)
            integer, intent(inout)            :: ldc2
            double precision, intent(inout)   :: d2(ldd2, *)
            integer, intent(inout)            :: ldd2
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: info
        end subroutine ab05od
    end interface
    public :: ab05od
    
    interface 
        subroutine ab05pd(over, n1, m, p, n2, alpha, a1, lda1, &
                       b1, ldb1, c1, ldc1, d1, ldd1, a2, lda2, &
                       b2, ldb2, c2, ldc2, d2, ldd2, n, a, &
                       lda, b, ldb, c, ldc, d, ldd, info)
            character, intent(inout)          :: over
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: n2
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a1(lda1, *)
            integer, intent(inout)            :: lda1
            double precision, intent(inout)   :: b1(ldb1, *)
            integer, intent(inout)            :: ldb1
            double precision, intent(inout)   :: c1(ldc1, *)
            integer, intent(inout)            :: ldc1
            double precision, intent(inout)   :: d1(ldd1, *)
            integer, intent(inout)            :: ldd1
            double precision, intent(inout)   :: a2(lda2, *)
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: b2(ldb2, *)
            integer, intent(inout)            :: ldb2
            double precision, intent(inout)   :: c2(ldc2, *)
            integer, intent(inout)            :: ldc2
            double precision, intent(inout)   :: d2(ldd2, *)
            integer, intent(inout)            :: ldd2
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: info
        end subroutine ab05pd
    end interface
    public :: ab05pd
    
    interface 
        subroutine ab05qd(over, n1, m1, p1, n2, m2, p2, a1, &
                       lda1, b1, ldb1, c1, ldc1, d1, ldd1, a2, &
                       lda2, b2, ldb2, c2, ldc2, d2, ldd2, n, &
                       m, p, a, lda, b, ldb, c, ldc, &
                       d, ldd, info)
            character, intent(inout)          :: over
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: m1
            integer, intent(inout)            :: p1
            integer, intent(inout)            :: n2
            integer, intent(inout)            :: m2
            integer, intent(inout)            :: p2
            double precision, intent(inout)   :: a1(lda1, *)
            integer, intent(inout)            :: lda1
            double precision, intent(inout)   :: b1(ldb1, *)
            integer, intent(inout)            :: ldb1
            double precision, intent(inout)   :: c1(ldc1, *)
            integer, intent(inout)            :: ldc1
            double precision, intent(inout)   :: d1(ldd1, *)
            integer, intent(inout)            :: ldd1
            double precision, intent(inout)   :: a2(lda2, *)
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: b2(ldb2, *)
            integer, intent(inout)            :: ldb2
            double precision, intent(inout)   :: c2(ldc2, *)
            integer, intent(inout)            :: ldc2
            double precision, intent(inout)   :: d2(ldd2, *)
            integer, intent(inout)            :: ldd2
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: info
        end subroutine ab05qd
    end interface
    public :: ab05qd
    
    interface 
        subroutine ab05rd(fbtype, jobd, n, m, p, mv, pz, alpha, &
                       beta, a, lda, b, ldb, c, ldc, d, &
                       ldd, f, ldf, k, ldk, g, ldg, h, &
                       ldh, rcond, bc, ldbc, cc, ldcc, dc, lddc, &
                       iwork, dwork, ldwork, info)
            character, intent(inout)          :: fbtype
            character, intent(inout)          :: jobd
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: mv
            integer, intent(inout)            :: pz
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: k(ldk, *)
            integer, intent(inout)            :: ldk
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: bc(ldbc, *)
            integer, intent(inout)            :: ldbc
            double precision, intent(inout)   :: cc(ldcc, *)
            integer, intent(inout)            :: ldcc
            double precision, intent(inout)   :: dc(lddc, *)
            integer, intent(inout)            :: lddc
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab05rd
    end interface
    public :: ab05rd
    
    interface 
        subroutine ab05sd(fbtype, jobd, n, m, p, alpha, a, lda, &
                       b, ldb, c, ldc, d, ldd, f, ldf, &
                       rcond, iwork, dwork, ldwork, info)
            character, intent(inout)          :: fbtype
            character, intent(inout)          :: jobd
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: rcond
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab05sd
    end interface
    public :: ab05sd
    
    interface 
        subroutine ab07md(jobd, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, info)
            character, intent(inout)          :: jobd
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: info
        end subroutine ab07md
    end interface
    public :: ab07md
    
    interface 
        subroutine ab07nd(n, m, a, lda, b, ldb, c, ldc, &
                       d, ldd, rcond, iwork, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: rcond
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab07nd
    end interface
    public :: ab07nd
    
    interface 
        subroutine ab08md(equil, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, rank, tol, iwork, dwork, &
                       ldwork, info)
            character, intent(inout)          :: equil
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab08md
    end interface
    public :: ab08md
    
    interface 
        subroutine ab08mz(equil, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, rank, tol, iwork, dwork, &
                       zwork, lzwork, info)
            character, intent(inout)          :: equil
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(inout)            :: ldc
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine ab08mz
    end interface
    public :: ab08mz
    
    interface 
        subroutine ab08nd(equil, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, nu, rank, dinfz, nkror, &
                       nkrol, infz, kronr, kronl, af, ldaf, bf, ldbf, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: equil
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nu
            integer, intent(inout)            :: rank
            integer, intent(inout)            :: dinfz
            integer, intent(inout)            :: nkror
            integer, intent(inout)            :: nkrol
            integer, intent(inout)            :: infz(*)
            integer, intent(inout)            :: kronr(*)
            integer, intent(inout)            :: kronl(*)
            double precision, intent(inout)   :: af(ldaf, *)
            integer, intent(inout)            :: ldaf
            double precision, intent(inout)   :: bf(ldbf, *)
            integer, intent(inout)            :: ldbf
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab08nd
    end interface
    public :: ab08nd
    
    interface 
        subroutine ab08nw(equil, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, nfz, nrank, niz, dinfz, &
                       nkror, ninfe, nkrol, infz, kronr, infe, kronl, e, &
                       lde, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: equil
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nfz
            integer, intent(inout)            :: nrank
            integer, intent(inout)            :: niz
            integer, intent(inout)            :: dinfz
            integer, intent(inout)            :: nkror
            integer, intent(inout)            :: ninfe
            integer, intent(inout)            :: nkrol
            integer, intent(inout)            :: infz(*)
            integer, intent(inout)            :: kronr(*)
            integer, intent(inout)            :: infe(*)
            integer, intent(inout)            :: kronl(*)
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab08nw
    end interface
    public :: ab08nw
    
    interface 
        subroutine ab08nx(n, m, p, ro, sigma, svlmax, abcd, ldabcd, &
                       ninfz, infz, kronl, mu, nu, nkrol, tol, iwork, &
                       dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ro
            integer, intent(inout)            :: sigma
            double precision, intent(inout)   :: svlmax
            double precision, intent(inout)   :: abcd(ldabcd, *)
            integer, intent(inout)            :: ldabcd
            integer, intent(inout)            :: ninfz
            integer, intent(inout)            :: infz(*)
            integer, intent(inout)            :: kronl(*)
            integer, intent(inout)            :: mu
            integer, intent(inout)            :: nu
            integer, intent(inout)            :: nkrol
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab08nx
    end interface
    public :: ab08nx
    
    interface 
        subroutine ab08ny(first, n, m, p, svlmax, abcd, ldabcd, ninfz, &
                       nr, pr, dinfz, nkronl, infz, kronl, tol, iwork, &
                       dwork, ldwork, info)
            logical, intent(inout)            :: first
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: svlmax
            double precision, intent(inout)   :: abcd(ldabcd, *)
            integer, intent(inout)            :: ldabcd
            integer, intent(inout)            :: ninfz
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: pr
            integer, intent(inout)            :: dinfz
            integer, intent(inout)            :: nkronl
            integer, intent(inout)            :: infz(*)
            integer, intent(inout)            :: kronl(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab08ny
    end interface
    public :: ab08ny
    
    interface 
        subroutine ab08nz(equil, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, nu, rank, dinfz, nkror, &
                       nkrol, infz, kronr, kronl, af, ldaf, bf, ldbf, &
                       tol, iwork, dwork, zwork, lzwork, info)
            character, intent(inout)          :: equil
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(inout)            :: ldc
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nu
            integer, intent(inout)            :: rank
            integer, intent(inout)            :: dinfz
            integer, intent(inout)            :: nkror
            integer, intent(inout)            :: nkrol
            integer, intent(inout)            :: infz(*)
            integer, intent(inout)            :: kronr(*)
            integer, intent(inout)            :: kronl(*)
            complex*16, intent(inout)         :: af(ldaf, *)
            integer, intent(inout)            :: ldaf
            complex*16, intent(inout)         :: bf(ldbf, *)
            integer, intent(inout)            :: ldbf
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine ab08nz
    end interface
    public :: ab08nz
    
    interface 
        subroutine ab09ad(dico, job, equil, ordsel, n, m, p, nr, &
                       a, lda, b, ldb, c, ldc, hsv, tol, &
                       iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: job
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09ad
    end interface
    public :: ab09ad
    
    interface 
        subroutine ab09ax(dico, job, ordsel, n, m, p, nr, a, &
                       lda, b, ldb, c, ldc, hsv, t, ldt, &
                       ti, ldti, tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: job
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: ti(ldti, *)
            integer, intent(inout)            :: ldti
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09ax
    end interface
    public :: ab09ax
    
    interface 
        subroutine ab09bd(dico, job, equil, ordsel, n, m, p, nr, &
                       a, lda, b, ldb, c, ldc, d, ldd, &
                       hsv, tol1, tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: job
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09bd
    end interface
    public :: ab09bd
    
    interface 
        subroutine ab09bx(dico, job, ordsel, n, m, p, nr, a, &
                       lda, b, ldb, c, ldc, d, ldd, hsv, &
                       t, ldt, ti, ldti, tol1, tol2, iwork, dwork, &
                       ldwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: job
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: ti(ldti, *)
            integer, intent(inout)            :: ldti
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09bx
    end interface
    public :: ab09bx
    
    interface 
        subroutine ab09cd(dico, equil, ordsel, n, m, p, nr, a, &
                       lda, b, ldb, c, ldc, d, ldd, hsv, &
                       tol1, tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09cd
    end interface
    public :: ab09cd
    
    interface 
        subroutine ab09cx(dico, ordsel, n, m, p, nr, a, lda, &
                       b, ldb, c, ldc, d, ldd, hsv, tol1, &
                       tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09cx
    end interface
    public :: ab09cx
    
    interface 
        subroutine ab09dd(dico, n, m, p, nr, a, lda, b, &
                       ldb, c, ldc, d, ldd, rcond, iwork, dwork, &
                       info)
            character, intent(inout)          :: dico
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: rcond
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine ab09dd
    end interface
    public :: ab09dd
    
    interface 
        subroutine ab09ed(dico, equil, ordsel, n, m, p, nr, alpha, &
                       a, lda, b, ldb, c, ldc, d, ldd, &
                       ns, hsv, tol1, tol2, iwork, dwork, ldwork, iwarn, &
                       info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: ns
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09ed
    end interface
    public :: ab09ed
    
    interface 
        subroutine ab09fd(dico, jobcf, fact, jobmr, equil, ordsel, n, m, &
                       p, nr, alpha, a, lda, b, ldb, c, &
                       ldc, nq, hsv, tol1, tol2, iwork, dwork, ldwork, &
                       iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobcf
            character, intent(inout)          :: fact
            character, intent(inout)          :: jobmr
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: nq
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09fd
    end interface
    public :: ab09fd
    
    interface 
        subroutine ab09gd(dico, jobcf, fact, jobmr, equil, ordsel, n, m, &
                       p, nr, alpha, a, lda, b, ldb, c, &
                       ldc, d, ldd, nq, hsv, tol1, tol2, tol3, &
                       iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobcf
            character, intent(inout)          :: fact
            character, intent(inout)          :: jobmr
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nq
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            double precision, intent(inout)   :: tol3
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09gd
    end interface
    public :: ab09gd
    
    interface 
        subroutine ab09hd(dico, job, equil, ordsel, n, m, p, nr, &
                       alpha, beta, a, lda, b, ldb, c, ldc, &
                       d, ldd, ns, hsv, tol1, tol2, iwork, dwork, &
                       ldwork, bwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: job
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: ns
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09hd
    end interface
    public :: ab09hd
    
    interface 
        subroutine ab09hx(dico, job, ordsel, n, m, p, nr, a, &
                       lda, b, ldb, c, ldc, d, ldd, hsv, &
                       t, ldt, ti, ldti, tol1, tol2, iwork, dwork, &
                       ldwork, bwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: job
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: ti(ldti, *)
            integer, intent(inout)            :: ldti
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09hx
    end interface
    public :: ab09hx
    
    interface 
        subroutine ab09hy(n, m, p, a, lda, b, ldb, c, &
                       ldc, d, ldd, scalec, scaleo, s, lds, r, &
                       ldr, iwork, dwork, ldwork, bwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: scalec
            double precision, intent(inout)   :: scaleo
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine ab09hy
    end interface
    public :: ab09hy
    
    interface 
        subroutine ab09id(dico, jobc, jobo, job, weight, equil, ordsel, n, &
                       m, p, nv, pv, nw, mw, nr, alpha, &
                       alphac, alphao, a, lda, b, ldb, c, ldc, &
                       d, ldd, av, ldav, bv, ldbv, cv, ldcv, &
                       dv, lddv, aw, ldaw, bw, ldbw, cw, ldcw, &
                       dw, lddw, ns, hsv, tol1, tol2, iwork, dwork, &
                       ldwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobc
            character, intent(inout)          :: jobo
            character, intent(inout)          :: job
            character, intent(inout)          :: weight
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nv
            integer, intent(inout)            :: pv
            integer, intent(inout)            :: nw
            integer, intent(inout)            :: mw
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: alphac
            double precision, intent(inout)   :: alphao
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: av(ldav, *)
            integer, intent(inout)            :: ldav
            double precision, intent(inout)   :: bv(ldbv, *)
            integer, intent(inout)            :: ldbv
            double precision, intent(inout)   :: cv(ldcv, *)
            integer, intent(inout)            :: ldcv
            double precision, intent(inout)   :: dv(lddv, *)
            integer, intent(inout)            :: lddv
            double precision, intent(inout)   :: aw(ldaw, *)
            integer, intent(inout)            :: ldaw
            double precision, intent(inout)   :: bw(ldbw, *)
            integer, intent(inout)            :: ldbw
            double precision, intent(inout)   :: cw(ldcw, *)
            integer, intent(inout)            :: ldcw
            double precision, intent(inout)   :: dw(lddw, *)
            integer, intent(inout)            :: lddw
            integer, intent(inout)            :: ns
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09id
    end interface
    public :: ab09id
    
    interface 
        subroutine ab09ix(dico, job, fact, ordsel, n, m, p, nr, &
                       scalec, scaleo, a, lda, b, ldb, c, ldc, &
                       d, ldd, ti, ldti, t, ldt, nminr, hsv, &
                       tol1, tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: job
            character, intent(inout)          :: fact
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: scalec
            double precision, intent(inout)   :: scaleo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: ti(ldti, *)
            integer, intent(inout)            :: ldti
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            integer, intent(inout)            :: nminr
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09ix
    end interface
    public :: ab09ix
    
    interface 
        subroutine ab09iy(dico, jobc, jobo, weight, n, m, p, nv, &
                       pv, nw, mw, alphac, alphao, a, lda, b, &
                       ldb, c, ldc, av, ldav, bv, ldbv, cv, &
                       ldcv, dv, lddv, aw, ldaw, bw, ldbw, cw, &
                       ldcw, dw, lddw, scalec, scaleo, s, lds, r, &
                       ldr, dwork, ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobc
            character, intent(inout)          :: jobo
            character, intent(inout)          :: weight
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nv
            integer, intent(inout)            :: pv
            integer, intent(inout)            :: nw
            integer, intent(inout)            :: mw
            double precision, intent(inout)   :: alphac
            double precision, intent(inout)   :: alphao
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: av(ldav, *)
            integer, intent(inout)            :: ldav
            double precision, intent(inout)   :: bv(ldbv, *)
            integer, intent(inout)            :: ldbv
            double precision, intent(inout)   :: cv(ldcv, *)
            integer, intent(inout)            :: ldcv
            double precision, intent(inout)   :: dv(lddv, *)
            integer, intent(inout)            :: lddv
            double precision, intent(inout)   :: aw(ldaw, *)
            integer, intent(inout)            :: ldaw
            double precision, intent(inout)   :: bw(ldbw, *)
            integer, intent(inout)            :: ldbw
            double precision, intent(inout)   :: cw(ldcw, *)
            integer, intent(inout)            :: ldcw
            double precision, intent(inout)   :: dw(lddw, *)
            integer, intent(inout)            :: lddw
            double precision, intent(inout)   :: scalec
            double precision, intent(inout)   :: scaleo
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab09iy
    end interface
    public :: ab09iy
    
    interface 
        subroutine ab09jd(jobv, jobw, jobinv, dico, equil, ordsel, n, nv, &
                       nw, m, p, nr, alpha, a, lda, b, &
                       ldb, c, ldc, d, ldd, av, ldav, bv, &
                       ldbv, cv, ldcv, dv, lddv, aw, ldaw, bw, &
                       ldbw, cw, ldcw, dw, lddw, ns, hsv, tol1, &
                       tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: jobv
            character, intent(inout)          :: jobw
            character, intent(inout)          :: jobinv
            character, intent(inout)          :: dico
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nv
            integer, intent(inout)            :: nw
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: av(ldav, *)
            integer, intent(inout)            :: ldav
            double precision, intent(inout)   :: bv(ldbv, *)
            integer, intent(inout)            :: ldbv
            double precision, intent(inout)   :: cv(ldcv, *)
            integer, intent(inout)            :: ldcv
            double precision, intent(inout)   :: dv(lddv, *)
            integer, intent(inout)            :: lddv
            double precision, intent(inout)   :: aw(ldaw, *)
            integer, intent(inout)            :: ldaw
            double precision, intent(inout)   :: bw(ldbw, *)
            integer, intent(inout)            :: ldbw
            double precision, intent(inout)   :: cw(ldcw, *)
            integer, intent(inout)            :: ldcw
            double precision, intent(inout)   :: dw(lddw, *)
            integer, intent(inout)            :: lddw
            integer, intent(inout)            :: ns
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09jd
    end interface
    public :: ab09jd
    
    interface 
        subroutine ab09jv(job, dico, jobev, stbchk, n, m, p, nv, &
                       pv, a, lda, b, ldb, c, ldc, d, &
                       ldd, av, ldav, ev, ldev, bv, ldbv, cv, &
                       ldcv, dv, lddv, iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobev
            character, intent(inout)          :: stbchk
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nv
            integer, intent(inout)            :: pv
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: av(ldav, *)
            integer, intent(inout)            :: ldav
            double precision, intent(inout)   :: ev(ldev, *)
            integer, intent(inout)            :: ldev
            double precision, intent(inout)   :: bv(ldbv, *)
            integer, intent(inout)            :: ldbv
            double precision, intent(inout)   :: cv(ldcv, *)
            integer, intent(inout)            :: ldcv
            double precision, intent(inout)   :: dv(lddv, *)
            integer, intent(inout)            :: lddv
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab09jv
    end interface
    public :: ab09jv
    
    interface 
        subroutine ab09jw(job, dico, jobew, stbchk, n, m, p, nw, &
                       mw, a, lda, b, ldb, c, ldc, d, &
                       ldd, aw, ldaw, ew, ldew, bw, ldbw, cw, &
                       ldcw, dw, lddw, iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobew
            character, intent(inout)          :: stbchk
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nw
            integer, intent(inout)            :: mw
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: aw(ldaw, *)
            integer, intent(inout)            :: ldaw
            double precision, intent(inout)   :: ew(ldew, *)
            integer, intent(inout)            :: ldew
            double precision, intent(inout)   :: bw(ldbw, *)
            integer, intent(inout)            :: ldbw
            double precision, intent(inout)   :: cw(ldcw, *)
            integer, intent(inout)            :: ldcw
            double precision, intent(inout)   :: dw(lddw, *)
            integer, intent(inout)            :: lddw
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab09jw
    end interface
    public :: ab09jw
    
    interface 
        subroutine ab09jx(dico, stdom, evtype, n, alpha, er, ei, ed, &
                       tolinf, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: stdom
            character, intent(inout)          :: evtype
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: er(*)
            double precision, intent(inout)   :: ei(*)
            double precision, intent(inout)   :: ed(*)
            double precision, intent(inout)   :: tolinf
            integer, intent(inout)            :: info
        end subroutine ab09jx
    end interface
    public :: ab09jx
    
    interface 
        subroutine ab09kd(job, dico, weight, equil, ordsel, n, nv, nw, &
                       m, p, nr, alpha, a, lda, b, ldb, &
                       c, ldc, d, ldd, av, ldav, bv, ldbv, &
                       cv, ldcv, dv, lddv, aw, ldaw, bw, ldbw, &
                       cw, ldcw, dw, lddw, ns, hsv, tol1, tol2, &
                       iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: dico
            character, intent(inout)          :: weight
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nv
            integer, intent(inout)            :: nw
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: av(ldav, *)
            integer, intent(inout)            :: ldav
            double precision, intent(inout)   :: bv(ldbv, *)
            integer, intent(inout)            :: ldbv
            double precision, intent(inout)   :: cv(ldcv, *)
            integer, intent(inout)            :: ldcv
            double precision, intent(inout)   :: dv(lddv, *)
            integer, intent(inout)            :: lddv
            double precision, intent(inout)   :: aw(ldaw, *)
            integer, intent(inout)            :: ldaw
            double precision, intent(inout)   :: bw(ldbw, *)
            integer, intent(inout)            :: ldbw
            double precision, intent(inout)   :: cw(ldcw, *)
            integer, intent(inout)            :: ldcw
            double precision, intent(inout)   :: dw(lddw, *)
            integer, intent(inout)            :: lddw
            integer, intent(inout)            :: ns
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09kd
    end interface
    public :: ab09kd
    
    interface 
        subroutine ab09kx(job, dico, weight, n, nv, nw, m, p, &
                       a, lda, b, ldb, c, ldc, d, ldd, &
                       av, ldav, bv, ldbv, cv, ldcv, dv, lddv, &
                       aw, ldaw, bw, ldbw, cw, ldcw, dw, lddw, &
                       dwork, ldwork, iwarn, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: dico
            character, intent(inout)          :: weight
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nv
            integer, intent(inout)            :: nw
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: av(ldav, *)
            integer, intent(inout)            :: ldav
            double precision, intent(inout)   :: bv(ldbv, *)
            integer, intent(inout)            :: ldbv
            double precision, intent(inout)   :: cv(ldcv, *)
            integer, intent(inout)            :: ldcv
            double precision, intent(inout)   :: dv(lddv, *)
            integer, intent(inout)            :: lddv
            double precision, intent(inout)   :: aw(ldaw, *)
            integer, intent(inout)            :: ldaw
            double precision, intent(inout)   :: bw(ldbw, *)
            integer, intent(inout)            :: ldbw
            double precision, intent(inout)   :: cw(ldcw, *)
            integer, intent(inout)            :: ldcw
            double precision, intent(inout)   :: dw(lddw, *)
            integer, intent(inout)            :: lddw
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09kx
    end interface
    public :: ab09kx
    
    interface 
        subroutine ab09md(dico, job, equil, ordsel, n, m, p, nr, &
                       alpha, a, lda, b, ldb, c, ldc, ns, &
                       hsv, tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: job
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: ns
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09md
    end interface
    public :: ab09md
    
    interface 
        subroutine ab09nd(dico, job, equil, ordsel, n, m, p, nr, &
                       alpha, a, lda, b, ldb, c, ldc, d, &
                       ldd, ns, hsv, tol1, tol2, iwork, dwork, ldwork, &
                       iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: job
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: ns
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab09nd
    end interface
    public :: ab09nd
    
    interface 
        double precision function ab13ad (dico,equil,n,m,p,alpha,a,lda, &
                       b,ldb,c,ldc,ns,hsv,dwork,ldwork, &
                       info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: equil
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: ns
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end function ab13ad
    end interface
    public :: ab13ad
    
    interface 
        double precision function ab13ax (dico,n,m,p,a,lda,b,ldb, &
                       c,ldc,hsv,dwork,ldwork,info)
            character, intent(inout)          :: dico
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end function ab13ax
    end interface
    public :: ab13ax
    
    interface 
        double precision function ab13bd (dico,jobn,n,m,p,a,lda,b, &
                       ldb,c,ldc,d,ldd,nq,tol,dwork, &
                       ldwork,iwarn,info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobn
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nq
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end function ab13bd
    end interface
    public :: ab13bd
    
    interface 
        double precision function ab13cd (n,m,np,a,lda,b,ldb,c, &
                       ldc,d,ldd,tol,iwork,dwork,ldwork,cwork, &
                       lcwork,bwork,info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: cwork(*)
            integer, intent(inout)            :: lcwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end function ab13cd
    end interface
    public :: ab13cd
    
    interface 
        subroutine ab13dd(dico, jobe, equil, jobd, n, m, p, fpeak, &
                       a, lda, e, lde, b, ldb, c, ldc, &
                       d, ldd, gpeak, tol, iwork, dwork, ldwork, cwork, &
                       lcwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobe
            character, intent(inout)          :: equil
            character, intent(inout)          :: jobd
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: fpeak(2)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: gpeak(2)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: cwork(*)
            integer, intent(inout)            :: lcwork
            integer, intent(inout)            :: info
        end subroutine ab13dd
    end interface
    public :: ab13dd
    
    interface 
        double precision function ab13dx (dico,jobe,jobd,n,m,p,omega,a, &
                       lda,e,lde,b,ldb,c,ldc,d, &
                       ldd,iwork,dwork,ldwork,zwork,lzwork,info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobe
            character, intent(inout)          :: jobd
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: omega
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end function ab13dx
    end interface
    public :: ab13dx
    
    interface 
        subroutine ab13ed(n, a, lda, low, high, tol, dwork, ldwork, &
                       info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: low
            double precision, intent(inout)   :: high
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ab13ed
    end interface
    public :: ab13ed
    
    interface 
        subroutine ab13fd(n, a, lda, beta, omega, tol, dwork, ldwork, &
                       cwork, lcwork, info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: omega
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: cwork(*)
            integer, intent(inout)            :: lcwork
            integer, intent(inout)            :: info
        end subroutine ab13fd
    end interface
    public :: ab13fd
    
    interface 
        subroutine ab13hd(dico, jobe, equil, jobd, ckprop, reduce, poles, n, &
                       m, p, ranke, fpeak, a, lda, e, lde, &
                       b, ldb, c, ldc, d, ldd, nr, gpeak, &
                       tol, iwork, dwork, ldwork, zwork, lzwork, bwork, iwarn, &
                       info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobe
            character, intent(inout)          :: equil
            character, intent(inout)          :: jobd
            character, intent(inout)          :: ckprop
            character, intent(inout)          :: reduce
            character, intent(inout)          :: poles
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ranke
            double precision, intent(inout)   :: fpeak(2)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: gpeak(2)
            double precision, intent(inout)   :: tol(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ab13hd
    end interface
    public :: ab13hd
    
    interface 
        logical function ab13id (jobsys,jobeig,equil,cksing,restor,update,n,m, &
                       p,a,lda,e,lde,b,ldb,c, &
                       ldc,nr,ranke,tol,iwork,dwork,ldwork,iwarn, &
                       info)
            character, intent(inout)          :: jobsys
            character, intent(inout)          :: jobeig
            character, intent(inout)          :: equil
            character, intent(inout)          :: cksing
            character, intent(inout)          :: restor
            character, intent(inout)          :: update
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: ranke
            double precision, intent(inout)   :: tol(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end function ab13id
    end interface
    public :: ab13id
    
    interface 
        subroutine ab13md(fact, n, z, ldz, m, nblock, itype, x, &
                       bound, d, g, iwork, dwork, ldwork, zwork, lzwork, &
                       info)
            character, intent(inout)          :: fact
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: m
            integer, intent(inout)            :: nblock(*)
            integer, intent(inout)            :: itype(*)
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: bound
            double precision, intent(inout)   :: d(*)
            double precision, intent(inout)   :: g(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine ab13md
    end interface
    public :: ab13md
    
    interface 
        subroutine ab8nxz(n, m, p, ro, sigma, svlmax, abcd, ldabcd, &
                       ninfz, infz, kronl, mu, nu, nkrol, tol, iwork, &
                       dwork, zwork, lzwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ro
            integer, intent(inout)            :: sigma
            double precision, intent(inout)   :: svlmax
            complex*16, intent(inout)         :: abcd(ldabcd, *)
            integer, intent(inout)            :: ldabcd
            integer, intent(inout)            :: ninfz
            integer, intent(inout)            :: infz(*)
            integer, intent(inout)            :: kronl(*)
            integer, intent(inout)            :: mu
            integer, intent(inout)            :: nu
            integer, intent(inout)            :: nkrol
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine ab8nxz
    end interface
    public :: ab8nxz
    
    interface 
        subroutine ag07bd(jobe, n, m, a, lda, e, lde, b, &
                       ldb, c, ldc, d, ldd, ai, ldai, ei, &
                       ldei, bi, ldbi, ci, ldci, di, lddi, info)
            character, intent(inout)          :: jobe
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: ai(ldai, *)
            integer, intent(inout)            :: ldai
            double precision, intent(inout)   :: ei(ldei, *)
            integer, intent(inout)            :: ldei
            double precision, intent(inout)   :: bi(ldbi, *)
            integer, intent(inout)            :: ldbi
            double precision, intent(inout)   :: ci(ldci, *)
            integer, intent(inout)            :: ldci
            double precision, intent(inout)   :: di(lddi, *)
            integer, intent(inout)            :: lddi
            integer, intent(inout)            :: info
        end subroutine ag07bd
    end interface
    public :: ag07bd
    
    interface 
        subroutine ag08bd(equil, l, n, m, p, a, lda, e, &
                       lde, b, ldb, c, ldc, d, ldd, nfz, &
                       nrank, niz, dinfz, nkror, ninfe, nkrol, infz, kronr, &
                       infe, kronl, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: equil
            integer, intent(inout)            :: l
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nfz
            integer, intent(inout)            :: nrank
            integer, intent(inout)            :: niz
            integer, intent(inout)            :: dinfz
            integer, intent(inout)            :: nkror
            integer, intent(inout)            :: ninfe
            integer, intent(inout)            :: nkrol
            integer, intent(inout)            :: infz(*)
            integer, intent(inout)            :: kronr(*)
            integer, intent(inout)            :: infe(*)
            integer, intent(inout)            :: kronl(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ag08bd
    end interface
    public :: ag08bd
    
    interface 
        subroutine ag08by(first, n, m, p, svlmax, abcd, ldabcd, e, &
                       lde, nr, pr, ninfz, dinfz, nkronl, infz, kronl, &
                       tol, iwork, dwork, ldwork, info)
            logical, intent(inout)            :: first
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: svlmax
            double precision, intent(inout)   :: abcd(ldabcd, *)
            integer, intent(inout)            :: ldabcd
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: pr
            integer, intent(inout)            :: ninfz
            integer, intent(inout)            :: dinfz
            integer, intent(inout)            :: nkronl
            integer, intent(inout)            :: infz(*)
            integer, intent(inout)            :: kronl(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine ag08by
    end interface
    public :: ag08by
    
    interface 
        subroutine ag08bz(equil, l, n, m, p, a, lda, e, &
                       lde, b, ldb, c, ldc, d, ldd, nfz, &
                       nrank, niz, dinfz, nkror, ninfe, nkrol, infz, kronr, &
                       infe, kronl, tol, iwork, dwork, zwork, lzwork, info)
            character, intent(inout)          :: equil
            integer, intent(inout)            :: l
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(inout)            :: lde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(inout)            :: ldc
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nfz
            integer, intent(inout)            :: nrank
            integer, intent(inout)            :: niz
            integer, intent(inout)            :: dinfz
            integer, intent(inout)            :: nkror
            integer, intent(inout)            :: ninfe
            integer, intent(inout)            :: nkrol
            integer, intent(inout)            :: infz(*)
            integer, intent(inout)            :: kronr(*)
            integer, intent(inout)            :: infe(*)
            integer, intent(inout)            :: kronl(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine ag08bz
    end interface
    public :: ag08bz
    
    interface 
        subroutine ag8byz(first, n, m, p, svlmax, abcd, ldabcd, e, &
                       lde, nr, pr, ninfz, dinfz, nkronl, infz, kronl, &
                       tol, iwork, dwork, zwork, lzwork, info)
            logical, intent(inout)            :: first
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: svlmax
            complex*16, intent(inout)         :: abcd(ldabcd, *)
            integer, intent(inout)            :: ldabcd
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(inout)            :: lde
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: pr
            integer, intent(inout)            :: ninfz
            integer, intent(inout)            :: dinfz
            integer, intent(inout)            :: nkronl
            integer, intent(inout)            :: infz(*)
            integer, intent(inout)            :: kronl(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine ag8byz
    end interface
    public :: ag8byz
    
    interface 
        subroutine bb01ad(def, nr, dpar, ipar, bpar, chpar, vec, n, &
                       m, p, a, lda, b, ldb, c, ldc, &
                       g, ldg, q, ldq, x, ldx, dwork, ldwork, &
                       info)
            character, intent(inout)          :: def
            integer, intent(inout)            :: nr(2)
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ipar(4)
            logical, intent(inout)            :: bpar(6)
            character, intent(inout)          :: chpar*(*)
            logical, intent(inout)            :: vec(9)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: g(*)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: q(*)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine bb01ad
    end interface
    public :: bb01ad
    
    interface 
        subroutine bb02ad(def, nr, dpar, ipar, bpar, chpar, vec, n, &
                       m, p, a, lda, b, ldb, c, ldc, &
                       q, ldq, r, ldr, s, lds, x, ldx, &
                       dwork, ldwork, info)
            character, intent(inout)          :: def
            integer, intent(inout)            :: nr(2)
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ipar(3)
            logical, intent(inout)            :: bpar(7)
            character, intent(inout)          :: chpar*255
            logical, intent(inout)            :: vec(10)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(*)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: r(*)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine bb02ad
    end interface
    public :: bb02ad
    
    interface 
        subroutine bb03ad(def, nr, dpar, ipar, vec, n, m, e, &
                       lde, a, lda, y, ldy, b, ldb, x, &
                       ldx, u, ldu, note, dwork, ldwork, info)
            character, intent(inout)          :: def
            integer, intent(inout)            :: nr(*)
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ipar(*)
            logical, intent(inout)            :: vec(8)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            character*70, intent(inout)       :: note
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine bb03ad
    end interface
    public :: bb03ad
    
    interface 
        subroutine bb04ad(def, nr, dpar, ipar, vec, n, m, e, &
                       lde, a, lda, y, ldy, b, ldb, x, &
                       ldx, u, ldu, note, dwork, ldwork, info)
            character, intent(inout)          :: def
            integer, intent(inout)            :: nr(*)
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ipar(*)
            logical, intent(inout)            :: vec(8)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            character*70, intent(inout)       :: note
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine bb04ad
    end interface
    public :: bb04ad
    
    interface 
        subroutine bd01ad(def, nr, dpar, ipar, vec, n, m, p, &
                       e, lde, a, lda, b, ldb, c, ldc, &
                       d, ldd, note, dwork, ldwork, info)
            character, intent(inout)          :: def
            integer, intent(inout)            :: nr(*)
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ipar(*)
            logical, intent(inout)            :: vec(8)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            character*70, intent(inout)       :: note
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine bd01ad
    end interface
    public :: bd01ad
    
    interface 
        subroutine bd02ad(def, nr, dpar, ipar, vec, n, m, p, &
                       e, lde, a, lda, b, ldb, c, ldc, &
                       d, ldd, note, dwork, ldwork, info)
            character, intent(inout)          :: def
            integer, intent(inout)            :: nr(*)
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ipar(*)
            logical, intent(inout)            :: vec(8)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            character*70, intent(inout)       :: note
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine bd02ad
    end interface
    public :: bd02ad
    
    interface 
        subroutine de01od(conv, n, a, b, info)
            character, intent(inout)          :: conv
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(*)
            double precision, intent(inout)   :: b(*)
            integer, intent(inout)            :: info
        end subroutine de01od
    end interface
    public :: de01od
    
    interface 
        subroutine de01pd(conv, wght, n, a, b, w, info)
            character, intent(inout)          :: conv
            character, intent(inout)          :: wght
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(*)
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: w(*)
            integer, intent(inout)            :: info
        end subroutine de01pd
    end interface
    public :: de01pd
    
    interface 
        logical function delctg (par1,par2,par3)
            double precision, intent(inout)   :: par1
            double precision, intent(inout)   :: par2
            double precision, intent(inout)   :: par3
        end function delctg
    end interface
    public :: delctg
    
    interface 
        subroutine df01md(sico, n, dt, a, dwork, info)
            character, intent(inout)          :: sico
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: dt
            double precision, intent(inout)   :: a(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine df01md
    end interface
    public :: df01md
    
    interface 
        subroutine dg01md(indi, n, xr, xi, info)
            character, intent(inout)          :: indi
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: xr(*)
            double precision, intent(inout)   :: xi(*)
            integer, intent(inout)            :: info
        end subroutine dg01md
    end interface
    public :: dg01md
    
    interface 
        subroutine dg01nd(indi, n, xr, xi, info)
            character, intent(inout)          :: indi
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: xr(*)
            double precision, intent(inout)   :: xi(*)
            integer, intent(inout)            :: info
        end subroutine dg01nd
    end interface
    public :: dg01nd
    
    interface 
        subroutine dg01ny(indi, n, xr, xi)
            character, intent(inout)          :: indi
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: xr(*)
            double precision, intent(inout)   :: xi(*)
        end subroutine dg01ny
    end interface
    public :: dg01ny
    
    interface 
        subroutine dg01od(scr, wght, n, a, w, info)
            character, intent(inout)          :: scr
            character, intent(inout)          :: wght
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(*)
            double precision, intent(inout)   :: w(*)
            integer, intent(inout)            :: info
        end subroutine dg01od
    end interface
    public :: dg01od
    
    interface 
        subroutine dk01md(type, n, a, info)
            character, intent(inout)          :: type
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(*)
            integer, intent(inout)            :: info
        end subroutine dk01md
    end interface
    public :: dk01md
    
    interface 
        subroutine fb01qd(jobk, multbq, n, m, p, s, lds, a, &
                       lda, b, ldb, q, ldq, c, ldc, r, &
                       ldr, k, ldk, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: jobk
            character, intent(inout)          :: multbq
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: k(ldk, *)
            integer, intent(inout)            :: ldk
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine fb01qd
    end interface
    public :: fb01qd
    
    interface 
        subroutine fb01rd(jobk, multbq, n, m, p, s, lds, a, &
                       lda, b, ldb, q, ldq, c, ldc, r, &
                       ldr, k, ldk, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: jobk
            character, intent(inout)          :: multbq
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: k(ldk, *)
            integer, intent(inout)            :: ldk
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine fb01rd
    end interface
    public :: fb01rd
    
    interface 
        subroutine fb01sd(jobx, multab, multrc, n, m, p, sinv, ldsinv, &
                       ainv, ldainv, b, ldb, rinv, ldrinv, c, ldc, &
                       qinv, ldqinv, x, rinvy, z, e, tol, iwork, &
                       dwork, ldwork, info)
            character, intent(inout)          :: jobx
            character, intent(inout)          :: multab
            character, intent(inout)          :: multrc
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: sinv(ldsinv, *)
            integer, intent(inout)            :: ldsinv
            double precision, intent(inout)   :: ainv(ldainv, *)
            integer, intent(inout)            :: ldainv
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: rinv(ldrinv, *)
            integer, intent(inout)            :: ldrinv
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: qinv(ldqinv, *)
            integer, intent(inout)            :: ldqinv
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: rinvy(*)
            double precision, intent(inout)   :: z(*)
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine fb01sd
    end interface
    public :: fb01sd
    
    interface 
        subroutine fb01td(jobx, multrc, n, m, p, sinv, ldsinv, ainv, &
                       ldainv, ainvb, ldainb, rinv, ldrinv, c, ldc, qinv, &
                       ldqinv, x, rinvy, z, e, tol, iwork, dwork, &
                       ldwork, info)
            character, intent(inout)          :: jobx
            character, intent(inout)          :: multrc
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: sinv(ldsinv, *)
            integer, intent(inout)            :: ldsinv
            double precision, intent(inout)   :: ainv(ldainv, *)
            integer, intent(inout)            :: ldainv
            double precision, intent(inout)   :: ainvb(ldainb, *)
            integer, intent(inout)            :: ldainb
            double precision, intent(inout)   :: rinv(ldrinv, *)
            integer, intent(inout)            :: ldrinv
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: qinv(ldqinv, *)
            integer, intent(inout)            :: ldqinv
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: rinvy(*)
            double precision, intent(inout)   :: z(*)
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine fb01td
    end interface
    public :: fb01td
    
    interface 
        subroutine fb01vd(n, m, l, p, ldp, a, lda, b, &
                       ldb, c, ldc, q, ldq, r, ldr, k, &
                       ldk, tol, iwork, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: p(ldp, *)
            integer, intent(inout)            :: ldp
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: k(ldk, *)
            integer, intent(inout)            :: ldk
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine fb01vd
    end interface
    public :: fb01vd
    
    interface 
        subroutine fd01ad(jp, l, lambda, xin, yin, efor, xf, epsbck, &
                       cteta, steta, yq, epos, eout, salph, iwarn, info)
            character, intent(inout)          :: jp
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: lambda
            double precision, intent(inout)   :: xin
            double precision, intent(inout)   :: yin
            double precision, intent(inout)   :: efor
            double precision, intent(inout)   :: xf(*)
            double precision, intent(inout)   :: epsbck(*)
            double precision, intent(inout)   :: cteta(*)
            double precision, intent(inout)   :: steta(*)
            double precision, intent(inout)   :: yq(*)
            double precision, intent(inout)   :: epos
            double precision, intent(inout)   :: eout
            double precision, intent(inout)   :: salph(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine fd01ad
    end interface
    public :: fd01ad
    
    interface 
        subroutine ib01ad(meth, alg, jobd, batch, conct, ctrl, nobr, m, &
                       l, nsmp, u, ldu, y, ldy, n, r, &
                       ldr, sv, rcond, tol, iwork, dwork, ldwork, iwarn, &
                       info)
            character, intent(inout)          :: meth
            character, intent(inout)          :: alg
            character, intent(inout)          :: jobd
            character, intent(inout)          :: batch
            character, intent(inout)          :: conct
            character, intent(inout)          :: ctrl
            integer, intent(inout)            :: nobr
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            integer, intent(inout)            :: nsmp
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: sv(*)
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib01ad
    end interface
    public :: ib01ad
    
    interface 
        subroutine ib01bd(meth, job, jobck, nobr, n, m, l, nsmpl, &
                       r, ldr, a, lda, c, ldc, b, ldb, &
                       d, ldd, q, ldq, ry, ldry, s, lds, &
                       k, ldk, tol, iwork, dwork, ldwork, bwork, iwarn, &
                       info)
            character, intent(inout)          :: meth
            character, intent(inout)          :: job
            character, intent(inout)          :: jobck
            integer, intent(inout)            :: nobr
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            integer, intent(inout)            :: nsmpl
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: ry(ldry, *)
            integer, intent(inout)            :: ldry
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: k(ldk, *)
            integer, intent(inout)            :: ldk
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib01bd
    end interface
    public :: ib01bd
    
    interface 
        subroutine ib01cd(jobx0, comuse, job, n, m, l, nsmp, a, &
                       lda, b, ldb, c, ldc, d, ldd, u, &
                       ldu, y, ldy, x0, v, ldv, tol, iwork, &
                       dwork, ldwork, iwarn, info)
            character, intent(inout)          :: jobx0
            character, intent(inout)          :: comuse
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            integer, intent(inout)            :: nsmp
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: x0(*)
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib01cd
    end interface
    public :: ib01cd
    
    interface 
        subroutine ib01md(meth, alg, batch, conct, nobr, m, l, nsmp, &
                       u, ldu, y, ldy, r, ldr, iwork, dwork, &
                       ldwork, iwarn, info)
            character, intent(inout)          :: meth
            character, intent(inout)          :: alg
            character, intent(inout)          :: batch
            character, intent(inout)          :: conct
            integer, intent(inout)            :: nobr
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            integer, intent(inout)            :: nsmp
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib01md
    end interface
    public :: ib01md
    
    interface 
        subroutine ib01my(meth, batch, conct, nobr, m, l, nsmp, u, &
                       ldu, y, ldy, r, ldr, iwork, dwork, ldwork, &
                       iwarn, info)
            character, intent(inout)          :: meth
            character, intent(inout)          :: batch
            character, intent(inout)          :: conct
            integer, intent(inout)            :: nobr
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            integer, intent(inout)            :: nsmp
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib01my
    end interface
    public :: ib01my
    
    interface 
        subroutine ib01nd(meth, jobd, nobr, m, l, r, ldr, sv, &
                       tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: meth
            character, intent(inout)          :: jobd
            integer, intent(inout)            :: nobr
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: sv(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib01nd
    end interface
    public :: ib01nd
    
    interface 
        subroutine ib01od(ctrl, nobr, l, sv, n, tol, iwarn, info)
            character, intent(inout)          :: ctrl
            integer, intent(inout)            :: nobr
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: sv(*)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib01od
    end interface
    public :: ib01od
    
    interface 
        subroutine ib01oy(ns, nmax, n, sv, info)
            integer, intent(inout)            :: ns
            integer, intent(inout)            :: nmax
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: sv(*)
            integer, intent(inout)            :: info
        end subroutine ib01oy
    end interface
    public :: ib01oy
    
    interface 
        subroutine ib01pd(meth, job, jobcv, nobr, n, m, l, nsmpl, &
                       r, ldr, a, lda, c, ldc, b, ldb, &
                       d, ldd, q, ldq, ry, ldry, s, lds, &
                       o, ldo, tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: meth
            character, intent(inout)          :: job
            character, intent(inout)          :: jobcv
            integer, intent(inout)            :: nobr
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            integer, intent(inout)            :: nsmpl
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: ry(ldry, *)
            integer, intent(inout)            :: ldry
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: o(ldo, *)
            integer, intent(inout)            :: ldo
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib01pd
    end interface
    public :: ib01pd
    
    interface 
        subroutine ib01px(job, nobr, n, m, l, uf, lduf, un, &
                       ldun, ul, ldul, pgal, ldpgal, k, ldk, r, &
                       ldr, x, b, ldb, d, ldd, tol, iwork, &
                       dwork, ldwork, iwarn, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: nobr
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: uf(lduf, *)
            integer, intent(inout)            :: lduf
            double precision, intent(inout)   :: un(ldun, *)
            integer, intent(inout)            :: ldun
            double precision, intent(inout)   :: ul(ldul, *)
            integer, intent(inout)            :: ldul
            double precision, intent(inout)   :: pgal(ldpgal, *)
            integer, intent(inout)            :: ldpgal
            double precision, intent(inout)   :: k(ldk, *)
            integer, intent(inout)            :: ldk
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib01px
    end interface
    public :: ib01px
    
    interface 
        subroutine ib01py(meth, job, nobr, n, m, l, rankr1, ul, &
                       ldul, r1, ldr1, tau1, pgal, ldpgal, k, ldk, &
                       r, ldr, h, ldh, b, ldb, d, ldd, &
                       tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: meth
            character, intent(inout)          :: job
            integer, intent(inout)            :: nobr
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            integer, intent(inout)            :: rankr1
            double precision, intent(inout)   :: ul(ldul, *)
            integer, intent(inout)            :: ldul
            double precision, intent(inout)   :: r1(ldr1, *)
            integer, intent(inout)            :: ldr1
            double precision, intent(inout)   :: tau1(*)
            double precision, intent(inout)   :: pgal(ldpgal, *)
            integer, intent(inout)            :: ldpgal
            double precision, intent(inout)   :: k(ldk, *)
            integer, intent(inout)            :: ldk
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib01py
    end interface
    public :: ib01py
    
    interface 
        subroutine ib01qd(jobx0, job, n, m, l, nsmp, a, lda, &
                       c, ldc, u, ldu, y, ldy, x0, b, &
                       ldb, d, ldd, tol, iwork, dwork, ldwork, iwarn, &
                       info)
            character, intent(inout)          :: jobx0
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            integer, intent(inout)            :: nsmp
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: x0(*)
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib01qd
    end interface
    public :: ib01qd
    
    interface 
        subroutine ib01rd(job, n, m, l, nsmp, a, lda, b, &
                       ldb, c, ldc, d, ldd, u, ldu, y, &
                       ldy, x0, tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            integer, intent(inout)            :: nsmp
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: x0(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib01rd
    end interface
    public :: ib01rd
    
    interface 
        subroutine ib03ad(init, alg, stor, nobr, m, l, nsmp, n, &
                       nn, itmax1, itmax2, nprint, u, ldu, y, ldy, &
                       x, lx, tol1, tol2, iwork, dwork, ldwork, iwarn, &
                       info)
            character, intent(inout)          :: init
            character, intent(inout)          :: alg
            character, intent(inout)          :: stor
            integer, intent(inout)            :: nobr
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            integer, intent(inout)            :: nsmp
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nn
            integer, intent(inout)            :: itmax1
            integer, intent(inout)            :: itmax2
            integer, intent(inout)            :: nprint
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: lx
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib03ad
    end interface
    public :: ib03ad
    
    interface 
        subroutine ib03bd(init, nobr, m, l, nsmp, n, nn, itmax1, &
                       itmax2, nprint, u, ldu, y, ldy, x, lx, &
                       tol1, tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: init
            integer, intent(inout)            :: nobr
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            integer, intent(inout)            :: nsmp
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nn
            integer, intent(inout)            :: itmax1
            integer, intent(inout)            :: itmax2
            integer, intent(inout)            :: nprint
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: lx
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine ib03bd
    end interface
    public :: ib03bd
    
    interface 
        logical function lfdum (x,y)
            double precision, intent(inout)   :: x
            double precision, intent(inout)   :: y
        end function lfdum
    end interface
    public :: lfdum
    
    interface 
        subroutine ma01ad(xr, xi, yr, yi)
            double precision, intent(inout)   :: xr
            double precision, intent(inout)   :: xi
            double precision, intent(inout)   :: yr
            double precision, intent(inout)   :: yi
        end subroutine ma01ad
    end interface
    public :: ma01ad
    
    interface 
        subroutine ma01bd(base, lgbas, k, s, a, inca, alpha, beta, &
                       scal)
            double precision, intent(inout)   :: base
            double precision, intent(inout)   :: lgbas
            integer, intent(inout)            :: k
            integer, intent(inout)            :: s(*)
            double precision, intent(inout)   :: a(*)
            integer, intent(inout)            :: inca
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            integer, intent(inout)            :: scal
        end subroutine ma01bd
    end interface
    public :: ma01bd
    
    interface 
        subroutine ma01bz(base, k, s, a, inca, alpha, beta, scal)
            double precision, intent(inout)   :: base
            integer, intent(inout)            :: k
            integer, intent(inout)            :: s(*)
            complex*16, intent(inout)         :: a(*)
            integer, intent(inout)            :: inca
            complex*16, intent(inout)         :: alpha
            complex*16, intent(inout)         :: beta
            integer, intent(inout)            :: scal
        end subroutine ma01bz
    end interface
    public :: ma01bz
    
    interface 
        integer function ma01cd (a,ia,b,ib)
            double precision, intent(inout)   :: a
            integer, intent(inout)            :: ia
            double precision, intent(inout)   :: b
            integer, intent(inout)            :: ib
        end function ma01cd
    end interface
    public :: ma01cd
    
    interface 
        subroutine ma01dd(ar1, ai1, ar2, ai2, eps, safemn, d)
            double precision, intent(inout)   :: ar1
            double precision, intent(inout)   :: ai1
            double precision, intent(inout)   :: ar2
            double precision, intent(inout)   :: ai2
            double precision, intent(inout)   :: eps
            double precision, intent(inout)   :: safemn
            double precision, intent(inout)   :: d
        end subroutine ma01dd
    end interface
    public :: ma01dd
    
    interface 
        subroutine ma01dz(ar1, ai1, b1, ar2, ai2, b2, eps, safemn, &
                       d1, d2, iwarn)
            double precision, intent(inout)   :: ar1
            double precision, intent(inout)   :: ai1
            double precision, intent(inout)   :: b1
            double precision, intent(inout)   :: ar2
            double precision, intent(inout)   :: ai2
            double precision, intent(inout)   :: b2
            double precision, intent(inout)   :: eps
            double precision, intent(inout)   :: safemn
            double precision, intent(inout)   :: d1
            double precision, intent(inout)   :: d2
            integer, intent(inout)            :: iwarn
        end subroutine ma01dz
    end interface
    public :: ma01dz
    
    interface 
        subroutine ma02ad(job, m, n, a, lda, b, ldb)
            character, intent(inout)          :: job
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
        end subroutine ma02ad
    end interface
    public :: ma02ad
    
    interface 
        subroutine ma02az(trans, job, m, n, a, lda, b, ldb)
            character, intent(inout)    :: trans
            character, intent(inout)    :: job
            integer, intent(inout)      :: m
            integer, intent(inout)      :: n
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(inout)      :: lda
            complex*16, intent(inout)   :: b(ldb, *)
            integer, intent(inout)      :: ldb
        end subroutine ma02az
    end interface
    public :: ma02az
    
    interface 
        subroutine ma02bd(side, m, n, a, lda)
            character, intent(inout)          :: side
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
        end subroutine ma02bd
    end interface
    public :: ma02bd
    
    interface 
        subroutine ma02bz(side, m, n, a, lda)
            character, intent(inout)    :: side
            integer, intent(inout)      :: m
            integer, intent(inout)      :: n
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(inout)      :: lda
        end subroutine ma02bz
    end interface
    public :: ma02bz
    
    interface 
        subroutine ma02cd(n, kl, ku, a, lda)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: kl
            integer, intent(inout)            :: ku
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
        end subroutine ma02cd
    end interface
    public :: ma02cd
    
    interface 
        subroutine ma02cz(n, kl, ku, a, lda)
            integer, intent(inout)      :: n
            integer, intent(inout)      :: kl
            integer, intent(inout)      :: ku
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(inout)      :: lda
        end subroutine ma02cz
    end interface
    public :: ma02cz
    
    interface 
        subroutine ma02dd(job, uplo, n, a, lda, ap)
            character, intent(inout)          :: job
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: ap(*)
        end subroutine ma02dd
    end interface
    public :: ma02dd
    
    interface 
        subroutine ma02ed(uplo, n, a, lda)
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
        end subroutine ma02ed
    end interface
    public :: ma02ed
    
    interface 
        subroutine ma02es(uplo, n, a, lda)
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
        end subroutine ma02es
    end interface
    public :: ma02es
    
    interface 
        subroutine ma02ez(uplo, trans, skew, n, a, lda)
            character, intent(inout)    :: uplo
            character, intent(inout)    :: trans
            character, intent(inout)    :: skew
            integer, intent(inout)      :: n
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(inout)      :: lda
        end subroutine ma02ez
    end interface
    public :: ma02ez
    
    interface 
        subroutine ma02fd(x1, x2, c, s, info)
            double precision, intent(inout)   :: x1
            double precision, intent(inout)   :: x2
            double precision, intent(inout)   :: c
            double precision, intent(inout)   :: s
            integer, intent(inout)            :: info
        end subroutine ma02fd
    end interface
    public :: ma02fd
    
    interface 
        subroutine ma02gd(n, a, lda, k1, k2, ipiv, incx)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: k1
            integer, intent(inout)            :: k2
            integer, intent(inout)            :: ipiv(*)
            integer, intent(inout)            :: incx
        end subroutine ma02gd
    end interface
    public :: ma02gd
    
    interface 
        subroutine ma02gz(n, a, lda, k1, k2, ipiv, incx)
            integer, intent(inout)      :: n
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(inout)      :: lda
            integer, intent(inout)      :: k1
            integer, intent(inout)      :: k2
            integer, intent(inout)      :: ipiv(*)
            integer, intent(inout)      :: incx
        end subroutine ma02gz
    end interface
    public :: ma02gz
    
    interface 
        logical function ma02hd (job,m,n,diag,a,lda)
            character, intent(inout)          :: job
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: diag
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
        end function ma02hd
    end interface
    public :: ma02hd
    
    interface 
        logical function ma02hz (job,m,n,diag,a,lda)
            character, intent(inout)    :: job
            integer, intent(inout)      :: m
            integer, intent(inout)      :: n
            complex*16, intent(inout)   :: diag
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(inout)      :: lda
        end function ma02hz
    end interface
    public :: ma02hz
    
    interface 
        double precision function ma02id (typ,norm,n,a,lda,qg,ldqg,dwork)
            character, intent(inout)          :: typ
            character, intent(inout)          :: norm
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            double precision, intent(inout)   :: dwork(*)
        end function ma02id
    end interface
    public :: ma02id
    
    interface 
        double precision function ma02iz (typ,norm,n,a,lda,qg,ldqg,dwork)
            character, intent(inout)          :: typ
            character, intent(inout)          :: norm
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            double precision, intent(inout)   :: dwork(*)
        end function ma02iz
    end interface
    public :: ma02iz
    
    interface 
        double precision function ma02jd (ltran1,ltran2,n,q1,ldq1,q2,ldq2,res, &
                       ldres)
            logical, intent(inout)            :: ltran1
            logical, intent(inout)            :: ltran2
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)            :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)            :: ldq2
            double precision, intent(inout)   :: res(ldres, *)
            integer, intent(inout)            :: ldres
        end function ma02jd
    end interface
    public :: ma02jd
    
    interface 
        double precision function ma02jz (ltran1,ltran2,n,q1,ldq1,q2,ldq2,res, &
                       ldres)
            logical, intent(inout)      :: ltran1
            logical, intent(inout)      :: ltran2
            integer, intent(inout)      :: n
            complex*16, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)      :: ldq1
            complex*16, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)      :: ldq2
            complex*16, intent(inout)   :: res(ldres, *)
            integer, intent(inout)      :: ldres
        end function ma02jz
    end interface
    public :: ma02jz
    
    interface 
        double precision function ma02md (norm,uplo,n,a,lda,dwork)
            character, intent(inout)          :: norm
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: dwork(*)
        end function ma02md
    end interface
    public :: ma02md
    
    interface 
        double precision function ma02mz (norm,uplo,n,a,lda,dwork)
            character, intent(inout)          :: norm
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: dwork(*)
        end function ma02mz
    end interface
    public :: ma02mz
    
    interface 
        subroutine ma02nz(uplo, trans, skew, n, k, l, a, lda)
            character, intent(inout)    :: uplo
            character, intent(inout)    :: trans
            character, intent(inout)    :: skew
            integer, intent(inout)      :: n
            integer, intent(inout)      :: k
            integer, intent(inout)      :: l
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(inout)      :: lda
        end subroutine ma02nz
    end interface
    public :: ma02nz
    
    interface 
        integer function ma02od (skew,m,a,lda,de,ldde)
            character, intent(inout)          :: skew
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(inout)            :: ldde
        end function ma02od
    end interface
    public :: ma02od
    
    interface 
        integer function ma02oz (skew,m,a,lda,de,ldde)
            character, intent(inout)    :: skew
            integer, intent(inout)      :: m
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(inout)      :: lda
            complex*16, intent(inout)   :: de(ldde, *)
            integer, intent(inout)      :: ldde
        end function ma02oz
    end interface
    public :: ma02oz
    
    interface 
        subroutine ma02pd(m, n, a, lda, nzr, nzc)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: nzr
            integer, intent(inout)            :: nzc
        end subroutine ma02pd
    end interface
    public :: ma02pd
    
    interface 
        subroutine ma02pz(m, n, a, lda, nzr, nzc)
            integer, intent(inout)      :: m
            integer, intent(inout)      :: n
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(inout)      :: lda
            integer, intent(inout)      :: nzr
            integer, intent(inout)      :: nzc
        end subroutine ma02pz
    end interface
    public :: ma02pz
    
    interface 
        subroutine ma02rd(id, n, d, e, info)
            character, intent(inout)          :: id
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: d(*)
            double precision, intent(inout)   :: e(*)
            integer, intent(inout)            :: info
        end subroutine ma02rd
    end interface
    public :: ma02rd
    
    interface 
        double precision function ma02sd (m,n,a,lda)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
        end function ma02sd
    end interface
    public :: ma02sd
    
    interface 
        subroutine mb01kd(uplo, trans, n, k, alpha, a, lda, b, &
                       ldb, beta, c, ldc, info)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: info
        end subroutine mb01kd
    end interface
    public :: mb01kd
    
    interface 
        subroutine mb01ld(uplo, trans, m, n, alpha, beta, r, ldr, &
                       a, lda, x, ldx, dwork, ldwork, info)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb01ld
    end interface
    public :: mb01ld
    
    interface 
        subroutine mb01md(uplo, n, alpha, a, lda, x, incx, beta, &
                       y, incy)
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: incx
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: y(*)
            integer, intent(inout)            :: incy
        end subroutine mb01md
    end interface
    public :: mb01md
    
    interface 
        subroutine mb01nd(uplo, n, alpha, x, incx, y, incy, a, &
                       lda)
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: incx
            double precision, intent(inout)   :: y(*)
            integer, intent(inout)            :: incy
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
        end subroutine mb01nd
    end interface
    public :: mb01nd
    
    interface 
        subroutine mb01oc(uplo, trans, n, alpha, beta, r, ldr, h, &
                       ldh, x, ldx, info)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            integer, intent(inout)            :: info
        end subroutine mb01oc
    end interface
    public :: mb01oc
    
    interface 
        subroutine mb01od(uplo, trans, n, alpha, beta, r, ldr, h, &
                       ldh, x, ldx, e, lde, dwork, ldwork, info)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb01od
    end interface
    public :: mb01od
    
    interface 
        subroutine mb01oe(uplo, trans, n, alpha, beta, r, ldr, h, &
                       ldh, e, lde)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
        end subroutine mb01oe
    end interface
    public :: mb01oe
    
    interface 
        subroutine mb01oh(uplo, trans, n, alpha, beta, r, ldr, h, &
                       ldh, a, lda)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
        end subroutine mb01oh
    end interface
    public :: mb01oh
    
    interface 
        subroutine mb01oo(uplo, trans, n, h, ldh, x, ldx, e, &
                       lde, p, ldp, info)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: p(ldp, *)
            integer, intent(inout)            :: ldp
            integer, intent(inout)            :: info
        end subroutine mb01oo
    end interface
    public :: mb01oo
    
    interface 
        subroutine mb01os(uplo, trans, n, h, ldh, x, ldx, p, &
                       ldp, info)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: p(ldp, *)
            integer, intent(inout)            :: ldp
            integer, intent(inout)            :: info
        end subroutine mb01os
    end interface
    public :: mb01os
    
    interface 
        subroutine mb01ot(uplo, trans, n, alpha, beta, r, ldr, e, &
                       lde, t, ldt)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
        end subroutine mb01ot
    end interface
    public :: mb01ot
    
    interface 
        subroutine mb01pd(scun, type, m, n, kl, ku, anrm, nbl, &
                       nrows, a, lda, info)
            character, intent(inout)          :: scun
            character, intent(inout)          :: type
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: kl
            integer, intent(inout)            :: ku
            double precision, intent(inout)   :: anrm
            integer, intent(inout)            :: nbl
            integer, intent(inout)            :: nrows(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: info
        end subroutine mb01pd
    end interface
    public :: mb01pd
    
    interface 
        subroutine mb01qd(type, m, n, kl, ku, cfrom, cto, nbl, &
                       nrows, a, lda, info)
            character, intent(inout)          :: type
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: kl
            integer, intent(inout)            :: ku
            double precision, intent(inout)   :: cfrom
            double precision, intent(inout)   :: cto
            integer, intent(inout)            :: nbl
            integer, intent(inout)            :: nrows(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: info
        end subroutine mb01qd
    end interface
    public :: mb01qd
    
    interface 
        subroutine mb01rb(side, uplo, trans, m, n, alpha, beta, r, &
                       ldr, a, lda, b, ldb, info)
            character, intent(inout)          :: side
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            integer, intent(inout)            :: info
        end subroutine mb01rb
    end interface
    public :: mb01rb
    
    interface 
        subroutine mb01rd(uplo, trans, m, n, alpha, beta, r, ldr, &
                       a, lda, x, ldx, dwork, ldwork, info)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb01rd
    end interface
    public :: mb01rd
    
    interface 
        subroutine mb01rh(uplo, trans, n, alpha, beta, r, ldr, h, &
                       ldh, x, ldx, dwork, ldwork, info)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb01rh
    end interface
    public :: mb01rh
    
    interface 
        subroutine mb01rt(uplo, trans, n, alpha, beta, r, ldr, e, &
                       lde, x, ldx, dwork, ldwork, info)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb01rt
    end interface
    public :: mb01rt
    
    interface 
        subroutine mb01ru(uplo, trans, m, n, alpha, beta, r, ldr, &
                       a, lda, x, ldx, dwork, ldwork, info)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb01ru
    end interface
    public :: mb01ru
    
    interface 
        subroutine mb01rw(uplo, trans, m, n, a, lda, z, ldz, &
                       dwork, info)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb01rw
    end interface
    public :: mb01rw
    
    interface 
        subroutine mb01rx(side, uplo, trans, m, n, alpha, beta, r, &
                       ldr, a, lda, b, ldb, info)
            character, intent(inout)          :: side
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            integer, intent(inout)            :: info
        end subroutine mb01rx
    end interface
    public :: mb01rx
    
    interface 
        subroutine mb01ry(side, uplo, trans, m, alpha, beta, r, ldr, &
                       h, ldh, b, ldb, dwork, info)
            character, intent(inout)          :: side
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb01ry
    end interface
    public :: mb01ry
    
    interface 
        subroutine mb01sd(jobs, m, n, a, lda, r, c)
            character, intent(inout)          :: jobs
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: r(*)
            double precision, intent(inout)   :: c(*)
        end subroutine mb01sd
    end interface
    public :: mb01sd
    
    interface 
        subroutine mb01ss(jobs, uplo, n, a, lda, d)
            character, intent(inout)          :: jobs
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: d(*)
        end subroutine mb01ss
    end interface
    public :: mb01ss
    
    interface 
        subroutine mb01td(n, a, lda, b, ldb, dwork, info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb01td
    end interface
    public :: mb01td
    
    interface 
        subroutine mb01ud(side, trans, m, n, alpha, h, ldh, a, &
                       lda, b, ldb, info)
            character, intent(inout)          :: side
            character, intent(inout)          :: trans
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            integer, intent(inout)            :: info
        end subroutine mb01ud
    end interface
    public :: mb01ud
    
    interface 
        subroutine mb01uw(side, trans, m, n, alpha, h, ldh, a, &
                       lda, dwork, ldwork, info)
            character, intent(inout)          :: side
            character, intent(inout)          :: trans
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb01uw
    end interface
    public :: mb01uw
    
    interface 
        subroutine mb01ux(side, uplo, trans, m, n, alpha, t, ldt, &
                       a, lda, dwork, ldwork, info)
            character, intent(inout)          :: side
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb01ux
    end interface
    public :: mb01ux
    
    interface 
        subroutine mb01uy(side, uplo, trans, m, n, alpha, t, ldt, &
                       a, lda, dwork, ldwork, info)
            character, intent(inout)          :: side
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb01uy
    end interface
    public :: mb01uy
    
    interface 
        subroutine mb01uz(side, uplo, trans, m, n, alpha, t, ldt, &
                       a, lda, zwork, lzwork, info)
            character, intent(inout)    :: side
            character, intent(inout)    :: uplo
            character, intent(inout)    :: trans
            integer, intent(inout)      :: m
            integer, intent(inout)      :: n
            complex*16, intent(inout)   :: alpha
            complex*16, intent(inout)   :: t(ldt, *)
            integer, intent(inout)      :: ldt
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(inout)      :: lda
            complex*16, intent(inout)   :: zwork(*)
            integer, intent(inout)      :: lzwork
            integer, intent(inout)      :: info
        end subroutine mb01uz
    end interface
    public :: mb01uz
    
    interface 
        subroutine mb01vd(trana, tranb, ma, na, mb, nb, alpha, beta, &
                       a, lda, b, ldb, c, ldc, mc, nc, &
                       info)
            character, intent(inout)          :: trana
            character, intent(inout)          :: tranb
            integer, intent(inout)            :: ma
            integer, intent(inout)            :: na
            integer, intent(inout)            :: mb
            integer, intent(inout)            :: nb
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: mc
            integer, intent(inout)            :: nc
            integer, intent(inout)            :: info
        end subroutine mb01vd
    end interface
    public :: mb01vd
    
    interface 
        subroutine mb01wd(dico, uplo, trans, hess, n, alpha, beta, r, &
                       ldr, a, lda, t, ldt, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            character, intent(inout)          :: hess
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            integer, intent(inout)            :: info
        end subroutine mb01wd
    end interface
    public :: mb01wd
    
    interface 
        subroutine mb01xd(uplo, n, a, lda, info)
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: info
        end subroutine mb01xd
    end interface
    public :: mb01xd
    
    interface 
        subroutine mb01xy(uplo, n, a, lda, info)
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: info
        end subroutine mb01xy
    end interface
    public :: mb01xy
    
    interface 
        subroutine mb01yd(uplo, trans, n, k, l, alpha, beta, a, &
                       lda, c, ldc, info)
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: k
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: info
        end subroutine mb01yd
    end interface
    public :: mb01yd
    
    interface 
        subroutine mb01zd(side, uplo, transt, diag, m, n, l, alpha, &
                       t, ldt, h, ldh, info)
            character, intent(inout)          :: side
            character, intent(inout)          :: uplo
            character, intent(inout)          :: transt
            character, intent(inout)          :: diag
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            integer, intent(inout)            :: info
        end subroutine mb01zd
    end interface
    public :: mb01zd
    
    interface 
        subroutine mb02cd(job, typet, k, n, t, ldt, g, ldg, &
                       r, ldr, l, ldl, cs, lcs, dwork, ldwork, &
                       info)
            character, intent(inout)          :: job
            character, intent(inout)          :: typet
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: l(ldl, *)
            integer, intent(inout)            :: ldl
            double precision, intent(inout)   :: cs(*)
            integer, intent(inout)            :: lcs
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02cd
    end interface
    public :: mb02cd
    
    interface 
        subroutine mb02cu(typeg, k, p, q, nb, a1, lda1, a2, &
                       lda2, b, ldb, rnk, ipvt, cs, tol, dwork, &
                       ldwork, info)
            character, intent(inout)          :: typeg
            integer, intent(inout)            :: k
            integer, intent(inout)            :: p
            integer, intent(inout)            :: q
            integer, intent(inout)            :: nb
            double precision, intent(inout)   :: a1(lda1, *)
            integer, intent(inout)            :: lda1
            double precision, intent(inout)   :: a2(lda2, *)
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            integer, intent(inout)            :: rnk
            integer, intent(inout)            :: ipvt(*)
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02cu
    end interface
    public :: mb02cu
    
    interface 
        subroutine mb02cv(typeg, strucg, k, n, p, q, nb, rnk, &
                       a1, lda1, a2, lda2, b, ldb, f1, ldf1, &
                       f2, ldf2, g, ldg, cs, dwork, ldwork, info)
            character, intent(inout)          :: typeg
            character, intent(inout)          :: strucg
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: p
            integer, intent(inout)            :: q
            integer, intent(inout)            :: nb
            integer, intent(inout)            :: rnk
            double precision, intent(inout)   :: a1(lda1, *)
            integer, intent(inout)            :: lda1
            double precision, intent(inout)   :: a2(lda2, *)
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: f1(ldf1, *)
            integer, intent(inout)            :: ldf1
            double precision, intent(inout)   :: f2(ldf2, *)
            integer, intent(inout)            :: ldf2
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02cv
    end interface
    public :: mb02cv
    
    interface 
        subroutine mb02cx(typet, p, q, k, a, lda, b, ldb, &
                       cs, lcs, dwork, ldwork, info)
            character, intent(inout)          :: typet
            integer, intent(inout)            :: p
            integer, intent(inout)            :: q
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: cs(*)
            integer, intent(inout)            :: lcs
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02cx
    end interface
    public :: mb02cx
    
    interface 
        subroutine mb02cy(typet, strucg, p, q, n, k, a, lda, &
                       b, ldb, h, ldh, cs, lcs, dwork, ldwork, &
                       info)
            character, intent(inout)          :: typet
            character, intent(inout)          :: strucg
            integer, intent(inout)            :: p
            integer, intent(inout)            :: q
            integer, intent(inout)            :: n
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: cs(*)
            integer, intent(inout)            :: lcs
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02cy
    end interface
    public :: mb02cy
    
    interface 
        subroutine mb02dd(job, typet, k, m, n, ta, ldta, t, &
                       ldt, g, ldg, r, ldr, l, ldl, cs, &
                       lcs, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: typet
            integer, intent(inout)            :: k
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: ta(ldta, *)
            integer, intent(inout)            :: ldta
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: l(ldl, *)
            integer, intent(inout)            :: ldl
            double precision, intent(inout)   :: cs(*)
            integer, intent(inout)            :: lcs
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02dd
    end interface
    public :: mb02dd
    
    interface 
        subroutine mb02ed(typet, k, n, nrhs, t, ldt, b, ldb, &
                       dwork, ldwork, info)
            character, intent(inout)          :: typet
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nrhs
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02ed
    end interface
    public :: mb02ed
    
    interface 
        subroutine mb02fd(typet, k, n, p, s, t, ldt, r, &
                       ldr, dwork, ldwork, info)
            character, intent(inout)          :: typet
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: p
            integer, intent(inout)            :: s
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02fd
    end interface
    public :: mb02fd
    
    interface 
        subroutine mb02gd(typet, triu, k, n, nl, p, s, t, &
                       ldt, rb, ldrb, dwork, ldwork, info)
            character, intent(inout)          :: typet
            character, intent(inout)          :: triu
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nl
            integer, intent(inout)            :: p
            integer, intent(inout)            :: s
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: rb(ldrb, *)
            integer, intent(inout)            :: ldrb
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02gd
    end interface
    public :: mb02gd
    
    interface 
        subroutine mb02hd(triu, k, l, m, ml, n, nu, p, &
                       s, tc, ldtc, tr, ldtr, rb, ldrb, dwork, &
                       ldwork, info)
            character, intent(inout)          :: triu
            integer, intent(inout)            :: k
            integer, intent(inout)            :: l
            integer, intent(inout)            :: m
            integer, intent(inout)            :: ml
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nu
            integer, intent(inout)            :: p
            integer, intent(inout)            :: s
            double precision, intent(inout)   :: tc(ldtc, *)
            integer, intent(inout)            :: ldtc
            double precision, intent(inout)   :: tr(ldtr, *)
            integer, intent(inout)            :: ldtr
            double precision, intent(inout)   :: rb(ldrb, *)
            integer, intent(inout)            :: ldrb
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02hd
    end interface
    public :: mb02hd
    
    interface 
        subroutine mb02id(job, k, l, m, n, rb, rc, tc, &
                       ldtc, tr, ldtr, b, ldb, c, ldc, dwork, &
                       ldwork, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: k
            integer, intent(inout)            :: l
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: rb
            integer, intent(inout)            :: rc
            double precision, intent(inout)   :: tc(ldtc, *)
            integer, intent(inout)            :: ldtc
            double precision, intent(inout)   :: tr(ldtr, *)
            integer, intent(inout)            :: ldtr
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02id
    end interface
    public :: mb02id
    
    interface 
        subroutine mb02jd(job, k, l, m, n, p, s, tc, &
                       ldtc, tr, ldtr, q, ldq, r, ldr, dwork, &
                       ldwork, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: k
            integer, intent(inout)            :: l
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: p
            integer, intent(inout)            :: s
            double precision, intent(inout)   :: tc(ldtc, *)
            integer, intent(inout)            :: ldtc
            double precision, intent(inout)   :: tr(ldtr, *)
            integer, intent(inout)            :: ldtr
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02jd
    end interface
    public :: mb02jd
    
    interface 
        subroutine mb02jx(job, k, l, m, n, tc, ldtc, tr, &
                       ldtr, rnk, q, ldq, r, ldr, jpvt, tol1, &
                       tol2, dwork, ldwork, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: k
            integer, intent(inout)            :: l
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: tc(ldtc, *)
            integer, intent(inout)            :: ldtc
            double precision, intent(inout)   :: tr(ldtr, *)
            integer, intent(inout)            :: ldtr
            integer, intent(inout)            :: rnk
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            integer, intent(inout)            :: jpvt(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02jx
    end interface
    public :: mb02jx
    
    interface 
        subroutine mb02kd(ldblk, trans, k, l, m, n, r, alpha, &
                       beta, tc, ldtc, tr, ldtr, b, ldb, c, &
                       ldc, dwork, ldwork, info)
            character, intent(inout)          :: ldblk
            character, intent(inout)          :: trans
            integer, intent(inout)            :: k
            integer, intent(inout)            :: l
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: r
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: tc(ldtc, *)
            integer, intent(inout)            :: ldtc
            double precision, intent(inout)   :: tr(ldtr, *)
            integer, intent(inout)            :: ldtr
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02kd
    end interface
    public :: mb02kd
    
    interface 
        subroutine mb02md(job, m, n, l, rank, c, ldc, s, &
                       x, ldx, tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: l
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: s(*)
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mb02md
    end interface
    public :: mb02md
    
    interface 
        subroutine mb02nd(m, n, l, rank, theta, c, ldc, x, &
                       ldx, q, inul, tol, reltol, iwork, dwork, ldwork, &
                       bwork, iwarn, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: l
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: theta
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: q(*)
            logical, intent(inout)            :: inul(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: reltol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mb02nd
    end interface
    public :: mb02nd
    
    interface 
        subroutine mb02ny(updatu, updatv, m, n, i, k, q, e, &
                       u, ldu, v, ldv, dwork)
            logical, intent(inout)            :: updatu
            logical, intent(inout)            :: updatv
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: i
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: q(*)
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb02ny
    end interface
    public :: mb02ny
    
    interface 
        subroutine mb02od(side, uplo, trans, diag, norm, m, n, alpha, &
                       a, lda, b, ldb, rcond, tol, iwork, dwork, &
                       info)
            character, intent(inout)          :: side
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            character, intent(inout)          :: diag
            character, intent(inout)          :: norm
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb02od
    end interface
    public :: mb02od
    
    interface 
        subroutine mb02pd(fact, trans, n, nrhs, a, lda, af, ldaf, &
                       ipiv, equed, r, c, b, ldb, x, ldx, &
                       rcond, ferr, berr, iwork, dwork, info)
            character, intent(inout)          :: fact
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nrhs
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: af(ldaf, *)
            integer, intent(inout)            :: ldaf
            integer, intent(inout)            :: ipiv(*)
            character, intent(inout)          :: equed
            double precision, intent(inout)   :: r(*)
            double precision, intent(inout)   :: c(*)
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: ferr(*)
            double precision, intent(inout)   :: berr(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb02pd
    end interface
    public :: mb02pd
    
    interface 
        subroutine mb02qd(job, iniper, m, n, nrhs, rcond, svlmax, a, &
                       lda, b, ldb, y, jpvt, rank, sval, dwork, &
                       ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: iniper
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nrhs
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: svlmax
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: y(*)
            integer, intent(inout)            :: jpvt(*)
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: sval(3)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02qd
    end interface
    public :: mb02qd
    
    interface 
        subroutine mb02qy(m, n, nrhs, rank, a, lda, jpvt, b, &
                       ldb, tau, dwork, ldwork, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nrhs
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: jpvt(*)
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02qy
    end interface
    public :: mb02qy
    
    interface 
        subroutine mb02rd(trans, n, nrhs, h, ldh, ipiv, b, ldb, &
                       info)
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nrhs
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            integer, intent(inout)            :: ipiv(*)
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            integer, intent(inout)            :: info
        end subroutine mb02rd
    end interface
    public :: mb02rd
    
    interface 
        subroutine mb02rz(trans, n, nrhs, h, ldh, ipiv, b, ldb, &
                       info)
            character, intent(inout)    :: trans
            integer, intent(inout)      :: n
            integer, intent(inout)      :: nrhs
            complex*16, intent(inout)   :: h(ldh, *)
            integer, intent(inout)      :: ldh
            integer, intent(inout)      :: ipiv(*)
            complex*16, intent(inout)   :: b(ldb, *)
            integer, intent(inout)      :: ldb
            integer, intent(inout)      :: info
        end subroutine mb02rz
    end interface
    public :: mb02rz
    
    interface 
        subroutine mb02sd(n, h, ldh, ipiv, info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            integer, intent(inout)            :: ipiv(*)
            integer, intent(inout)            :: info
        end subroutine mb02sd
    end interface
    public :: mb02sd
    
    interface 
        subroutine mb02sz(n, h, ldh, ipiv, info)
            integer, intent(inout)      :: n
            complex*16, intent(inout)   :: h(ldh, *)
            integer, intent(inout)      :: ldh
            integer, intent(inout)      :: ipiv(*)
            integer, intent(inout)      :: info
        end subroutine mb02sz
    end interface
    public :: mb02sz
    
    interface 
        subroutine mb02td(norm, n, hnorm, h, ldh, ipiv, rcond, iwork, &
                       dwork, info)
            character, intent(inout)          :: norm
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: hnorm
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            integer, intent(inout)            :: ipiv(*)
            double precision, intent(inout)   :: rcond
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb02td
    end interface
    public :: mb02td
    
    interface 
        subroutine mb02tz(norm, n, hnorm, h, ldh, ipiv, rcond, dwork, &
                       zwork, info)
            character, intent(inout)          :: norm
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: hnorm
            complex*16, intent(inout)         :: h(ldh, *)
            integer, intent(inout)            :: ldh
            integer, intent(inout)            :: ipiv(*)
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: info
        end subroutine mb02tz
    end interface
    public :: mb02tz
    
    interface 
        subroutine mb02ud(fact, side, trans, jobp, m, n, alpha, rcond, &
                       rank, r, ldr, q, ldq, sv, b, ldb, &
                       rp, ldrp, dwork, ldwork, info)
            character, intent(inout)          :: fact
            character, intent(inout)          :: side
            character, intent(inout)          :: trans
            character, intent(inout)          :: jobp
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: rcond
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: sv(*)
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: rp(ldrp, *)
            integer, intent(inout)            :: ldrp
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02ud
    end interface
    public :: mb02ud
    
    interface 
        subroutine mb02uu(n, a, lda, rhs, ipiv, jpiv, scale)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: rhs(*)
            integer, intent(inout)            :: ipiv(*)
            integer, intent(inout)            :: jpiv(*)
            double precision, intent(inout)   :: scale
        end subroutine mb02uu
    end interface
    public :: mb02uu
    
    interface 
        subroutine mb02uv(n, a, lda, ipiv, jpiv, info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: ipiv(*)
            integer, intent(inout)            :: jpiv(*)
            integer, intent(inout)            :: info
        end subroutine mb02uv
    end interface
    public :: mb02uv
    
    interface 
        subroutine mb02uw(ltrans, n, m, par, a, lda, b, ldb, &
                       scale, iwarn)
            logical, intent(inout)            :: ltrans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: par(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: scale
            integer, intent(inout)            :: iwarn
        end subroutine mb02uw
    end interface
    public :: mb02uw
    
    interface 
        subroutine mb02vd(trans, m, n, a, lda, ipiv, b, ldb, &
                       info)
            character, intent(inout)          :: trans
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: ipiv(*)
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            integer, intent(inout)            :: info
        end subroutine mb02vd
    end interface
    public :: mb02vd
    
    interface 
        subroutine mb02wd(form, f, n, ipar, lipar, dpar, ldpar, itmax, &
                       a, lda, b, incb, x, incx, tol, dwork, &
                       ldwork, iwarn, info)
            character, intent(inout)          :: form
            external                :: f
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ldpar
            integer, intent(inout)            :: itmax
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(*)
            integer, intent(inout)            :: incb
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: incx
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mb02wd
    end interface
    public :: mb02wd
    
    interface 
        subroutine mb02xd(form, stor, uplo, f, m, n, nrhs, ipar, &
                       lipar, dpar, ldpar, a, lda, b, ldb, ata, &
                       ldata, dwork, ldwork, info)
            character, intent(inout)          :: form
            character, intent(inout)          :: stor
            character, intent(inout)          :: uplo
            external                :: f
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nrhs
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ldpar
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: ata(*)
            integer, intent(inout)            :: ldata
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02xd
    end interface
    public :: mb02xd
    
    interface 
        subroutine mb02yd(cond, n, r, ldr, ipvt, diag, qtb, rank, &
                       x, tol, dwork, ldwork, info)
            character, intent(inout)          :: cond
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            integer, intent(inout)            :: ipvt(*)
            double precision, intent(inout)   :: diag(*)
            double precision, intent(inout)   :: qtb(*)
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb02yd
    end interface
    public :: mb02yd
    
    interface 
        subroutine mb03ab(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, w1, w2, c1, s1, c2, s2)
            character, intent(inout)          :: shft
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: amap(*)
            integer, intent(inout)            :: s(*)
            integer, intent(inout)            :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: w1
            double precision, intent(inout)   :: w2
            double precision, intent(inout)   :: c1
            double precision, intent(inout)   :: s1
            double precision, intent(inout)   :: c2
            double precision, intent(inout)   :: s2
        end subroutine mb03ab
    end interface
    public :: mb03ab
    
    interface 
        subroutine mb03ad(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, c1, s1, c2, s2)
            character, intent(inout)          :: shft
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: amap(*)
            integer, intent(inout)            :: s(*)
            integer, intent(inout)            :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: c1
            double precision, intent(inout)   :: s1
            double precision, intent(inout)   :: c2
            double precision, intent(inout)   :: s2
        end subroutine mb03ad
    end interface
    public :: mb03ad
    
    interface 
        subroutine mb03ae(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, c1, s1, c2, s2)
            character, intent(inout)          :: shft
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: amap(*)
            integer, intent(inout)            :: s(*)
            integer, intent(inout)            :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: c1
            double precision, intent(inout)   :: s1
            double precision, intent(inout)   :: c2
            double precision, intent(inout)   :: s2
        end subroutine mb03ae
    end interface
    public :: mb03ae
    
    interface 
        subroutine mb03af(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, c1, s1, c2, s2)
            character, intent(inout)          :: shft
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: amap(*)
            integer, intent(inout)            :: s(*)
            integer, intent(inout)            :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: c1
            double precision, intent(inout)   :: s1
            double precision, intent(inout)   :: c2
            double precision, intent(inout)   :: s2
        end subroutine mb03af
    end interface
    public :: mb03af
    
    interface 
        subroutine mb03ag(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, c1, s1, c2, s2, iwork, dwork)
            character, intent(inout)          :: shft
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: amap(*)
            integer, intent(inout)            :: s(*)
            integer, intent(inout)            :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: c1
            double precision, intent(inout)   :: s1
            double precision, intent(inout)   :: c2
            double precision, intent(inout)   :: s2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb03ag
    end interface
    public :: mb03ag
    
    interface 
        subroutine mb03ah(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, c1, s1, c2, s2)
            character, intent(inout)          :: shft
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: amap(*)
            integer, intent(inout)            :: s(*)
            integer, intent(inout)            :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: c1
            double precision, intent(inout)   :: s1
            double precision, intent(inout)   :: c2
            double precision, intent(inout)   :: s2
        end subroutine mb03ah
    end interface
    public :: mb03ah
    
    interface 
        subroutine mb03ai(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, c1, s1, c2, s2, dwork)
            character, intent(inout)          :: shft
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: amap(*)
            integer, intent(inout)            :: s(*)
            integer, intent(inout)            :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: c1
            double precision, intent(inout)   :: s1
            double precision, intent(inout)   :: c2
            double precision, intent(inout)   :: s2
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb03ai
    end interface
    public :: mb03ai
    
    interface 
        subroutine mb03ba(k, h, s, smult, amap, qmap)
            integer, intent(inout)   :: k
            integer, intent(inout)   :: h
            integer, intent(inout)   :: s(*)
            integer, intent(inout)   :: smult
            integer, intent(inout)   :: amap(*)
            integer, intent(inout)   :: qmap(*)
        end subroutine mb03ba
    end interface
    public :: mb03ba
    
    interface 
        subroutine mb03bb(base, lgbas, ulp, k, amap, s, sinv, a, &
                       lda1, lda2, alphar, alphai, beta, scal, dwork, info)
            double precision, intent(inout)   :: base
            double precision, intent(inout)   :: lgbas
            double precision, intent(inout)   :: ulp
            integer, intent(inout)            :: k
            integer, intent(inout)            :: amap(*)
            integer, intent(inout)            :: s(*)
            integer, intent(inout)            :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: alphar(2)
            double precision, intent(inout)   :: alphai(2)
            double precision, intent(inout)   :: beta(2)
            integer, intent(inout)            :: scal(2)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03bb
    end interface
    public :: mb03bb
    
    interface 
        subroutine mb03bc(k, amap, s, sinv, a, lda1, lda2, macpar, &
                       cv, sv, dwork)
            integer, intent(inout)            :: k
            integer, intent(inout)            :: amap(*)
            integer, intent(inout)            :: s(*)
            integer, intent(inout)            :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: macpar(*)
            double precision, intent(inout)   :: cv(*)
            double precision, intent(inout)   :: sv(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb03bc
    end interface
    public :: mb03bc
    
    interface 
        subroutine mb03bd(job, defl, compq, qind, k, n, h, ilo, &
                       ihi, s, a, lda1, lda2, q, ldq1, ldq2, &
                       alphar, alphai, beta, scal, iwork, liwork, dwork, ldwork, &
                       iwarn, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: defl
            character, intent(inout)          :: compq
            integer, intent(inout)            :: qind(*)
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: h
            integer, intent(inout)            :: ilo
            integer, intent(inout)            :: ihi
            integer, intent(inout)            :: s(*)
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: q(ldq1, ldq2, *)
            integer, intent(inout)            :: ldq1
            integer, intent(inout)            :: ldq2
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: scal(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mb03bd
    end interface
    public :: mb03bd
    
    interface 
        subroutine mb03be(k, amap, s, sinv, a, lda1, lda2)
            integer, intent(inout)            :: k
            integer, intent(inout)            :: amap(*)
            integer, intent(inout)            :: s(*)
            integer, intent(inout)            :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
        end subroutine mb03be
    end interface
    public :: mb03be
    
    interface 
        subroutine mb03bf(k, amap, s, sinv, a, lda1, lda2, ulp)
            integer, intent(inout)            :: k
            integer, intent(inout)            :: amap(*)
            integer, intent(inout)            :: s(*)
            integer, intent(inout)            :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: ulp
        end subroutine mb03bf
    end interface
    public :: mb03bf
    
    interface 
        subroutine mb03bg(k, n, amap, s, sinv, a, lda1, lda2, &
                       wr, wi)
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: amap(*)
            integer, intent(inout)            :: s(*)
            integer, intent(inout)            :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
        end subroutine mb03bg
    end interface
    public :: mb03bg
    
    interface 
        subroutine mb03bz(job, compq, k, n, ilo, ihi, s, a, &
                       lda1, lda2, q, ldq1, ldq2, alpha, beta, scal, &
                       dwork, ldwork, zwork, lzwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: compq
            integer, intent(inout)            :: k
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            integer, intent(inout)            :: ihi
            integer, intent(inout)            :: s(*)
            complex*16, intent(inout)         :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            complex*16, intent(inout)         :: q(ldq1, ldq2, *)
            integer, intent(inout)            :: ldq1
            integer, intent(inout)            :: ldq2
            complex*16, intent(inout)         :: alpha(*)
            complex*16, intent(inout)         :: beta(*)
            integer, intent(inout)            :: scal(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine mb03bz
    end interface
    public :: mb03bz
    
    interface 
        subroutine mb03cd(uplo, n1, n2, prec, a, lda, b, ldb, &
                       d, ldd, q1, ldq1, q2, ldq2, q3, ldq3, &
                       dwork, ldwork, info)
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: n2
            double precision, intent(inout)   :: prec
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)            :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)            :: ldq2
            double precision, intent(inout)   :: q3(ldq3, *)
            integer, intent(inout)            :: ldq3
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03cd
    end interface
    public :: mb03cd
    
    interface 
        subroutine mb03cz(a, lda, b, ldb, d, ldd, co1, si1, &
                       co2, si2, co3, si3)
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: co1
            complex*16, intent(inout)         :: si1
            double precision, intent(inout)   :: co2
            complex*16, intent(inout)         :: si2
            double precision, intent(inout)   :: co3
            complex*16, intent(inout)         :: si3
        end subroutine mb03cz
    end interface
    public :: mb03cz
    
    interface 
        subroutine mb03dd(uplo, n1, n2, prec, a, lda, b, ldb, &
                       q1, ldq1, q2, ldq2, dwork, ldwork, info)
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: n2
            double precision, intent(inout)   :: prec
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)            :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)            :: ldq2
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03dd
    end interface
    public :: mb03dd
    
    interface 
        subroutine mb03dz(a, lda, b, ldb, co1, si1, co2, si2)
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: co1
            complex*16, intent(inout)         :: si1
            double precision, intent(inout)   :: co2
            complex*16, intent(inout)         :: si2
        end subroutine mb03dz
    end interface
    public :: mb03dz
    
    interface 
        subroutine mb03ed(n, prec, a, lda, b, ldb, d, ldd, &
                       q1, ldq1, q2, ldq2, q3, ldq3, dwork, ldwork, &
                       info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: prec
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)            :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)            :: ldq2
            double precision, intent(inout)   :: q3(ldq3, *)
            integer, intent(inout)            :: ldq3
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03ed
    end interface
    public :: mb03ed
    
    interface 
        subroutine mb03fd(n, prec, a, lda, b, ldb, q1, ldq1, &
                       q2, ldq2, dwork, ldwork, info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: prec
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)            :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)            :: ldq2
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03fd
    end interface
    public :: mb03fd
    
    interface 
        subroutine mb03fz(compq, compu, orth, n, z, ldz, b, ldb, &
                       fg, ldfg, neig, d, ldd, c, ldc, q, &
                       ldq, u, ldu, alphar, alphai, beta, iwork, liwork, &
                       dwork, ldwork, zwork, lzwork, bwork, info)
            character, intent(inout)          :: compq
            character, intent(inout)          :: compu
            character, intent(inout)          :: orth
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: z(ldz, *)
            integer, intent(inout)            :: ldz
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: fg(ldfg, *)
            integer, intent(inout)            :: ldfg
            integer, intent(inout)            :: neig
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(inout)            :: ldd
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(inout)            :: ldc
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(inout)            :: ldq
            complex*16, intent(inout)         :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03fz
    end interface
    public :: mb03fz
    
    interface 
        subroutine mb03gd(n, b, ldb, d, ldd, macpar, q, ldq, &
                       u, ldu, dwork, ldwork, info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: macpar(*)
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03gd
    end interface
    public :: mb03gd
    
    interface 
        subroutine mb03gz(z11, z12, z22, h11, h12, co1, si1, co2, &
                       si2)
            complex*16, intent(inout)         :: z11
            complex*16, intent(inout)         :: z12
            complex*16, intent(inout)         :: z22
            complex*16, intent(inout)         :: h11
            complex*16, intent(inout)         :: h12
            double precision, intent(inout)   :: co1
            complex*16, intent(inout)         :: si1
            double precision, intent(inout)   :: co2
            complex*16, intent(inout)         :: si2
        end subroutine mb03gz
    end interface
    public :: mb03gz
    
    interface 
        subroutine mb03hd(n, a, lda, b, ldb, macpar, q, ldq, &
                       dwork, info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: macpar(*)
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03hd
    end interface
    public :: mb03hd
    
    interface 
        subroutine mb03hz(s11, s12, h11, h12, co, si)
            complex*16, intent(inout)         :: s11
            complex*16, intent(inout)         :: s12
            complex*16, intent(inout)         :: h11
            complex*16, intent(inout)         :: h12
            double precision, intent(inout)   :: co
            complex*16, intent(inout)         :: si
        end subroutine mb03hz
    end interface
    public :: mb03hz
    
    interface 
        subroutine mb03id(compq, compu, n, a, lda, c, ldc, d, &
                       ldd, b, ldb, f, ldf, q, ldq, u1, &
                       ldu1, u2, ldu2, neig, iwork, liwork, dwork, ldwork, &
                       info)
            character, intent(inout)          :: compq
            character, intent(inout)          :: compu
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(inout)            :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(inout)            :: ldu2
            integer, intent(inout)            :: neig
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03id
    end interface
    public :: mb03id
    
    interface 
        subroutine mb03iz(compq, compu, n, a, lda, c, ldc, d, &
                       ldd, b, ldb, f, ldf, q, ldq, u1, &
                       ldu1, u2, ldu2, neig, tol, info)
            character, intent(inout)          :: compq
            character, intent(inout)          :: compu
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(inout)            :: ldc
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(inout)            :: ldd
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: f(ldf, *)
            integer, intent(inout)            :: ldf
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(inout)            :: ldq
            complex*16, intent(inout)         :: u1(ldu1, *)
            integer, intent(inout)            :: ldu1
            complex*16, intent(inout)         :: u2(ldu2, *)
            integer, intent(inout)            :: ldu2
            integer, intent(inout)            :: neig
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: info
        end subroutine mb03iz
    end interface
    public :: mb03iz
    
    interface 
        subroutine mb03jd(compq, n, a, lda, d, ldd, b, ldb, &
                       f, ldf, q, ldq, neig, iwork, liwork, dwork, &
                       ldwork, info)
            character, intent(inout)          :: compq
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            integer, intent(inout)            :: neig
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03jd
    end interface
    public :: mb03jd
    
    interface 
        subroutine mb03jp(compq, n, a, lda, d, ldd, b, ldb, &
                       f, ldf, q, ldq, neig, iwork, liwork, dwork, &
                       ldwork, info)
            character, intent(inout)          :: compq
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            integer, intent(inout)            :: neig
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03jp
    end interface
    public :: mb03jp
    
    interface 
        subroutine mb03jz(compq, n, a, lda, d, ldd, b, ldb, &
                       f, ldf, q, ldq, neig, tol, info)
            character, intent(inout)          :: compq
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(inout)            :: ldd
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: f(ldf, *)
            integer, intent(inout)            :: ldf
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(inout)            :: ldq
            integer, intent(inout)            :: neig
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: info
        end subroutine mb03jz
    end interface
    public :: mb03jz
    
    interface 
        subroutine mb03ka(compq, whichq, ws, k, nc, kschur, ifst, ilst, &
                       n, ni, s, t, ldt, ixt, q, ldq, &
                       ixq, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: compq
            integer, intent(inout)            :: whichq(*)
            logical, intent(inout)            :: ws
            integer, intent(inout)            :: k
            integer, intent(inout)            :: nc
            integer, intent(inout)            :: kschur
            integer, intent(inout)            :: ifst
            integer, intent(inout)            :: ilst
            integer, intent(inout)            :: n(*)
            integer, intent(inout)            :: ni(*)
            integer, intent(inout)            :: s(*)
            double precision, intent(inout)   :: t(*)
            integer, intent(inout)            :: ldt(*)
            integer, intent(inout)            :: ixt(*)
            double precision, intent(inout)   :: q(*)
            integer, intent(inout)            :: ldq(*)
            integer, intent(inout)            :: ixq(*)
            double precision, intent(inout)   :: tol(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03ka
    end interface
    public :: mb03ka
    
    interface 
        subroutine mb03kb(compq, whichq, ws, k, nc, kschur, j1, n1, &
                       n2, n, ni, s, t, ldt, ixt, q, &
                       ldq, ixq, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: compq
            integer, intent(inout)            :: whichq(*)
            logical, intent(inout)            :: ws
            integer, intent(inout)            :: k
            integer, intent(inout)            :: nc
            integer, intent(inout)            :: kschur
            integer, intent(inout)            :: j1
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: n2
            integer, intent(inout)            :: n(*)
            integer, intent(inout)            :: ni(*)
            integer, intent(inout)            :: s(*)
            double precision, intent(inout)   :: t(*)
            integer, intent(inout)            :: ldt(*)
            integer, intent(inout)            :: ixt(*)
            double precision, intent(inout)   :: q(*)
            integer, intent(inout)            :: ldq(*)
            integer, intent(inout)            :: ixq(*)
            double precision, intent(inout)   :: tol(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03kb
    end interface
    public :: mb03kb
    
    interface 
        subroutine mb03kc(k, khess, n, r, s, a, lda, v, &
                       tau)
            integer, intent(inout)            :: k
            integer, intent(inout)            :: khess
            integer, intent(inout)            :: n
            integer, intent(inout)            :: r
            integer, intent(inout)            :: s(*)
            double precision, intent(inout)   :: a(*)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: v(*)
            double precision, intent(inout)   :: tau(*)
        end subroutine mb03kc
    end interface
    public :: mb03kc
    
    interface 
        subroutine mb03kd(compq, whichq, strong, k, nc, kschur, n, ni, &
                       s, select, t, ldt, ixt, q, ldq, ixq, &
                       m, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: compq
            integer, intent(inout)            :: whichq(*)
            character, intent(inout)          :: strong
            integer, intent(inout)            :: k
            integer, intent(inout)            :: nc
            integer, intent(inout)            :: kschur
            integer, intent(inout)            :: n(*)
            integer, intent(inout)            :: ni(*)
            integer, intent(inout)            :: s(*)
            logical, intent(inout)            :: select(*)
            double precision, intent(inout)   :: t(*)
            integer, intent(inout)            :: ldt(*)
            integer, intent(inout)            :: ixt(*)
            double precision, intent(inout)   :: q(*)
            integer, intent(inout)            :: ldq(*)
            integer, intent(inout)            :: ixq(*)
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03kd
    end interface
    public :: mb03kd
    
    interface 
        subroutine mb03ke(trana, tranb, isgn, k, m, n, prec, smin, &
                       s, a, b, c, scale, dwork, ldwork, info)
            logical, intent(inout)            :: trana
            logical, intent(inout)            :: tranb
            integer, intent(inout)            :: isgn
            integer, intent(inout)            :: k
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: prec
            double precision, intent(inout)   :: smin
            integer, intent(inout)            :: s(*)
            double precision, intent(inout)   :: a(*)
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: c(*)
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03ke
    end interface
    public :: mb03ke
    
    interface 
        subroutine mb03ld(compq, orth, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, neig, q, ldq, alphar, alphai, &
                       beta, iwork, liwork, dwork, ldwork, bwork, info)
            character, intent(inout)          :: compq
            character, intent(inout)          :: orth
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(inout)            :: ldde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: fg(ldfg, *)
            integer, intent(inout)            :: ldfg
            integer, intent(inout)            :: neig
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03ld
    end interface
    public :: mb03ld
    
    interface 
        subroutine mb03lf(compq, compu, orth, n, z, ldz, b, ldb, &
                       fg, ldfg, neig, q, ldq, u, ldu, alphar, &
                       alphai, beta, iwork, liwork, dwork, ldwork, bwork, iwarn, &
                       info)
            character, intent(inout)          :: compq
            character, intent(inout)          :: compu
            character, intent(inout)          :: orth
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: fg(ldfg, *)
            integer, intent(inout)            :: ldfg
            integer, intent(inout)            :: neig
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mb03lf
    end interface
    public :: mb03lf
    
    interface 
        subroutine mb03lp(compq, orth, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, neig, q, ldq, alphar, alphai, &
                       beta, iwork, liwork, dwork, ldwork, bwork, info)
            character, intent(inout)          :: compq
            character, intent(inout)          :: orth
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(inout)            :: ldde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: fg(ldfg, *)
            integer, intent(inout)            :: ldfg
            integer, intent(inout)            :: neig
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03lp
    end interface
    public :: mb03lp
    
    interface 
        subroutine mb03lz(compq, orth, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, neig, q, ldq, alphar, alphai, &
                       beta, iwork, dwork, ldwork, zwork, lzwork, bwork, info)
            character, intent(inout)          :: compq
            character, intent(inout)          :: orth
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: de(ldde, *)
            integer, intent(inout)            :: ldde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: fg(ldfg, *)
            integer, intent(inout)            :: ldfg
            integer, intent(inout)            :: neig
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03lz
    end interface
    public :: mb03lz
    
    interface 
        subroutine mb03md(n, l, theta, q, e, q2, e2, pivmin, &
                       tol, reltol, iwarn, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: theta
            double precision, intent(inout)   :: q(*)
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: q2(*)
            double precision, intent(inout)   :: e2(*)
            double precision, intent(inout)   :: pivmin
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: reltol
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mb03md
    end interface
    public :: mb03md
    
    interface 
        double precision function mb03my (nx,x,incx)
            integer, intent(inout)            :: nx
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: incx
        end function mb03my
    end interface
    public :: mb03my
    
    interface 
        integer function mb03nd (n,theta,q2,e2,pivmin,info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: theta
            double precision, intent(inout)   :: q2(*)
            double precision, intent(inout)   :: e2(*)
            double precision, intent(inout)   :: pivmin
            integer, intent(inout)            :: info
        end function mb03nd
    end interface
    public :: mb03nd
    
    interface 
        double precision function mb03ny (n,omega,a,lda,s,dwork,ldwork,cwork, &
                       lcwork,info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: omega
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: s(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: cwork(*)
            integer, intent(inout)            :: lcwork
            integer, intent(inout)            :: info
        end function mb03ny
    end interface
    public :: mb03ny
    
    interface 
        subroutine mb03od(jobqr, m, n, a, lda, jpvt, rcond, svlmax, &
                       tau, rank, sval, dwork, ldwork, info)
            character, intent(inout)          :: jobqr
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: jpvt(*)
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: svlmax
            double precision, intent(inout)   :: tau(*)
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: sval(3)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03od
    end interface
    public :: mb03od
    
    interface 
        subroutine mb03oy(m, n, a, lda, rcond, svlmax, rank, sval, &
                       jpvt, tau, dwork, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: svlmax
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: sval(3)
            integer, intent(inout)            :: jpvt(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03oy
    end interface
    public :: mb03oy
    
    interface 
        subroutine mb03pd(jobrq, m, n, a, lda, jpvt, rcond, svlmax, &
                       tau, rank, sval, dwork, info)
            character, intent(inout)          :: jobrq
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: jpvt(*)
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: svlmax
            double precision, intent(inout)   :: tau(*)
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: sval(3)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03pd
    end interface
    public :: mb03pd
    
    interface 
        subroutine mb03py(m, n, a, lda, rcond, svlmax, rank, sval, &
                       jpvt, tau, dwork, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: svlmax
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: sval(3)
            integer, intent(inout)            :: jpvt(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03py
    end interface
    public :: mb03py
    
    interface 
        subroutine mb03qd(dico, stdom, jobu, n, nlow, nsup, alpha, a, &
                       lda, u, ldu, ndim, dwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: stdom
            character, intent(inout)          :: jobu
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nlow
            integer, intent(inout)            :: nsup
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            integer, intent(inout)            :: ndim
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03qd
    end interface
    public :: mb03qd
    
    interface 
        subroutine mb03qg(dico, stdom, jobu, jobv, n, nlow, nsup, alpha, &
                       a, lda, e, lde, u, ldu, v, ldv, &
                       ndim, dwork, ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: stdom
            character, intent(inout)          :: jobu
            character, intent(inout)          :: jobv
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nlow
            integer, intent(inout)            :: nsup
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            integer, intent(inout)            :: ndim
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03qg
    end interface
    public :: mb03qg
    
    interface 
        subroutine mb03qv(n, s, lds, t, ldt, alphar, alphai, beta, &
                       info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: info
        end subroutine mb03qv
    end interface
    public :: mb03qv
    
    interface 
        subroutine mb03qw(n, l, a, lda, e, lde, u, ldu, &
                       v, ldv, alphar, alphai, beta, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: info
        end subroutine mb03qw
    end interface
    public :: mb03qw
    
    interface 
        subroutine mb03qx(n, t, ldt, wr, wi, info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(inout)            :: info
        end subroutine mb03qx
    end interface
    public :: mb03qx
    
    interface 
        subroutine mb03qy(n, l, a, lda, u, ldu, e1, e2, &
                       info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: e1
            double precision, intent(inout)   :: e2
            integer, intent(inout)            :: info
        end subroutine mb03qy
    end interface
    public :: mb03qy
    
    interface 
        subroutine mb03rd(jobx, sort, n, pmax, a, lda, x, ldx, &
                       nblcks, blsize, wr, wi, tol, dwork, info)
            character, intent(inout)          :: jobx
            character, intent(inout)          :: sort
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: pmax
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            integer, intent(inout)            :: nblcks
            integer, intent(inout)            :: blsize(*)
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03rd
    end interface
    public :: mb03rd
    
    interface 
        subroutine mb03rw(m, n, pmax, a, lda, b, ldb, c, &
                       ldc, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: pmax
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: info
        end subroutine mb03rw
    end interface
    public :: mb03rw
    
    interface 
        subroutine mb03rx(jobv, n, kl, ku, a, lda, x, ldx, &
                       wr, wi, dwork)
            character, intent(inout)          :: jobv
            integer, intent(inout)            :: n
            integer, intent(inout)            :: kl
            integer, intent(inout)            :: ku
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb03rx
    end interface
    public :: mb03rx
    
    interface 
        subroutine mb03ry(m, n, pmax, a, lda, b, ldb, c, &
                       ldc, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: pmax
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: info
        end subroutine mb03ry
    end interface
    public :: mb03ry
    
    interface 
        subroutine mb03rz(jobx, sort, n, pmax, a, lda, x, ldx, &
                       nblcks, blsize, w, tol, info)
            character, intent(inout)          :: jobx
            character, intent(inout)          :: sort
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: pmax
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: x(ldx, *)
            integer, intent(inout)            :: ldx
            integer, intent(inout)            :: nblcks
            integer, intent(inout)            :: blsize(*)
            complex*16, intent(inout)         :: w(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: info
        end subroutine mb03rz
    end interface
    public :: mb03rz
    
    interface 
        subroutine mb03sd(jobscl, n, a, lda, qg, ldqg, wr, wi, &
                       dwork, ldwork, info)
            character, intent(inout)          :: jobscl
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03sd
    end interface
    public :: mb03sd
    
    interface 
        subroutine mb03td(typ, compu, select, lower, n, a, lda, g, &
                       ldg, u1, ldu1, u2, ldu2, wr, wi, m, &
                       dwork, ldwork, info)
            character, intent(inout)          :: typ
            character, intent(inout)          :: compu
            logical, intent(inout)            :: select(*)
            logical, intent(inout)            :: lower(*)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(inout)            :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(inout)            :: ldu2
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03td
    end interface
    public :: mb03td
    
    interface 
        subroutine mb03ts(isham, wantu, n, a, lda, g, ldg, u1, &
                       ldu1, u2, ldu2, j1, n1, n2, dwork, info)
            logical, intent(inout)            :: isham
            logical, intent(inout)            :: wantu
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(inout)            :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(inout)            :: ldu2
            integer, intent(inout)            :: j1
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: n2
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03ts
    end interface
    public :: mb03ts
    
    interface 
        subroutine mb03ud(jobq, jobp, n, a, lda, q, ldq, sv, &
                       dwork, ldwork, info)
            character, intent(inout)          :: jobq
            character, intent(inout)          :: jobp
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: sv(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03ud
    end interface
    public :: mb03ud
    
    interface 
        subroutine mb03vd(n, p, ilo, ihi, a, lda1, lda2, tau, &
                       ldtau, dwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ilo
            integer, intent(inout)            :: ihi
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: tau(ldtau, *)
            integer, intent(inout)            :: ldtau
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03vd
    end interface
    public :: mb03vd
    
    interface 
        subroutine mb03vw(compq, qind, triu, n, k, h, ilo, ihi, &
                       s, a, lda1, lda2, q, ldq1, ldq2, iwork, &
                       liwork, dwork, ldwork, info)
            character, intent(inout)          :: compq
            integer, intent(inout)            :: qind(*)
            character, intent(inout)          :: triu
            integer, intent(inout)            :: n
            integer, intent(inout)            :: k
            integer, intent(inout)            :: h
            integer, intent(inout)            :: ilo
            integer, intent(inout)            :: ihi
            integer, intent(inout)            :: s(*)
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: q(ldq1, ldq2, *)
            integer, intent(inout)            :: ldq1
            integer, intent(inout)            :: ldq2
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03vw
    end interface
    public :: mb03vw
    
    interface 
        subroutine mb03vy(n, p, ilo, ihi, a, lda1, lda2, tau, &
                       ldtau, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ilo
            integer, intent(inout)            :: ihi
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(inout)            :: lda1
            integer, intent(inout)            :: lda2
            double precision, intent(inout)   :: tau(ldtau, *)
            integer, intent(inout)            :: ldtau
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03vy
    end interface
    public :: mb03vy
    
    interface 
        subroutine mb03wa(wantq, wantz, n1, n2, a, lda, b, ldb, &
                       q, ldq, z, ldz, info)
            logical, intent(inout)            :: wantq
            logical, intent(inout)            :: wantz
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: n2
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: info
        end subroutine mb03wa
    end interface
    public :: mb03wa
    
    interface 
        subroutine mb03wd(job, compz, n, p, ilo, ihi, iloz, ihiz, &
                       h, ldh1, ldh2, z, ldz1, ldz2, wr, wi, &
                       dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: compz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ilo
            integer, intent(inout)            :: ihi
            integer, intent(inout)            :: iloz
            integer, intent(inout)            :: ihiz
            double precision, intent(inout)   :: h(ldh1, ldh2, *)
            integer, intent(inout)            :: ldh1
            integer, intent(inout)            :: ldh2
            double precision, intent(inout)   :: z(ldz1, ldz2, *)
            integer, intent(inout)            :: ldz1
            integer, intent(inout)            :: ldz2
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03wd
    end interface
    public :: mb03wd
    
    interface 
        subroutine mb03wx(n, p, t, ldt1, ldt2, wr, wi, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: t(ldt1, ldt2, *)
            integer, intent(inout)            :: ldt1
            integer, intent(inout)            :: ldt2
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(inout)            :: info
        end subroutine mb03wx
    end interface
    public :: mb03wx
    
    interface 
        subroutine mb03xd(balanc, job, jobu, jobv, n, a, lda, qg, &
                       ldqg, t, ldt, u1, ldu1, u2, ldu2, v1, &
                       ldv1, v2, ldv2, wr, wi, ilo, scale, dwork, &
                       ldwork, info)
            character, intent(inout)          :: balanc
            character, intent(inout)          :: job
            character, intent(inout)          :: jobu
            character, intent(inout)          :: jobv
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(inout)            :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(inout)            :: ldu2
            double precision, intent(inout)   :: v1(ldv1, *)
            integer, intent(inout)            :: ldv1
            double precision, intent(inout)   :: v2(ldv2, *)
            integer, intent(inout)            :: ldv2
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: scale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03xd
    end interface
    public :: mb03xd
    
    interface 
        subroutine mb03xp(job, compq, compz, n, ilo, ihi, a, lda, &
                       b, ldb, q, ldq, z, ldz, alphar, alphai, &
                       beta, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: compq
            character, intent(inout)          :: compz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            integer, intent(inout)            :: ihi
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03xp
    end interface
    public :: mb03xp
    
    interface 
        subroutine mb03xs(jobu, n, a, lda, qg, ldqg, u1, ldu1, &
                       u2, ldu2, wr, wi, dwork, ldwork, info)
            character, intent(inout)          :: jobu
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(inout)            :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(inout)            :: ldu2
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03xs
    end interface
    public :: mb03xs
    
    interface 
        subroutine mb03xu(ltra, ltrb, n, k, nb, a, lda, b, &
                       ldb, g, ldg, q, ldq, xa, ldxa, xb, &
                       ldxb, xg, ldxg, xq, ldxq, ya, ldya, yb, &
                       ldyb, yg, ldyg, yq, ldyq, csl, csr, taul, &
                       taur, dwork)
            logical, intent(inout)            :: ltra
            logical, intent(inout)            :: ltrb
            integer, intent(inout)            :: n
            integer, intent(inout)            :: k
            integer, intent(inout)            :: nb
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: xa(ldxa, *)
            integer, intent(inout)            :: ldxa
            double precision, intent(inout)   :: xb(ldxb, *)
            integer, intent(inout)            :: ldxb
            double precision, intent(inout)   :: xg(ldxg, *)
            integer, intent(inout)            :: ldxg
            double precision, intent(inout)   :: xq(ldxq, *)
            integer, intent(inout)            :: ldxq
            double precision, intent(inout)   :: ya(ldya, *)
            integer, intent(inout)            :: ldya
            double precision, intent(inout)   :: yb(ldyb, *)
            integer, intent(inout)            :: ldyb
            double precision, intent(inout)   :: yg(ldyg, *)
            integer, intent(inout)            :: ldyg
            double precision, intent(inout)   :: yq(ldyq, *)
            integer, intent(inout)            :: ldyq
            double precision, intent(inout)   :: csl(*)
            double precision, intent(inout)   :: csr(*)
            double precision, intent(inout)   :: taul(*)
            double precision, intent(inout)   :: taur(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb03xu
    end interface
    public :: mb03xu
    
    interface 
        subroutine mb03xz(balanc, job, jobu, n, a, lda, qg, ldqg, &
                       u1, ldu1, u2, ldu2, wr, wi, ilo, scale, &
                       dwork, ldwork, zwork, lzwork, bwork, info)
            character, intent(inout)          :: balanc
            character, intent(inout)          :: job
            character, intent(inout)          :: jobu
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            complex*16, intent(inout)         :: u1(ldu1, *)
            integer, intent(inout)            :: ldu1
            complex*16, intent(inout)         :: u2(ldu2, *)
            integer, intent(inout)            :: ldu2
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: scale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine mb03xz
    end interface
    public :: mb03xz
    
    interface 
        subroutine mb03ya(wantt, wantq, wantz, n, ilo, ihi, iloq, ihiq, &
                       pos, a, lda, b, ldb, q, ldq, z, &
                       ldz, info)
            logical, intent(inout)            :: wantt
            logical, intent(inout)            :: wantq
            logical, intent(inout)            :: wantz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            integer, intent(inout)            :: ihi
            integer, intent(inout)            :: iloq
            integer, intent(inout)            :: ihiq
            integer, intent(inout)            :: pos
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: info
        end subroutine mb03ya
    end interface
    public :: mb03ya
    
    interface 
        subroutine mb03yd(wantt, wantq, wantz, n, ilo, ihi, iloq, ihiq, &
                       a, lda, b, ldb, q, ldq, z, ldz, &
                       alphar, alphai, beta, dwork, ldwork, info)
            logical, intent(inout)            :: wantt
            logical, intent(inout)            :: wantq
            logical, intent(inout)            :: wantz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            integer, intent(inout)            :: ihi
            integer, intent(inout)            :: iloq
            integer, intent(inout)            :: ihiq
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03yd
    end interface
    public :: mb03yd
    
    interface 
        subroutine mb03yt(a, lda, b, ldb, alphar, alphai, beta, csl, &
                       snl, csr, snr)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: alphar(2)
            double precision, intent(inout)   :: alphai(2)
            double precision, intent(inout)   :: beta(2)
            double precision, intent(inout)   :: csl
            double precision, intent(inout)   :: snl
            double precision, intent(inout)   :: csr
            double precision, intent(inout)   :: snr
        end subroutine mb03yt
    end interface
    public :: mb03yt
    
    interface 
        subroutine mb03za(compc, compu, compv, compw, which, select, n, a, &
                       lda, b, ldb, c, ldc, u1, ldu1, u2, &
                       ldu2, v1, ldv1, v2, ldv2, w, ldw, wr, &
                       wi, m, dwork, ldwork, info)
            character, intent(inout)          :: compc
            character, intent(inout)          :: compu
            character, intent(inout)          :: compv
            character, intent(inout)          :: compw
            character, intent(inout)          :: which
            logical, intent(inout)            :: select(*)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(inout)            :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(inout)            :: ldu2
            double precision, intent(inout)   :: v1(ldv1, *)
            integer, intent(inout)            :: ldv1
            double precision, intent(inout)   :: v2(ldv2, *)
            integer, intent(inout)            :: ldv2
            double precision, intent(inout)   :: w(ldw, *)
            integer, intent(inout)            :: ldw
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03za
    end interface
    public :: mb03za
    
    interface 
        subroutine mb03zd(which, meth, stab, balanc, ortbal, select, n, mm, &
                       ilo, scale, s, lds, t, ldt, g, ldg, &
                       u1, ldu1, u2, ldu2, v1, ldv1, v2, ldv2, &
                       m, wr, wi, us, ldus, uu, lduu, lwork, &
                       iwork, dwork, ldwork, info)
            character, intent(inout)          :: which
            character, intent(inout)          :: meth
            character, intent(inout)          :: stab
            character, intent(inout)          :: balanc
            character, intent(inout)          :: ortbal
            logical, intent(inout)            :: select(*)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: mm
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: scale(*)
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(inout)            :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(inout)            :: ldu2
            double precision, intent(inout)   :: v1(ldv1, *)
            integer, intent(inout)            :: ldv1
            double precision, intent(inout)   :: v2(ldv2, *)
            integer, intent(inout)            :: ldv2
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: us(ldus, *)
            integer, intent(inout)            :: ldus
            double precision, intent(inout)   :: uu(lduu, *)
            integer, intent(inout)            :: lduu
            logical, intent(inout)            :: lwork(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb03zd
    end interface
    public :: mb03zd
    
    interface 
        subroutine mb04ad(job, compq1, compq2, compu1, compu2, n, z, ldz, &
                       h, ldh, q1, ldq1, q2, ldq2, u11, ldu11, &
                       u12, ldu12, u21, ldu21, u22, ldu22, t, ldt, &
                       alphar, alphai, beta, iwork, liwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: compq1
            character, intent(inout)          :: compq2
            character, intent(inout)          :: compu1
            character, intent(inout)          :: compu2
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)            :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)            :: ldq2
            double precision, intent(inout)   :: u11(ldu11, *)
            integer, intent(inout)            :: ldu11
            double precision, intent(inout)   :: u12(ldu12, *)
            integer, intent(inout)            :: ldu12
            double precision, intent(inout)   :: u21(ldu21, *)
            integer, intent(inout)            :: ldu21
            double precision, intent(inout)   :: u22(ldu22, *)
            integer, intent(inout)            :: ldu22
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04ad
    end interface
    public :: mb04ad
    
    interface 
        subroutine mb04az(job, compq, compu, n, z, ldz, b, ldb, &
                       fg, ldfg, d, ldd, c, ldc, q, ldq, &
                       u, ldu, alphar, alphai, beta, iwork, liwork, dwork, &
                       ldwork, zwork, lzwork, bwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: compq
            character, intent(inout)          :: compu
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: z(ldz, *)
            integer, intent(inout)            :: ldz
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: fg(ldfg, *)
            integer, intent(inout)            :: ldfg
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(inout)            :: ldd
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(inout)            :: ldc
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(inout)            :: ldq
            complex*16, intent(inout)         :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine mb04az
    end interface
    public :: mb04az
    
    interface 
        subroutine mb04bd(job, compq1, compq2, n, a, lda, de, ldde, &
                       c1, ldc1, vw, ldvw, q1, ldq1, q2, ldq2, &
                       b, ldb, f, ldf, c2, ldc2, alphar, alphai, &
                       beta, iwork, liwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: compq1
            character, intent(inout)          :: compq2
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(inout)            :: ldde
            double precision, intent(inout)   :: c1(ldc1, *)
            integer, intent(inout)            :: ldc1
            double precision, intent(inout)   :: vw(ldvw, *)
            integer, intent(inout)            :: ldvw
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)            :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)            :: ldq2
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: c2(ldc2, *)
            integer, intent(inout)            :: ldc2
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04bd
    end interface
    public :: mb04bd
    
    interface 
        subroutine mb04bp(job, compq1, compq2, n, a, lda, de, ldde, &
                       c1, ldc1, vw, ldvw, q1, ldq1, q2, ldq2, &
                       b, ldb, f, ldf, c2, ldc2, alphar, alphai, &
                       beta, iwork, liwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: compq1
            character, intent(inout)          :: compq2
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(inout)            :: ldde
            double precision, intent(inout)   :: c1(ldc1, *)
            integer, intent(inout)            :: ldc1
            double precision, intent(inout)   :: vw(ldvw, *)
            integer, intent(inout)            :: ldvw
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)            :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)            :: ldq2
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: c2(ldc2, *)
            integer, intent(inout)            :: ldc2
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04bp
    end interface
    public :: mb04bp
    
    interface 
        subroutine mb04bz(job, compq, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, q, ldq, alphar, alphai, beta, &
                       iwork, dwork, ldwork, zwork, lzwork, bwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: compq
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: de(ldde, *)
            integer, intent(inout)            :: ldde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: fg(ldfg, *)
            integer, intent(inout)            :: ldfg
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine mb04bz
    end interface
    public :: mb04bz
    
    interface 
        subroutine mb04cd(compq1, compq2, compq3, n, a, lda, b, ldb, &
                       d, ldd, q1, ldq1, q2, ldq2, q3, ldq3, &
                       iwork, liwork, dwork, ldwork, bwork, info)
            character, intent(inout)          :: compq1
            character, intent(inout)          :: compq2
            character, intent(inout)          :: compq3
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)            :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)            :: ldq2
            double precision, intent(inout)   :: q3(ldq3, *)
            integer, intent(inout)            :: ldq3
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine mb04cd
    end interface
    public :: mb04cd
    
    interface 
        subroutine mb04db(job, sgn, n, ilo, lscale, rscale, m, v1, &
                       ldv1, v2, ldv2, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: sgn
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: lscale(*)
            double precision, intent(inout)   :: rscale(*)
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: v1(ldv1, *)
            integer, intent(inout)            :: ldv1
            double precision, intent(inout)   :: v2(ldv2, *)
            integer, intent(inout)            :: ldv2
            integer, intent(inout)            :: info
        end subroutine mb04db
    end interface
    public :: mb04db
    
    interface 
        subroutine mb04dd(job, n, a, lda, qg, ldqg, ilo, scale, &
                       info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: scale(*)
            integer, intent(inout)            :: info
        end subroutine mb04dd
    end interface
    public :: mb04dd
    
    interface 
        subroutine mb04di(job, sgn, n, ilo, scale, m, v1, ldv1, &
                       v2, ldv2, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: sgn
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: scale(*)
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: v1(ldv1, *)
            integer, intent(inout)            :: ldv1
            double precision, intent(inout)   :: v2(ldv2, *)
            integer, intent(inout)            :: ldv2
            integer, intent(inout)            :: info
        end subroutine mb04di
    end interface
    public :: mb04di
    
    interface 
        subroutine mb04dl(job, n, thresh, a, lda, b, ldb, ilo, &
                       ihi, lscale, rscale, dwork, iwarn, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: thresh
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            integer, intent(inout)            :: ilo
            integer, intent(inout)            :: ihi
            double precision, intent(inout)   :: lscale(*)
            double precision, intent(inout)   :: rscale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mb04dl
    end interface
    public :: mb04dl
    
    interface 
        subroutine mb04dp(job, n, thresh, a, lda, de, ldde, c, &
                       ldc, vw, ldvw, ilo, lscale, rscale, dwork, iwarn, &
                       info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: thresh
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(inout)            :: ldde
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: vw(ldvw, *)
            integer, intent(inout)            :: ldvw
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: lscale(*)
            double precision, intent(inout)   :: rscale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mb04dp
    end interface
    public :: mb04dp
    
    interface 
        subroutine mb04ds(job, n, a, lda, qg, ldqg, ilo, scale, &
                       info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: scale(*)
            integer, intent(inout)            :: info
        end subroutine mb04ds
    end interface
    public :: mb04ds
    
    interface 
        subroutine mb04dy(jobscl, n, a, lda, qg, ldqg, d, dwork, &
                       info)
            character, intent(inout)          :: jobscl
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            double precision, intent(inout)   :: d(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb04dy
    end interface
    public :: mb04dy
    
    interface 
        subroutine mb04dz(job, n, a, lda, qg, ldqg, ilo, scale, &
                       info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: scale(*)
            integer, intent(inout)            :: info
        end subroutine mb04dz
    end interface
    public :: mb04dz
    
    interface 
        subroutine mb04ed(job, compq, compu, n, z, ldz, b, ldb, &
                       fg, ldfg, q, ldq, u1, ldu1, u2, ldu2, &
                       alphar, alphai, beta, iwork, liwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: compq
            character, intent(inout)          :: compu
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: fg(ldfg, *)
            integer, intent(inout)            :: ldfg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(inout)            :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(inout)            :: ldu2
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04ed
    end interface
    public :: mb04ed
    
    interface 
        subroutine mb04fd(job, compq, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, q, ldq, alphar, alphai, beta, &
                       iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: compq
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(inout)            :: ldde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: fg(ldfg, *)
            integer, intent(inout)            :: ldfg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04fd
    end interface
    public :: mb04fd
    
    interface 
        subroutine mb04fp(job, compq, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, q, ldq, alphar, alphai, beta, &
                       iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: compq
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(inout)            :: ldde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: fg(ldfg, *)
            integer, intent(inout)            :: ldfg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04fp
    end interface
    public :: mb04fp
    
    interface 
        subroutine mb04gd(m, n, a, lda, jpvt, tau, dwork, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: jpvt(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb04gd
    end interface
    public :: mb04gd
    
    interface 
        subroutine mb04hd(compq1, compq2, n, a, lda, b, ldb, q1, &
                       ldq1, q2, ldq2, iwork, liwork, dwork, ldwork, bwork, &
                       info)
            character, intent(inout)          :: compq1
            character, intent(inout)          :: compq2
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)            :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)            :: ldq2
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine mb04hd
    end interface
    public :: mb04hd
    
    interface 
        subroutine mb04id(n, m, p, l, a, lda, b, ldb, &
                       tau, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04id
    end interface
    public :: mb04id
    
    interface 
        subroutine mb04iy(side, trans, n, m, k, p, a, lda, &
                       tau, c, ldc, dwork, ldwork, info)
            character, intent(inout)          :: side
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: k
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04iy
    end interface
    public :: mb04iy
    
    interface 
        subroutine mb04iz(n, m, p, l, a, lda, b, ldb, &
                       tau, zwork, lzwork, info)
            integer, intent(inout)      :: n
            integer, intent(inout)      :: m
            integer, intent(inout)      :: p
            integer, intent(inout)      :: l
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(inout)      :: lda
            complex*16, intent(inout)   :: b(ldb, *)
            integer, intent(inout)      :: ldb
            complex*16, intent(inout)   :: tau(*)
            complex*16, intent(inout)   :: zwork(*)
            integer, intent(inout)      :: lzwork
            integer, intent(inout)      :: info
        end subroutine mb04iz
    end interface
    public :: mb04iz
    
    interface 
        subroutine mb04jd(n, m, p, l, a, lda, b, ldb, &
                       tau, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04jd
    end interface
    public :: mb04jd
    
    interface 
        subroutine mb04kd(uplo, n, m, p, r, ldr, a, lda, &
                       b, ldb, c, ldc, tau, dwork)
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04kd
    end interface
    public :: mb04kd
    
    interface 
        subroutine mb04ld(uplo, n, m, p, l, ldl, a, lda, &
                       b, ldb, c, ldc, tau, dwork)
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: l(ldl, *)
            integer, intent(inout)            :: ldl
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04ld
    end interface
    public :: mb04ld
    
    interface 
        subroutine mb04md(n, maxred, a, lda, scale, info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: maxred
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: scale(*)
            integer, intent(inout)            :: info
        end subroutine mb04md
    end interface
    public :: mb04md
    
    interface 
        subroutine mb04nd(uplo, n, m, p, r, ldr, a, lda, &
                       b, ldb, c, ldc, tau, dwork)
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04nd
    end interface
    public :: mb04nd
    
    interface 
        subroutine mb04ny(m, n, v, incv, tau, a, lda, b, &
                       ldb, dwork)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: v(*)
            integer, intent(inout)            :: incv
            double precision, intent(inout)   :: tau
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04ny
    end interface
    public :: mb04ny
    
    interface 
        subroutine mb04od(uplo, n, m, p, r, ldr, a, lda, &
                       b, ldb, c, ldc, tau, dwork)
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04od
    end interface
    public :: mb04od
    
    interface 
        subroutine mb04ow(m, n, p, a, lda, t, ldt, x, &
                       incx, b, ldb, c, ldc, d, incd)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: incx
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: incd
        end subroutine mb04ow
    end interface
    public :: mb04ow
    
    interface 
        subroutine mb04ox(n, a, lda, x, incx)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: incx
        end subroutine mb04ox
    end interface
    public :: mb04ox
    
    interface 
        subroutine mb04oy(m, n, v, tau, a, lda, b, ldb, &
                       dwork)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: v(*)
            double precision, intent(inout)   :: tau
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04oy
    end interface
    public :: mb04oy
    
    interface 
        subroutine mb04pa(lham, n, k, nb, a, lda, qg, ldqg, &
                       xa, ldxa, xg, ldxg, xq, ldxq, ya, ldya, &
                       cs, tau, dwork)
            logical, intent(inout)            :: lham
            integer, intent(inout)            :: n
            integer, intent(inout)            :: k
            integer, intent(inout)            :: nb
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            double precision, intent(inout)   :: xa(ldxa, *)
            integer, intent(inout)            :: ldxa
            double precision, intent(inout)   :: xg(ldxg, *)
            integer, intent(inout)            :: ldxg
            double precision, intent(inout)   :: xq(ldxq, *)
            integer, intent(inout)            :: ldxq
            double precision, intent(inout)   :: ya(ldya, *)
            integer, intent(inout)            :: ldya
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04pa
    end interface
    public :: mb04pa
    
    interface 
        subroutine mb04pb(n, ilo, a, lda, qg, ldqg, cs, tau, &
                       dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04pb
    end interface
    public :: mb04pb
    
    interface 
        subroutine mb04pu(n, ilo, a, lda, qg, ldqg, cs, tau, &
                       dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04pu
    end interface
    public :: mb04pu
    
    interface 
        subroutine mb04py(side, m, n, v, tau, c, ldc, dwork)
            character, intent(inout)          :: side
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: v(*)
            double precision, intent(inout)   :: tau
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04py
    end interface
    public :: mb04py
    
    interface 
        subroutine mb04qb(tranc, trand, tranq, storev, storew, m, n, k, &
                       v, ldv, w, ldw, c, ldc, d, ldd, &
                       cs, tau, dwork, ldwork, info)
            character, intent(inout)          :: tranc
            character, intent(inout)          :: trand
            character, intent(inout)          :: tranq
            character, intent(inout)          :: storev
            character, intent(inout)          :: storew
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            double precision, intent(inout)   :: w(ldw, *)
            integer, intent(inout)            :: ldw
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04qb
    end interface
    public :: mb04qb
    
    interface 
        subroutine mb04qc(strab, trana, tranb, tranq, direct, storev, storew, m, &
                       n, k, v, ldv, w, ldw, rs, ldrs, &
                       t, ldt, a, lda, b, ldb, dwork)
            character, intent(inout)          :: strab
            character, intent(inout)          :: trana
            character, intent(inout)          :: tranb
            character, intent(inout)          :: tranq
            character, intent(inout)          :: direct
            character, intent(inout)          :: storev
            character, intent(inout)          :: storew
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            double precision, intent(inout)   :: w(ldw, *)
            integer, intent(inout)            :: ldw
            double precision, intent(inout)   :: rs(ldrs, *)
            integer, intent(inout)            :: ldrs
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04qc
    end interface
    public :: mb04qc
    
    interface 
        subroutine mb04qf(direct, storev, storew, n, k, v, ldv, w, &
                       ldw, cs, tau, rs, ldrs, t, ldt, dwork)
            character, intent(inout)          :: direct
            character, intent(inout)          :: storev
            character, intent(inout)          :: storew
            integer, intent(inout)            :: n
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            double precision, intent(inout)   :: w(ldw, *)
            integer, intent(inout)            :: ldw
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: rs(ldrs, *)
            integer, intent(inout)            :: ldrs
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04qf
    end interface
    public :: mb04qf
    
    interface 
        subroutine mb04qs(tranc, trand, tranu, m, n, ilo, v, ldv, &
                       w, ldw, c, ldc, d, ldd, cs, tau, &
                       dwork, ldwork, info)
            character, intent(inout)          :: tranc
            character, intent(inout)          :: trand
            character, intent(inout)          :: tranu
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            double precision, intent(inout)   :: w(ldw, *)
            integer, intent(inout)            :: ldw
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04qs
    end interface
    public :: mb04qs
    
    interface 
        subroutine mb04qu(tranc, trand, tranq, storev, storew, m, n, k, &
                       v, ldv, w, ldw, c, ldc, d, ldd, &
                       cs, tau, dwork, ldwork, info)
            character, intent(inout)          :: tranc
            character, intent(inout)          :: trand
            character, intent(inout)          :: tranq
            character, intent(inout)          :: storev
            character, intent(inout)          :: storew
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            double precision, intent(inout)   :: w(ldw, *)
            integer, intent(inout)            :: ldw
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04qu
    end interface
    public :: mb04qu
    
    interface 
        subroutine mb04rb(n, ilo, a, lda, qg, ldqg, cs, tau, &
                       dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04rb
    end interface
    public :: mb04rb
    
    interface 
        subroutine mb04rd(jobx, joby, sort, n, pmax, a, lda, b, &
                       ldb, x, ldx, y, ldy, nblcks, blsize, alphar, &
                       alphai, beta, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: jobx
            character, intent(inout)          :: joby
            character, intent(inout)          :: sort
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: pmax
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            integer, intent(inout)            :: nblcks
            integer, intent(inout)            :: blsize(*)
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04rd
    end interface
    public :: mb04rd
    
    interface 
        subroutine mb04rs(m, n, pmax, a, lda, b, ldb, c, &
                       ldc, d, ldd, e, lde, f, ldf, scale, &
                       iwork, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: pmax
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: scale
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: info
        end subroutine mb04rs
    end interface
    public :: mb04rs
    
    interface 
        subroutine mb04rt(m, n, pmax, a, lda, b, ldb, c, &
                       ldc, d, ldd, e, lde, f, ldf, scale, &
                       iwork, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: pmax
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: scale
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: info
        end subroutine mb04rt
    end interface
    public :: mb04rt
    
    interface 
        subroutine mb04ru(n, ilo, a, lda, qg, ldqg, cs, tau, &
                       dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04ru
    end interface
    public :: mb04ru
    
    interface 
        subroutine mb04rv(m, n, pmax, a, lda, b, ldb, c, &
                       ldc, d, ldd, e, lde, f, ldf, scale, &
                       info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: pmax
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(inout)            :: ldc
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(inout)            :: ldd
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(inout)            :: lde
            complex*16, intent(inout)         :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: scale
            integer, intent(inout)            :: info
        end subroutine mb04rv
    end interface
    public :: mb04rv
    
    interface 
        subroutine mb04rw(m, n, pmax, a, lda, b, ldb, c, &
                       ldc, d, ldd, e, lde, f, ldf, scale, &
                       iwork, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: pmax
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(inout)            :: ldc
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(inout)            :: ldd
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(inout)            :: lde
            complex*16, intent(inout)         :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: scale
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: info
        end subroutine mb04rw
    end interface
    public :: mb04rw
    
    interface 
        subroutine mb04rz(jobx, joby, sort, n, pmax, a, lda, b, &
                       ldb, x, ldx, y, ldy, nblcks, blsize, alpha, &
                       beta, tol, iwork, info)
            character, intent(inout)          :: jobx
            character, intent(inout)          :: joby
            character, intent(inout)          :: sort
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: pmax
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: x(ldx, *)
            integer, intent(inout)            :: ldx
            complex*16, intent(inout)         :: y(ldy, *)
            integer, intent(inout)            :: ldy
            integer, intent(inout)            :: nblcks
            integer, intent(inout)            :: blsize(*)
            complex*16, intent(inout)         :: alpha(*)
            complex*16, intent(inout)         :: beta(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: info
        end subroutine mb04rz
    end interface
    public :: mb04rz
    
    interface 
        subroutine mb04su(m, n, a, lda, b, ldb, cs, tau, &
                       dwork, ldwork, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04su
    end interface
    public :: mb04su
    
    interface 
        subroutine mb04tb(trana, tranb, n, ilo, a, lda, b, ldb, &
                       g, ldg, q, ldq, csl, csr, taul, taur, &
                       dwork, ldwork, info)
            character, intent(inout)          :: trana
            character, intent(inout)          :: tranb
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: csl(*)
            double precision, intent(inout)   :: csr(*)
            double precision, intent(inout)   :: taul(*)
            double precision, intent(inout)   :: taur(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04tb
    end interface
    public :: mb04tb
    
    interface 
        subroutine mb04ts(trana, tranb, n, ilo, a, lda, b, ldb, &
                       g, ldg, q, ldq, csl, csr, taul, taur, &
                       dwork, ldwork, info)
            character, intent(inout)          :: trana
            character, intent(inout)          :: tranb
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: csl(*)
            double precision, intent(inout)   :: csr(*)
            double precision, intent(inout)   :: taul(*)
            double precision, intent(inout)   :: taur(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04ts
    end interface
    public :: mb04ts
    
    interface 
        subroutine mb04tt(updatq, updatz, m, n, ifira, ifica, nca, a, &
                       lda, e, lde, q, ldq, z, ldz, istair, &
                       rank, tol, iwork)
            logical, intent(inout)            :: updatq
            logical, intent(inout)            :: updatz
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ifira
            integer, intent(inout)            :: ifica
            integer, intent(inout)            :: nca
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: istair(*)
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
        end subroutine mb04tt
    end interface
    public :: mb04tt
    
    interface 
        subroutine mb04tu(n, x, incx, y, incy, c, s)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: incx
            double precision, intent(inout)   :: y(*)
            integer, intent(inout)            :: incy
            double precision, intent(inout)   :: c
            double precision, intent(inout)   :: s
        end subroutine mb04tu
    end interface
    public :: mb04tu
    
    interface 
        subroutine mb04tv(updatz, n, nra, nca, ifira, ifica, a, lda, &
                       e, lde, z, ldz)
            logical, intent(inout)            :: updatz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nra
            integer, intent(inout)            :: nca
            integer, intent(inout)            :: ifira
            integer, intent(inout)            :: ifica
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
        end subroutine mb04tv
    end interface
    public :: mb04tv
    
    interface 
        subroutine mb04tw(updatq, m, n, nre, nce, ifire, ifice, ifica, &
                       a, lda, e, lde, q, ldq)
            logical, intent(inout)            :: updatq
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nre
            integer, intent(inout)            :: nce
            integer, intent(inout)            :: ifire
            integer, intent(inout)            :: ifice
            integer, intent(inout)            :: ifica
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
        end subroutine mb04tw
    end interface
    public :: mb04tw
    
    interface 
        subroutine mb04tx(updatq, updatz, m, n, nblcks, inuk, imuk, a, &
                       lda, e, lde, q, ldq, z, ldz, mnei)
            logical, intent(inout)            :: updatq
            logical, intent(inout)            :: updatz
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nblcks
            integer, intent(inout)            :: inuk(*)
            integer, intent(inout)            :: imuk(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: mnei(4)
        end subroutine mb04tx
    end interface
    public :: mb04tx
    
    interface 
        subroutine mb04ty(updatq, updatz, m, n, nblcks, inuk, imuk, a, &
                       lda, e, lde, q, ldq, z, ldz, info)
            logical, intent(inout)            :: updatq
            logical, intent(inout)            :: updatz
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nblcks
            integer, intent(inout)            :: inuk(*)
            integer, intent(inout)            :: imuk(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: info
        end subroutine mb04ty
    end interface
    public :: mb04ty
    
    interface 
        subroutine mb04ud(jobq, jobz, m, n, a, lda, e, lde, &
                       q, ldq, z, ldz, ranke, istair, tol, dwork, &
                       info)
            character, intent(inout)          :: jobq
            character, intent(inout)          :: jobz
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: ranke
            integer, intent(inout)            :: istair(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb04ud
    end interface
    public :: mb04ud
    
    interface 
        subroutine mb04vd(mode, jobq, jobz, m, n, ranke, a, lda, &
                       e, lde, q, ldq, z, ldz, istair, nblcks, &
                       nblcki, imuk, inuk, imuk0, mnei, tol, iwork, info)
            character, intent(inout)          :: mode
            character, intent(inout)          :: jobq
            character, intent(inout)          :: jobz
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ranke
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: istair(*)
            integer, intent(inout)            :: nblcks
            integer, intent(inout)            :: nblcki
            integer, intent(inout)            :: imuk(*)
            integer, intent(inout)            :: inuk(*)
            integer, intent(inout)            :: imuk0(*)
            integer, intent(inout)            :: mnei(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: info
        end subroutine mb04vd
    end interface
    public :: mb04vd
    
    interface 
        subroutine mb04vx(updatq, updatz, m, n, nblcks, inuk, imuk, a, &
                       lda, e, lde, q, ldq, z, ldz, mnei)
            logical, intent(inout)            :: updatq
            logical, intent(inout)            :: updatz
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nblcks
            integer, intent(inout)            :: inuk(*)
            integer, intent(inout)            :: imuk(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: mnei(3)
        end subroutine mb04vx
    end interface
    public :: mb04vx
    
    interface 
        subroutine mb04wd(tranq1, tranq2, m, n, k, q1, ldq1, q2, &
                       ldq2, cs, tau, dwork, ldwork, info)
            character, intent(inout)          :: tranq1
            character, intent(inout)          :: tranq2
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)            :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)            :: ldq2
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04wd
    end interface
    public :: mb04wd
    
    interface 
        subroutine mb04wp(n, ilo, u1, ldu1, u2, ldu2, cs, tau, &
                       dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(inout)            :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(inout)            :: ldu2
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04wp
    end interface
    public :: mb04wp
    
    interface 
        subroutine mb04wr(job, trans, n, ilo, q1, ldq1, q2, ldq2, &
                       cs, tau, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)            :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)            :: ldq2
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04wr
    end interface
    public :: mb04wr
    
    interface 
        subroutine mb04wu(tranq1, tranq2, m, n, k, q1, ldq1, q2, &
                       ldq2, cs, tau, dwork, ldwork, info)
            character, intent(inout)          :: tranq1
            character, intent(inout)          :: tranq2
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(inout)            :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(inout)            :: ldq2
            double precision, intent(inout)   :: cs(*)
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb04wu
    end interface
    public :: mb04wu
    
    interface 
        subroutine mb04xd(jobu, jobv, m, n, rank, theta, a, lda, &
                       u, ldu, v, ldv, q, inul, tol, reltol, &
                       dwork, ldwork, iwarn, info)
            character, intent(inout)          :: jobu
            character, intent(inout)          :: jobv
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: theta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            double precision, intent(inout)   :: q(*)
            logical, intent(inout)            :: inul(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: reltol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mb04xd
    end interface
    public :: mb04xd
    
    interface 
        subroutine mb04xy(jobu, jobv, m, n, x, ldx, taup, tauq, &
                       u, ldu, v, ldv, inul, info)
            character, intent(inout)          :: jobu
            character, intent(inout)          :: jobv
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: taup(*)
            double precision, intent(inout)   :: tauq(*)
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            logical, intent(inout)            :: inul(*)
            integer, intent(inout)            :: info
        end subroutine mb04xy
    end interface
    public :: mb04xy
    
    interface 
        subroutine mb04yd(jobu, jobv, m, n, rank, theta, q, e, &
                       u, ldu, v, ldv, inul, tol, reltol, dwork, &
                       ldwork, iwarn, info)
            character, intent(inout)          :: jobu
            character, intent(inout)          :: jobv
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: theta
            double precision, intent(inout)   :: q(*)
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            logical, intent(inout)            :: inul(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: reltol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mb04yd
    end interface
    public :: mb04yd
    
    interface 
        subroutine mb04yw(qrit, updatu, updatv, m, n, l, k, shift, &
                       d, e, u, ldu, v, ldv, dwork)
            logical, intent(inout)            :: qrit
            logical, intent(inout)            :: updatu
            logical, intent(inout)            :: updatv
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: l
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: shift
            double precision, intent(inout)   :: d(*)
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04yw
    end interface
    public :: mb04yw
    
    interface 
        subroutine mb04zd(compu, n, a, lda, qg, ldqg, u, ldu, &
                       dwork, info)
            character, intent(inout)          :: compu
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(inout)            :: ldqg
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mb04zd
    end interface
    public :: mb04zd
    
    interface 
        subroutine mb05md(balanc, n, delta, a, lda, v, ldv, y, &
                       ldy, valr, vali, iwork, dwork, ldwork, info)
            character, intent(inout)          :: balanc
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: delta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: valr(*)
            double precision, intent(inout)   :: vali(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb05md
    end interface
    public :: mb05md
    
    interface 
        subroutine mb05my(balanc, n, a, lda, wr, wi, r, ldr, &
                       q, ldq, dwork, ldwork, info)
            character, intent(inout)          :: balanc
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb05my
    end interface
    public :: mb05my
    
    interface 
        subroutine mb05nd(n, delta, a, lda, ex, ldex, exint, ldexin, &
                       tol, iwork, dwork, ldwork, info)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: delta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: ex(ldex, *)
            integer, intent(inout)            :: ldex
            double precision, intent(inout)   :: exint(ldexin, *)
            integer, intent(inout)            :: ldexin
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mb05nd
    end interface
    public :: mb05nd
    
    interface 
        subroutine mb05od(balanc, n, ndiag, delta, a, lda, mdig, idig, &
                       iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: balanc
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ndiag
            double precision, intent(inout)   :: delta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: mdig
            integer, intent(inout)            :: idig
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mb05od
    end interface
    public :: mb05od
    
    interface 
        subroutine mb05oy(job, n, low, igh, a, lda, scale, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            integer, intent(inout)            :: low
            integer, intent(inout)            :: igh
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: scale(*)
            integer, intent(inout)            :: info
        end subroutine mb05oy
    end interface
    public :: mb05oy
    
    interface 
        subroutine mb3jzp(compq, n, a, lda, d, ldd, b, ldb, &
                       f, ldf, q, ldq, neig, tol, dwork, zwork, &
                       info)
            character, intent(inout)          :: compq
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(inout)            :: ldd
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: f(ldf, *)
            integer, intent(inout)            :: ldf
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(inout)            :: ldq
            integer, intent(inout)            :: neig
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: info
        end subroutine mb3jzp
    end interface
    public :: mb3jzp
    
    interface 
        subroutine mb3lzp(compq, orth, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, neig, q, ldq, alphar, alphai, &
                       beta, iwork, dwork, ldwork, zwork, lzwork, bwork, info)
            character, intent(inout)          :: compq
            character, intent(inout)          :: orth
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: de(ldde, *)
            integer, intent(inout)            :: ldde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: fg(ldfg, *)
            integer, intent(inout)            :: ldfg
            integer, intent(inout)            :: neig
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine mb3lzp
    end interface
    public :: mb3lzp
    
    interface 
        subroutine mb3oyz(m, n, a, lda, rcond, svlmax, rank, sval, &
                       jpvt, tau, dwork, zwork, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: svlmax
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: sval(3)
            integer, intent(inout)            :: jpvt(*)
            complex*16, intent(inout)         :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: info
        end subroutine mb3oyz
    end interface
    public :: mb3oyz
    
    interface 
        subroutine mb3pyz(m, n, a, lda, rcond, svlmax, rank, sval, &
                       jpvt, tau, dwork, zwork, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: svlmax
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: sval(3)
            integer, intent(inout)            :: jpvt(*)
            complex*16, intent(inout)         :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: info
        end subroutine mb3pyz
    end interface
    public :: mb3pyz
    
    interface 
        subroutine mb4dbz(job, sgn, n, ilo, lscale, rscale, m, v1, &
                       ldv1, v2, ldv2, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: sgn
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: lscale(*)
            double precision, intent(inout)   :: rscale(*)
            integer, intent(inout)            :: m
            complex*16, intent(inout)         :: v1(ldv1, *)
            integer, intent(inout)            :: ldv1
            complex*16, intent(inout)         :: v2(ldv2, *)
            integer, intent(inout)            :: ldv2
            integer, intent(inout)            :: info
        end subroutine mb4dbz
    end interface
    public :: mb4dbz
    
    interface 
        subroutine mb4dlz(job, n, thresh, a, lda, b, ldb, ilo, &
                       ihi, lscale, rscale, dwork, iwarn, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: thresh
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            integer, intent(inout)            :: ilo
            integer, intent(inout)            :: ihi
            double precision, intent(inout)   :: lscale(*)
            double precision, intent(inout)   :: rscale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mb4dlz
    end interface
    public :: mb4dlz
    
    interface 
        subroutine mb4dpz(job, n, thresh, a, lda, de, ldde, c, &
                       ldc, vw, ldvw, ilo, lscale, rscale, dwork, iwarn, &
                       info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: thresh
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: de(ldde, *)
            integer, intent(inout)            :: ldde
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(inout)            :: ldc
            complex*16, intent(inout)         :: vw(ldvw, *)
            integer, intent(inout)            :: ldvw
            integer, intent(inout)            :: ilo
            double precision, intent(inout)   :: lscale(*)
            double precision, intent(inout)   :: rscale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mb4dpz
    end interface
    public :: mb4dpz
    
    interface 
        subroutine mc01md(dp, alpha, k, p, q, info)
            integer, intent(inout)            :: dp
            double precision, intent(inout)   :: alpha
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: p(*)
            double precision, intent(inout)   :: q(*)
            integer, intent(inout)            :: info
        end subroutine mc01md
    end interface
    public :: mc01md
    
    interface 
        subroutine mc01nd(dp, xr, xi, p, vr, vi, info)
            integer, intent(inout)            :: dp
            double precision, intent(inout)   :: xr
            double precision, intent(inout)   :: xi
            double precision, intent(inout)   :: p(*)
            double precision, intent(inout)   :: vr
            double precision, intent(inout)   :: vi
            integer, intent(inout)            :: info
        end subroutine mc01nd
    end interface
    public :: mc01nd
    
    interface 
        subroutine mc01od(k, rez, imz, rep, imp, dwork, info)
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: rez(*)
            double precision, intent(inout)   :: imz(*)
            double precision, intent(inout)   :: rep(*)
            double precision, intent(inout)   :: imp(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mc01od
    end interface
    public :: mc01od
    
    interface 
        subroutine mc01pd(k, rez, imz, p, dwork, info)
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: rez(*)
            double precision, intent(inout)   :: imz(*)
            double precision, intent(inout)   :: p(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mc01pd
    end interface
    public :: mc01pd
    
    interface 
        subroutine mc01py(k, rez, imz, p, dwork, info)
            integer, intent(inout)            :: k
            double precision, intent(inout)   :: rez(*)
            double precision, intent(inout)   :: imz(*)
            double precision, intent(inout)   :: p(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mc01py
    end interface
    public :: mc01py
    
    interface 
        subroutine mc01qd(da, db, a, b, rq, iwarn, info)
            integer, intent(inout)            :: da
            integer, intent(inout)            :: db
            double precision, intent(inout)   :: a(*)
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: rq(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mc01qd
    end interface
    public :: mc01qd
    
    interface 
        subroutine mc01rd(dp1, dp2, dp3, alpha, p1, p2, p3, info)
            integer, intent(inout)            :: dp1
            integer, intent(inout)            :: dp2
            integer, intent(inout)            :: dp3
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: p1(*)
            double precision, intent(inout)   :: p2(*)
            double precision, intent(inout)   :: p3(*)
            integer, intent(inout)            :: info
        end subroutine mc01rd
    end interface
    public :: mc01rd
    
    interface 
        subroutine mc01sd(dp, p, s, t, mant, e, iwork, info)
            integer, intent(inout)            :: dp
            double precision, intent(inout)   :: p(*)
            integer, intent(inout)            :: s
            integer, intent(inout)            :: t
            double precision, intent(inout)   :: mant(*)
            integer, intent(inout)            :: e(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: info
        end subroutine mc01sd
    end interface
    public :: mc01sd
    
    interface 
        subroutine mc01sw(a, b, m, e)
            double precision, intent(inout)   :: a
            integer, intent(inout)            :: b
            double precision, intent(inout)   :: m
            integer, intent(inout)            :: e
        end subroutine mc01sw
    end interface
    public :: mc01sw
    
    interface 
        integer function mc01sx (lb,ub,e,mant)
            integer, intent(inout)            :: lb
            integer, intent(inout)            :: ub
            integer, intent(inout)            :: e(*)
            double precision, intent(inout)   :: mant(*)
        end function mc01sx
    end interface
    public :: mc01sx
    
    interface 
        subroutine mc01sy(m, e, b, a, ovflow)
            double precision, intent(inout)   :: m
            integer, intent(inout)            :: e
            integer, intent(inout)            :: b
            double precision, intent(inout)   :: a
            logical, intent(inout)            :: ovflow
        end subroutine mc01sy
    end interface
    public :: mc01sy
    
    interface 
        subroutine mc01td(dico, dp, p, stable, nz, dwork, iwarn, info)
            character, intent(inout)          :: dico
            integer, intent(inout)            :: dp
            double precision, intent(inout)   :: p(*)
            logical, intent(inout)            :: stable
            integer, intent(inout)            :: nz
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine mc01td
    end interface
    public :: mc01td
    
    interface 
        subroutine mc01vd(a, b, c, z1re, z1im, z2re, z2im, info)
            double precision, intent(inout)   :: a
            double precision, intent(inout)   :: b
            double precision, intent(inout)   :: c
            double precision, intent(inout)   :: z1re
            double precision, intent(inout)   :: z1im
            double precision, intent(inout)   :: z2re
            double precision, intent(inout)   :: z2im
            integer, intent(inout)            :: info
        end subroutine mc01vd
    end interface
    public :: mc01vd
    
    interface 
        subroutine mc01wd(dp, p, u1, u2, q, info)
            integer, intent(inout)            :: dp
            double precision, intent(inout)   :: p(*)
            double precision, intent(inout)   :: u1
            double precision, intent(inout)   :: u2
            double precision, intent(inout)   :: q(*)
            integer, intent(inout)            :: info
        end subroutine mc01wd
    end interface
    public :: mc01wd
    
    interface 
        subroutine mc01xd(alpha, beta, gamma, delta, evr, evi, evq, dwork, &
                       ldwork, info)
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: beta
            double precision, intent(inout)   :: gamma
            double precision, intent(inout)   :: delta
            double precision, intent(inout)   :: evr(3)
            double precision, intent(inout)   :: evi(3)
            double precision, intent(inout)   :: evq(3)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mc01xd
    end interface
    public :: mc01xd
    
    interface 
        subroutine mc03md(rp1, cp1, cp2, dp1, dp2, dp3, alpha, p1, &
                       ldp11, ldp12, p2, ldp21, ldp22, p3, ldp31, ldp32, &
                       dwork, info)
            integer, intent(inout)            :: rp1
            integer, intent(inout)            :: cp1
            integer, intent(inout)            :: cp2
            integer, intent(inout)            :: dp1
            integer, intent(inout)            :: dp2
            integer, intent(inout)            :: dp3
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: p1(ldp11, ldp12, *)
            integer, intent(inout)            :: ldp11
            integer, intent(inout)            :: ldp12
            double precision, intent(inout)   :: p2(ldp21, ldp22, *)
            integer, intent(inout)            :: ldp21
            integer, intent(inout)            :: ldp22
            double precision, intent(inout)   :: p3(ldp31, ldp32, *)
            integer, intent(inout)            :: ldp31
            integer, intent(inout)            :: ldp32
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine mc03md
    end interface
    public :: mc03md
    
    interface 
        subroutine mc03nd(mp, np, dp, p, ldp1, ldp2, dk, gam, &
                       nullsp, ldnull, ker, ldker1, ldker2, tol, iwork, dwork, &
                       ldwork, info)
            integer, intent(inout)            :: mp
            integer, intent(inout)            :: np
            integer, intent(inout)            :: dp
            double precision, intent(inout)   :: p(ldp1, ldp2, *)
            integer, intent(inout)            :: ldp1
            integer, intent(inout)            :: ldp2
            integer, intent(inout)            :: dk
            integer, intent(inout)            :: gam(*)
            double precision, intent(inout)   :: nullsp(ldnull, *)
            integer, intent(inout)            :: ldnull
            double precision, intent(inout)   :: ker(ldker1, ldker2, *)
            integer, intent(inout)            :: ldker1
            integer, intent(inout)            :: ldker2
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine mc03nd
    end interface
    public :: mc03nd
    
    interface 
        subroutine mc03nx(mp, np, dp, p, ldp1, ldp2, a, lda, &
                       e, lde)
            integer, intent(inout)            :: mp
            integer, intent(inout)            :: np
            integer, intent(inout)            :: dp
            double precision, intent(inout)   :: p(ldp1, ldp2, *)
            integer, intent(inout)            :: ldp1
            integer, intent(inout)            :: ldp2
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
        end subroutine mc03nx
    end interface
    public :: mc03nx
    
    interface 
        subroutine mc03ny(nblcks, nra, nca, a, lda, e, lde, imuk, &
                       inuk, veps, ldveps, info)
            integer, intent(inout)            :: nblcks
            integer, intent(inout)            :: nra
            integer, intent(inout)            :: nca
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            integer, intent(inout)            :: imuk(*)
            integer, intent(inout)            :: inuk(*)
            double precision, intent(inout)   :: veps(ldveps, *)
            integer, intent(inout)            :: ldveps
            integer, intent(inout)            :: info
        end subroutine mc03ny
    end interface
    public :: mc03ny
    
    interface 
        subroutine md03ad(xinit, alg, stor, uplo, fcn, jpj, m, n, &
                       itmax, nprint, ipar, lipar, dpar1, ldpar1, dpar2, ldpar2, &
                       x, nfev, njev, tol, cgtol, dwork, ldwork, iwarn, &
                       info)
            character, intent(inout)          :: xinit
            character, intent(inout)          :: alg
            character, intent(inout)          :: stor
            character, intent(inout)          :: uplo
            external                :: fcn
            external                :: jpj
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: itmax
            integer, intent(inout)            :: nprint
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: dpar1(ldpar1, *)
            integer, intent(inout)            :: ldpar1
            double precision, intent(inout)   :: dpar2(ldpar2, *)
            integer, intent(inout)            :: ldpar2
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: nfev
            integer, intent(inout)            :: njev
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: cgtol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine md03ad
    end interface
    public :: md03ad
    
    interface 
        subroutine md03ba(n, ipar, lipar, fnorm, j, ldj, e, jnorms, &
                       gnorm, ipvt, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: fnorm
            double precision, intent(inout)   :: j(*)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: jnorms(*)
            double precision, intent(inout)   :: gnorm
            integer, intent(inout)            :: ipvt(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine md03ba
    end interface
    public :: md03ba
    
    interface 
        subroutine md03bb(cond, n, ipar, lipar, r, ldr, ipvt, diag, &
                       qtb, delta, par, ranks, x, rx, tol, dwork, &
                       ldwork, info)
            character, intent(inout)          :: cond
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            integer, intent(inout)            :: ipvt(*)
            double precision, intent(inout)   :: diag(*)
            double precision, intent(inout)   :: qtb(*)
            double precision, intent(inout)   :: delta
            double precision, intent(inout)   :: par
            integer, intent(inout)            :: ranks(*)
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: rx(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine md03bb
    end interface
    public :: md03bb
    
    interface 
        subroutine md03bd(xinit, scale, cond, fcn, qrfact, lmparm, m, n, &
                       itmax, factor, nprint, ipar, lipar, dpar1, ldpar1, dpar2, &
                       ldpar2, x, diag, nfev, njev, ftol, xtol, gtol, &
                       tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: xinit
            character, intent(inout)          :: scale
            character, intent(inout)          :: cond
            external                :: fcn
            external                :: qrfact
            external                :: lmparm
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: itmax
            double precision, intent(inout)   :: factor
            integer, intent(inout)            :: nprint
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: dpar1(*)
            integer, intent(inout)            :: ldpar1
            double precision, intent(inout)   :: dpar2(*)
            integer, intent(inout)            :: ldpar2
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: diag(*)
            integer, intent(inout)            :: nfev
            integer, intent(inout)            :: njev
            double precision, intent(inout)   :: ftol
            double precision, intent(inout)   :: xtol
            double precision, intent(inout)   :: gtol
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine md03bd
    end interface
    public :: md03bd
    
    interface 
        subroutine md03bf(iflag, m, n, ipar, lipar, dpar1, ldpar1, dpar2, &
                       ldpar2, x, nfevl, e, j, ldj, dwork, ldwork, &
                       info)
            integer, intent(inout)            :: iflag
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: dpar1(*)
            integer, intent(inout)            :: ldpar1
            double precision, intent(inout)   :: dpar2(*)
            integer, intent(inout)            :: ldpar2
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: nfevl
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine md03bf
    end interface
    public :: md03bf
    
    interface 
        subroutine md03bx(m, n, fnorm, j, ldj, e, jnorms, gnorm, &
                       ipvt, dwork, ldwork, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: fnorm
            double precision, intent(inout)   :: j(*)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: jnorms(*)
            double precision, intent(inout)   :: gnorm
            integer, intent(inout)            :: ipvt(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine md03bx
    end interface
    public :: md03bx
    
    interface 
        subroutine md03by(cond, n, r, ldr, ipvt, diag, qtb, delta, &
                       par, rank, x, rx, tol, dwork, ldwork, info)
            character, intent(inout)          :: cond
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            integer, intent(inout)            :: ipvt(*)
            double precision, intent(inout)   :: diag(*)
            double precision, intent(inout)   :: qtb(*)
            double precision, intent(inout)   :: delta
            double precision, intent(inout)   :: par
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: rx(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine md03by
    end interface
    public :: md03by
    
    interface 
        subroutine nf01ad(nsmp, m, l, ipar, lipar, x, lx, u, &
                       ldu, y, ldy, dwork, ldwork, info)
            integer, intent(inout)            :: nsmp
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: lx
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01ad
    end interface
    public :: nf01ad
    
    interface 
        subroutine nf01ay(nsmp, nz, l, ipar, lipar, wb, lwb, z, &
                       ldz, y, ldy, dwork, ldwork, info)
            integer, intent(inout)            :: nsmp
            integer, intent(inout)            :: nz
            integer, intent(inout)            :: l
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: wb(*)
            integer, intent(inout)            :: lwb
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01ay
    end interface
    public :: nf01ay
    
    interface 
        subroutine nf01ba(iflag, nsmp, n, ipar, lipar, z, ldz, y, &
                       ldy, x, nfevl, e, j, ldj, jte, dwork, &
                       ldwork, info)
            integer, intent(inout)            :: iflag
            integer, intent(inout)            :: nsmp
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: nfevl
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: jte(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01ba
    end interface
    public :: nf01ba
    
    interface 
        subroutine nf01bb(iflag, nfun, lx, ipar, lipar, u, ldu, y, &
                       ldy, x, nfevl, e, j, ldj, jte, dwork, &
                       ldwork, info)
            integer, intent(inout)            :: iflag
            integer, intent(inout)            :: nfun
            integer, intent(inout)            :: lx
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: nfevl
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: jte(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01bb
    end interface
    public :: nf01bb
    
    interface 
        subroutine nf01bd(cjte, nsmp, m, l, ipar, lipar, x, lx, &
                       u, ldu, e, j, ldj, jte, dwork, ldwork, &
                       info)
            character, intent(inout)          :: cjte
            integer, intent(inout)            :: nsmp
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: lx
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: jte(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01bd
    end interface
    public :: nf01bd
    
    interface 
        subroutine nf01be(iflag, nsmp, n, ipar, lipar, z, ldz, y, &
                       ldy, x, nfevl, e, j, ldj, dwork, ldwork, &
                       info)
            integer, intent(inout)            :: iflag
            integer, intent(inout)            :: nsmp
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: nfevl
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01be
    end interface
    public :: nf01be
    
    interface 
        subroutine nf01bf(iflag, nfun, lx, ipar, lipar, u, ldu, y, &
                       ldy, x, nfevl, e, j, ldj, dwork, ldwork, &
                       info)
            integer, intent(inout)            :: iflag
            integer, intent(inout)            :: nfun
            integer, intent(inout)            :: lx
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: nfevl
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01bf
    end interface
    public :: nf01bf
    
    interface 
        subroutine nf01bp(cond, n, ipar, lipar, r, ldr, ipvt, diag, &
                       qtb, delta, par, ranks, x, rx, tol, dwork, &
                       ldwork, info)
            character, intent(inout)          :: cond
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            integer, intent(inout)            :: ipvt(*)
            double precision, intent(inout)   :: diag(*)
            double precision, intent(inout)   :: qtb(*)
            double precision, intent(inout)   :: delta
            double precision, intent(inout)   :: par
            integer, intent(inout)            :: ranks(*)
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: rx(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01bp
    end interface
    public :: nf01bp
    
    interface 
        subroutine nf01bq(cond, n, ipar, lipar, r, ldr, ipvt, diag, &
                       qtb, ranks, x, tol, dwork, ldwork, info)
            character, intent(inout)          :: cond
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            integer, intent(inout)            :: ipvt(*)
            double precision, intent(inout)   :: diag(*)
            double precision, intent(inout)   :: qtb(*)
            integer, intent(inout)            :: ranks(*)
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01bq
    end interface
    public :: nf01bq
    
    interface 
        subroutine nf01br(cond, uplo, trans, n, ipar, lipar, r, ldr, &
                       sdiag, s, lds, b, ranks, tol, dwork, ldwork, &
                       info)
            character, intent(inout)          :: cond
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: sdiag(*)
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: b(*)
            integer, intent(inout)            :: ranks(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01br
    end interface
    public :: nf01br
    
    interface 
        subroutine nf01bs(n, ipar, lipar, fnorm, j, ldj, e, jnorms, &
                       gnorm, ipvt, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: fnorm
            double precision, intent(inout)   :: j(*)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: jnorms(*)
            double precision, intent(inout)   :: gnorm
            integer, intent(inout)            :: ipvt(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01bs
    end interface
    public :: nf01bs
    
    interface 
        subroutine nf01bu(stor, uplo, n, ipar, lipar, dpar, ldpar, j, &
                       ldj, jtj, ldjtj, dwork, ldwork, info)
            character, intent(inout)          :: stor
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ldpar
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: jtj(*)
            integer, intent(inout)            :: ldjtj
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01bu
    end interface
    public :: nf01bu
    
    interface 
        subroutine nf01bv(stor, uplo, n, ipar, lipar, dpar, ldpar, j, &
                       ldj, jtj, ldjtj, dwork, ldwork, info)
            character, intent(inout)          :: stor
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ldpar
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: jtj(*)
            integer, intent(inout)            :: ldjtj
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01bv
    end interface
    public :: nf01bv
    
    interface 
        subroutine nf01bw(n, ipar, lipar, dpar, ldpar, j, ldj, x, &
                       incx, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ldpar
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: incx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01bw
    end interface
    public :: nf01bw
    
    interface 
        subroutine nf01bx(n, ipar, lipar, dpar, ldpar, j, ldj, x, &
                       incx, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ldpar
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: incx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01bx
    end interface
    public :: nf01bx
    
    interface 
        subroutine nf01by(cjte, nsmp, nz, l, ipar, lipar, wb, lwb, &
                       z, ldz, e, j, ldj, jte, dwork, ldwork, &
                       info)
            character, intent(inout)          :: cjte
            integer, intent(inout)            :: nsmp
            integer, intent(inout)            :: nz
            integer, intent(inout)            :: l
            integer, intent(inout)            :: ipar(*)
            integer, intent(inout)            :: lipar
            double precision, intent(inout)   :: wb(*)
            integer, intent(inout)            :: lwb
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: jte(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine nf01by
    end interface
    public :: nf01by
    
    interface 
        subroutine sb01bd(dico, n, m, np, alpha, a, lda, b, &
                       ldb, wr, wi, nfp, nap, nup, f, ldf, &
                       z, ldz, tol, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(inout)            :: nfp
            integer, intent(inout)            :: nap
            integer, intent(inout)            :: nup
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine sb01bd
    end interface
    public :: sb01bd
    
    interface 
        subroutine sb01bx(reig, n, xr, xi, wr, wi, s, p)
            logical, intent(inout)            :: reig
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: xr
            double precision, intent(inout)   :: xi
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: s
            double precision, intent(inout)   :: p
        end subroutine sb01bx
    end interface
    public :: sb01bx
    
    interface 
        subroutine sb01by(n, m, s, p, a, b, f, tol, &
                       dwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: s
            double precision, intent(inout)   :: p
            double precision, intent(inout)   :: a(n, *)
            double precision, intent(inout)   :: b(n, *)
            double precision, intent(inout)   :: f(m, *)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine sb01by
    end interface
    public :: sb01by
    
    interface 
        subroutine sb01dd(n, m, indcon, a, lda, b, ldb, nblk, &
                       wr, wi, z, ldz, y, count, g, ldg, &
                       tol, iwork, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: indcon
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            integer, intent(inout)            :: nblk(*)
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: y(*)
            integer, intent(inout)            :: count
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb01dd
    end interface
    public :: sb01dd
    
    interface 
        subroutine sb01fy(discr, n, m, a, lda, b, ldb, f, &
                       ldf, v, ldv, info)
            logical, intent(inout)            :: discr
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            integer, intent(inout)            :: info
        end subroutine sb01fy
    end interface
    public :: sb01fy
    
    interface 
        subroutine sb01md(ncont, n, a, lda, b, wr, wi, z, &
                       ldz, g, dwork, info)
            integer, intent(inout)            :: ncont
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: g(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine sb01md
    end interface
    public :: sb01md
    
    interface 
        logical function sb02cx (reig,ieig)
            double precision, intent(inout)   :: reig
            double precision, intent(inout)   :: ieig
        end function sb02cx
    end interface
    public :: sb02cx
    
    interface 
        subroutine sb02md(dico, hinv, uplo, scal, sort, n, a, lda, &
                       g, ldg, q, ldq, rcond, wr, wi, s, &
                       lds, u, ldu, iwork, dwork, ldwork, bwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: hinv
            character, intent(inout)          :: uplo
            character, intent(inout)          :: scal
            character, intent(inout)          :: sort
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine sb02md
    end interface
    public :: sb02md
    
    interface 
        logical function sb02mr (reig,ieig)
            double precision, intent(inout)   :: reig
            double precision, intent(inout)   :: ieig
        end function sb02mr
    end interface
    public :: sb02mr
    
    interface 
        logical function sb02ms (reig,ieig)
            double precision, intent(inout)   :: reig
            double precision, intent(inout)   :: ieig
        end function sb02ms
    end interface
    public :: sb02ms
    
    interface 
        subroutine sb02mt(jobg, jobl, fact, uplo, n, m, a, lda, &
                       b, ldb, q, ldq, r, ldr, l, ldl, &
                       ipiv, oufact, g, ldg, iwork, dwork, ldwork, info)
            character, intent(inout)          :: jobg
            character, intent(inout)          :: jobl
            character, intent(inout)          :: fact
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: l(ldl, *)
            integer, intent(inout)            :: ldl
            integer, intent(inout)            :: ipiv(*)
            integer, intent(inout)            :: oufact
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb02mt
    end interface
    public :: sb02mt
    
    interface 
        subroutine sb02mu(dico, hinv, uplo, n, a, lda, g, ldg, &
                       q, ldq, s, lds, iwork, dwork, ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: hinv
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb02mu
    end interface
    public :: sb02mu
    
    interface 
        logical function sb02mv (reig,ieig)
            double precision, intent(inout)   :: reig
            double precision, intent(inout)   :: ieig
        end function sb02mv
    end interface
    public :: sb02mv
    
    interface 
        logical function sb02mw (reig,ieig)
            double precision, intent(inout)   :: reig
            double precision, intent(inout)   :: ieig
        end function sb02mw
    end interface
    public :: sb02mw
    
    interface 
        subroutine sb02mx(jobg, jobl, fact, uplo, trans, flag, def, n, &
                       m, a, lda, b, ldb, q, ldq, r, &
                       ldr, l, ldl, ipiv, oufact, g, ldg, iwork, &
                       dwork, ldwork, info)
            character, intent(inout)          :: jobg
            character, intent(inout)          :: jobl
            character, intent(inout)          :: fact
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            character, intent(inout)          :: flag
            character, intent(inout)          :: def
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: l(ldl, *)
            integer, intent(inout)            :: ldl
            integer, intent(inout)            :: ipiv(*)
            integer, intent(inout)            :: oufact
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb02mx
    end interface
    public :: sb02mx
    
    interface 
        subroutine sb02nd(dico, fact, uplo, jobl, n, m, p, a, &
                       lda, b, ldb, r, ldr, ipiv, l, ldl, &
                       x, ldx, rnorm, f, ldf, oufact, iwork, dwork, &
                       ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: fact
            character, intent(inout)          :: uplo
            character, intent(inout)          :: jobl
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            integer, intent(inout)            :: ipiv(*)
            double precision, intent(inout)   :: l(ldl, *)
            integer, intent(inout)            :: ldl
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: rnorm
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            integer, intent(inout)            :: oufact(2)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb02nd
    end interface
    public :: sb02nd
    
    interface 
        subroutine sb02od(dico, jobb, fact, uplo, jobl, sort, n, m, &
                       p, a, lda, b, ldb, q, ldq, r, &
                       ldr, l, ldl, rcond, x, ldx, alfar, alfai, &
                       beta, s, lds, t, ldt, u, ldu, tol, &
                       iwork, dwork, ldwork, bwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobb
            character, intent(inout)          :: fact
            character, intent(inout)          :: uplo
            character, intent(inout)          :: jobl
            character, intent(inout)          :: sort
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: l(ldl, *)
            integer, intent(inout)            :: ldl
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: alfar(*)
            double precision, intent(inout)   :: alfai(*)
            double precision, intent(inout)   :: beta(*)
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine sb02od
    end interface
    public :: sb02od
    
    interface 
        logical function sb02ou (alphar,alphai,beta)
            double precision, intent(inout)   :: alphar
            double precision, intent(inout)   :: alphai
            double precision, intent(inout)   :: beta
        end function sb02ou
    end interface
    public :: sb02ou
    
    interface 
        logical function sb02ov (alphar,alphai,beta)
            double precision, intent(inout)   :: alphar
            double precision, intent(inout)   :: alphai
            double precision, intent(inout)   :: beta
        end function sb02ov
    end interface
    public :: sb02ov
    
    interface 
        logical function sb02ow (alphar,alphai,beta)
            double precision, intent(inout)   :: alphar
            double precision, intent(inout)   :: alphai
            double precision, intent(inout)   :: beta
        end function sb02ow
    end interface
    public :: sb02ow
    
    interface 
        logical function sb02ox (alphar,alphai,beta)
            double precision, intent(inout)   :: alphar
            double precision, intent(inout)   :: alphai
            double precision, intent(inout)   :: beta
        end function sb02ox
    end interface
    public :: sb02ox
    
    interface 
        subroutine sb02oy(type, dico, jobb, fact, uplo, jobl, jobe, n, &
                       m, p, a, lda, b, ldb, q, ldq, &
                       r, ldr, l, ldl, e, lde, af, ldaf, &
                       bf, ldbf, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: type
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobb
            character, intent(inout)          :: fact
            character, intent(inout)          :: uplo
            character, intent(inout)          :: jobl
            character, intent(inout)          :: jobe
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: l(ldl, *)
            integer, intent(inout)            :: ldl
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: af(ldaf, *)
            integer, intent(inout)            :: ldaf
            double precision, intent(inout)   :: bf(ldbf, *)
            integer, intent(inout)            :: ldbf
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb02oy
    end interface
    public :: sb02oy
    
    interface 
        subroutine sb02pd(job, trana, uplo, n, a, lda, g, ldg, &
                       q, ldq, x, ldx, rcond, ferr, wr, wi, &
                       iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: trana
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: ferr
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb02pd
    end interface
    public :: sb02pd
    
    interface 
        subroutine sb02qd(job, fact, trana, uplo, lyapun, n, a, lda, &
                       t, ldt, u, ldu, g, ldg, q, ldq, &
                       x, ldx, sep, rcond, ferr, iwork, dwork, ldwork, &
                       info)
            character, intent(inout)          :: job
            character, intent(inout)          :: fact
            character, intent(inout)          :: trana
            character, intent(inout)          :: uplo
            character, intent(inout)          :: lyapun
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: sep
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: ferr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb02qd
    end interface
    public :: sb02qd
    
    interface 
        subroutine sb02rd(job, dico, hinv, trana, uplo, scal, sort, fact, &
                       lyapun, n, a, lda, t, ldt, v, ldv, &
                       g, ldg, q, ldq, x, ldx, sep, rcond, &
                       ferr, wr, wi, s, lds, iwork, dwork, ldwork, &
                       bwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: dico
            character, intent(inout)          :: hinv
            character, intent(inout)          :: trana
            character, intent(inout)          :: uplo
            character, intent(inout)          :: scal
            character, intent(inout)          :: sort
            character, intent(inout)          :: fact
            character, intent(inout)          :: lyapun
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: sep
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: ferr
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine sb02rd
    end interface
    public :: sb02rd
    
    interface 
        subroutine sb02ru(dico, hinv, trana, uplo, n, a, lda, g, &
                       ldg, q, ldq, s, lds, iwork, dwork, ldwork, &
                       info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: hinv
            character, intent(inout)          :: trana
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb02ru
    end interface
    public :: sb02ru
    
    interface 
        subroutine sb02sd(job, fact, trana, uplo, lyapun, n, a, lda, &
                       t, ldt, u, ldu, g, ldg, q, ldq, &
                       x, ldx, sepd, rcond, ferr, iwork, dwork, ldwork, &
                       info)
            character, intent(inout)          :: job
            character, intent(inout)          :: fact
            character, intent(inout)          :: trana
            character, intent(inout)          :: uplo
            character, intent(inout)          :: lyapun
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: sepd
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: ferr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb02sd
    end interface
    public :: sb02sd
    
    interface 
        subroutine sb03md(dico, job, fact, trana, n, a, lda, u, &
                       ldu, c, ldc, scale, sep, ferr, wr, wi, &
                       iwork, dwork, ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: job
            character, intent(inout)          :: fact
            character, intent(inout)          :: trana
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: sep
            double precision, intent(inout)   :: ferr
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb03md
    end interface
    public :: sb03md
    
    interface 
        subroutine sb03mu(ltranl, ltranr, isgn, n1, n2, tl, ldtl, tr, &
                       ldtr, b, ldb, scale, x, ldx, xnorm, info)
            logical, intent(inout)            :: ltranl
            logical, intent(inout)            :: ltranr
            integer, intent(inout)            :: isgn
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: n2
            double precision, intent(inout)   :: tl(ldtl, *)
            integer, intent(inout)            :: ldtl
            double precision, intent(inout)   :: tr(ldtr, *)
            integer, intent(inout)            :: ldtr
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: xnorm
            integer, intent(inout)            :: info
        end subroutine sb03mu
    end interface
    public :: sb03mu
    
    interface 
        subroutine sb03mv(ltran, lupper, t, ldt, b, ldb, scale, x, &
                       ldx, xnorm, info)
            logical, intent(inout)            :: ltran
            logical, intent(inout)            :: lupper
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: xnorm
            integer, intent(inout)            :: info
        end subroutine sb03mv
    end interface
    public :: sb03mv
    
    interface 
        subroutine sb03mw(ltran, lupper, t, ldt, b, ldb, scale, x, &
                       ldx, xnorm, info)
            logical, intent(inout)            :: ltran
            logical, intent(inout)            :: lupper
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: xnorm
            integer, intent(inout)            :: info
        end subroutine sb03mw
    end interface
    public :: sb03mw
    
    interface 
        subroutine sb03mx(trana, n, a, lda, c, ldc, scale, dwork, &
                       info)
            character, intent(inout)          :: trana
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine sb03mx
    end interface
    public :: sb03mx
    
    interface 
        subroutine sb03my(trana, n, a, lda, c, ldc, scale, info)
            character, intent(inout)          :: trana
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: scale
            integer, intent(inout)            :: info
        end subroutine sb03my
    end interface
    public :: sb03my
    
    interface 
        subroutine sb03od(dico, fact, trans, n, m, a, lda, q, &
                       ldq, b, ldb, scale, wr, wi, dwork, ldwork, &
                       info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: fact
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb03od
    end interface
    public :: sb03od
    
    interface 
        subroutine sb03or(discr, ltrans, n, m, s, lds, a, lda, &
                       c, ldc, scale, info)
            logical, intent(inout)            :: discr
            logical, intent(inout)            :: ltrans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: scale
            integer, intent(inout)            :: info
        end subroutine sb03or
    end interface
    public :: sb03or
    
    interface 
        subroutine sb03os(discr, ltrans, n, s, lds, r, ldr, scale, &
                       dwork, zwork, info)
            logical, intent(inout)            :: discr
            logical, intent(inout)            :: ltrans
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: s(lds, *)
            integer, intent(inout)            :: lds
            complex*16, intent(inout)         :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: info
        end subroutine sb03os
    end interface
    public :: sb03os
    
    interface 
        subroutine sb03ot(discr, ltrans, n, s, lds, r, ldr, scale, &
                       dwork, info)
            logical, intent(inout)            :: discr
            logical, intent(inout)            :: ltrans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine sb03ot
    end interface
    public :: sb03ot
    
    interface 
        subroutine sb03ou(discr, ltrans, n, m, a, lda, b, ldb, &
                       tau, u, ldu, scale, dwork, ldwork, info)
            logical, intent(inout)            :: discr
            logical, intent(inout)            :: ltrans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb03ou
    end interface
    public :: sb03ou
    
    interface 
        subroutine sb03ov(a, b, small, c, s)
            double precision, intent(inout)   :: a(2)
            double precision, intent(inout)   :: b
            double precision, intent(inout)   :: small
            double precision, intent(inout)   :: c(2)
            double precision, intent(inout)   :: s
        end subroutine sb03ov
    end interface
    public :: sb03ov
    
    interface 
        subroutine sb03oy(discr, ltrans, isgn, s, lds, r, ldr, a, &
                       lda, scale, info)
            logical, intent(inout)            :: discr
            logical, intent(inout)            :: ltrans
            integer, intent(inout)            :: isgn
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: scale
            integer, intent(inout)            :: info
        end subroutine sb03oy
    end interface
    public :: sb03oy
    
    interface 
        subroutine sb03oz(dico, fact, trans, n, m, a, lda, q, &
                       ldq, b, ldb, scale, w, dwork, zwork, lzwork, &
                       info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: fact
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(inout)            :: ldq
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: scale
            complex*16, intent(inout)         :: w(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine sb03oz
    end interface
    public :: sb03oz
    
    interface 
        subroutine sb03pd(job, fact, trana, n, a, lda, u, ldu, &
                       c, ldc, scale, sepd, ferr, wr, wi, iwork, &
                       dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: fact
            character, intent(inout)          :: trana
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: sepd
            double precision, intent(inout)   :: ferr
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb03pd
    end interface
    public :: sb03pd
    
    interface 
        subroutine sb03qd(job, fact, trana, uplo, lyapun, n, scale, a, &
                       lda, t, ldt, u, ldu, c, ldc, x, &
                       ldx, sep, rcond, ferr, iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: fact
            character, intent(inout)          :: trana
            character, intent(inout)          :: uplo
            character, intent(inout)          :: lyapun
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: sep
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: ferr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb03qd
    end interface
    public :: sb03qd
    
    interface 
        subroutine sb03qx(trana, uplo, lyapun, n, xanorm, t, ldt, u, &
                       ldu, r, ldr, ferr, iwork, dwork, ldwork, info)
            character, intent(inout)          :: trana
            character, intent(inout)          :: uplo
            character, intent(inout)          :: lyapun
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: xanorm
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: ferr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb03qx
    end interface
    public :: sb03qx
    
    interface 
        subroutine sb03qy(job, trana, lyapun, n, t, ldt, u, ldu, &
                       x, ldx, sep, thnorm, iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: trana
            character, intent(inout)          :: lyapun
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: sep
            double precision, intent(inout)   :: thnorm
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb03qy
    end interface
    public :: sb03qy
    
    interface 
        subroutine sb03rd(job, fact, trana, n, a, lda, u, ldu, &
                       c, ldc, scale, sep, ferr, wr, wi, iwork, &
                       dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: fact
            character, intent(inout)          :: trana
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: sep
            double precision, intent(inout)   :: ferr
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb03rd
    end interface
    public :: sb03rd
    
    interface 
        subroutine sb03sd(job, fact, trana, uplo, lyapun, n, scale, a, &
                       lda, t, ldt, u, ldu, c, ldc, x, &
                       ldx, sepd, rcond, ferr, iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: fact
            character, intent(inout)          :: trana
            character, intent(inout)          :: uplo
            character, intent(inout)          :: lyapun
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: sepd
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: ferr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb03sd
    end interface
    public :: sb03sd
    
    interface 
        subroutine sb03sx(trana, uplo, lyapun, n, xanorm, t, ldt, u, &
                       ldu, r, ldr, ferr, iwork, dwork, ldwork, info)
            character, intent(inout)          :: trana
            character, intent(inout)          :: uplo
            character, intent(inout)          :: lyapun
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: xanorm
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: ferr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb03sx
    end interface
    public :: sb03sx
    
    interface 
        subroutine sb03sy(job, trana, lyapun, n, t, ldt, u, ldu, &
                       xa, ldxa, sepd, thnorm, iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: trana
            character, intent(inout)          :: lyapun
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: xa(ldxa, *)
            integer, intent(inout)            :: ldxa
            double precision, intent(inout)   :: sepd
            double precision, intent(inout)   :: thnorm
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb03sy
    end interface
    public :: sb03sy
    
    interface 
        subroutine sb03td(job, fact, trana, uplo, lyapun, n, scale, a, &
                       lda, t, ldt, u, ldu, c, ldc, x, &
                       ldx, sep, rcond, ferr, wr, wi, iwork, dwork, &
                       ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: fact
            character, intent(inout)          :: trana
            character, intent(inout)          :: uplo
            character, intent(inout)          :: lyapun
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: sep
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: ferr
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb03td
    end interface
    public :: sb03td
    
    interface 
        subroutine sb03ud(job, fact, trana, uplo, lyapun, n, scale, a, &
                       lda, t, ldt, u, ldu, c, ldc, x, &
                       ldx, sepd, rcond, ferr, wr, wi, iwork, dwork, &
                       ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: fact
            character, intent(inout)          :: trana
            character, intent(inout)          :: uplo
            character, intent(inout)          :: lyapun
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: sepd
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: ferr
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb03ud
    end interface
    public :: sb03ud
    
    interface 
        subroutine sb04md(n, m, a, lda, b, ldb, c, ldc, &
                       z, ldz, iwork, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb04md
    end interface
    public :: sb04md
    
    interface 
        subroutine sb04mr(m, d, ipr, info)
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: ipr(*)
            integer, intent(inout)            :: info
        end subroutine sb04mr
    end interface
    public :: sb04mr
    
    interface 
        subroutine sb04mu(n, m, ind, a, lda, b, ldb, c, &
                       ldc, d, ipr, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: ind
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: ipr(*)
            integer, intent(inout)            :: info
        end subroutine sb04mu
    end interface
    public :: sb04mu
    
    interface 
        subroutine sb04mw(m, d, ipr, info)
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: ipr(*)
            integer, intent(inout)            :: info
        end subroutine sb04mw
    end interface
    public :: sb04mw
    
    interface 
        subroutine sb04my(n, m, ind, a, lda, b, ldb, c, &
                       ldc, d, ipr, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: ind
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: ipr(*)
            integer, intent(inout)            :: info
        end subroutine sb04my
    end interface
    public :: sb04my
    
    interface 
        subroutine sb04nd(abschu, ula, ulb, n, m, a, lda, b, &
                       ldb, c, ldc, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: abschu
            character, intent(inout)          :: ula
            character, intent(inout)          :: ulb
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb04nd
    end interface
    public :: sb04nd
    
    interface 
        subroutine sb04nv(abschr, ul, n, m, c, ldc, indx, ab, &
                       ldab, d)
            character, intent(inout)          :: abschr
            character, intent(inout)          :: ul
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: indx
            double precision, intent(inout)   :: ab(ldab, *)
            integer, intent(inout)            :: ldab
            double precision, intent(inout)   :: d(*)
        end subroutine sb04nv
    end interface
    public :: sb04nv
    
    interface 
        subroutine sb04nw(abschr, ul, n, m, c, ldc, indx, ab, &
                       ldab, d)
            character, intent(inout)          :: abschr
            character, intent(inout)          :: ul
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: indx
            double precision, intent(inout)   :: ab(ldab, *)
            integer, intent(inout)            :: ldab
            double precision, intent(inout)   :: d(*)
        end subroutine sb04nw
    end interface
    public :: sb04nw
    
    interface 
        subroutine sb04nx(rc, ul, m, a, lda, lambd1, lambd2, lambd3, &
                       lambd4, d, tol, iwork, dwork, lddwor, info)
            character, intent(inout)          :: rc
            character, intent(inout)          :: ul
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: lambd1
            double precision, intent(inout)   :: lambd2
            double precision, intent(inout)   :: lambd3
            double precision, intent(inout)   :: lambd4
            double precision, intent(inout)   :: d(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(lddwor, *)
            integer, intent(inout)            :: lddwor
            integer, intent(inout)            :: info
        end subroutine sb04nx
    end interface
    public :: sb04nx
    
    interface 
        subroutine sb04ny(rc, ul, m, a, lda, lambda, d, tol, &
                       iwork, dwork, lddwor, info)
            character, intent(inout)          :: rc
            character, intent(inout)          :: ul
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: lambda
            double precision, intent(inout)   :: d(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(lddwor, *)
            integer, intent(inout)            :: lddwor
            integer, intent(inout)            :: info
        end subroutine sb04ny
    end interface
    public :: sb04ny
    
    interface 
        subroutine sb04od(reduce, trans, jobd, m, n, a, lda, b, &
                       ldb, c, ldc, d, ldd, e, lde, f, &
                       ldf, scale, dif, p, ldp, q, ldq, u, &
                       ldu, v, ldv, iwork, dwork, ldwork, info)
            character, intent(inout)          :: reduce
            character, intent(inout)          :: trans
            character, intent(inout)          :: jobd
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: dif
            double precision, intent(inout)   :: p(ldp, *)
            integer, intent(inout)            :: ldp
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb04od
    end interface
    public :: sb04od
    
    interface 
        subroutine sb04ow(m, n, a, lda, b, ldb, c, ldc, &
                       d, ldd, e, lde, f, ldf, scale, iwork, &
                       info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: scale
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: info
        end subroutine sb04ow
    end interface
    public :: sb04ow
    
    interface 
        subroutine sb04pd(dico, facta, factb, trana, tranb, isgn, m, n, &
                       a, lda, u, ldu, b, ldb, v, ldv, &
                       c, ldc, scale, dwork, ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: facta
            character, intent(inout)          :: factb
            character, intent(inout)          :: trana
            character, intent(inout)          :: tranb
            integer, intent(inout)            :: isgn
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb04pd
    end interface
    public :: sb04pd
    
    interface 
        subroutine sb04px(ltranl, ltranr, isgn, n1, n2, tl, ldtl, tr, &
                       ldtr, b, ldb, scale, x, ldx, xnorm, info)
            logical, intent(inout)            :: ltranl
            logical, intent(inout)            :: ltranr
            integer, intent(inout)            :: isgn
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: n2
            double precision, intent(inout)   :: tl(ldtl, *)
            integer, intent(inout)            :: ldtl
            double precision, intent(inout)   :: tr(ldtr, *)
            integer, intent(inout)            :: ldtr
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: xnorm
            integer, intent(inout)            :: info
        end subroutine sb04px
    end interface
    public :: sb04px
    
    interface 
        subroutine sb04py(trana, tranb, isgn, m, n, a, lda, b, &
                       ldb, c, ldc, scale, dwork, info)
            character, intent(inout)          :: trana
            character, intent(inout)          :: tranb
            integer, intent(inout)            :: isgn
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine sb04py
    end interface
    public :: sb04py
    
    interface 
        subroutine sb04qd(n, m, a, lda, b, ldb, c, ldc, &
                       z, ldz, iwork, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb04qd
    end interface
    public :: sb04qd
    
    interface 
        subroutine sb04qr(m, d, ipr, info)
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: ipr(*)
            integer, intent(inout)            :: info
        end subroutine sb04qr
    end interface
    public :: sb04qr
    
    interface 
        subroutine sb04qu(n, m, ind, a, lda, b, ldb, c, &
                       ldc, d, ipr, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: ind
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: ipr(*)
            integer, intent(inout)            :: info
        end subroutine sb04qu
    end interface
    public :: sb04qu
    
    interface 
        subroutine sb04qy(n, m, ind, a, lda, b, ldb, c, &
                       ldc, d, ipr, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: ind
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: ipr(*)
            integer, intent(inout)            :: info
        end subroutine sb04qy
    end interface
    public :: sb04qy
    
    interface 
        subroutine sb04rd(abschu, ula, ulb, n, m, a, lda, b, &
                       ldb, c, ldc, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: abschu
            character, intent(inout)          :: ula
            character, intent(inout)          :: ulb
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb04rd
    end interface
    public :: sb04rd
    
    interface 
        subroutine sb04rv(abschr, ul, n, m, c, ldc, indx, ab, &
                       ldab, ba, ldba, d, dwork)
            character, intent(inout)          :: abschr
            character, intent(inout)          :: ul
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: indx
            double precision, intent(inout)   :: ab(ldab, *)
            integer, intent(inout)            :: ldab
            double precision, intent(inout)   :: ba(ldba, *)
            integer, intent(inout)            :: ldba
            double precision, intent(inout)   :: d(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine sb04rv
    end interface
    public :: sb04rv
    
    interface 
        subroutine sb04rw(abschr, ul, n, m, c, ldc, indx, ab, &
                       ldab, ba, ldba, d, dwork)
            character, intent(inout)          :: abschr
            character, intent(inout)          :: ul
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: indx
            double precision, intent(inout)   :: ab(ldab, *)
            integer, intent(inout)            :: ldab
            double precision, intent(inout)   :: ba(ldba, *)
            integer, intent(inout)            :: ldba
            double precision, intent(inout)   :: d(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine sb04rw
    end interface
    public :: sb04rw
    
    interface 
        subroutine sb04rx(rc, ul, m, a, lda, lambd1, lambd2, lambd3, &
                       lambd4, d, tol, iwork, dwork, lddwor, info)
            character, intent(inout)          :: rc
            character, intent(inout)          :: ul
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: lambd1
            double precision, intent(inout)   :: lambd2
            double precision, intent(inout)   :: lambd3
            double precision, intent(inout)   :: lambd4
            double precision, intent(inout)   :: d(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(lddwor, *)
            integer, intent(inout)            :: lddwor
            integer, intent(inout)            :: info
        end subroutine sb04rx
    end interface
    public :: sb04rx
    
    interface 
        subroutine sb04ry(rc, ul, m, a, lda, lambda, d, tol, &
                       iwork, dwork, lddwor, info)
            character, intent(inout)          :: rc
            character, intent(inout)          :: ul
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: lambda
            double precision, intent(inout)   :: d(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(lddwor, *)
            integer, intent(inout)            :: lddwor
            integer, intent(inout)            :: info
        end subroutine sb04ry
    end interface
    public :: sb04ry
    
    interface 
        subroutine sb06nd(n, m, kmax, a, lda, b, ldb, kstair, &
                       u, ldu, f, ldf, dwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: kmax
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            integer, intent(inout)            :: kstair(*)
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine sb06nd
    end interface
    public :: sb06nd
    
    interface 
        subroutine sb08cd(dico, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, nq, nr, br, ldbr, &
                       dr, lddr, tol, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nq
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: br(ldbr, *)
            integer, intent(inout)            :: ldbr
            double precision, intent(inout)   :: dr(lddr, *)
            integer, intent(inout)            :: lddr
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine sb08cd
    end interface
    public :: sb08cd
    
    interface 
        subroutine sb08dd(dico, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, nq, nr, cr, ldcr, &
                       dr, lddr, tol, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nq
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: cr(ldcr, *)
            integer, intent(inout)            :: ldcr
            double precision, intent(inout)   :: dr(lddr, *)
            integer, intent(inout)            :: lddr
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine sb08dd
    end interface
    public :: sb08dd
    
    interface 
        subroutine sb08ed(dico, n, m, p, alpha, a, lda, b, &
                       ldb, c, ldc, d, ldd, nq, nr, br, &
                       ldbr, dr, lddr, tol, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: alpha(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nq
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: br(ldbr, *)
            integer, intent(inout)            :: ldbr
            double precision, intent(inout)   :: dr(lddr, *)
            integer, intent(inout)            :: lddr
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine sb08ed
    end interface
    public :: sb08ed
    
    interface 
        subroutine sb08fd(dico, n, m, p, alpha, a, lda, b, &
                       ldb, c, ldc, d, ldd, nq, nr, cr, &
                       ldcr, dr, lddr, tol, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: alpha(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nq
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: cr(ldcr, *)
            integer, intent(inout)            :: ldcr
            double precision, intent(inout)   :: dr(lddr, *)
            integer, intent(inout)            :: lddr
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine sb08fd
    end interface
    public :: sb08fd
    
    interface 
        subroutine sb08gd(n, m, p, a, lda, b, ldb, c, &
                       ldc, d, ldd, br, ldbr, dr, lddr, iwork, &
                       dwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: br(ldbr, *)
            integer, intent(inout)            :: ldbr
            double precision, intent(inout)   :: dr(lddr, *)
            integer, intent(inout)            :: lddr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine sb08gd
    end interface
    public :: sb08gd
    
    interface 
        subroutine sb08hd(n, m, p, a, lda, b, ldb, c, &
                       ldc, d, ldd, cr, ldcr, dr, lddr, iwork, &
                       dwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: cr(ldcr, *)
            integer, intent(inout)            :: ldcr
            double precision, intent(inout)   :: dr(lddr, *)
            integer, intent(inout)            :: lddr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine sb08hd
    end interface
    public :: sb08hd
    
    interface 
        subroutine sb08md(acona, da, a, res, e, dwork, ldwork, info)
            character, intent(inout)          :: acona
            integer, intent(inout)            :: da
            double precision, intent(inout)   :: a(*)
            double precision, intent(inout)   :: res
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb08md
    end interface
    public :: sb08md
    
    interface 
        subroutine sb08my(da, a, b, epsb)
            integer, intent(inout)            :: da
            double precision, intent(inout)   :: a(*)
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: epsb
        end subroutine sb08my
    end interface
    public :: sb08my
    
    interface 
        subroutine sb08nd(acona, da, a, res, e, dwork, ldwork, info)
            character, intent(inout)          :: acona
            integer, intent(inout)            :: da
            double precision, intent(inout)   :: a(*)
            double precision, intent(inout)   :: res
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb08nd
    end interface
    public :: sb08nd
    
    interface 
        subroutine sb08ny(da, a, b, epsb)
            integer, intent(inout)            :: da
            double precision, intent(inout)   :: a(*)
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: epsb
        end subroutine sb08ny
    end interface
    public :: sb08ny
    
    interface 
        subroutine sb09md(n, nc, nb, h1, ldh1, h2, ldh2, ss, &
                       ldss, se, ldse, pre, ldpre, tol, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nc
            integer, intent(inout)            :: nb
            double precision, intent(inout)   :: h1(ldh1, *)
            integer, intent(inout)            :: ldh1
            double precision, intent(inout)   :: h2(ldh2, *)
            integer, intent(inout)            :: ldh2
            double precision, intent(inout)   :: ss(ldss, *)
            integer, intent(inout)            :: ldss
            double precision, intent(inout)   :: se(ldse, *)
            integer, intent(inout)            :: ldse
            double precision, intent(inout)   :: pre(ldpre, *)
            integer, intent(inout)            :: ldpre
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: info
        end subroutine sb09md
    end interface
    public :: sb09md
    
    interface 
        subroutine sb10ad(job, n, m, np, ncon, nmeas, gamma, a, &
                       lda, b, ldb, c, ldc, d, ldd, ak, &
                       ldak, bk, ldbk, ck, ldck, dk, lddk, ac, &
                       ldac, bc, ldbc, cc, ldcc, dc, lddc, rcond, &
                       gtol, actol, iwork, liwork, dwork, ldwork, bwork, lbwork, &
                       info)
            integer, intent(inout)            :: job
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: gamma
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(inout)            :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(inout)            :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(inout)            :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(inout)            :: lddk
            double precision, intent(inout)   :: ac(ldac, *)
            integer, intent(inout)            :: ldac
            double precision, intent(inout)   :: bc(ldbc, *)
            integer, intent(inout)            :: ldbc
            double precision, intent(inout)   :: cc(ldcc, *)
            integer, intent(inout)            :: ldcc
            double precision, intent(inout)   :: dc(lddc, *)
            integer, intent(inout)            :: lddc
            double precision, intent(inout)   :: rcond(4)
            double precision, intent(inout)   :: gtol
            double precision, intent(inout)   :: actol
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: lbwork
            integer, intent(inout)            :: info
        end subroutine sb10ad
    end interface
    public :: sb10ad
    
    interface 
        subroutine sb10dd(n, m, np, ncon, nmeas, gamma, a, lda, &
                       b, ldb, c, ldc, d, ldd, ak, ldak, &
                       bk, ldbk, ck, ldck, dk, lddk, x, ldx, &
                       z, ldz, rcond, tol, iwork, dwork, ldwork, bwork, &
                       info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: gamma
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(inout)            :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(inout)            :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(inout)            :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(inout)            :: lddk
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: rcond(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine sb10dd
    end interface
    public :: sb10dd
    
    interface 
        subroutine sb10ed(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, d, ldd, ak, ldak, bk, &
                       ldbk, ck, ldck, dk, lddk, rcond, tol, iwork, &
                       dwork, ldwork, bwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(inout)            :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(inout)            :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(inout)            :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(inout)            :: lddk
            double precision, intent(inout)   :: rcond(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine sb10ed
    end interface
    public :: sb10ed
    
    interface 
        subroutine sb10fd(n, m, np, ncon, nmeas, gamma, a, lda, &
                       b, ldb, c, ldc, d, ldd, ak, ldak, &
                       bk, ldbk, ck, ldck, dk, lddk, rcond, tol, &
                       iwork, dwork, ldwork, bwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: gamma
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(inout)            :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(inout)            :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(inout)            :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(inout)            :: lddk
            double precision, intent(inout)   :: rcond(4)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine sb10fd
    end interface
    public :: sb10fd
    
    interface 
        subroutine sb10hd(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, d, ldd, ak, ldak, bk, &
                       ldbk, ck, ldck, dk, lddk, rcond, tol, iwork, &
                       dwork, ldwork, bwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(inout)            :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(inout)            :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(inout)            :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(inout)            :: lddk
            double precision, intent(inout)   :: rcond(4)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine sb10hd
    end interface
    public :: sb10hd
    
    interface 
        subroutine sb10id(n, m, np, a, lda, b, ldb, c, &
                       ldc, d, ldd, factor, nk, ak, ldak, bk, &
                       ldbk, ck, ldck, dk, lddk, rcond, iwork, dwork, &
                       ldwork, bwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: factor
            integer, intent(inout)            :: nk
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(inout)            :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(inout)            :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(inout)            :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(inout)            :: lddk
            double precision, intent(inout)   :: rcond(2)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine sb10id
    end interface
    public :: sb10id
    
    interface 
        subroutine sb10jd(n, m, np, a, lda, b, ldb, c, &
                       ldc, d, ldd, e, lde, nsys, dwork, ldwork, &
                       info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            integer, intent(inout)            :: nsys
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb10jd
    end interface
    public :: sb10jd
    
    interface 
        subroutine sb10kd(n, m, np, a, lda, b, ldb, c, &
                       ldc, factor, ak, ldak, bk, ldbk, ck, ldck, &
                       dk, lddk, rcond, iwork, dwork, ldwork, bwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: factor
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(inout)            :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(inout)            :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(inout)            :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(inout)            :: lddk
            double precision, intent(inout)   :: rcond(4)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine sb10kd
    end interface
    public :: sb10kd
    
    interface 
        subroutine sb10ld(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, d, ldd, ak, ldak, bk, &
                       ldbk, ck, ldck, dk, lddk, ac, ldac, bc, &
                       ldbc, cc, ldcc, dc, lddc, iwork, dwork, ldwork, &
                       info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(inout)            :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(inout)            :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(inout)            :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(inout)            :: lddk
            double precision, intent(inout)   :: ac(ldac, *)
            integer, intent(inout)            :: ldac
            double precision, intent(inout)   :: bc(ldbc, *)
            integer, intent(inout)            :: ldbc
            double precision, intent(inout)   :: cc(ldcc, *)
            integer, intent(inout)            :: ldcc
            double precision, intent(inout)   :: dc(lddc, *)
            integer, intent(inout)            :: lddc
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb10ld
    end interface
    public :: sb10ld
    
    interface 
        subroutine sb10md(nc, mp, lendat, f, ord, mnb, nblock, itype, &
                       qutol, a, lda, b, ldb, c, ldc, d, &
                       ldd, omega, totord, ad, ldad, bd, ldbd, cd, &
                       ldcd, dd, lddd, mju, iwork, liwork, dwork, ldwork, &
                       zwork, lzwork, info)
            integer, intent(inout)            :: nc
            integer, intent(inout)            :: mp
            integer, intent(inout)            :: lendat
            integer, intent(inout)            :: f
            integer, intent(inout)            :: ord
            integer, intent(inout)            :: mnb
            integer, intent(inout)            :: nblock(*)
            integer, intent(inout)            :: itype(*)
            double precision, intent(inout)   :: qutol
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: omega(*)
            integer, intent(inout)            :: totord
            double precision, intent(inout)   :: ad(ldad, *)
            integer, intent(inout)            :: ldad
            double precision, intent(inout)   :: bd(ldbd, *)
            integer, intent(inout)            :: ldbd
            double precision, intent(inout)   :: cd(ldcd, *)
            integer, intent(inout)            :: ldcd
            double precision, intent(inout)   :: dd(lddd, *)
            integer, intent(inout)            :: lddd
            double precision, intent(inout)   :: mju(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine sb10md
    end interface
    public :: sb10md
    
    interface 
        subroutine sb10pd(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, d, ldd, tu, ldtu, ty, &
                       ldty, rcond, tol, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: tu(ldtu, *)
            integer, intent(inout)            :: ldtu
            double precision, intent(inout)   :: ty(ldty, *)
            integer, intent(inout)            :: ldty
            double precision, intent(inout)   :: rcond(2)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb10pd
    end interface
    public :: sb10pd
    
    interface 
        subroutine sb10qd(n, m, np, ncon, nmeas, gamma, a, lda, &
                       b, ldb, c, ldc, d, ldd, f, ldf, &
                       h, ldh, x, ldx, y, ldy, xycond, iwork, &
                       dwork, ldwork, bwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: gamma
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: xycond(2)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine sb10qd
    end interface
    public :: sb10qd
    
    interface 
        subroutine sb10rd(n, m, np, ncon, nmeas, gamma, a, lda, &
                       b, ldb, c, ldc, d, ldd, f, ldf, &
                       h, ldh, tu, ldtu, ty, ldty, x, ldx, &
                       y, ldy, ak, ldak, bk, ldbk, ck, ldck, &
                       dk, lddk, iwork, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: gamma
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: tu(ldtu, *)
            integer, intent(inout)            :: ldtu
            double precision, intent(inout)   :: ty(ldty, *)
            integer, intent(inout)            :: ldty
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(inout)            :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(inout)            :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(inout)            :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(inout)            :: lddk
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb10rd
    end interface
    public :: sb10rd
    
    interface 
        subroutine sb10sd(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, d, ldd, ak, ldak, bk, &
                       ldbk, ck, ldck, dk, lddk, x, ldx, y, &
                       ldy, rcond, tol, iwork, dwork, ldwork, bwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(inout)            :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(inout)            :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(inout)            :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(inout)            :: lddk
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: rcond(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine sb10sd
    end interface
    public :: sb10sd
    
    interface 
        subroutine sb10td(n, m, np, ncon, nmeas, d, ldd, tu, &
                       ldtu, ty, ldty, ak, ldak, bk, ldbk, ck, &
                       ldck, dk, lddk, rcond, tol, iwork, dwork, ldwork, &
                       info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: tu(ldtu, *)
            integer, intent(inout)            :: ldtu
            double precision, intent(inout)   :: ty(ldty, *)
            integer, intent(inout)            :: ldty
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(inout)            :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(inout)            :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(inout)            :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(inout)            :: lddk
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb10td
    end interface
    public :: sb10td
    
    interface 
        subroutine sb10ud(n, m, np, ncon, nmeas, b, ldb, c, &
                       ldc, d, ldd, tu, ldtu, ty, ldty, rcond, &
                       tol, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: tu(ldtu, *)
            integer, intent(inout)            :: ldtu
            double precision, intent(inout)   :: ty(ldty, *)
            integer, intent(inout)            :: ldty
            double precision, intent(inout)   :: rcond(2)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb10ud
    end interface
    public :: sb10ud
    
    interface 
        subroutine sb10vd(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, f, ldf, h, ldh, x, &
                       ldx, y, ldy, xycond, iwork, dwork, ldwork, bwork, &
                       info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: xycond(2)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine sb10vd
    end interface
    public :: sb10vd
    
    interface 
        subroutine sb10wd(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, d, ldd, f, ldf, h, &
                       ldh, tu, ldtu, ty, ldty, ak, ldak, bk, &
                       ldbk, ck, ldck, dk, lddk, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            integer, intent(inout)            :: ncon
            integer, intent(inout)            :: nmeas
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: tu(ldtu, *)
            integer, intent(inout)            :: ldtu
            double precision, intent(inout)   :: ty(ldty, *)
            integer, intent(inout)            :: ldty
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(inout)            :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(inout)            :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(inout)            :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(inout)            :: lddk
            integer, intent(inout)            :: info
        end subroutine sb10wd
    end interface
    public :: sb10wd
    
    interface 
        subroutine sb10yd(discfl, flag, lendat, rfrdat, ifrdat, omega, n, a, &
                       lda, b, c, d, tol, iwork, dwork, ldwork, &
                       zwork, lzwork, info)
            integer, intent(inout)            :: discfl
            integer, intent(inout)            :: flag
            integer, intent(inout)            :: lendat
            double precision, intent(inout)   :: rfrdat(*)
            double precision, intent(inout)   :: ifrdat(*)
            double precision, intent(inout)   :: omega(*)
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: c(*)
            double precision, intent(inout)   :: d(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine sb10yd
    end interface
    public :: sb10yd
    
    interface 
        subroutine sb10zd(n, m, np, a, lda, b, ldb, c, &
                       ldc, d, ldd, factor, ak, ldak, bk, ldbk, &
                       ck, ldck, dk, lddk, rcond, tol, iwork, dwork, &
                       ldwork, bwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: np
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: factor
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(inout)            :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(inout)            :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(inout)            :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(inout)            :: lddk
            double precision, intent(inout)   :: rcond(6)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: info
        end subroutine sb10zd
    end interface
    public :: sb10zd
    
    interface 
        subroutine sb10zp(discfl, n, a, lda, b, c, d, iwork, &
                       dwork, ldwork, info)
            integer, intent(inout)            :: discfl
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: c(*)
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb10zp
    end interface
    public :: sb10zp
    
    interface 
        subroutine sb16ad(dico, jobc, jobo, jobmr, weight, equil, ordsel, n, &
                       m, p, nc, ncr, alpha, a, lda, b, &
                       ldb, c, ldc, d, ldd, ac, ldac, bc, &
                       ldbc, cc, ldcc, dc, lddc, ncs, hsvc, tol1, &
                       tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobc
            character, intent(inout)          :: jobo
            character, intent(inout)          :: jobmr
            character, intent(inout)          :: weight
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nc
            integer, intent(inout)            :: ncr
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: ac(ldac, *)
            integer, intent(inout)            :: ldac
            double precision, intent(inout)   :: bc(ldbc, *)
            integer, intent(inout)            :: ldbc
            double precision, intent(inout)   :: cc(ldcc, *)
            integer, intent(inout)            :: ldcc
            double precision, intent(inout)   :: dc(lddc, *)
            integer, intent(inout)            :: lddc
            integer, intent(inout)            :: ncs
            double precision, intent(inout)   :: hsvc(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine sb16ad
    end interface
    public :: sb16ad
    
    interface 
        subroutine sb16ay(dico, jobc, jobo, weight, n, m, p, nc, &
                       ncs, a, lda, b, ldb, c, ldc, d, &
                       ldd, ac, ldac, bc, ldbc, cc, ldcc, dc, &
                       lddc, scalec, scaleo, s, lds, r, ldr, iwork, &
                       dwork, ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobc
            character, intent(inout)          :: jobo
            character, intent(inout)          :: weight
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nc
            integer, intent(inout)            :: ncs
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: ac(ldac, *)
            integer, intent(inout)            :: ldac
            double precision, intent(inout)   :: bc(ldbc, *)
            integer, intent(inout)            :: ldbc
            double precision, intent(inout)   :: cc(ldcc, *)
            integer, intent(inout)            :: ldcc
            double precision, intent(inout)   :: dc(lddc, *)
            integer, intent(inout)            :: lddc
            double precision, intent(inout)   :: scalec
            double precision, intent(inout)   :: scaleo
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb16ay
    end interface
    public :: sb16ay
    
    interface 
        subroutine sb16bd(dico, jobd, jobmr, jobcf, equil, ordsel, n, m, &
                       p, ncr, a, lda, b, ldb, c, ldc, &
                       d, ldd, f, ldf, g, ldg, dc, lddc, &
                       hsv, tol1, tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobd
            character, intent(inout)          :: jobmr
            character, intent(inout)          :: jobcf
            character, intent(inout)          :: equil
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ncr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: dc(lddc, *)
            integer, intent(inout)            :: lddc
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine sb16bd
    end interface
    public :: sb16bd
    
    interface 
        subroutine sb16cd(dico, jobd, jobmr, jobcf, ordsel, n, m, p, &
                       ncr, a, lda, b, ldb, c, ldc, d, &
                       ldd, f, ldf, g, ldg, hsv, tol, iwork, &
                       dwork, ldwork, iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobd
            character, intent(inout)          :: jobmr
            character, intent(inout)          :: jobcf
            character, intent(inout)          :: ordsel
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ncr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: hsv(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine sb16cd
    end interface
    public :: sb16cd
    
    interface 
        subroutine sb16cy(dico, jobcf, n, m, p, a, lda, b, &
                       ldb, c, ldc, f, ldf, g, ldg, scalec, &
                       scaleo, s, lds, r, ldr, dwork, ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobcf
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: scalec
            double precision, intent(inout)   :: scaleo
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sb16cy
    end interface
    public :: sb16cy
    
    interface 
        logical function select (par1,par2)
            double precision, intent(inout)   :: par1
            double precision, intent(inout)   :: par2
        end function select
    end interface
    public :: select
    
    interface 
        subroutine sg02ad(dico, jobb, fact, uplo, jobl, scal, sort, acc, &
                       n, m, p, a, lda, e, lde, b, &
                       ldb, q, ldq, r, ldr, l, ldl, rcondu, &
                       x, ldx, alfar, alfai, beta, s, lds, t, &
                       ldt, u, ldu, tol, iwork, dwork, ldwork, bwork, &
                       iwarn, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobb
            character, intent(inout)          :: fact
            character, intent(inout)          :: uplo
            character, intent(inout)          :: jobl
            character, intent(inout)          :: scal
            character, intent(inout)          :: sort
            character, intent(inout)          :: acc
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: l(ldl, *)
            integer, intent(inout)            :: ldl
            double precision, intent(inout)   :: rcondu
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: alfar(*)
            double precision, intent(inout)   :: alfai(*)
            double precision, intent(inout)   :: beta(*)
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine sg02ad
    end interface
    public :: sg02ad
    
    interface 
        subroutine sg02cv(dico, job, jobe, uplo, trans, n, a, lda, &
                       e, lde, x, ldx, r, ldr, norms, dwork, &
                       ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: job
            character, intent(inout)          :: jobe
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: norms(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sg02cv
    end interface
    public :: sg02cv
    
    interface 
        subroutine sg02cw(dico, job, jobe, flag, jobg, uplo, trans, n, &
                       m, a, lda, e, lde, g, ldg, x, &
                       ldx, f, ldf, k, ldk, xe, ldxe, r, &
                       ldr, c, ldc, norms, dwork, ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: job
            character, intent(inout)          :: jobe
            character, intent(inout)          :: flag
            character, intent(inout)          :: jobg
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(inout)            :: ldf
            double precision, intent(inout)   :: k(ldk, *)
            integer, intent(inout)            :: ldk
            double precision, intent(inout)   :: xe(ldxe, *)
            integer, intent(inout)            :: ldxe
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: norms(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sg02cw
    end interface
    public :: sg02cw
    
    interface 
        subroutine sg02cx(jobe, flag, jobg, uplo, trans, n, m, e, &
                       lde, r, ldr, s, lds, g, ldg, alpha, &
                       rnorm, dwork, ldwork, iwarn, info)
            character, intent(inout)          :: jobe
            character, intent(inout)          :: flag
            character, intent(inout)          :: jobg
            character, intent(inout)          :: uplo
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: rnorm
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: iwarn
            integer, intent(inout)            :: info
        end subroutine sg02cx
    end interface
    public :: sg02cx
    
    interface 
        subroutine sg02nd(dico, jobe, job, jobx, fact, uplo, jobl, trans, &
                       n, m, p, a, lda, e, lde, b, &
                       ldb, r, ldr, ipiv, l, ldl, x, ldx, &
                       rnorm, k, ldk, h, ldh, xe, ldxe, oufact, &
                       iwork, dwork, ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: jobe
            character, intent(inout)          :: job
            character, intent(inout)          :: jobx
            character, intent(inout)          :: fact
            character, intent(inout)          :: uplo
            character, intent(inout)          :: jobl
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(inout)            :: ldr
            integer, intent(inout)            :: ipiv(*)
            double precision, intent(inout)   :: l(ldl, *)
            integer, intent(inout)            :: ldl
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: rnorm
            double precision, intent(inout)   :: k(ldk, *)
            integer, intent(inout)            :: ldk
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: xe(ldxe, *)
            integer, intent(inout)            :: ldxe
            integer, intent(inout)            :: oufact(2)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sg02nd
    end interface
    public :: sg02nd
    
    interface 
        subroutine sg03ad(dico, job, fact, trans, uplo, n, a, lda, &
                       e, lde, q, ldq, z, ldz, x, ldx, &
                       scale, sep, ferr, alphar, alphai, beta, iwork, dwork, &
                       ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: job
            character, intent(inout)          :: fact
            character, intent(inout)          :: trans
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: sep
            double precision, intent(inout)   :: ferr
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sg03ad
    end interface
    public :: sg03ad
    
    interface 
        subroutine sg03ax(trans, n, a, lda, e, lde, x, ldx, &
                       scale, info)
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: scale
            integer, intent(inout)            :: info
        end subroutine sg03ax
    end interface
    public :: sg03ax
    
    interface 
        subroutine sg03ay(trans, n, a, lda, e, lde, x, ldx, &
                       scale, info)
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: scale
            integer, intent(inout)            :: info
        end subroutine sg03ay
    end interface
    public :: sg03ay
    
    interface 
        subroutine sg03bd(dico, fact, trans, n, m, a, lda, e, &
                       lde, q, ldq, z, ldz, b, ldb, scale, &
                       alphar, alphai, beta, dwork, ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: fact
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine sg03bd
    end interface
    public :: sg03bd
    
    interface 
        subroutine sg03br(xr, xi, yr, yi, c, sr, si, zr, &
                       zi)
            double precision, intent(inout)   :: xr
            double precision, intent(inout)   :: xi
            double precision, intent(inout)   :: yr
            double precision, intent(inout)   :: yi
            double precision, intent(inout)   :: c
            double precision, intent(inout)   :: sr
            double precision, intent(inout)   :: si
            double precision, intent(inout)   :: zr
            double precision, intent(inout)   :: zi
        end subroutine sg03br
    end interface
    public :: sg03br
    
    interface 
        subroutine sg03bs(trans, n, a, lda, e, lde, b, ldb, &
                       scale, dwork, zwork, info)
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(inout)            :: lde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: info
        end subroutine sg03bs
    end interface
    public :: sg03bs
    
    interface 
        subroutine sg03bt(trans, n, a, lda, e, lde, b, ldb, &
                       scale, dwork, zwork, info)
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(inout)            :: lde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: info
        end subroutine sg03bt
    end interface
    public :: sg03bt
    
    interface 
        subroutine sg03bu(trans, n, a, lda, e, lde, b, ldb, &
                       scale, dwork, info)
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine sg03bu
    end interface
    public :: sg03bu
    
    interface 
        subroutine sg03bv(trans, n, a, lda, e, lde, b, ldb, &
                       scale, dwork, info)
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine sg03bv
    end interface
    public :: sg03bv
    
    interface 
        subroutine sg03bw(trans, m, n, a, lda, c, ldc, e, &
                       lde, d, ldd, x, ldx, scale, info)
            character, intent(inout)          :: trans
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: scale
            integer, intent(inout)            :: info
        end subroutine sg03bw
    end interface
    public :: sg03bw
    
    interface 
        subroutine sg03bx(dico, trans, a, lda, e, lde, b, ldb, &
                       u, ldu, scale, m1, ldm1, m2, ldm2, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: trans
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: scale
            double precision, intent(inout)   :: m1(ldm1, *)
            integer, intent(inout)            :: ldm1
            double precision, intent(inout)   :: m2(ldm2, *)
            integer, intent(inout)            :: ldm2
            integer, intent(inout)            :: info
        end subroutine sg03bx
    end interface
    public :: sg03bx
    
    interface 
        subroutine sg03by(xr, xi, yr, yi, cr, ci, sr, si, &
                       z)
            double precision, intent(inout)   :: xr
            double precision, intent(inout)   :: xi
            double precision, intent(inout)   :: yr
            double precision, intent(inout)   :: yi
            double precision, intent(inout)   :: cr
            double precision, intent(inout)   :: ci
            double precision, intent(inout)   :: sr
            double precision, intent(inout)   :: si
            double precision, intent(inout)   :: z
        end subroutine sg03by
    end interface
    public :: sg03by
    
    interface 
        subroutine sg03bz(dico, fact, trans, n, m, a, lda, e, &
                       lde, q, ldq, z, ldz, b, ldb, scale, &
                       alpha, beta, dwork, zwork, lzwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: fact
            character, intent(inout)          :: trans
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(inout)            :: lde
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(inout)            :: ldq
            complex*16, intent(inout)         :: z(ldz, *)
            integer, intent(inout)            :: ldz
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: scale
            complex*16, intent(inout)         :: alpha(*)
            complex*16, intent(inout)         :: beta(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine sg03bz
    end interface
    public :: sg03bz
    
    interface 
        subroutine tb01id(job, n, m, p, maxred, a, lda, b, &
                       ldb, c, ldc, scale, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: maxred
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: scale(*)
            integer, intent(inout)            :: info
        end subroutine tb01id
    end interface
    public :: tb01id
    
    interface 
        subroutine tb01iz(job, n, m, p, maxred, a, lda, b, &
                       ldb, c, ldc, scale, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: maxred
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: scale(*)
            integer, intent(inout)            :: info
        end subroutine tb01iz
    end interface
    public :: tb01iz
    
    interface 
        subroutine tb01kd(dico, stdom, joba, n, m, p, alpha, a, &
                       lda, b, ldb, c, ldc, ndim, u, ldu, &
                       wr, wi, dwork, ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: stdom
            character, intent(inout)          :: joba
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: ndim
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb01kd
    end interface
    public :: tb01kd
    
    interface 
        subroutine tb01kx(n, m, p, ndim, a, lda, b, ldb, &
                       c, ldc, u, ldu, v, ldv, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ndim
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(inout)            :: ldv
            integer, intent(inout)            :: info
        end subroutine tb01kx
    end interface
    public :: tb01kx
    
    interface 
        subroutine tb01ld(dico, stdom, joba, n, m, p, alpha, a, &
                       lda, b, ldb, c, ldc, ndim, u, ldu, &
                       wr, wi, dwork, ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: stdom
            character, intent(inout)          :: joba
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: ndim
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb01ld
    end interface
    public :: tb01ld
    
    interface 
        subroutine tb01md(jobu, uplo, n, m, a, lda, b, ldb, &
                       u, ldu, dwork, info)
            character, intent(inout)          :: jobu
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine tb01md
    end interface
    public :: tb01md
    
    interface 
        subroutine tb01nd(jobu, uplo, n, p, a, lda, c, ldc, &
                       u, ldu, dwork, info)
            character, intent(inout)          :: jobu
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine tb01nd
    end interface
    public :: tb01nd
    
    interface 
        subroutine tb01pd(job, equil, n, m, p, a, lda, b, &
                       ldb, c, ldc, nr, tol, iwork, dwork, ldwork, &
                       info)
            character, intent(inout)          :: job
            character, intent(inout)          :: equil
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb01pd
    end interface
    public :: tb01pd
    
    interface 
        subroutine tb01px(job, equil, n, m, p, a, lda, b, &
                       ldb, c, ldc, nr, infred, tol, iwork, dwork, &
                       ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: equil
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: infred(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb01px
    end interface
    public :: tb01px
    
    interface 
        subroutine tb01td(n, m, p, a, lda, b, ldb, c, &
                       ldc, d, ldd, low, igh, scstat, scin, scout, &
                       dwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: low
            integer, intent(inout)            :: igh
            double precision, intent(inout)   :: scstat(*)
            double precision, intent(inout)   :: scin(*)
            double precision, intent(inout)   :: scout(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine tb01td
    end interface
    public :: tb01td
    
    interface 
        subroutine tb01ty(mode, ioff, joff, nrow, ncol, size, x, ldx, &
                       bvect)
            integer, intent(inout)            :: mode
            integer, intent(inout)            :: ioff
            integer, intent(inout)            :: joff
            integer, intent(inout)            :: nrow
            integer, intent(inout)            :: ncol
            double precision, intent(inout)   :: size
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(inout)            :: ldx
            double precision, intent(inout)   :: bvect(*)
        end subroutine tb01ty
    end interface
    public :: tb01ty
    
    interface 
        subroutine tb01ud(jobz, n, m, p, a, lda, b, ldb, &
                       c, ldc, ncont, indcon, nblk, z, ldz, tau, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: jobz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: ncont
            integer, intent(inout)            :: indcon
            integer, intent(inout)            :: nblk(*)
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb01ud
    end interface
    public :: tb01ud
    
    interface 
        subroutine tb01ux(compz, n, m, p, a, lda, b, ldb, &
                       c, ldc, z, ldz, nobsv, nlblck, ctau, tol, &
                       iwork, dwork, info)
            character, intent(inout)          :: compz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: nobsv
            integer, intent(inout)            :: nlblck
            integer, intent(inout)            :: ctau(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine tb01ux
    end interface
    public :: tb01ux
    
    interface 
        subroutine tb01uy(jobz, n, m1, m2, p, a, lda, b, &
                       ldb, c, ldc, ncont, indcon, nblk, z, ldz, &
                       tau, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: jobz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m1
            integer, intent(inout)            :: m2
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: ncont
            integer, intent(inout)            :: indcon
            integer, intent(inout)            :: nblk(*)
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb01uy
    end interface
    public :: tb01uy
    
    interface 
        subroutine tb01vd(apply, n, m, l, a, lda, b, ldb, &
                       c, ldc, d, ldd, x0, theta, ltheta, dwork, &
                       ldwork, info)
            character, intent(inout)          :: apply
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: x0(*)
            double precision, intent(inout)   :: theta(*)
            integer, intent(inout)            :: ltheta
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb01vd
    end interface
    public :: tb01vd
    
    interface 
        subroutine tb01vy(apply, n, m, l, theta, ltheta, a, lda, &
                       b, ldb, c, ldc, d, ldd, x0, dwork, &
                       ldwork, info)
            character, intent(inout)          :: apply
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: theta(*)
            integer, intent(inout)            :: ltheta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: x0(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb01vy
    end interface
    public :: tb01vy
    
    interface 
        subroutine tb01wd(n, m, p, a, lda, b, ldb, c, &
                       ldc, u, ldu, wr, wi, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb01wd
    end interface
    public :: tb01wd
    
    interface 
        subroutine tb01wx(compu, n, m, p, a, lda, b, ldb, &
                       c, ldc, u, ldu, dwork, ldwork, info)
            character, intent(inout)          :: compu
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb01wx
    end interface
    public :: tb01wx
    
    interface 
        subroutine tb01xd(jobd, n, m, p, kl, ku, a, lda, &
                       b, ldb, c, ldc, d, ldd, info)
            character, intent(inout)          :: jobd
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: kl
            integer, intent(inout)            :: ku
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: info
        end subroutine tb01xd
    end interface
    public :: tb01xd
    
    interface 
        subroutine tb01xz(jobd, n, m, p, kl, ku, a, lda, &
                       b, ldb, c, ldc, d, ldd, info)
            character, intent(inout)    :: jobd
            integer, intent(inout)      :: n
            integer, intent(inout)      :: m
            integer, intent(inout)      :: p
            integer, intent(inout)      :: kl
            integer, intent(inout)      :: ku
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(inout)      :: lda
            complex*16, intent(inout)   :: b(ldb, *)
            integer, intent(inout)      :: ldb
            complex*16, intent(inout)   :: c(ldc, *)
            integer, intent(inout)      :: ldc
            complex*16, intent(inout)   :: d(ldd, *)
            integer, intent(inout)      :: ldd
            integer, intent(inout)      :: info
        end subroutine tb01xz
    end interface
    public :: tb01xz
    
    interface 
        subroutine tb01yd(n, m, p, a, lda, b, ldb, c, &
                       ldc, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: info
        end subroutine tb01yd
    end interface
    public :: tb01yd
    
    interface 
        subroutine tb01zd(jobz, n, p, a, lda, b, c, ldc, &
                       ncont, z, ldz, tau, tol, dwork, ldwork, info)
            character, intent(inout)          :: jobz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: ncont
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb01zd
    end interface
    public :: tb01zd
    
    interface 
        subroutine tb03ad(leri, equil, n, m, p, a, lda, b, &
                       ldb, c, ldc, d, ldd, nr, index, pcoeff, &
                       ldpco1, ldpco2, qcoeff, ldqco1, ldqco2, vcoeff, ldvco1, ldvco2, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: leri
            character, intent(inout)          :: equil
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: index(*)
            double precision, intent(inout)   :: pcoeff(ldpco1, ldpco2, *)
            integer, intent(inout)            :: ldpco1
            integer, intent(inout)            :: ldpco2
            double precision, intent(inout)   :: qcoeff(ldqco1, ldqco2, *)
            integer, intent(inout)            :: ldqco1
            integer, intent(inout)            :: ldqco2
            double precision, intent(inout)   :: vcoeff(ldvco1, ldvco2, *)
            integer, intent(inout)            :: ldvco1
            integer, intent(inout)            :: ldvco2
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb03ad
    end interface
    public :: tb03ad
    
    interface 
        subroutine tb03ay(nr, a, lda, indblk, nblk, vcoeff, ldvco1, ldvco2, &
                       pcoeff, ldpco1, ldpco2, info)
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: indblk
            integer, intent(inout)            :: nblk(*)
            double precision, intent(inout)   :: vcoeff(ldvco1, ldvco2, *)
            integer, intent(inout)            :: ldvco1
            integer, intent(inout)            :: ldvco2
            double precision, intent(inout)   :: pcoeff(ldpco1, ldpco2, *)
            integer, intent(inout)            :: ldpco1
            integer, intent(inout)            :: ldpco2
            integer, intent(inout)            :: info
        end subroutine tb03ay
    end interface
    public :: tb03ay
    
    interface 
        subroutine tb04ad(rowcol, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, nr, index, dcoeff, lddcoe, &
                       ucoeff, lduco1, lduco2, tol1, tol2, iwork, dwork, ldwork, &
                       info)
            character, intent(inout)          :: rowcol
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: index(*)
            double precision, intent(inout)   :: dcoeff(lddcoe, *)
            integer, intent(inout)            :: lddcoe
            double precision, intent(inout)   :: ucoeff(lduco1, lduco2, *)
            integer, intent(inout)            :: lduco1
            integer, intent(inout)            :: lduco2
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb04ad
    end interface
    public :: tb04ad
    
    interface 
        subroutine tb04ay(n, mwork, pwork, a, lda, b, ldb, c, &
                       ldc, d, ldd, ncont, indexd, dcoeff, lddcoe, ucoeff, &
                       lduco1, lduco2, at, n1, tau, tol1, tol2, iwork, &
                       dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: mwork
            integer, intent(inout)            :: pwork
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: ncont
            integer, intent(inout)            :: indexd(*)
            double precision, intent(inout)   :: dcoeff(lddcoe, *)
            integer, intent(inout)            :: lddcoe
            double precision, intent(inout)   :: ucoeff(lduco1, lduco2, *)
            integer, intent(inout)            :: lduco1
            integer, intent(inout)            :: lduco2
            double precision, intent(inout)   :: at(n1, *)
            integer, intent(inout)            :: n1
            double precision, intent(inout)   :: tau(*)
            double precision, intent(inout)   :: tol1
            double precision, intent(inout)   :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb04ay
    end interface
    public :: tb04ay
    
    interface 
        subroutine tb04bd(jobd, order, equil, n, m, p, md, a, &
                       lda, b, ldb, c, ldc, d, ldd, ign, &
                       ldign, igd, ldigd, gn, gd, tol, iwork, dwork, &
                       ldwork, info)
            character, intent(inout)          :: jobd
            character, intent(inout)          :: order
            character, intent(inout)          :: equil
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: md
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: ign(ldign, *)
            integer, intent(inout)            :: ldign
            integer, intent(inout)            :: igd(ldigd, *)
            integer, intent(inout)            :: ldigd
            double precision, intent(inout)   :: gn(*)
            double precision, intent(inout)   :: gd(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb04bd
    end interface
    public :: tb04bd
    
    interface 
        subroutine tb04bv(order, p, m, md, ign, ldign, igd, ldigd, &
                       gn, gd, d, ldd, tol, info)
            character, intent(inout)          :: order
            integer, intent(inout)            :: p
            integer, intent(inout)            :: m
            integer, intent(inout)            :: md
            integer, intent(inout)            :: ign(ldign, *)
            integer, intent(inout)            :: ldign
            integer, intent(inout)            :: igd(ldigd, *)
            integer, intent(inout)            :: ldigd
            double precision, intent(inout)   :: gn(*)
            double precision, intent(inout)   :: gd(*)
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: info
        end subroutine tb04bv
    end interface
    public :: tb04bv
    
    interface 
        subroutine tb04bw(order, p, m, md, ign, ldign, igd, ldigd, &
                       gn, gd, d, ldd, info)
            character, intent(inout)          :: order
            integer, intent(inout)            :: p
            integer, intent(inout)            :: m
            integer, intent(inout)            :: md
            integer, intent(inout)            :: ign(ldign, *)
            integer, intent(inout)            :: ldign
            integer, intent(inout)            :: igd(ldigd, *)
            integer, intent(inout)            :: ldigd
            double precision, intent(inout)   :: gn(*)
            double precision, intent(inout)   :: gd(*)
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: info
        end subroutine tb04bw
    end interface
    public :: tb04bw
    
    interface 
        subroutine tb04bx(ip, iz, a, lda, b, c, d, pr, &
                       pi, zr, zi, gain, iwork)
            integer, intent(inout)            :: ip
            integer, intent(inout)            :: iz
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: c(*)
            double precision, intent(inout)   :: d
            double precision, intent(inout)   :: pr(*)
            double precision, intent(inout)   :: pi(*)
            double precision, intent(inout)   :: zr(*)
            double precision, intent(inout)   :: zi(*)
            double precision, intent(inout)   :: gain
            integer, intent(inout)            :: iwork(*)
        end subroutine tb04bx
    end interface
    public :: tb04bx
    
    interface 
        subroutine tb04cd(jobd, equil, n, m, p, npz, a, lda, &
                       b, ldb, c, ldc, d, ldd, nz, ldnz, &
                       np, ldnp, zerosr, zerosi, polesr, polesi, gains, ldgain, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: jobd
            character, intent(inout)          :: equil
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: npz
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: nz(ldnz, *)
            integer, intent(inout)            :: ldnz
            integer, intent(inout)            :: np(ldnp, *)
            integer, intent(inout)            :: ldnp
            double precision, intent(inout)   :: zerosr(*)
            double precision, intent(inout)   :: zerosi(*)
            double precision, intent(inout)   :: polesr(*)
            double precision, intent(inout)   :: polesi(*)
            double precision, intent(inout)   :: gains(ldgain, *)
            integer, intent(inout)            :: ldgain
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb04cd
    end interface
    public :: tb04cd
    
    interface 
        subroutine tb05ad(baleig, inita, n, m, p, freq, a, lda, &
                       b, ldb, c, ldc, rcond, g, ldg, evre, &
                       evim, hinvb, ldhinv, iwork, dwork, ldwork, zwork, lzwork, &
                       info)
            character, intent(inout)          :: baleig
            character, intent(inout)          :: inita
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            complex*16, intent(inout)         :: freq
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: rcond
            complex*16, intent(inout)         :: g(ldg, *)
            integer, intent(inout)            :: ldg
            double precision, intent(inout)   :: evre(*)
            double precision, intent(inout)   :: evim(*)
            complex*16, intent(inout)         :: hinvb(ldhinv, *)
            integer, intent(inout)            :: ldhinv
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine tb05ad
    end interface
    public :: tb05ad
    
    interface 
        subroutine tc01od(leri, m, p, indlim, pcoeff, ldpco1, ldpco2, qcoeff, &
                       ldqco1, ldqco2, info)
            character, intent(inout)          :: leri
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: indlim
            double precision, intent(inout)   :: pcoeff(ldpco1, ldpco2, *)
            integer, intent(inout)            :: ldpco1
            integer, intent(inout)            :: ldpco2
            double precision, intent(inout)   :: qcoeff(ldqco1, ldqco2, *)
            integer, intent(inout)            :: ldqco1
            integer, intent(inout)            :: ldqco2
            integer, intent(inout)            :: info
        end subroutine tc01od
    end interface
    public :: tc01od
    
    interface 
        subroutine tc04ad(leri, m, p, index, pcoeff, ldpco1, ldpco2, qcoeff, &
                       ldqco1, ldqco2, n, rcond, a, lda, b, ldb, &
                       c, ldc, d, ldd, iwork, dwork, ldwork, info)
            character, intent(inout)          :: leri
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: index(*)
            double precision, intent(inout)   :: pcoeff(ldpco1, ldpco2, *)
            integer, intent(inout)            :: ldpco1
            integer, intent(inout)            :: ldpco2
            double precision, intent(inout)   :: qcoeff(ldqco1, ldqco2, *)
            integer, intent(inout)            :: ldqco1
            integer, intent(inout)            :: ldqco2
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: rcond
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tc04ad
    end interface
    public :: tc04ad
    
    interface 
        subroutine tc05ad(leri, m, p, sval, index, pcoeff, ldpco1, ldpco2, &
                       qcoeff, ldqco1, ldqco2, rcond, cfreqr, ldcfre, iwork, dwork, &
                       zwork, info)
            character, intent(inout)          :: leri
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            complex*16, intent(inout)         :: sval
            integer, intent(inout)            :: index(*)
            double precision, intent(inout)   :: pcoeff(ldpco1, ldpco2, *)
            integer, intent(inout)            :: ldpco1
            integer, intent(inout)            :: ldpco2
            double precision, intent(inout)   :: qcoeff(ldqco1, ldqco2, *)
            integer, intent(inout)            :: ldqco1
            integer, intent(inout)            :: ldqco2
            double precision, intent(inout)   :: rcond
            complex*16, intent(inout)         :: cfreqr(ldcfre, *)
            integer, intent(inout)            :: ldcfre
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: info
        end subroutine tc05ad
    end interface
    public :: tc05ad
    
    interface 
        subroutine td03ad(rowcol, leri, equil, m, p, indexd, dcoeff, lddcoe, &
                       ucoeff, lduco1, lduco2, nr, a, lda, b, ldb, &
                       c, ldc, d, ldd, indexp, pcoeff, ldpco1, ldpco2, &
                       qcoeff, ldqco1, ldqco2, vcoeff, ldvco1, ldvco2, tol, iwork, &
                       dwork, ldwork, info)
            character, intent(inout)          :: rowcol
            character, intent(inout)          :: leri
            character, intent(inout)          :: equil
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: indexd(*)
            double precision, intent(inout)   :: dcoeff(lddcoe, *)
            integer, intent(inout)            :: lddcoe
            double precision, intent(inout)   :: ucoeff(lduco1, lduco2, *)
            integer, intent(inout)            :: lduco1
            integer, intent(inout)            :: lduco2
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: indexp(*)
            double precision, intent(inout)   :: pcoeff(ldpco1, ldpco2, *)
            integer, intent(inout)            :: ldpco1
            integer, intent(inout)            :: ldpco2
            double precision, intent(inout)   :: qcoeff(ldqco1, ldqco2, *)
            integer, intent(inout)            :: ldqco1
            integer, intent(inout)            :: ldqco2
            double precision, intent(inout)   :: vcoeff(ldvco1, ldvco2, *)
            integer, intent(inout)            :: ldvco1
            integer, intent(inout)            :: ldvco2
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine td03ad
    end interface
    public :: td03ad
    
    interface 
        subroutine td03ay(mwork, pwork, index, dcoeff, lddcoe, ucoeff, lduco1, lduco2, &
                       n, a, lda, b, ldb, c, ldc, d, &
                       ldd, info)
            integer, intent(inout)            :: mwork
            integer, intent(inout)            :: pwork
            integer, intent(inout)            :: index(*)
            double precision, intent(inout)   :: dcoeff(lddcoe, *)
            integer, intent(inout)            :: lddcoe
            double precision, intent(inout)   :: ucoeff(lduco1, lduco2, *)
            integer, intent(inout)            :: lduco1
            integer, intent(inout)            :: lduco2
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: info
        end subroutine td03ay
    end interface
    public :: td03ay
    
    interface 
        subroutine td04ad(rowcol, m, p, index, dcoeff, lddcoe, ucoeff, lduco1, &
                       lduco2, nr, a, lda, b, ldb, c, ldc, &
                       d, ldd, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: rowcol
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: index(*)
            double precision, intent(inout)   :: dcoeff(lddcoe, *)
            integer, intent(inout)            :: lddcoe
            double precision, intent(inout)   :: ucoeff(lduco1, lduco2, *)
            integer, intent(inout)            :: lduco1
            integer, intent(inout)            :: lduco2
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine td04ad
    end interface
    public :: td04ad
    
    interface 
        subroutine td05ad(unitf, output, np1, mp1, w, a, b, valr, &
                       vali, info)
            character, intent(inout)          :: unitf
            character, intent(inout)          :: output
            integer, intent(inout)            :: np1
            integer, intent(inout)            :: mp1
            double precision, intent(inout)   :: w
            double precision, intent(inout)   :: a(*)
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: valr
            double precision, intent(inout)   :: vali
            integer, intent(inout)            :: info
        end subroutine td05ad
    end interface
    public :: td05ad
    
    interface 
        subroutine tf01md(n, m, p, ny, a, lda, b, ldb, &
                       c, ldc, d, ldd, u, ldu, x, y, &
                       ldy, dwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ny
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine tf01md
    end interface
    public :: tf01md
    
    interface 
        subroutine tf01mx(n, m, p, ny, s, lds, u, ldu, &
                       x, y, ldy, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ny
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(inout)            :: lds
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tf01mx
    end interface
    public :: tf01mx
    
    interface 
        subroutine tf01my(n, m, p, ny, a, lda, b, ldb, &
                       c, ldc, d, ldd, u, ldu, x, y, &
                       ldy, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ny
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tf01my
    end interface
    public :: tf01my
    
    interface 
        subroutine tf01nd(uplo, n, m, p, ny, a, lda, b, &
                       ldb, c, ldc, d, ldd, u, ldu, x, &
                       y, ldy, dwork, info)
            character, intent(inout)          :: uplo
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ny
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(inout)            :: ldu
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(inout)            :: ldy
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine tf01nd
    end interface
    public :: tf01nd
    
    interface 
        subroutine tf01od(nh1, nh2, nr, nc, h, ldh, t, ldt, &
                       info)
            integer, intent(inout)            :: nh1
            integer, intent(inout)            :: nh2
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: nc
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            integer, intent(inout)            :: info
        end subroutine tf01od
    end interface
    public :: tf01od
    
    interface 
        subroutine tf01pd(nh1, nh2, nr, nc, h, ldh, t, ldt, &
                       info)
            integer, intent(inout)            :: nh1
            integer, intent(inout)            :: nh2
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: nc
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(inout)            :: ldt
            integer, intent(inout)            :: info
        end subroutine tf01pd
    end interface
    public :: tf01pd
    
    interface 
        subroutine tf01qd(nc, nb, n, iord, ar, ma, h, ldh, &
                       info)
            integer, intent(inout)            :: nc
            integer, intent(inout)            :: nb
            integer, intent(inout)            :: n
            integer, intent(inout)            :: iord(*)
            double precision, intent(inout)   :: ar(*)
            double precision, intent(inout)   :: ma(*)
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            integer, intent(inout)            :: info
        end subroutine tf01qd
    end interface
    public :: tf01qd
    
    interface 
        subroutine tf01rd(na, nb, nc, n, a, lda, b, ldb, &
                       c, ldc, h, ldh, dwork, ldwork, info)
            integer, intent(inout)            :: na
            integer, intent(inout)            :: nb
            integer, intent(inout)            :: nc
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(inout)            :: ldh
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tf01rd
    end interface
    public :: tf01rd
    
    interface 
        subroutine tg01ad(job, l, n, m, p, thresh, a, lda, &
                       e, lde, b, ldb, c, ldc, lscale, rscale, &
                       dwork, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: l
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: thresh
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: lscale(*)
            double precision, intent(inout)   :: rscale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine tg01ad
    end interface
    public :: tg01ad
    
    interface 
        subroutine tg01az(job, l, n, m, p, thresh, a, lda, &
                       e, lde, b, ldb, c, ldc, lscale, rscale, &
                       dwork, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: l
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: thresh
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(inout)            :: lde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: lscale(*)
            double precision, intent(inout)   :: rscale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine tg01az
    end interface
    public :: tg01az
    
    interface 
        subroutine tg01bd(jobe, compq, compz, n, m, p, ilo, ihi, &
                       a, lda, e, lde, b, ldb, c, ldc, &
                       q, ldq, z, ldz, dwork, ldwork, info)
            character, intent(inout)          :: jobe
            character, intent(inout)          :: compq
            character, intent(inout)          :: compz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ilo
            integer, intent(inout)            :: ihi
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01bd
    end interface
    public :: tg01bd
    
    interface 
        subroutine tg01cd(compq, l, n, m, a, lda, e, lde, &
                       b, ldb, q, ldq, dwork, ldwork, info)
            character, intent(inout)          :: compq
            integer, intent(inout)            :: l
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01cd
    end interface
    public :: tg01cd
    
    interface 
        subroutine tg01dd(compz, l, n, p, a, lda, e, lde, &
                       c, ldc, z, ldz, dwork, ldwork, info)
            character, intent(inout)          :: compz
            integer, intent(inout)            :: l
            integer, intent(inout)            :: n
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01dd
    end interface
    public :: tg01dd
    
    interface 
        subroutine tg01ed(joba, l, n, m, p, a, lda, e, &
                       lde, b, ldb, c, ldc, q, ldq, z, &
                       ldz, ranke, rnka22, tol, dwork, ldwork, info)
            character, intent(inout)          :: joba
            integer, intent(inout)            :: l
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: ranke
            integer, intent(inout)            :: rnka22
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01ed
    end interface
    public :: tg01ed
    
    interface 
        subroutine tg01fd(compq, compz, joba, l, n, m, p, a, &
                       lda, e, lde, b, ldb, c, ldc, q, &
                       ldq, z, ldz, ranke, rnka22, tol, iwork, dwork, &
                       ldwork, info)
            character, intent(inout)          :: compq
            character, intent(inout)          :: compz
            character, intent(inout)          :: joba
            integer, intent(inout)            :: l
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: ranke
            integer, intent(inout)            :: rnka22
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01fd
    end interface
    public :: tg01fd
    
    interface 
        subroutine tg01fz(compq, compz, joba, l, n, m, p, a, &
                       lda, e, lde, b, ldb, c, ldc, q, &
                       ldq, z, ldz, ranke, rnka22, tol, iwork, dwork, &
                       zwork, lzwork, info)
            character, intent(inout)          :: compq
            character, intent(inout)          :: compz
            character, intent(inout)          :: joba
            integer, intent(inout)            :: l
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(inout)            :: lda
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(inout)            :: lde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(inout)            :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(inout)            :: ldc
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(inout)            :: ldq
            complex*16, intent(inout)         :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: ranke
            integer, intent(inout)            :: rnka22
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine tg01fz
    end interface
    public :: tg01fz
    
    interface 
        subroutine tg01gd(jobs, l, n, m, p, a, lda, e, &
                       lde, b, ldb, c, ldc, d, ldd, lr, &
                       nr, ranke, infred, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: jobs
            integer, intent(inout)            :: l
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(inout)            :: ldd
            integer, intent(inout)            :: lr
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: ranke
            integer, intent(inout)            :: infred
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01gd
    end interface
    public :: tg01gd
    
    interface 
        subroutine tg01hd(jobcon, compq, compz, n, m, p, a, lda, &
                       e, lde, b, ldb, c, ldc, q, ldq, &
                       z, ldz, ncont, niucon, nrblck, rtau, tol, iwork, &
                       dwork, info)
            character, intent(inout)          :: jobcon
            character, intent(inout)          :: compq
            character, intent(inout)          :: compz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: ncont
            integer, intent(inout)            :: niucon
            integer, intent(inout)            :: nrblck
            integer, intent(inout)            :: rtau(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine tg01hd
    end interface
    public :: tg01hd
    
    interface 
        subroutine tg01hu(compq, compz, l, n, m1, m2, p, n1, &
                       lbe, a, lda, e, lde, b, ldb, c, &
                       ldc, q, ldq, z, ldz, nr, nrblck, rtau, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: compq
            character, intent(inout)          :: compz
            integer, intent(inout)            :: l
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m1
            integer, intent(inout)            :: m2
            integer, intent(inout)            :: p
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: lbe
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: nrblck
            integer, intent(inout)            :: rtau(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01hu
    end interface
    public :: tg01hu
    
    interface 
        subroutine tg01hx(compq, compz, l, n, m, p, n1, lbe, &
                       a, lda, e, lde, b, ldb, c, ldc, &
                       q, ldq, z, ldz, nr, nrblck, rtau, tol, &
                       iwork, dwork, info)
            character, intent(inout)          :: compq
            character, intent(inout)          :: compz
            integer, intent(inout)            :: l
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: lbe
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: nrblck
            integer, intent(inout)            :: rtau(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine tg01hx
    end interface
    public :: tg01hx
    
    interface 
        subroutine tg01hy(compq, compz, l, n, m, p, n1, lbe, &
                       a, lda, e, lde, b, ldb, c, ldc, &
                       q, ldq, z, ldz, nr, nrblck, rtau, tol, &
                       iwork, dwork, ldwork, info)
            character, intent(inout)          :: compq
            character, intent(inout)          :: compz
            integer, intent(inout)            :: l
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: lbe
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: nrblck
            integer, intent(inout)            :: rtau(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01hy
    end interface
    public :: tg01hy
    
    interface 
        subroutine tg01id(jobobs, compq, compz, n, m, p, a, lda, &
                       e, lde, b, ldb, c, ldc, q, ldq, &
                       z, ldz, nobsv, niuobs, nlblck, ctau, tol, iwork, &
                       dwork, info)
            character, intent(inout)          :: jobobs
            character, intent(inout)          :: compq
            character, intent(inout)          :: compz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: nobsv
            integer, intent(inout)            :: niuobs
            integer, intent(inout)            :: nlblck
            integer, intent(inout)            :: ctau(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: info
        end subroutine tg01id
    end interface
    public :: tg01id
    
    interface 
        subroutine tg01jd(job, systyp, equil, n, m, p, a, lda, &
                       e, lde, b, ldb, c, ldc, nr, infred, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: systyp
            character, intent(inout)          :: equil
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: infred(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01jd
    end interface
    public :: tg01jd
    
    interface 
        subroutine tg01jy(job, systyp, equil, cksing, restor, n, m, p, &
                       a, lda, e, lde, b, ldb, c, ldc, &
                       nr, infred, tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: systyp
            character, intent(inout)          :: equil
            character, intent(inout)          :: cksing
            character, intent(inout)          :: restor
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: nr
            integer, intent(inout)            :: infred(*)
            double precision, intent(inout)   :: tol(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01jy
    end interface
    public :: tg01jy
    
    interface 
        subroutine tg01kd(jobe, compc, compq, compz, n, a, lda, e, &
                       lde, b, c, incc, q, ldq, z, ldz, &
                       info)
            character, intent(inout)          :: jobe
            character, intent(inout)          :: compc
            character, intent(inout)          :: compq
            character, intent(inout)          :: compz
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: c(*)
            integer, intent(inout)            :: incc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: info
        end subroutine tg01kd
    end interface
    public :: tg01kd
    
    interface 
        subroutine tg01kz(jobe, compc, compq, compz, n, a, lda, e, &
                       lde, b, c, incc, q, ldq, z, ldz, &
                       info)
            character, intent(inout)    :: jobe
            character, intent(inout)    :: compc
            character, intent(inout)    :: compq
            character, intent(inout)    :: compz
            integer, intent(inout)      :: n
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(inout)      :: lda
            complex*16, intent(inout)   :: e(lde, *)
            integer, intent(inout)      :: lde
            complex*16, intent(inout)   :: b(*)
            complex*16, intent(inout)   :: c(*)
            integer, intent(inout)      :: incc
            complex*16, intent(inout)   :: q(ldq, *)
            integer, intent(inout)      :: ldq
            complex*16, intent(inout)   :: z(ldz, *)
            integer, intent(inout)      :: ldz
            integer, intent(inout)      :: info
        end subroutine tg01kz
    end interface
    public :: tg01kz
    
    interface 
        subroutine tg01ld(job, joba, compq, compz, n, m, p, a, &
                       lda, e, lde, b, ldb, c, ldc, q, &
                       ldq, z, ldz, nf, nd, niblck, iblck, tol, &
                       iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: joba
            character, intent(inout)          :: compq
            character, intent(inout)          :: compz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: nf
            integer, intent(inout)            :: nd
            integer, intent(inout)            :: niblck
            integer, intent(inout)            :: iblck(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01ld
    end interface
    public :: tg01ld
    
    interface 
        subroutine tg01ly(compq, compz, n, m, p, ranke, rnka22, a, &
                       lda, e, lde, b, ldb, c, ldc, q, &
                       ldq, z, ldz, nf, niblck, iblck, tol, iwork, &
                       dwork, ldwork, info)
            logical, intent(inout)            :: compq
            logical, intent(inout)            :: compz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ranke
            integer, intent(inout)            :: rnka22
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: nf
            integer, intent(inout)            :: niblck
            integer, intent(inout)            :: iblck(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01ly
    end interface
    public :: tg01ly
    
    interface 
        subroutine tg01md(job, n, m, p, a, lda, e, lde, &
                       b, ldb, c, ldc, alphar, alphai, beta, q, &
                       ldq, z, ldz, nf, nd, niblck, iblck, tol, &
                       iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: nf
            integer, intent(inout)            :: nd
            integer, intent(inout)            :: niblck
            integer, intent(inout)            :: iblck(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01md
    end interface
    public :: tg01md
    
    interface 
        subroutine tg01nd(job, jobt, n, m, p, a, lda, e, &
                       lde, b, ldb, c, ldc, alphar, alphai, beta, &
                       q, ldq, z, ldz, nf, nd, niblck, iblck, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(inout)          :: job
            character, intent(inout)          :: jobt
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: nf
            integer, intent(inout)            :: nd
            integer, intent(inout)            :: niblck
            integer, intent(inout)            :: iblck(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01nd
    end interface
    public :: tg01nd
    
    interface 
        subroutine tg01nx(jobt, n, m, p, ndim, a, lda, e, &
                       lde, b, ldb, c, ldc, q, ldq, z, &
                       ldz, iwork, info)
            character, intent(inout)          :: jobt
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: ndim
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: iwork(*)
            integer, intent(inout)            :: info
        end subroutine tg01nx
    end interface
    public :: tg01nx
    
    interface 
        subroutine tg01oa(jobe, n, dcba, lddcba, e, lde, info)
            character, intent(inout)          :: jobe
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: dcba(lddcba, *)
            integer, intent(inout)            :: lddcba
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            integer, intent(inout)            :: info
        end subroutine tg01oa
    end interface
    public :: tg01oa
    
    interface 
        subroutine tg01ob(jobe, n, dcba, lddcba, e, lde, info)
            character, intent(inout)    :: jobe
            integer, intent(inout)      :: n
            complex*16, intent(inout)   :: dcba(lddcba, *)
            integer, intent(inout)      :: lddcba
            complex*16, intent(inout)   :: e(lde, *)
            integer, intent(inout)      :: lde
            integer, intent(inout)      :: info
        end subroutine tg01ob
    end interface
    public :: tg01ob
    
    interface 
        subroutine tg01od(jobe, n, dcba, lddcba, e, lde, nz, g, &
                       tol, dwork, ldwork, info)
            character, intent(inout)          :: jobe
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: dcba(lddcba, *)
            integer, intent(inout)            :: lddcba
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            integer, intent(inout)            :: nz
            double precision, intent(inout)   :: g
            double precision, intent(inout)   :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01od
    end interface
    public :: tg01od
    
    interface 
        subroutine tg01oz(jobe, n, dcba, lddcba, e, lde, nz, g, &
                       tol, zwork, lzwork, info)
            character, intent(inout)          :: jobe
            integer, intent(inout)            :: n
            complex*16, intent(inout)         :: dcba(lddcba, *)
            integer, intent(inout)            :: lddcba
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(inout)            :: lde
            integer, intent(inout)            :: nz
            complex*16, intent(inout)         :: g
            double precision, intent(inout)   :: tol
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(inout)            :: lzwork
            integer, intent(inout)            :: info
        end subroutine tg01oz
    end interface
    public :: tg01oz
    
    interface 
        subroutine tg01pd(dico, stdom, jobae, compq, compz, n, m, p, &
                       nlow, nsup, alpha, a, lda, e, lde, b, &
                       ldb, c, ldc, q, ldq, z, ldz, ndim, &
                       alphar, alphai, beta, dwork, ldwork, info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: stdom
            character, intent(inout)          :: jobae
            character, intent(inout)          :: compq
            character, intent(inout)          :: compz
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            integer, intent(inout)            :: nlow
            integer, intent(inout)            :: nsup
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            integer, intent(inout)            :: ndim
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01pd
    end interface
    public :: tg01pd
    
    interface 
        subroutine tg01qd(dico, stdom, jobfi, n, m, p, alpha, a, &
                       lda, e, lde, b, ldb, c, ldc, n1, &
                       n2, n3, nd, niblck, iblck, q, ldq, z, &
                       ldz, alphar, alphai, beta, tol, iwork, dwork, ldwork, &
                       info)
            character, intent(inout)          :: dico
            character, intent(inout)          :: stdom
            character, intent(inout)          :: jobfi
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: n2
            integer, intent(inout)            :: n3
            integer, intent(inout)            :: nd
            integer, intent(inout)            :: niblck
            integer, intent(inout)            :: iblck(*)
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            double precision, intent(inout)   :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01qd
    end interface
    public :: tg01qd
    
    interface 
        subroutine tg01wd(n, m, p, a, lda, e, lde, b, &
                       ldb, c, ldc, q, ldq, z, ldz, alphar, &
                       alphai, beta, dwork, ldwork, info)
            integer, intent(inout)            :: n
            integer, intent(inout)            :: m
            integer, intent(inout)            :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(inout)            :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(inout)            :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(inout)            :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(inout)            :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(inout)            :: ldz
            double precision, intent(inout)   :: alphar(*)
            double precision, intent(inout)   :: alphai(*)
            double precision, intent(inout)   :: beta(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(inout)            :: ldwork
            integer, intent(inout)            :: info
        end subroutine tg01wd
    end interface
    public :: tg01wd
    
    interface 
        subroutine ud01bd(mp, np, dp, nin, p, ldp1, ldp2, info)
            integer, intent(inout)            :: mp
            integer, intent(inout)            :: np
            integer, intent(inout)            :: dp
            integer, intent(inout)            :: nin
            double precision, intent(inout)   :: p(ldp1, ldp2, *)
            integer, intent(inout)            :: ldp1
            integer, intent(inout)            :: ldp2
            integer, intent(inout)            :: info
        end subroutine ud01bd
    end interface
    public :: ud01bd
    
    interface 
        subroutine ud01cd(mp, np, dp, nin, p, ldp1, ldp2, info)
            integer, intent(inout)            :: mp
            integer, intent(inout)            :: np
            integer, intent(inout)            :: dp
            integer, intent(inout)            :: nin
            double precision, intent(inout)   :: p(ldp1, ldp2, *)
            integer, intent(inout)            :: ldp1
            integer, intent(inout)            :: ldp2
            integer, intent(inout)            :: info
        end subroutine ud01cd
    end interface
    public :: ud01cd
    
    interface 
        subroutine ud01dd(m, n, nin, a, lda, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: nin
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            integer, intent(inout)            :: info
        end subroutine ud01dd
    end interface
    public :: ud01dd
    
    interface 
        subroutine ud01md(m, n, l, nout, a, lda, text, info)
            integer, intent(inout)            :: m
            integer, intent(inout)            :: n
            integer, intent(inout)            :: l
            integer, intent(inout)            :: nout
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(inout)            :: lda
            character*(*), intent(inout)      :: text
            integer, intent(inout)            :: info
        end subroutine ud01md
    end interface
    public :: ud01md
    
    interface 
        subroutine ud01mz(m, n, l, nout, a, lda, text, info)
            integer, intent(inout)         :: m
            integer, intent(inout)         :: n
            integer, intent(inout)         :: l
            integer, intent(inout)         :: nout
            complex*16, intent(inout)      :: a(lda, *)
            integer, intent(inout)         :: lda
            character*(*), intent(inout)   :: text
            integer, intent(inout)         :: info
        end subroutine ud01mz
    end interface
    public :: ud01mz
    
    interface 
        subroutine ud01nd(mp, np, dp, l, nout, p, ldp1, ldp2, &
                       text, info)
            integer, intent(inout)            :: mp
            integer, intent(inout)            :: np
            integer, intent(inout)            :: dp
            integer, intent(inout)            :: l
            integer, intent(inout)            :: nout
            double precision, intent(inout)   :: p(ldp1, ldp2, *)
            integer, intent(inout)            :: ldp1
            integer, intent(inout)            :: ldp2
            character*(*), intent(inout)      :: text
            integer, intent(inout)            :: info
        end subroutine ud01nd
    end interface
    public :: ud01nd
    
    interface 
        integer function ue01md (ispec,name,opts,n1,n2,n3)
            integer, intent(inout)         :: ispec
            character*(*), intent(inout)   :: name
            character*(*), intent(inout)   :: opts
            integer, intent(inout)         :: n1
            integer, intent(inout)         :: n2
            integer, intent(inout)         :: n3
        end function ue01md
    end interface
    public :: ue01md
    
    interface 
        logical function zelctg (par1,par2)
            complex*16, intent(inout)   :: par1
            complex*16, intent(inout)   :: par2
        end function zelctg
    end interface
    public :: zelctg
    
end module slicot