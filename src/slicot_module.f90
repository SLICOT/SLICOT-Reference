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
            character, intent(in)             :: jobz
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(*)
            integer, intent(out)              :: ncont
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(out)     :: tau(*)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab01md
    end interface
    public :: ab01md
    
    interface
        subroutine ab01nd(jobz, n, m, a, lda, b, ldb, ncont, &
                       indcon, nblk, z, ldz, tau, tol, iwork, dwork, &
                       ldwork, info)
            character, intent(in)             :: jobz
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            integer, intent(out)              :: ncont
            integer, intent(out)              :: indcon
            integer, intent(out)              :: nblk(*)
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(out)     :: tau(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab01nd
    end interface
    public :: ab01nd
    
    interface
        subroutine ab01od(stages, jobu, jobv, n, m, a, lda, b, &
                       ldb, u, ldu, v, ldv, ncont, indcon, kstair, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: stages
            character, intent(in)             :: jobu
            character, intent(in)             :: jobv
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: v(ldv, *)
            integer, intent(in)               :: ldv
            integer, intent(inout)            :: ncont
            integer, intent(inout)            :: indcon
            integer, intent(inout)            :: kstair(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab01od
    end interface
    public :: ab01od
    
    interface
        subroutine ab04md(type, n, m, p, alpha, beta, a, lda, &
                       b, ldb, c, ldc, d, ldd, iwork, dwork, &
                       ldwork, info)
            character, intent(in)             :: type
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab04md
    end interface
    public :: ab04md
    
    interface
        subroutine ab05md(uplo, over, n1, m1, p1, n2, p2, a1, &
                       lda1, b1, ldb1, c1, ldc1, d1, ldd1, a2, &
                       lda2, b2, ldb2, c2, ldc2, d2, ldd2, n, &
                       a, lda, b, ldb, c, ldc, d, ldd, &
                       dwork, ldwork, info)
            character, intent(in)             :: uplo
            character, intent(in)             :: over
            integer, intent(in)               :: n1
            integer, intent(in)               :: m1
            integer, intent(in)               :: p1
            integer, intent(in)               :: n2
            integer, intent(in)               :: p2
            double precision, intent(in)      :: a1(lda1, *)
            integer, intent(in)               :: lda1
            double precision, intent(in)      :: b1(ldb1, *)
            integer, intent(in)               :: ldb1
            double precision, intent(in)      :: c1(ldc1, *)
            integer, intent(in)               :: ldc1
            double precision, intent(in)      :: d1(ldd1, *)
            integer, intent(in)               :: ldd1
            double precision, intent(in)      :: a2(lda2, *)
            integer, intent(in)               :: lda2
            double precision, intent(in)      :: b2(ldb2, *)
            integer, intent(in)               :: ldb2
            double precision, intent(in)      :: c2(ldc2, *)
            integer, intent(in)               :: ldc2
            double precision, intent(in)      :: d2(ldd2, *)
            integer, intent(in)               :: ldd2
            integer, intent(out)              :: n
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab05md
    end interface
    public :: ab05md
    
    interface
        subroutine ab05nd(over, n1, m1, p1, n2, alpha, a1, lda1, &
                       b1, ldb1, c1, ldc1, d1, ldd1, a2, lda2, &
                       b2, ldb2, c2, ldc2, d2, ldd2, n, a, &
                       lda, b, ldb, c, ldc, d, ldd, iwork, &
                       dwork, ldwork, info)
            character, intent(in)             :: over
            integer, intent(in)               :: n1
            integer, intent(in)               :: m1
            integer, intent(in)               :: p1
            integer, intent(in)               :: n2
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: a1(lda1, *)
            integer, intent(in)               :: lda1
            double precision, intent(in)      :: b1(ldb1, *)
            integer, intent(in)               :: ldb1
            double precision, intent(in)      :: c1(ldc1, *)
            integer, intent(in)               :: ldc1
            double precision, intent(in)      :: d1(ldd1, *)
            integer, intent(in)               :: ldd1
            double precision, intent(in)      :: a2(lda2, *)
            integer, intent(in)               :: lda2
            double precision, intent(in)      :: b2(ldb2, *)
            integer, intent(in)               :: ldb2
            double precision, intent(in)      :: c2(ldc2, *)
            integer, intent(in)               :: ldc2
            double precision, intent(in)      :: d2(ldd2, *)
            integer, intent(in)               :: ldd2
            integer, intent(out)              :: n
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab05nd
    end interface
    public :: ab05nd
    
    interface
        subroutine ab05od(over, n1, m1, p1, n2, m2, alpha, a1, &
                       lda1, b1, ldb1, c1, ldc1, d1, ldd1, a2, &
                       lda2, b2, ldb2, c2, ldc2, d2, ldd2, n, &
                       m, a, lda, b, ldb, c, ldc, d, &
                       ldd, info)
            character, intent(in)           :: over
            integer, intent(in)             :: n1
            integer, intent(in)             :: m1
            integer, intent(in)             :: p1
            integer, intent(in)             :: n2
            integer, intent(in)             :: m2
            double precision, intent(in)    :: alpha
            double precision, intent(in)    :: a1(lda1, *)
            integer, intent(in)             :: lda1
            double precision, intent(in)    :: b1(ldb1, *)
            integer, intent(in)             :: ldb1
            double precision, intent(in)    :: c1(ldc1, *)
            integer, intent(in)             :: ldc1
            double precision, intent(in)    :: d1(ldd1, *)
            integer, intent(in)             :: ldd1
            double precision, intent(in)    :: a2(lda2, *)
            integer, intent(in)             :: lda2
            double precision, intent(in)    :: b2(ldb2, *)
            integer, intent(in)             :: ldb2
            double precision, intent(in)    :: c2(ldc2, *)
            integer, intent(in)             :: ldc2
            double precision, intent(in)    :: d2(ldd2, *)
            integer, intent(in)             :: ldd2
            integer, intent(out)            :: n
            integer, intent(out)            :: m
            double precision, intent(out)   :: a(lda, *)
            integer, intent(in)             :: lda
            double precision, intent(out)   :: b(ldb, *)
            integer, intent(in)             :: ldb
            double precision, intent(out)   :: c(ldc, *)
            integer, intent(in)             :: ldc
            double precision, intent(out)   :: d(ldd, *)
            integer, intent(in)             :: ldd
            integer, intent(out)            :: info
        end subroutine ab05od
    end interface
    public :: ab05od
    
    interface
        subroutine ab05pd(over, n1, m, p, n2, alpha, a1, lda1, &
                       b1, ldb1, c1, ldc1, d1, ldd1, a2, lda2, &
                       b2, ldb2, c2, ldc2, d2, ldd2, n, a, &
                       lda, b, ldb, c, ldc, d, ldd, info)
            character, intent(in)           :: over
            integer, intent(in)             :: n1
            integer, intent(in)             :: m
            integer, intent(in)             :: p
            integer, intent(in)             :: n2
            double precision, intent(in)    :: alpha
            double precision, intent(in)    :: a1(lda1, *)
            integer, intent(in)             :: lda1
            double precision, intent(in)    :: b1(ldb1, *)
            integer, intent(in)             :: ldb1
            double precision, intent(in)    :: c1(ldc1, *)
            integer, intent(in)             :: ldc1
            double precision, intent(in)    :: d1(ldd1, *)
            integer, intent(in)             :: ldd1
            double precision, intent(in)    :: a2(lda2, *)
            integer, intent(in)             :: lda2
            double precision, intent(in)    :: b2(ldb2, *)
            integer, intent(in)             :: ldb2
            double precision, intent(in)    :: c2(ldc2, *)
            integer, intent(in)             :: ldc2
            double precision, intent(in)    :: d2(ldd2, *)
            integer, intent(in)             :: ldd2
            integer, intent(out)            :: n
            double precision, intent(out)   :: a(lda, *)
            integer, intent(in)             :: lda
            double precision, intent(out)   :: b(ldb, *)
            integer, intent(in)             :: ldb
            double precision, intent(out)   :: c(ldc, *)
            integer, intent(in)             :: ldc
            double precision, intent(out)   :: d(ldd, *)
            integer, intent(in)             :: ldd
            integer, intent(out)            :: info
        end subroutine ab05pd
    end interface
    public :: ab05pd
    
    interface
        subroutine ab05qd(over, n1, m1, p1, n2, m2, p2, a1, &
                       lda1, b1, ldb1, c1, ldc1, d1, ldd1, a2, &
                       lda2, b2, ldb2, c2, ldc2, d2, ldd2, n, &
                       m, p, a, lda, b, ldb, c, ldc, &
                       d, ldd, info)
            character, intent(in)           :: over
            integer, intent(in)             :: n1
            integer, intent(in)             :: m1
            integer, intent(in)             :: p1
            integer, intent(in)             :: n2
            integer, intent(in)             :: m2
            integer, intent(in)             :: p2
            double precision, intent(in)    :: a1(lda1, *)
            integer, intent(in)             :: lda1
            double precision, intent(in)    :: b1(ldb1, *)
            integer, intent(in)             :: ldb1
            double precision, intent(in)    :: c1(ldc1, *)
            integer, intent(in)             :: ldc1
            double precision, intent(in)    :: d1(ldd1, *)
            integer, intent(in)             :: ldd1
            double precision, intent(in)    :: a2(lda2, *)
            integer, intent(in)             :: lda2
            double precision, intent(in)    :: b2(ldb2, *)
            integer, intent(in)             :: ldb2
            double precision, intent(in)    :: c2(ldc2, *)
            integer, intent(in)             :: ldc2
            double precision, intent(in)    :: d2(ldd2, *)
            integer, intent(in)             :: ldd2
            integer, intent(out)            :: n
            integer, intent(out)            :: m
            integer, intent(out)            :: p
            double precision, intent(out)   :: a(lda, *)
            integer, intent(in)             :: lda
            double precision, intent(out)   :: b(ldb, *)
            integer, intent(in)             :: ldb
            double precision, intent(out)   :: c(ldc, *)
            integer, intent(in)             :: ldc
            double precision, intent(out)   :: d(ldd, *)
            integer, intent(in)             :: ldd
            integer, intent(out)            :: info
        end subroutine ab05qd
    end interface
    public :: ab05qd
    
    interface
        subroutine ab05rd(fbtype, jobd, n, m, p, mv, pz, alpha, &
                       beta, a, lda, b, ldb, c, ldc, d, &
                       ldd, f, ldf, k, ldk, g, ldg, h, &
                       ldh, rcond, bc, ldbc, cc, ldcc, dc, lddc, &
                       iwork, dwork, ldwork, info)
            character, intent(in)             :: fbtype
            character, intent(in)             :: jobd
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: mv
            integer, intent(in)               :: pz
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(in)      :: k(ldk, *)
            integer, intent(in)               :: ldk
            double precision, intent(in)      :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(in)      :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(out)     :: rcond
            double precision, intent(out)     :: bc(ldbc, *)
            integer, intent(in)               :: ldbc
            double precision, intent(out)     :: cc(ldcc, *)
            integer, intent(in)               :: ldcc
            double precision, intent(out)     :: dc(lddc, *)
            integer, intent(in)               :: lddc
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab05rd
    end interface
    public :: ab05rd
    
    interface
        subroutine ab05sd(fbtype, jobd, n, m, p, alpha, a, lda, &
                       b, ldb, c, ldc, d, ldd, f, ldf, &
                       rcond, iwork, dwork, ldwork, info)
            character, intent(in)             :: fbtype
            character, intent(in)             :: jobd
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(out)     :: rcond
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab05sd
    end interface
    public :: ab05sd
    
    interface
        subroutine ab07md(jobd, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, info)
            character, intent(in)             :: jobd
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: info
        end subroutine ab07md
    end interface
    public :: ab07md
    
    interface
        subroutine ab07nd(n, m, a, lda, b, ldb, c, ldc, &
                       d, ldd, rcond, iwork, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: rcond
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab07nd
    end interface
    public :: ab07nd
    
    interface
        subroutine ab08md(equil, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, rank, tol, iwork, dwork, &
                       ldwork, info)
            character, intent(in)             :: equil
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: rank
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab08md
    end interface
    public :: ab08md
    
    interface
        subroutine ab08mz(equil, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, rank, tol, iwork, dwork, &
                       zwork, lzwork, info)
            character, intent(in)             :: equil
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            complex*16, intent(in)            :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(in)            :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(in)            :: c(ldc, *)
            integer, intent(in)               :: ldc
            complex*16, intent(in)            :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: rank
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine ab08mz
    end interface
    public :: ab08mz
    
    interface
        subroutine ab08nd(equil, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, nu, rank, dinfz, nkror, &
                       nkrol, infz, kronr, kronl, af, ldaf, bf, ldbf, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: equil
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nu
            integer, intent(out)              :: rank
            integer, intent(out)              :: dinfz
            integer, intent(out)              :: nkror
            integer, intent(out)              :: nkrol
            integer, intent(out)              :: infz(*)
            integer, intent(out)              :: kronr(*)
            integer, intent(out)              :: kronl(*)
            double precision, intent(out)     :: af(ldaf, *)
            integer, intent(in)               :: ldaf
            double precision, intent(out)     :: bf(ldbf, *)
            integer, intent(in)               :: ldbf
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab08nd
    end interface
    public :: ab08nd
    
    interface
        subroutine ab08nw(equil, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, nfz, nrank, niz, dinfz, &
                       nkror, ninfe, nkrol, infz, kronr, infe, kronl, e, &
                       lde, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: equil
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nfz
            integer, intent(out)              :: nrank
            integer, intent(out)              :: niz
            integer, intent(out)              :: dinfz
            integer, intent(out)              :: nkror
            integer, intent(out)              :: ninfe
            integer, intent(out)              :: nkrol
            integer, intent(out)              :: infz(*)
            integer, intent(out)              :: kronr(*)
            integer, intent(out)              :: infe(*)
            integer, intent(out)              :: kronl(*)
            double precision, intent(out)     :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab08nw
    end interface
    public :: ab08nw
    
    interface
        subroutine ab08nx(n, m, p, ro, sigma, svlmax, abcd, ldabcd, &
                       ninfz, infz, kronl, mu, nu, nkrol, tol, iwork, &
                       dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: ro
            integer, intent(inout)            :: sigma
            double precision, intent(in)      :: svlmax
            double precision, intent(inout)   :: abcd(ldabcd, *)
            integer, intent(in)               :: ldabcd
            integer, intent(inout)            :: ninfz
            integer, intent(inout)            :: infz(*)
            integer, intent(inout)            :: kronl(*)
            integer, intent(out)              :: mu
            integer, intent(out)              :: nu
            integer, intent(out)              :: nkrol
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab08nx
    end interface
    public :: ab08nx
    
    interface
        subroutine ab08ny(first, n, m, p, svlmax, abcd, ldabcd, ninfz, &
                       nr, pr, dinfz, nkronl, infz, kronl, tol, iwork, &
                       dwork, ldwork, info)
            logical, intent(in)               :: first
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: svlmax
            double precision, intent(inout)   :: abcd(ldabcd, *)
            integer, intent(in)               :: ldabcd
            integer, intent(inout)            :: ninfz
            integer, intent(out)              :: nr
            integer, intent(out)              :: pr
            integer, intent(out)              :: dinfz
            integer, intent(out)              :: nkronl
            integer, intent(out)              :: infz(*)
            integer, intent(out)              :: kronl(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab08ny
    end interface
    public :: ab08ny
    
    interface
        subroutine ab08nz(equil, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, nu, rank, dinfz, nkror, &
                       nkrol, infz, kronr, kronl, af, ldaf, bf, ldbf, &
                       tol, iwork, dwork, zwork, lzwork, info)
            character, intent(in)             :: equil
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            complex*16, intent(in)            :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(in)            :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(in)            :: c(ldc, *)
            integer, intent(in)               :: ldc
            complex*16, intent(in)            :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nu
            integer, intent(out)              :: rank
            integer, intent(out)              :: dinfz
            integer, intent(out)              :: nkror
            integer, intent(out)              :: nkrol
            integer, intent(out)              :: infz(*)
            integer, intent(out)              :: kronr(*)
            integer, intent(out)              :: kronl(*)
            complex*16, intent(out)           :: af(ldaf, *)
            integer, intent(in)               :: ldaf
            complex*16, intent(out)           :: bf(ldbf, *)
            integer, intent(in)               :: ldbf
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine ab08nz
    end interface
    public :: ab08nz
    
    interface
        subroutine ab09ad(dico, job, equil, ordsel, n, m, p, nr, &
                       a, lda, b, ldb, c, ldc, hsv, tol, &
                       iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: job
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09ad
    end interface
    public :: ab09ad
    
    interface
        subroutine ab09ax(dico, job, ordsel, n, m, p, nr, a, &
                       lda, b, ldb, c, ldc, hsv, t, ldt, &
                       ti, ldti, tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: job
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: hsv(*)
            double precision, intent(out)     :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(out)     :: ti(ldti, *)
            integer, intent(in)               :: ldti
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09ax
    end interface
    public :: ab09ax
    
    interface
        subroutine ab09bd(dico, job, equil, ordsel, n, m, p, nr, &
                       a, lda, b, ldb, c, ldc, d, ldd, &
                       hsv, tol1, tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: job
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09bd
    end interface
    public :: ab09bd
    
    interface
        subroutine ab09bx(dico, job, ordsel, n, m, p, nr, a, &
                       lda, b, ldb, c, ldc, d, ldd, hsv, &
                       t, ldt, ti, ldti, tol1, tol2, iwork, dwork, &
                       ldwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: job
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: hsv(*)
            double precision, intent(out)     :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(out)     :: ti(ldti, *)
            integer, intent(in)               :: ldti
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09bx
    end interface
    public :: ab09bx
    
    interface
        subroutine ab09cd(dico, equil, ordsel, n, m, p, nr, a, &
                       lda, b, ldb, c, ldc, d, ldd, hsv, &
                       tol1, tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09cd
    end interface
    public :: ab09cd
    
    interface
        subroutine ab09cx(dico, ordsel, n, m, p, nr, a, lda, &
                       b, ldb, c, ldc, d, ldd, hsv, tol1, &
                       tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09cx
    end interface
    public :: ab09cx
    
    interface
        subroutine ab09dd(dico, n, m, p, nr, a, lda, b, &
                       ldb, c, ldc, d, ldd, rcond, iwork, dwork, &
                       info)
            character, intent(in)             :: dico
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: rcond
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine ab09dd
    end interface
    public :: ab09dd
    
    interface
        subroutine ab09ed(dico, equil, ordsel, n, m, p, nr, alpha, &
                       a, lda, b, ldb, c, ldc, d, ldd, &
                       ns, hsv, tol1, tol2, iwork, dwork, ldwork, iwarn, &
                       info)
            character, intent(in)             :: dico
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: ns
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09ed
    end interface
    public :: ab09ed
    
    interface
        subroutine ab09fd(dico, jobcf, fact, jobmr, equil, ordsel, n, m, &
                       p, nr, alpha, a, lda, b, ldb, c, &
                       ldc, nq, hsv, tol1, tol2, iwork, dwork, ldwork, &
                       iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobcf
            character, intent(in)             :: fact
            character, intent(in)             :: jobmr
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: nq
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09fd
    end interface
    public :: ab09fd
    
    interface
        subroutine ab09gd(dico, jobcf, fact, jobmr, equil, ordsel, n, m, &
                       p, nr, alpha, a, lda, b, ldb, c, &
                       ldc, d, ldd, nq, hsv, tol1, tol2, tol3, &
                       iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobcf
            character, intent(in)             :: fact
            character, intent(in)             :: jobmr
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nq
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            double precision, intent(in)      :: tol3
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09gd
    end interface
    public :: ab09gd
    
    interface
        subroutine ab09hd(dico, job, equil, ordsel, n, m, p, nr, &
                       alpha, beta, a, lda, b, ldb, c, ldc, &
                       d, ldd, ns, hsv, tol1, tol2, iwork, dwork, &
                       ldwork, bwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: job
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: ns
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09hd
    end interface
    public :: ab09hd
    
    interface
        subroutine ab09hx(dico, job, ordsel, n, m, p, nr, a, &
                       lda, b, ldb, c, ldc, d, ldd, hsv, &
                       t, ldt, ti, ldti, tol1, tol2, iwork, dwork, &
                       ldwork, bwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: job
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: hsv(*)
            double precision, intent(out)     :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(out)     :: ti(ldti, *)
            integer, intent(in)               :: ldti
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09hx
    end interface
    public :: ab09hx
    
    interface
        subroutine ab09hy(n, m, p, a, lda, b, ldb, c, &
                       ldc, d, ldd, scalec, scaleo, s, lds, r, &
                       ldr, iwork, dwork, ldwork, bwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: scalec
            double precision, intent(out)     :: scaleo
            double precision, intent(out)     :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(out)     :: r(ldr, *)
            integer, intent(in)               :: ldr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
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
            character, intent(in)             :: dico
            character, intent(in)             :: jobc
            character, intent(in)             :: jobo
            character, intent(in)             :: job
            character, intent(in)             :: weight
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: nv
            integer, intent(in)               :: pv
            integer, intent(in)               :: nw
            integer, intent(in)               :: mw
            integer, intent(inout)            :: nr
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: alphac
            double precision, intent(in)      :: alphao
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: av(ldav, *)
            integer, intent(in)               :: ldav
            double precision, intent(inout)   :: bv(ldbv, *)
            integer, intent(in)               :: ldbv
            double precision, intent(inout)   :: cv(ldcv, *)
            integer, intent(in)               :: ldcv
            double precision, intent(in)      :: dv(lddv, *)
            integer, intent(in)               :: lddv
            double precision, intent(inout)   :: aw(ldaw, *)
            integer, intent(in)               :: ldaw
            double precision, intent(inout)   :: bw(ldbw, *)
            integer, intent(in)               :: ldbw
            double precision, intent(inout)   :: cw(ldcw, *)
            integer, intent(in)               :: ldcw
            double precision, intent(in)      :: dw(lddw, *)
            integer, intent(in)               :: lddw
            integer, intent(out)              :: ns
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09id
    end interface
    public :: ab09id
    
    interface
        subroutine ab09ix(dico, job, fact, ordsel, n, m, p, nr, &
                       scalec, scaleo, a, lda, b, ldb, c, ldc, &
                       d, ldd, ti, ldti, t, ldt, nminr, hsv, &
                       tol1, tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: job
            character, intent(in)             :: fact
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(in)      :: scalec
            double precision, intent(in)      :: scaleo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: ti(ldti, *)
            integer, intent(in)               :: ldti
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            integer, intent(out)              :: nminr
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
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
            character, intent(in)             :: dico
            character, intent(in)             :: jobc
            character, intent(in)             :: jobo
            character, intent(in)             :: weight
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: nv
            integer, intent(in)               :: pv
            integer, intent(in)               :: nw
            integer, intent(in)               :: mw
            double precision, intent(in)      :: alphac
            double precision, intent(in)      :: alphao
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: av(ldav, *)
            integer, intent(in)               :: ldav
            double precision, intent(in)      :: bv(ldbv, *)
            integer, intent(in)               :: ldbv
            double precision, intent(in)      :: cv(ldcv, *)
            integer, intent(in)               :: ldcv
            double precision, intent(in)      :: dv(lddv, *)
            integer, intent(in)               :: lddv
            double precision, intent(in)      :: aw(ldaw, *)
            integer, intent(in)               :: ldaw
            double precision, intent(in)      :: bw(ldbw, *)
            integer, intent(in)               :: ldbw
            double precision, intent(in)      :: cw(ldcw, *)
            integer, intent(in)               :: ldcw
            double precision, intent(in)      :: dw(lddw, *)
            integer, intent(in)               :: lddw
            double precision, intent(out)     :: scalec
            double precision, intent(out)     :: scaleo
            double precision, intent(out)     :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(out)     :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
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
            character, intent(in)             :: jobv
            character, intent(in)             :: jobw
            character, intent(in)             :: jobinv
            character, intent(in)             :: dico
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: nv
            integer, intent(in)               :: nw
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: av(ldav, *)
            integer, intent(in)               :: ldav
            double precision, intent(inout)   :: bv(ldbv, *)
            integer, intent(in)               :: ldbv
            double precision, intent(inout)   :: cv(ldcv, *)
            integer, intent(in)               :: ldcv
            double precision, intent(in)      :: dv(lddv, *)
            integer, intent(in)               :: lddv
            double precision, intent(inout)   :: aw(ldaw, *)
            integer, intent(in)               :: ldaw
            double precision, intent(inout)   :: bw(ldbw, *)
            integer, intent(in)               :: ldbw
            double precision, intent(inout)   :: cw(ldcw, *)
            integer, intent(in)               :: ldcw
            double precision, intent(in)      :: dw(lddw, *)
            integer, intent(in)               :: lddw
            integer, intent(out)              :: ns
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09jd
    end interface
    public :: ab09jd
    
    interface
        subroutine ab09jv(job, dico, jobev, stbchk, n, m, p, nv, &
                       pv, a, lda, b, ldb, c, ldc, d, &
                       ldd, av, ldav, ev, ldev, bv, ldbv, cv, &
                       ldcv, dv, lddv, iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: dico
            character, intent(in)             :: jobev
            character, intent(in)             :: stbchk
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: nv
            integer, intent(in)               :: pv
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: av(ldav, *)
            integer, intent(in)               :: ldav
            double precision, intent(inout)   :: ev(ldev, *)
            integer, intent(in)               :: ldev
            double precision, intent(inout)   :: bv(ldbv, *)
            integer, intent(in)               :: ldbv
            double precision, intent(inout)   :: cv(ldcv, *)
            integer, intent(in)               :: ldcv
            double precision, intent(in)      :: dv(lddv, *)
            integer, intent(in)               :: lddv
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab09jv
    end interface
    public :: ab09jv
    
    interface
        subroutine ab09jw(job, dico, jobew, stbchk, n, m, p, nw, &
                       mw, a, lda, b, ldb, c, ldc, d, &
                       ldd, aw, ldaw, ew, ldew, bw, ldbw, cw, &
                       ldcw, dw, lddw, iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: dico
            character, intent(in)             :: jobew
            character, intent(in)             :: stbchk
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: nw
            integer, intent(in)               :: mw
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: aw(ldaw, *)
            integer, intent(in)               :: ldaw
            double precision, intent(inout)   :: ew(ldew, *)
            integer, intent(in)               :: ldew
            double precision, intent(inout)   :: bw(ldbw, *)
            integer, intent(in)               :: ldbw
            double precision, intent(inout)   :: cw(ldcw, *)
            integer, intent(in)               :: ldcw
            double precision, intent(in)      :: dw(lddw, *)
            integer, intent(in)               :: lddw
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab09jw
    end interface
    public :: ab09jw
    
    interface
        subroutine ab09jx(dico, stdom, evtype, n, alpha, er, ei, ed, &
                       tolinf, info)
            character, intent(in)           :: dico
            character, intent(in)           :: stdom
            character, intent(in)           :: evtype
            integer, intent(in)             :: n
            double precision, intent(in)    :: alpha
            double precision, intent(in)    :: er(*)
            double precision, intent(in)    :: ei(*)
            double precision, intent(in)    :: ed(*)
            double precision, intent(in)    :: tolinf
            integer, intent(out)            :: info
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
            character, intent(in)             :: job
            character, intent(in)             :: dico
            character, intent(in)             :: weight
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: nv
            integer, intent(in)               :: nw
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: av(ldav, *)
            integer, intent(in)               :: ldav
            double precision, intent(inout)   :: bv(ldbv, *)
            integer, intent(in)               :: ldbv
            double precision, intent(inout)   :: cv(ldcv, *)
            integer, intent(in)               :: ldcv
            double precision, intent(inout)   :: dv(lddv, *)
            integer, intent(in)               :: lddv
            double precision, intent(inout)   :: aw(ldaw, *)
            integer, intent(in)               :: ldaw
            double precision, intent(inout)   :: bw(ldbw, *)
            integer, intent(in)               :: ldbw
            double precision, intent(inout)   :: cw(ldcw, *)
            integer, intent(in)               :: ldcw
            double precision, intent(inout)   :: dw(lddw, *)
            integer, intent(in)               :: lddw
            integer, intent(out)              :: ns
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09kd
    end interface
    public :: ab09kd
    
    interface
        subroutine ab09kx(job, dico, weight, n, nv, nw, m, p, &
                       a, lda, b, ldb, c, ldc, d, ldd, &
                       av, ldav, bv, ldbv, cv, ldcv, dv, lddv, &
                       aw, ldaw, bw, ldbw, cw, ldcw, dw, lddw, &
                       dwork, ldwork, iwarn, info)
            character, intent(in)             :: job
            character, intent(in)             :: dico
            character, intent(in)             :: weight
            integer, intent(in)               :: n
            integer, intent(in)               :: nv
            integer, intent(in)               :: nw
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: av(ldav, *)
            integer, intent(in)               :: ldav
            double precision, intent(inout)   :: bv(ldbv, *)
            integer, intent(in)               :: ldbv
            double precision, intent(inout)   :: cv(ldcv, *)
            integer, intent(in)               :: ldcv
            double precision, intent(in)      :: dv(lddv, *)
            integer, intent(in)               :: lddv
            double precision, intent(inout)   :: aw(ldaw, *)
            integer, intent(in)               :: ldaw
            double precision, intent(inout)   :: bw(ldbw, *)
            integer, intent(in)               :: ldbw
            double precision, intent(inout)   :: cw(ldcw, *)
            integer, intent(in)               :: ldcw
            double precision, intent(in)      :: dw(lddw, *)
            integer, intent(in)               :: lddw
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09kx
    end interface
    public :: ab09kx
    
    interface
        subroutine ab09md(dico, job, equil, ordsel, n, m, p, nr, &
                       alpha, a, lda, b, ldb, c, ldc, ns, &
                       hsv, tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: job
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: ns
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09md
    end interface
    public :: ab09md
    
    interface
        subroutine ab09nd(dico, job, equil, ordsel, n, m, p, nr, &
                       alpha, a, lda, b, ldb, c, ldc, d, &
                       ldd, ns, hsv, tol1, tol2, iwork, dwork, ldwork, &
                       iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: job
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: nr
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: ns
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab09nd
    end interface
    public :: ab09nd
    
    interface
        double precision function ab13ad (dico,equil,n,m,p,alpha,a,lda, &
                       b,ldb,c,ldc,ns,hsv,dwork,ldwork, &
                       info)
            character, intent(in)             :: dico
            character, intent(in)             :: equil
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: ns
            double precision, intent(out)     :: hsv(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end function ab13ad
    end interface
    public :: ab13ad
    
    interface
        double precision function ab13ax (dico,n,m,p,a,lda,b,ldb, &
                       c,ldc,hsv,dwork,ldwork,info)
            character, intent(in)             :: dico
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: hsv(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end function ab13ax
    end interface
    public :: ab13ax
    
    interface
        double precision function ab13bd (dico,jobn,n,m,p,a,lda,b, &
                       ldb,c,ldc,d,ldd,nq,tol,dwork, &
                       ldwork,iwarn,info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobn
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nq
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end function ab13bd
    end interface
    public :: ab13bd
    
    interface
        double precision function ab13cd (n,m,np,a,lda,b,ldb,c, &
                       ldc,d,ldd,tol,iwork,dwork,ldwork,zwork, &
                       lzwork,bwork,info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end function ab13cd
    end interface
    public :: ab13cd
    
    interface
        subroutine ab13dd(dico, jobe, equil, jobd, n, m, p, fpeak, &
                       a, lda, e, lde, b, ldb, c, ldc, &
                       d, ldd, gpeak, tol, iwork, dwork, ldwork, zwork, &
                       lzwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobe
            character, intent(in)             :: equil
            character, intent(in)             :: jobd
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: fpeak(2)
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: gpeak(2)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine ab13dd
    end interface
    public :: ab13dd
    
    interface
        double precision function ab13dx (dico,jobe,jobd,n,m,p,omega,a, &
                       lda,e,lde,b,ldb,c,ldc,d, &
                       ldd,iwork,dwork,ldwork,zwork,lzwork,info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobe
            character, intent(in)             :: jobd
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: omega
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end function ab13dx
    end interface
    public :: ab13dx
    
    interface
        subroutine ab13ed(n, a, lda, low, high, tol, dwork, ldwork, &
                       info)
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: low
            double precision, intent(out)     :: high
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ab13ed
    end interface
    public :: ab13ed
    
    interface
        subroutine ab13fd(n, a, lda, beta, omega, tol, dwork, ldwork, &
                       zwork, lzwork, info)
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: beta
            double precision, intent(out)     :: omega
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine ab13fd
    end interface
    public :: ab13fd
    
    interface
        subroutine ab13hd(dico, jobe, equil, jobd, ckprop, reduce, poles, n, &
                       m, p, ranke, fpeak, a, lda, e, lde, &
                       b, ldb, c, ldc, d, ldd, nr, gpeak, &
                       tol, iwork, dwork, ldwork, zwork, lzwork, bwork, iwarn, &
                       info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobe
            character, intent(in)             :: equil
            character, intent(in)             :: jobd
            character, intent(in)             :: ckprop
            character, intent(in)             :: reduce
            character, intent(in)             :: poles
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: ranke
            double precision, intent(inout)   :: fpeak(2)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nr
            double precision, intent(out)     :: gpeak(2)
            double precision, intent(in)      :: tol(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ab13hd
    end interface
    public :: ab13hd
    
    interface
        logical function ab13id (jobsys,jobeig,equil,cksing,restor,update,n,m, &
                       p,a,lda,e,lde,b,ldb,c, &
                       ldc,nr,ranke,tol,iwork,dwork,ldwork,iwarn, &
                       info)
            character, intent(in)             :: jobsys
            character, intent(in)             :: jobeig
            character, intent(in)             :: equil
            character, intent(in)             :: cksing
            character, intent(in)             :: restor
            character, intent(in)             :: update
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: nr
            integer, intent(out)              :: ranke
            double precision, intent(in)      :: tol(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end function ab13id
    end interface
    public :: ab13id
    
    interface
        subroutine ab13md(fact, n, z, ldz, m, nblock, itype, x, &
                       bound, d, g, iwork, dwork, ldwork, zwork, lzwork, &
                       info)
            character, intent(in)             :: fact
            integer, intent(in)               :: n
            complex*16, intent(in)            :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(in)               :: m
            integer, intent(in)               :: nblock(*)
            integer, intent(in)               :: itype(*)
            double precision, intent(inout)   :: x(*)
            double precision, intent(out)     :: bound
            double precision, intent(out)     :: d(*)
            double precision, intent(out)     :: g(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine ab13md
    end interface
    public :: ab13md
    
    interface
        subroutine ab8nxz(n, m, p, ro, sigma, svlmax, abcd, ldabcd, &
                       ninfz, infz, kronl, mu, nu, nkrol, tol, iwork, &
                       dwork, zwork, lzwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: ro
            integer, intent(inout)            :: sigma
            double precision, intent(in)      :: svlmax
            complex*16, intent(inout)         :: abcd(ldabcd, *)
            integer, intent(in)               :: ldabcd
            integer, intent(inout)            :: ninfz
            integer, intent(inout)            :: infz(*)
            integer, intent(inout)            :: kronl(*)
            integer, intent(out)              :: mu
            integer, intent(out)              :: nu
            integer, intent(out)              :: nkrol
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine ab8nxz
    end interface
    public :: ab8nxz
    
    interface
        subroutine ag07bd(jobe, n, m, a, lda, e, lde, b, &
                       ldb, c, ldc, d, ldd, ai, ldai, ei, &
                       ldei, bi, ldbi, ci, ldci, di, lddi, info)
            character, intent(in)           :: jobe
            integer, intent(in)             :: n
            integer, intent(in)             :: m
            double precision, intent(in)    :: a(lda, *)
            integer, intent(in)             :: lda
            double precision, intent(in)    :: e(lde, *)
            integer, intent(in)             :: lde
            double precision, intent(in)    :: b(ldb, *)
            integer, intent(in)             :: ldb
            double precision, intent(in)    :: c(ldc, *)
            integer, intent(in)             :: ldc
            double precision, intent(in)    :: d(ldd, *)
            integer, intent(in)             :: ldd
            double precision, intent(out)   :: ai(ldai, *)
            integer, intent(in)             :: ldai
            double precision, intent(out)   :: ei(ldei, *)
            integer, intent(in)             :: ldei
            double precision, intent(out)   :: bi(ldbi, *)
            integer, intent(in)             :: ldbi
            double precision, intent(out)   :: ci(ldci, *)
            integer, intent(in)             :: ldci
            double precision, intent(out)   :: di(lddi, *)
            integer, intent(in)             :: lddi
            integer, intent(out)            :: info
        end subroutine ag07bd
    end interface
    public :: ag07bd
    
    interface
        subroutine ag08bd(equil, l, n, m, p, a, lda, e, &
                       lde, b, ldb, c, ldc, d, ldd, nfz, &
                       nrank, niz, dinfz, nkror, ninfe, nkrol, infz, kronr, &
                       infe, kronl, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: equil
            integer, intent(in)               :: l
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nfz
            integer, intent(out)              :: nrank
            integer, intent(out)              :: niz
            integer, intent(out)              :: dinfz
            integer, intent(out)              :: nkror
            integer, intent(out)              :: ninfe
            integer, intent(out)              :: nkrol
            integer, intent(out)              :: infz(*)
            integer, intent(out)              :: kronr(*)
            integer, intent(out)              :: infe(*)
            integer, intent(out)              :: kronl(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ag08bd
    end interface
    public :: ag08bd
    
    interface
        subroutine ag08by(first, n, m, p, svlmax, abcd, ldabcd, e, &
                       lde, nr, pr, ninfz, dinfz, nkronl, infz, kronl, &
                       tol, iwork, dwork, ldwork, info)
            logical, intent(in)               :: first
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: svlmax
            double precision, intent(inout)   :: abcd(ldabcd, *)
            integer, intent(in)               :: ldabcd
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            integer, intent(out)              :: nr
            integer, intent(out)              :: pr
            integer, intent(out)              :: ninfz
            integer, intent(out)              :: dinfz
            integer, intent(out)              :: nkronl
            integer, intent(out)              :: infz(*)
            integer, intent(out)              :: kronl(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine ag08by
    end interface
    public :: ag08by
    
    interface
        subroutine ag08bz(equil, l, n, m, p, a, lda, e, &
                       lde, b, ldb, c, ldc, d, ldd, nfz, &
                       nrank, niz, dinfz, nkror, ninfe, nkrol, infz, kronr, &
                       infe, kronl, tol, iwork, dwork, zwork, lzwork, info)
            character, intent(in)             :: equil
            integer, intent(in)               :: l
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(in)               :: lde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(in)               :: ldc
            complex*16, intent(in)            :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nfz
            integer, intent(out)              :: nrank
            integer, intent(out)              :: niz
            integer, intent(out)              :: dinfz
            integer, intent(out)              :: nkror
            integer, intent(out)              :: ninfe
            integer, intent(out)              :: nkrol
            integer, intent(out)              :: infz(*)
            integer, intent(out)              :: kronr(*)
            integer, intent(out)              :: infe(*)
            integer, intent(out)              :: kronl(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine ag08bz
    end interface
    public :: ag08bz
    
    interface
        subroutine ag8byz(first, n, m, p, svlmax, abcd, ldabcd, e, &
                       lde, nr, pr, ninfz, dinfz, nkronl, infz, kronl, &
                       tol, iwork, dwork, zwork, lzwork, info)
            logical, intent(in)               :: first
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: svlmax
            complex*16, intent(inout)         :: abcd(ldabcd, *)
            integer, intent(in)               :: ldabcd
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(in)               :: lde
            integer, intent(out)              :: nr
            integer, intent(out)              :: pr
            integer, intent(out)              :: ninfz
            integer, intent(out)              :: dinfz
            integer, intent(out)              :: nkronl
            integer, intent(out)              :: infz(*)
            integer, intent(out)              :: kronl(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine ag8byz
    end interface
    public :: ag8byz
    
    interface
        subroutine bb01ad(def, nr, dpar, ipar, bpar, chpar, vec, n, &
                       m, p, a, lda, b, ldb, c, ldc, &
                       g, ldg, q, ldq, x, ldx, dwork, ldwork, &
                       info)
            character*1, intent(in)           :: def
            integer, intent(in)               :: nr(2)
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ipar(4)
            logical, intent(in)               :: bpar(6)
            character, intent(inout)          :: chpar*(*)
            logical, intent(out)              :: vec(9)
            integer, intent(out)              :: n
            integer, intent(out)              :: m
            integer, intent(out)              :: p
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: g(*)
            integer, intent(in)               :: ldg
            double precision, intent(out)     :: q(*)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine bb01ad
    end interface
    public :: bb01ad
    
    interface
        subroutine bb02ad(def, nr, dpar, ipar, bpar, chpar, vec, n, &
                       m, p, a, lda, b, ldb, c, ldc, &
                       q, ldq, r, ldr, s, lds, x, ldx, &
                       dwork, ldwork, info)
            character, intent(in)             :: def
            integer, intent(in)               :: nr(2)
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ipar(3)
            logical, intent(in)               :: bpar(7)
            character, intent(out)            :: chpar*255
            logical, intent(out)              :: vec(10)
            integer, intent(out)              :: n
            integer, intent(out)              :: m
            integer, intent(out)              :: p
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: q(*)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: r(*)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine bb02ad
    end interface
    public :: bb02ad
    
    interface
        subroutine bb03ad(def, nr, dpar, ipar, vec, n, m, e, &
                       lde, a, lda, y, ldy, b, ldb, x, &
                       ldx, u, ldu, note, dwork, ldwork, info)
            character, intent(in)             :: def
            integer, intent(in)               :: nr(*)
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ipar(*)
            logical, intent(out)              :: vec(8)
            integer, intent(out)              :: n
            integer, intent(out)              :: m
            double precision, intent(out)     :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: u(ldu, *)
            integer, intent(in)               :: ldu
            character*70, intent(out)         :: note
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine bb03ad
    end interface
    public :: bb03ad
    
    interface
        subroutine bb04ad(def, nr, dpar, ipar, vec, n, m, e, &
                       lde, a, lda, y, ldy, b, ldb, x, &
                       ldx, u, ldu, note, dwork, ldwork, info)
            character, intent(in)             :: def
            integer, intent(in)               :: nr(*)
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ipar(*)
            logical, intent(out)              :: vec(8)
            integer, intent(out)              :: n
            integer, intent(out)              :: m
            double precision, intent(out)     :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: u(ldu, *)
            integer, intent(in)               :: ldu
            character*70, intent(out)         :: note
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine bb04ad
    end interface
    public :: bb04ad
    
    interface
        subroutine bd01ad(def, nr, dpar, ipar, vec, n, m, p, &
                       e, lde, a, lda, b, ldb, c, ldc, &
                       d, ldd, note, dwork, ldwork, info)
            character, intent(in)             :: def
            integer, intent(in)               :: nr(*)
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ipar(*)
            logical, intent(out)              :: vec(8)
            integer, intent(out)              :: n
            integer, intent(out)              :: m
            integer, intent(out)              :: p
            double precision, intent(out)     :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            character*70, intent(out)         :: note
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine bd01ad
    end interface
    public :: bd01ad
    
    interface
        subroutine bd02ad(def, nr, dpar, ipar, vec, n, m, p, &
                       e, lde, a, lda, b, ldb, c, ldc, &
                       d, ldd, note, dwork, ldwork, info)
            character, intent(in)             :: def
            integer, intent(in)               :: nr(*)
            double precision, intent(inout)   :: dpar(*)
            integer, intent(inout)            :: ipar(*)
            logical, intent(out)              :: vec(8)
            integer, intent(out)              :: n
            integer, intent(out)              :: m
            integer, intent(out)              :: p
            double precision, intent(out)     :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            character*70, intent(out)         :: note
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine bd02ad
    end interface
    public :: bd02ad
    
    interface
        subroutine de01od(conv, n, a, b, info)
            character, intent(in)             :: conv
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(*)
            double precision, intent(in)      :: b(*)
            integer, intent(out)              :: info
        end subroutine de01od
    end interface
    public :: de01od
    
    interface
        subroutine de01pd(conv, wght, n, a, b, w, info)
            character, intent(in)             :: conv
            character, intent(in)             :: wght
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(*)
            double precision, intent(in)      :: b(*)
            double precision, intent(inout)   :: w(*)
            integer, intent(out)              :: info
        end subroutine de01pd
    end interface
    public :: de01pd
    
    interface
        subroutine df01md(sico, n, dt, a, dwork, info)
            character, intent(in)             :: sico
            integer, intent(in)               :: n
            double precision, intent(in)      :: dt
            double precision, intent(inout)   :: a(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine df01md
    end interface
    public :: df01md
    
    interface
        subroutine dg01md(indi, n, xr, xi, info)
            character, intent(in)             :: indi
            integer, intent(in)               :: n
            double precision, intent(inout)   :: xr(*)
            double precision, intent(inout)   :: xi(*)
            integer, intent(out)              :: info
        end subroutine dg01md
    end interface
    public :: dg01md
    
    interface
        subroutine dg01nd(indi, n, xr, xi, info)
            character, intent(in)             :: indi
            integer, intent(in)               :: n
            double precision, intent(inout)   :: xr(*)
            double precision, intent(inout)   :: xi(*)
            integer, intent(out)              :: info
        end subroutine dg01nd
    end interface
    public :: dg01nd
    
    interface
        subroutine dg01ny(indi, n, xr, xi)
            character, intent(in)             :: indi
            integer, intent(in)               :: n
            double precision, intent(inout)   :: xr(*)
            double precision, intent(inout)   :: xi(*)
        end subroutine dg01ny
    end interface
    public :: dg01ny
    
    interface
        subroutine dg01od(scr, wght, n, a, w, info)
            character, intent(in)             :: scr
            character, intent(in)             :: wght
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(*)
            double precision, intent(inout)   :: w(*)
            integer, intent(out)              :: info
        end subroutine dg01od
    end interface
    public :: dg01od
    
    interface
        subroutine dk01md(type, n, a, info)
            character, intent(in)             :: type
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(*)
            integer, intent(out)              :: info
        end subroutine dk01md
    end interface
    public :: dk01md
    
    interface
        subroutine fb01qd(jobk, multbq, n, m, p, s, lds, a, &
                       lda, b, ldb, q, ldq, c, ldc, r, &
                       ldr, k, ldk, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: jobk
            character, intent(in)             :: multbq
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: k(ldk, *)
            integer, intent(in)               :: ldk
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine fb01qd
    end interface
    public :: fb01qd
    
    interface
        subroutine fb01rd(jobk, multbq, n, m, p, s, lds, a, &
                       lda, b, ldb, q, ldq, c, ldc, r, &
                       ldr, k, ldk, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: jobk
            character, intent(in)             :: multbq
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: k(ldk, *)
            integer, intent(in)               :: ldk
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine fb01rd
    end interface
    public :: fb01rd
    
    interface
        subroutine fb01sd(jobx, multab, multrc, n, m, p, sinv, ldsinv, &
                       ainv, ldainv, b, ldb, rinv, ldrinv, c, ldc, &
                       qinv, ldqinv, x, rinvy, z, e, tol, iwork, &
                       dwork, ldwork, info)
            character, intent(in)             :: jobx
            character, intent(in)             :: multab
            character, intent(in)             :: multrc
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: sinv(ldsinv, *)
            integer, intent(in)               :: ldsinv
            double precision, intent(in)      :: ainv(ldainv, *)
            integer, intent(in)               :: ldainv
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: rinv(ldrinv, *)
            integer, intent(in)               :: ldrinv
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: qinv(ldqinv, *)
            integer, intent(in)               :: ldqinv
            double precision, intent(inout)   :: x(*)
            double precision, intent(in)      :: rinvy(*)
            double precision, intent(in)      :: z(*)
            double precision, intent(out)     :: e(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine fb01sd
    end interface
    public :: fb01sd
    
    interface
        subroutine fb01td(jobx, multrc, n, m, p, sinv, ldsinv, ainv, &
                       ldainv, ainvb, ldainb, rinv, ldrinv, c, ldc, qinv, &
                       ldqinv, x, rinvy, z, e, tol, iwork, dwork, &
                       ldwork, info)
            character, intent(in)             :: jobx
            character, intent(in)             :: multrc
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: sinv(ldsinv, *)
            integer, intent(in)               :: ldsinv
            double precision, intent(in)      :: ainv(ldainv, *)
            integer, intent(in)               :: ldainv
            double precision, intent(in)      :: ainvb(ldainb, *)
            integer, intent(in)               :: ldainb
            double precision, intent(in)      :: rinv(ldrinv, *)
            integer, intent(in)               :: ldrinv
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: qinv(ldqinv, *)
            integer, intent(in)               :: ldqinv
            double precision, intent(inout)   :: x(*)
            double precision, intent(in)      :: rinvy(*)
            double precision, intent(in)      :: z(*)
            double precision, intent(out)     :: e(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine fb01td
    end interface
    public :: fb01td
    
    interface
        subroutine fb01vd(n, m, l, p, ldp, a, lda, b, &
                       ldb, c, ldc, q, ldq, r, ldr, k, &
                       ldk, tol, iwork, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            double precision, intent(inout)   :: p(ldp, *)
            integer, intent(in)               :: ldp
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: k(ldk, *)
            integer, intent(in)               :: ldk
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine fb01vd
    end interface
    public :: fb01vd
    
    interface
        subroutine fd01ad(jp, l, lambda, xin, yin, efor, xf, epsbck, &
                       cteta, steta, yq, epos, eout, salph, iwarn, info)
            character, intent(in)             :: jp
            integer, intent(in)               :: l
            double precision, intent(in)      :: lambda
            double precision, intent(in)      :: xin
            double precision, intent(in)      :: yin
            double precision, intent(inout)   :: efor
            double precision, intent(inout)   :: xf(*)
            double precision, intent(inout)   :: epsbck(*)
            double precision, intent(inout)   :: cteta(*)
            double precision, intent(inout)   :: steta(*)
            double precision, intent(inout)   :: yq(*)
            double precision, intent(out)     :: epos
            double precision, intent(out)     :: eout
            double precision, intent(out)     :: salph(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine fd01ad
    end interface
    public :: fd01ad
    
    interface
        subroutine ib01ad(meth, alg, jobd, batch, conct, ctrl, nobr, m, &
                       l, nsmp, u, ldu, y, ldy, n, r, &
                       ldr, sv, rcond, tol, iwork, dwork, ldwork, iwarn, &
                       info)
            character, intent(in)             :: meth
            character, intent(in)             :: alg
            character, intent(in)             :: jobd
            character, intent(in)             :: batch
            character, intent(in)             :: conct
            character, intent(in)             :: ctrl
            integer, intent(in)               :: nobr
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            integer, intent(in)               :: nsmp
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: y(ldy, *)
            integer, intent(in)               :: ldy
            integer, intent(out)              :: n
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: sv(*)
            double precision, intent(in)      :: rcond
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ib01ad
    end interface
    public :: ib01ad
    
    interface
        subroutine ib01bd(meth, job, jobck, nobr, n, m, l, nsmpl, &
                       r, ldr, a, lda, c, ldc, b, ldb, &
                       d, ldd, q, ldq, ry, ldry, s, lds, &
                       k, ldk, tol, iwork, dwork, ldwork, bwork, iwarn, &
                       info)
            character, intent(in)             :: meth
            character, intent(in)             :: job
            character, intent(in)             :: jobck
            integer, intent(in)               :: nobr
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            integer, intent(in)               :: nsmpl
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: ry(ldry, *)
            integer, intent(in)               :: ldry
            double precision, intent(out)     :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(out)     :: k(ldk, *)
            integer, intent(in)               :: ldk
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ib01bd
    end interface
    public :: ib01bd
    
    interface
        subroutine ib01cd(jobx0, comuse, job, n, m, l, nsmp, a, &
                       lda, b, ldb, c, ldc, d, ldd, u, &
                       ldu, y, ldy, x0, v, ldv, tol, iwork, &
                       dwork, ldwork, iwarn, info)
            character, intent(in)             :: jobx0
            character, intent(in)             :: comuse
            character, intent(in)             :: job
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            integer, intent(in)               :: nsmp
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(out)     :: x0(*)
            double precision, intent(out)     :: v(ldv, *)
            integer, intent(in)               :: ldv
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ib01cd
    end interface
    public :: ib01cd
    
    interface
        subroutine ib01md(meth, alg, batch, conct, nobr, m, l, nsmp, &
                       u, ldu, y, ldy, r, ldr, iwork, dwork, &
                       ldwork, iwarn, info)
            character, intent(in)             :: meth
            character, intent(in)             :: alg
            character, intent(in)             :: batch
            character, intent(in)             :: conct
            integer, intent(in)               :: nobr
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            integer, intent(in)               :: nsmp
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ib01md
    end interface
    public :: ib01md
    
    interface
        subroutine ib01my(meth, batch, conct, nobr, m, l, nsmp, u, &
                       ldu, y, ldy, r, ldr, iwork, dwork, ldwork, &
                       iwarn, info)
            character, intent(in)             :: meth
            character, intent(in)             :: batch
            character, intent(in)             :: conct
            integer, intent(in)               :: nobr
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            integer, intent(in)               :: nsmp
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(out)     :: r(ldr, *)
            integer, intent(in)               :: ldr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ib01my
    end interface
    public :: ib01my
    
    interface
        subroutine ib01nd(meth, jobd, nobr, m, l, r, ldr, sv, &
                       tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: meth
            character, intent(in)             :: jobd
            integer, intent(in)               :: nobr
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: sv(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ib01nd
    end interface
    public :: ib01nd
    
    interface
        subroutine ib01od(ctrl, nobr, l, sv, n, tol, iwarn, info)
            character, intent(in)           :: ctrl
            integer, intent(in)             :: nobr
            integer, intent(in)             :: l
            double precision, intent(in)    :: sv(*)
            integer, intent(out)            :: n
            double precision, intent(in)    :: tol
            integer, intent(out)            :: iwarn
            integer, intent(out)            :: info
        end subroutine ib01od
    end interface
    public :: ib01od
    
    interface
        subroutine ib01oy(ns, nmax, n, sv, info)
            integer, intent(in)               :: ns
            integer, intent(in)               :: nmax
            integer, intent(inout)            :: n
            double precision, intent(in)      :: sv(*)
            integer, intent(out)              :: info
        end subroutine ib01oy
    end interface
    public :: ib01oy
    
    interface
        subroutine ib01pd(meth, job, jobcv, nobr, n, m, l, nsmpl, &
                       r, ldr, a, lda, c, ldc, b, ldb, &
                       d, ldd, q, ldq, ry, ldry, s, lds, &
                       o, ldo, tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: meth
            character, intent(in)             :: job
            character, intent(in)             :: jobcv
            integer, intent(in)               :: nobr
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            integer, intent(in)               :: nsmpl
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: ry(ldry, *)
            integer, intent(in)               :: ldry
            double precision, intent(out)     :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(out)     :: o(ldo, *)
            integer, intent(in)               :: ldo
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ib01pd
    end interface
    public :: ib01pd
    
    interface
        subroutine ib01px(job, nobr, n, m, l, uf, lduf, un, &
                       ldun, ul, ldul, pgal, ldpgal, k, ldk, r, &
                       ldr, x, b, ldb, d, ldd, tol, iwork, &
                       dwork, ldwork, iwarn, info)
            character, intent(in)             :: job
            integer, intent(in)               :: nobr
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            double precision, intent(inout)   :: uf(lduf, *)
            integer, intent(in)               :: lduf
            double precision, intent(in)      :: un(ldun, *)
            integer, intent(in)               :: ldun
            double precision, intent(inout)   :: ul(ldul, *)
            integer, intent(in)               :: ldul
            double precision, intent(in)      :: pgal(ldpgal, *)
            integer, intent(in)               :: ldpgal
            double precision, intent(in)      :: k(ldk, *)
            integer, intent(in)               :: ldk
            double precision, intent(out)     :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: x(*)
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ib01px
    end interface
    public :: ib01px
    
    interface
        subroutine ib01py(meth, job, nobr, n, m, l, rankr1, ul, &
                       ldul, r1, ldr1, tau1, pgal, ldpgal, k, ldk, &
                       r, ldr, h, ldh, b, ldb, d, ldd, &
                       tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: meth
            character, intent(in)             :: job
            integer, intent(in)               :: nobr
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            integer, intent(in)               :: rankr1
            double precision, intent(inout)   :: ul(ldul, *)
            integer, intent(in)               :: ldul
            double precision, intent(in)      :: r1(ldr1, *)
            integer, intent(in)               :: ldr1
            double precision, intent(in)      :: tau1(*)
            double precision, intent(in)      :: pgal(ldpgal, *)
            integer, intent(in)               :: ldpgal
            double precision, intent(inout)   :: k(ldk, *)
            integer, intent(in)               :: ldk
            double precision, intent(out)     :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ib01py
    end interface
    public :: ib01py
    
    interface
        subroutine ib01qd(jobx0, job, n, m, l, nsmp, a, lda, &
                       c, ldc, u, ldu, y, ldy, x0, b, &
                       ldb, d, ldd, tol, iwork, dwork, ldwork, iwarn, &
                       info)
            character, intent(in)             :: jobx0
            character, intent(in)             :: job
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            integer, intent(in)               :: nsmp
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(out)     :: x0(*)
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ib01qd
    end interface
    public :: ib01qd
    
    interface
        subroutine ib01rd(job, n, m, l, nsmp, a, lda, b, &
                       ldb, c, ldc, d, ldd, u, ldu, y, &
                       ldy, x0, tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: job
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            integer, intent(in)               :: nsmp
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(out)     :: x0(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ib01rd
    end interface
    public :: ib01rd
    
    interface
        subroutine ib03ad(init, alg, stor, nobr, m, l, nsmp, n, &
                       nn, itmax1, itmax2, nprint, u, ldu, y, ldy, &
                       x, lx, tol1, tol2, iwork, dwork, ldwork, iwarn, &
                       info)
            character, intent(in)             :: init
            character, intent(in)             :: alg
            character, intent(in)             :: stor
            integer, intent(in)               :: nobr
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            integer, intent(in)               :: nsmp
            integer, intent(inout)            :: n
            integer, intent(in)               :: nn
            integer, intent(in)               :: itmax1
            integer, intent(in)               :: itmax2
            integer, intent(in)               :: nprint
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: lx
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ib03ad
    end interface
    public :: ib03ad
    
    interface
        subroutine ib03bd(init, nobr, m, l, nsmp, n, nn, itmax1, &
                       itmax2, nprint, u, ldu, y, ldy, x, lx, &
                       tol1, tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: init
            integer, intent(in)               :: nobr
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            integer, intent(in)               :: nsmp
            integer, intent(inout)            :: n
            integer, intent(in)               :: nn
            integer, intent(in)               :: itmax1
            integer, intent(in)               :: itmax2
            integer, intent(in)               :: nprint
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(inout)   :: x(*)
            integer, intent(inout)            :: lx
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine ib03bd
    end interface
    public :: ib03bd
    
    interface
        subroutine ma01ad(xr, xi, yr, yi)
            double precision, intent(in)    :: xr
            double precision, intent(in)    :: xi
            double precision, intent(out)   :: yr
            double precision, intent(out)   :: yi
        end subroutine ma01ad
    end interface
    public :: ma01ad
    
    interface
        subroutine ma01bd(base, lgbas, k, s, a, inca, alpha, beta, &
                       scal)
            double precision, intent(in)    :: base
            double precision, intent(in)    :: lgbas
            integer, intent(in)             :: k
            integer, intent(in)             :: s(*)
            double precision, intent(in)    :: a(*)
            integer, intent(in)             :: inca
            double precision, intent(out)   :: alpha
            double precision, intent(out)   :: beta
            integer, intent(out)            :: scal
        end subroutine ma01bd
    end interface
    public :: ma01bd
    
    interface
        subroutine ma01bz(base, k, s, a, inca, alpha, beta, scal)
            double precision, intent(in)    :: base
            integer, intent(in)             :: k
            integer, intent(in)             :: s(*)
            complex*16, intent(in)          :: a(*)
            integer, intent(in)             :: inca
            complex*16, intent(out)         :: alpha
            complex*16, intent(out)         :: beta
            integer, intent(out)            :: scal
        end subroutine ma01bz
    end interface
    public :: ma01bz
    
    interface
        integer function ma01cd (a,ia,b,ib)
            double precision, intent(in)   :: a
            integer, intent(in)            :: ia
            double precision, intent(in)   :: b
            integer, intent(in)            :: ib
        end function ma01cd
    end interface
    public :: ma01cd
    
    interface
        subroutine ma01dd(ar1, ai1, ar2, ai2, eps, safemn, d)
            double precision, intent(in)    :: ar1
            double precision, intent(in)    :: ai1
            double precision, intent(in)    :: ar2
            double precision, intent(in)    :: ai2
            double precision, intent(in)    :: eps
            double precision, intent(in)    :: safemn
            double precision, intent(out)   :: d
        end subroutine ma01dd
    end interface
    public :: ma01dd
    
    interface
        subroutine ma01dz(ar1, ai1, b1, ar2, ai2, b2, eps, safemn, &
                       d1, d2, iwarn)
            double precision, intent(in)    :: ar1
            double precision, intent(in)    :: ai1
            double precision, intent(in)    :: b1
            double precision, intent(in)    :: ar2
            double precision, intent(in)    :: ai2
            double precision, intent(in)    :: b2
            double precision, intent(in)    :: eps
            double precision, intent(in)    :: safemn
            double precision, intent(out)   :: d1
            double precision, intent(out)   :: d2
            integer, intent(out)            :: iwarn
        end subroutine ma01dz
    end interface
    public :: ma01dz
    
    interface
        subroutine ma02ad(job, m, n, a, lda, b, ldb)
            character, intent(in)           :: job
            integer, intent(in)             :: m
            integer, intent(in)             :: n
            double precision, intent(in)    :: a(lda, *)
            integer, intent(in)             :: lda
            double precision, intent(out)   :: b(ldb, *)
            integer, intent(in)             :: ldb
        end subroutine ma02ad
    end interface
    public :: ma02ad
    
    interface
        subroutine ma02az(trans, job, m, n, a, lda, b, ldb)
            character, intent(in)     :: trans
            character, intent(in)     :: job
            integer, intent(in)       :: m
            integer, intent(in)       :: n
            complex*16, intent(in)    :: a(lda, *)
            integer, intent(in)       :: lda
            complex*16, intent(out)   :: b(ldb, *)
            integer, intent(in)       :: ldb
        end subroutine ma02az
    end interface
    public :: ma02az
    
    interface
        subroutine ma02bd(side, m, n, a, lda)
            character, intent(in)             :: side
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
        end subroutine ma02bd
    end interface
    public :: ma02bd
    
    interface
        subroutine ma02bz(side, m, n, a, lda)
            character, intent(in)       :: side
            integer, intent(in)         :: m
            integer, intent(in)         :: n
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(in)         :: lda
        end subroutine ma02bz
    end interface
    public :: ma02bz
    
    interface
        subroutine ma02cd(n, kl, ku, a, lda)
            integer, intent(in)               :: n
            integer, intent(in)               :: kl
            integer, intent(in)               :: ku
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
        end subroutine ma02cd
    end interface
    public :: ma02cd
    
    interface
        subroutine ma02cz(n, kl, ku, a, lda)
            integer, intent(in)         :: n
            integer, intent(in)         :: kl
            integer, intent(in)         :: ku
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(in)         :: lda
        end subroutine ma02cz
    end interface
    public :: ma02cz
    
    interface
        subroutine ma02dd(job, uplo, n, a, lda, ap)
            character, intent(in)             :: job
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: ap(*)
        end subroutine ma02dd
    end interface
    public :: ma02dd
    
    interface
        subroutine ma02ed(uplo, n, a, lda)
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
        end subroutine ma02ed
    end interface
    public :: ma02ed
    
    interface
        subroutine ma02es(uplo, n, a, lda)
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
        end subroutine ma02es
    end interface
    public :: ma02es
    
    interface
        subroutine ma02ez(uplo, trans, skew, n, a, lda)
            character, intent(in)       :: uplo
            character, intent(in)       :: trans
            character, intent(in)       :: skew
            integer, intent(in)         :: n
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(in)         :: lda
        end subroutine ma02ez
    end interface
    public :: ma02ez
    
    interface
        subroutine ma02fd(x1, x2, c, s, info)
            double precision, intent(inout)   :: x1
            double precision, intent(in)      :: x2
            double precision, intent(out)     :: c
            double precision, intent(out)     :: s
            integer, intent(out)              :: info
        end subroutine ma02fd
    end interface
    public :: ma02fd
    
    interface
        subroutine ma02gd(n, a, lda, k1, k2, ipiv, incx)
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            integer, intent(in)               :: k1
            integer, intent(in)               :: k2
            integer, intent(in)               :: ipiv(*)
            integer, intent(in)               :: incx
        end subroutine ma02gd
    end interface
    public :: ma02gd
    
    interface
        subroutine ma02gz(n, a, lda, k1, k2, ipiv, incx)
            integer, intent(in)         :: n
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(in)         :: lda
            integer, intent(in)         :: k1
            integer, intent(in)         :: k2
            integer, intent(in)         :: ipiv(*)
            integer, intent(in)         :: incx
        end subroutine ma02gz
    end interface
    public :: ma02gz
    
    interface
        logical function ma02hd (job,m,n,diag,a,lda)
            character, intent(in)          :: job
            integer, intent(in)            :: m
            integer, intent(in)            :: n
            double precision, intent(in)   :: diag
            double precision, intent(in)   :: a(lda, *)
            integer, intent(in)            :: lda
        end function ma02hd
    end interface
    public :: ma02hd
    
    interface
        logical function ma02hz (job,m,n,diag,a,lda)
            character, intent(in)    :: job
            integer, intent(in)      :: m
            integer, intent(in)      :: n
            complex*16, intent(in)   :: diag
            complex*16, intent(in)   :: a(lda, *)
            integer, intent(in)      :: lda
        end function ma02hz
    end interface
    public :: ma02hz
    
    interface
        double precision function ma02id (typ,norm,n,a,lda,qg,ldqg,dwork)
            character, intent(in)             :: typ
            character, intent(in)             :: norm
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            double precision, intent(inout)   :: dwork(*)
        end function ma02id
    end interface
    public :: ma02id
    
    interface
        double precision function ma02iz (typ,norm,n,a,lda,qg,ldqg,dwork)
            character, intent(in)             :: typ
            character, intent(in)             :: norm
            integer, intent(in)               :: n
            complex*16, intent(in)            :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(in)            :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            double precision, intent(inout)   :: dwork(*)
        end function ma02iz
    end interface
    public :: ma02iz
    
    interface
        double precision function ma02jd (ltran1,ltran2,n,q1,ldq1,q2,ldq2,res, &
                       ldres)
            logical, intent(in)               :: ltran1
            logical, intent(in)               :: ltran2
            integer, intent(in)               :: n
            double precision, intent(in)      :: q1(ldq1, *)
            integer, intent(in)               :: ldq1
            double precision, intent(in)      :: q2(ldq2, *)
            integer, intent(in)               :: ldq2
            double precision, intent(inout)   :: res(ldres, *)
            integer, intent(in)               :: ldres
        end function ma02jd
    end interface
    public :: ma02jd
    
    interface
        double precision function ma02jz (ltran1,ltran2,n,q1,ldq1,q2,ldq2,res, &
                       ldres)
            logical, intent(in)         :: ltran1
            logical, intent(in)         :: ltran2
            integer, intent(in)         :: n
            complex*16, intent(in)      :: q1(ldq1, *)
            integer, intent(in)         :: ldq1
            complex*16, intent(in)      :: q2(ldq2, *)
            integer, intent(in)         :: ldq2
            complex*16, intent(inout)   :: res(ldres, *)
            integer, intent(in)         :: ldres
        end function ma02jz
    end interface
    public :: ma02jz
    
    interface
        double precision function ma02md (norm,uplo,n,a,lda,dwork)
            character, intent(in)             :: norm
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: dwork(*)
        end function ma02md
    end interface
    public :: ma02md
    
    interface
        double precision function ma02mz (norm,uplo,n,a,lda,dwork)
            character, intent(in)             :: norm
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            complex*16, intent(in)            :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: dwork(*)
        end function ma02mz
    end interface
    public :: ma02mz
    
    interface
        subroutine ma02nz(uplo, trans, skew, n, k, l, a, lda)
            character, intent(in)       :: uplo
            character, intent(in)       :: trans
            character, intent(in)       :: skew
            integer, intent(in)         :: n
            integer, intent(in)         :: k
            integer, intent(in)         :: l
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(in)         :: lda
        end subroutine ma02nz
    end interface
    public :: ma02nz
    
    interface
        integer function ma02od (skew,m,a,lda,de,ldde)
            character, intent(in)          :: skew
            integer, intent(in)            :: m
            double precision, intent(in)   :: a(lda, *)
            integer, intent(in)            :: lda
            double precision, intent(in)   :: de(ldde, *)
            integer, intent(in)            :: ldde
        end function ma02od
    end interface
    public :: ma02od
    
    interface
        integer function ma02oz (skew,m,a,lda,de,ldde)
            character, intent(in)    :: skew
            integer, intent(in)      :: m
            complex*16, intent(in)   :: a(lda, *)
            integer, intent(in)      :: lda
            complex*16, intent(in)   :: de(ldde, *)
            integer, intent(in)      :: ldde
        end function ma02oz
    end interface
    public :: ma02oz
    
    interface
        subroutine ma02pd(m, n, a, lda, nzr, nzc)
            integer, intent(in)             :: m
            integer, intent(in)             :: n
            double precision, intent(in)    :: a(lda, *)
            integer, intent(in)             :: lda
            integer, intent(out)            :: nzr
            integer, intent(out)            :: nzc
        end subroutine ma02pd
    end interface
    public :: ma02pd
    
    interface
        subroutine ma02pz(m, n, a, lda, nzr, nzc)
            integer, intent(in)       :: m
            integer, intent(in)       :: n
            complex*16, intent(in)    :: a(lda, *)
            integer, intent(in)       :: lda
            integer, intent(out)      :: nzr
            integer, intent(out)      :: nzc
        end subroutine ma02pz
    end interface
    public :: ma02pz
    
    interface
        subroutine ma02rd(id, n, d, e, info)
            character, intent(in)             :: id
            integer, intent(in)               :: n
            double precision, intent(inout)   :: d(*)
            double precision, intent(inout)   :: e(*)
            integer, intent(out)              :: info
        end subroutine ma02rd
    end interface
    public :: ma02rd
    
    interface
        double precision function ma02sd (m,n,a,lda)
            integer, intent(in)            :: m
            integer, intent(in)            :: n
            double precision, intent(in)   :: a(lda, *)
            integer, intent(in)            :: lda
        end function ma02sd
    end interface
    public :: ma02sd
    
    interface
        subroutine mb01kd(uplo, trans, n, k, alpha, a, lda, b, &
                       ldb, beta, c, ldc, info)
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: k
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: info
        end subroutine mb01kd
    end interface
    public :: mb01kd
    
    interface
        subroutine mb01ld(uplo, trans, m, n, alpha, beta, r, ldr, &
                       a, lda, x, ldx, dwork, ldwork, info)
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb01ld
    end interface
    public :: mb01ld
    
    interface
        subroutine mb01md(uplo, n, alpha, a, lda, x, incx, beta, &
                       y, incy)
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: x(*)
            integer, intent(in)               :: incx
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: y(*)
            integer, intent(in)               :: incy
        end subroutine mb01md
    end interface
    public :: mb01md
    
    interface
        subroutine mb01nd(uplo, n, alpha, x, incx, y, incy, a, &
                       lda)
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: x(*)
            integer, intent(in)               :: incx
            double precision, intent(in)      :: y(*)
            integer, intent(in)               :: incy
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
        end subroutine mb01nd
    end interface
    public :: mb01nd
    
    interface
        subroutine mb01oc(uplo, trans, n, alpha, beta, r, ldr, h, &
                       ldh, x, ldx, info)
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(in)      :: x(ldx, *)
            integer, intent(in)               :: ldx
            integer, intent(out)              :: info
        end subroutine mb01oc
    end interface
    public :: mb01oc
    
    interface
        subroutine mb01od(uplo, trans, n, alpha, beta, r, ldr, h, &
                       ldh, x, ldx, e, lde, dwork, ldwork, info)
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(in)      :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb01od
    end interface
    public :: mb01od
    
    interface
        subroutine mb01oe(uplo, trans, n, alpha, beta, r, ldr, h, &
                       ldh, e, lde)
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
        end subroutine mb01oe
    end interface
    public :: mb01oe
    
    interface
        subroutine mb01oh(uplo, trans, n, alpha, beta, r, ldr, h, &
                       ldh, a, lda)
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
        end subroutine mb01oh
    end interface
    public :: mb01oh
    
    interface
        subroutine mb01oo(uplo, trans, n, h, ldh, x, ldx, e, &
                       lde, p, ldp, info)
            character, intent(in)           :: uplo
            character, intent(in)           :: trans
            integer, intent(in)             :: n
            double precision, intent(in)    :: h(ldh, *)
            integer, intent(in)             :: ldh
            double precision, intent(in)    :: x(ldx, *)
            integer, intent(in)             :: ldx
            double precision, intent(in)    :: e(lde, *)
            integer, intent(in)             :: lde
            double precision, intent(out)   :: p(ldp, *)
            integer, intent(in)             :: ldp
            integer, intent(out)            :: info
        end subroutine mb01oo
    end interface
    public :: mb01oo
    
    interface
        subroutine mb01os(uplo, trans, n, h, ldh, x, ldx, p, &
                       ldp, info)
            character, intent(in)           :: uplo
            character, intent(in)           :: trans
            integer, intent(in)             :: n
            double precision, intent(in)    :: h(ldh, *)
            integer, intent(in)             :: ldh
            double precision, intent(in)    :: x(ldx, *)
            integer, intent(in)             :: ldx
            double precision, intent(out)   :: p(ldp, *)
            integer, intent(in)             :: ldp
            integer, intent(out)            :: info
        end subroutine mb01os
    end interface
    public :: mb01os
    
    interface
        subroutine mb01ot(uplo, trans, n, alpha, beta, r, ldr, e, &
                       lde, t, ldt)
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(in)      :: t(ldt, *)
            integer, intent(in)               :: ldt
        end subroutine mb01ot
    end interface
    public :: mb01ot
    
    interface
        subroutine mb01pd(scun, type, m, n, kl, ku, anrm, nbl, &
                       nrows, a, lda, info)
            character, intent(in)             :: scun
            character, intent(in)             :: type
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: kl
            integer, intent(in)               :: ku
            double precision, intent(in)      :: anrm
            integer, intent(in)               :: nbl
            integer, intent(in)               :: nrows(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            integer, intent(out)              :: info
        end subroutine mb01pd
    end interface
    public :: mb01pd
    
    interface
        subroutine mb01qd(type, m, n, kl, ku, cfrom, cto, nbl, &
                       nrows, a, lda, info)
            character, intent(in)             :: type
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: kl
            integer, intent(in)               :: ku
            double precision, intent(in)      :: cfrom
            double precision, intent(in)      :: cto
            integer, intent(in)               :: nbl
            integer, intent(in)               :: nrows(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            integer, intent(out)              :: info
        end subroutine mb01qd
    end interface
    public :: mb01qd
    
    interface
        subroutine mb01rb(side, uplo, trans, m, n, alpha, beta, r, &
                       ldr, a, lda, b, ldb, info)
            character, intent(in)             :: side
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            integer, intent(out)              :: info
        end subroutine mb01rb
    end interface
    public :: mb01rb
    
    interface
        subroutine mb01rd(uplo, trans, m, n, alpha, beta, r, ldr, &
                       a, lda, x, ldx, dwork, ldwork, info)
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb01rd
    end interface
    public :: mb01rd
    
    interface
        subroutine mb01rh(uplo, trans, n, alpha, beta, r, ldr, h, &
                       ldh, x, ldx, dwork, ldwork, info)
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(in)      :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb01rh
    end interface
    public :: mb01rh
    
    interface
        subroutine mb01rt(uplo, trans, n, alpha, beta, r, ldr, e, &
                       lde, x, ldx, dwork, ldwork, info)
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(in)      :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb01rt
    end interface
    public :: mb01rt
    
    interface
        subroutine mb01ru(uplo, trans, m, n, alpha, beta, r, ldr, &
                       a, lda, x, ldx, dwork, ldwork, info)
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb01ru
    end interface
    public :: mb01ru
    
    interface
        subroutine mb01rw(uplo, trans, m, n, a, lda, z, ldz, &
                       dwork, info)
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb01rw
    end interface
    public :: mb01rw
    
    interface
        subroutine mb01rx(side, uplo, trans, m, n, alpha, beta, r, &
                       ldr, a, lda, b, ldb, info)
            character, intent(in)             :: side
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            integer, intent(out)              :: info
        end subroutine mb01rx
    end interface
    public :: mb01rx
    
    interface
        subroutine mb01ry(side, uplo, trans, m, alpha, beta, r, ldr, &
                       h, ldh, b, ldb, dwork, info)
            character, intent(in)             :: side
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: m
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb01ry
    end interface
    public :: mb01ry
    
    interface
        subroutine mb01sd(jobs, m, n, a, lda, r, c)
            character, intent(in)             :: jobs
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: r(*)
            double precision, intent(in)      :: c(*)
        end subroutine mb01sd
    end interface
    public :: mb01sd
    
    interface
        subroutine mb01ss(jobs, uplo, n, a, lda, d)
            character, intent(in)             :: jobs
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: d(*)
        end subroutine mb01ss
    end interface
    public :: mb01ss
    
    interface
        subroutine mb01td(n, a, lda, b, ldb, dwork, info)
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb01td
    end interface
    public :: mb01td
    
    interface
        subroutine mb01ud(side, trans, m, n, alpha, h, ldh, a, &
                       lda, b, ldb, info)
            character, intent(in)           :: side
            character, intent(in)           :: trans
            integer, intent(in)             :: m
            integer, intent(in)             :: n
            double precision, intent(in)    :: alpha
            double precision, intent(in)    :: h(ldh, *)
            integer, intent(in)             :: ldh
            double precision, intent(in)    :: a(lda, *)
            integer, intent(in)             :: lda
            double precision, intent(out)   :: b(ldb, *)
            integer, intent(in)             :: ldb
            integer, intent(out)            :: info
        end subroutine mb01ud
    end interface
    public :: mb01ud
    
    interface
        subroutine mb01uw(side, trans, m, n, alpha, h, ldh, a, &
                       lda, dwork, ldwork, info)
            character, intent(in)             :: side
            character, intent(in)             :: trans
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb01uw
    end interface
    public :: mb01uw
    
    interface
        subroutine mb01ux(side, uplo, trans, m, n, alpha, t, ldt, &
                       a, lda, dwork, ldwork, info)
            character, intent(in)             :: side
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb01ux
    end interface
    public :: mb01ux
    
    interface
        subroutine mb01uy(side, uplo, trans, m, n, alpha, t, ldt, &
                       a, lda, dwork, ldwork, info)
            character, intent(in)             :: side
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb01uy
    end interface
    public :: mb01uy
    
    interface
        subroutine mb01uz(side, uplo, trans, m, n, alpha, t, ldt, &
                       a, lda, zwork, lzwork, info)
            character, intent(in)       :: side
            character, intent(in)       :: uplo
            character, intent(in)       :: trans
            integer, intent(in)         :: m
            integer, intent(in)         :: n
            complex*16, intent(in)      :: alpha
            complex*16, intent(inout)   :: t(ldt, *)
            integer, intent(in)         :: ldt
            complex*16, intent(in)      :: a(lda, *)
            integer, intent(in)         :: lda
            complex*16, intent(inout)   :: zwork(*)
            integer, intent(in)         :: lzwork
            integer, intent(out)        :: info
        end subroutine mb01uz
    end interface
    public :: mb01uz
    
    interface
        subroutine mb01vd(trana, tranb, ma, na, mb, nb, alpha, beta, &
                       a, lda, b, ldb, c, ldc, mc, nc, &
                       info)
            character, intent(in)             :: trana
            character, intent(in)             :: tranb
            integer, intent(in)               :: ma
            integer, intent(in)               :: na
            integer, intent(in)               :: mb
            integer, intent(in)               :: nb
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: mc
            integer, intent(out)              :: nc
            integer, intent(out)              :: info
        end subroutine mb01vd
    end interface
    public :: mb01vd
    
    interface
        subroutine mb01wd(dico, uplo, trans, hess, n, alpha, beta, r, &
                       ldr, a, lda, t, ldt, info)
            character, intent(in)             :: dico
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            character, intent(in)             :: hess
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: t(ldt, *)
            integer, intent(in)               :: ldt
            integer, intent(out)              :: info
        end subroutine mb01wd
    end interface
    public :: mb01wd
    
    interface
        subroutine mb01xd(uplo, n, a, lda, info)
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            integer, intent(out)              :: info
        end subroutine mb01xd
    end interface
    public :: mb01xd
    
    interface
        subroutine mb01xy(uplo, n, a, lda, info)
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            integer, intent(out)              :: info
        end subroutine mb01xy
    end interface
    public :: mb01xy
    
    interface
        subroutine mb01yd(uplo, trans, n, k, l, alpha, beta, a, &
                       lda, c, ldc, info)
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: k
            integer, intent(in)               :: l
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: info
        end subroutine mb01yd
    end interface
    public :: mb01yd
    
    interface
        subroutine mb01zd(side, uplo, transt, diag, m, n, l, alpha, &
                       t, ldt, h, ldh, info)
            character, intent(in)             :: side
            character, intent(in)             :: uplo
            character, intent(in)             :: transt
            character, intent(in)             :: diag
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: l
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(in)               :: ldh
            integer, intent(out)              :: info
        end subroutine mb01zd
    end interface
    public :: mb01zd
    
    interface
        subroutine mb02cd(job, typet, k, n, t, ldt, g, ldg, &
                       r, ldr, l, ldl, cs, lcs, dwork, ldwork, &
                       info)
            character, intent(in)             :: job
            character, intent(in)             :: typet
            integer, intent(in)               :: k
            integer, intent(in)               :: n
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(out)     :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(out)     :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: l(ldl, *)
            integer, intent(in)               :: ldl
            double precision, intent(out)     :: cs(*)
            integer, intent(in)               :: lcs
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02cd
    end interface
    public :: mb02cd
    
    interface
        subroutine mb02cu(typeg, k, p, q, nb, a1, lda1, a2, &
                       lda2, b, ldb, rnk, ipvt, cs, tol, dwork, &
                       ldwork, info)
            character, intent(in)             :: typeg
            integer, intent(in)               :: k
            integer, intent(in)               :: p
            integer, intent(in)               :: q
            integer, intent(in)               :: nb
            double precision, intent(inout)   :: a1(lda1, *)
            integer, intent(in)               :: lda1
            double precision, intent(inout)   :: a2(lda2, *)
            integer, intent(in)               :: lda2
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            integer, intent(out)              :: rnk
            integer, intent(out)              :: ipvt(*)
            double precision, intent(out)     :: cs(*)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02cu
    end interface
    public :: mb02cu
    
    interface
        subroutine mb02cv(typeg, strucg, k, n, p, q, nb, rnk, &
                       a1, lda1, a2, lda2, b, ldb, f1, ldf1, &
                       f2, ldf2, g, ldg, cs, dwork, ldwork, info)
            character, intent(in)             :: typeg
            character, intent(in)             :: strucg
            integer, intent(in)               :: k
            integer, intent(in)               :: n
            integer, intent(in)               :: p
            integer, intent(in)               :: q
            integer, intent(in)               :: nb
            integer, intent(in)               :: rnk
            double precision, intent(in)      :: a1(lda1, *)
            integer, intent(in)               :: lda1
            double precision, intent(in)      :: a2(lda2, *)
            integer, intent(in)               :: lda2
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: f1(ldf1, *)
            integer, intent(in)               :: ldf1
            double precision, intent(inout)   :: f2(ldf2, *)
            integer, intent(in)               :: ldf2
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(in)      :: cs(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02cv
    end interface
    public :: mb02cv
    
    interface
        subroutine mb02cx(typet, p, q, k, a, lda, b, ldb, &
                       cs, lcs, dwork, ldwork, info)
            character, intent(in)             :: typet
            integer, intent(in)               :: p
            integer, intent(in)               :: q
            integer, intent(in)               :: k
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: cs(*)
            integer, intent(in)               :: lcs
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02cx
    end interface
    public :: mb02cx
    
    interface
        subroutine mb02cy(typet, strucg, p, q, n, k, a, lda, &
                       b, ldb, h, ldh, cs, lcs, dwork, ldwork, &
                       info)
            character, intent(in)             :: typet
            character, intent(in)             :: strucg
            integer, intent(in)               :: p
            integer, intent(in)               :: q
            integer, intent(in)               :: n
            integer, intent(in)               :: k
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(in)      :: cs(*)
            integer, intent(in)               :: lcs
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02cy
    end interface
    public :: mb02cy
    
    interface
        subroutine mb02dd(job, typet, k, m, n, ta, ldta, t, &
                       ldt, g, ldg, r, ldr, l, ldl, cs, &
                       lcs, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: typet
            integer, intent(in)               :: k
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: ta(ldta, *)
            integer, intent(in)               :: ldta
            double precision, intent(in)      :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: l(ldl, *)
            integer, intent(in)               :: ldl
            double precision, intent(inout)   :: cs(*)
            integer, intent(in)               :: lcs
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02dd
    end interface
    public :: mb02dd
    
    interface
        subroutine mb02ed(typet, k, n, nrhs, t, ldt, b, ldb, &
                       dwork, ldwork, info)
            character, intent(in)             :: typet
            integer, intent(in)               :: k
            integer, intent(in)               :: n
            integer, intent(in)               :: nrhs
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02ed
    end interface
    public :: mb02ed
    
    interface
        subroutine mb02fd(typet, k, n, p, s, t, ldt, r, &
                       ldr, dwork, ldwork, info)
            character, intent(in)             :: typet
            integer, intent(in)               :: k
            integer, intent(in)               :: n
            integer, intent(in)               :: p
            integer, intent(in)               :: s
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02fd
    end interface
    public :: mb02fd
    
    interface
        subroutine mb02gd(typet, triu, k, n, nl, p, s, t, &
                       ldt, rb, ldrb, dwork, ldwork, info)
            character, intent(in)             :: typet
            character, intent(in)             :: triu
            integer, intent(in)               :: k
            integer, intent(in)               :: n
            integer, intent(in)               :: nl
            integer, intent(in)               :: p
            integer, intent(in)               :: s
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: rb(ldrb, *)
            integer, intent(in)               :: ldrb
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02gd
    end interface
    public :: mb02gd
    
    interface
        subroutine mb02hd(triu, k, l, m, ml, n, nu, p, &
                       s, tc, ldtc, tr, ldtr, rb, ldrb, dwork, &
                       ldwork, info)
            character, intent(in)             :: triu
            integer, intent(in)               :: k
            integer, intent(in)               :: l
            integer, intent(in)               :: m
            integer, intent(in)               :: ml
            integer, intent(in)               :: n
            integer, intent(in)               :: nu
            integer, intent(in)               :: p
            integer, intent(in)               :: s
            double precision, intent(in)      :: tc(ldtc, *)
            integer, intent(in)               :: ldtc
            double precision, intent(in)      :: tr(ldtr, *)
            integer, intent(in)               :: ldtr
            double precision, intent(out)     :: rb(ldrb, *)
            integer, intent(in)               :: ldrb
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02hd
    end interface
    public :: mb02hd
    
    interface
        subroutine mb02id(job, k, l, m, n, rb, rc, tc, &
                       ldtc, tr, ldtr, b, ldb, c, ldc, dwork, &
                       ldwork, info)
            character, intent(in)             :: job
            integer, intent(in)               :: k
            integer, intent(in)               :: l
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: rb
            integer, intent(in)               :: rc
            double precision, intent(in)      :: tc(ldtc, *)
            integer, intent(in)               :: ldtc
            double precision, intent(in)      :: tr(ldtr, *)
            integer, intent(in)               :: ldtr
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02id
    end interface
    public :: mb02id
    
    interface
        subroutine mb02jd(job, k, l, m, n, p, s, tc, &
                       ldtc, tr, ldtr, q, ldq, r, ldr, dwork, &
                       ldwork, info)
            character, intent(in)             :: job
            integer, intent(in)               :: k
            integer, intent(in)               :: l
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: p
            integer, intent(in)               :: s
            double precision, intent(in)      :: tc(ldtc, *)
            integer, intent(in)               :: ldtc
            double precision, intent(in)      :: tr(ldtr, *)
            integer, intent(in)               :: ldtr
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02jd
    end interface
    public :: mb02jd
    
    interface
        subroutine mb02jx(job, k, l, m, n, tc, ldtc, tr, &
                       ldtr, rnk, q, ldq, r, ldr, jpvt, tol1, &
                       tol2, dwork, ldwork, info)
            character, intent(in)             :: job
            integer, intent(in)               :: k
            integer, intent(in)               :: l
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: tc(ldtc, *)
            integer, intent(in)               :: ldtc
            double precision, intent(in)      :: tr(ldtr, *)
            integer, intent(in)               :: ldtr
            integer, intent(out)              :: rnk
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: r(ldr, *)
            integer, intent(in)               :: ldr
            integer, intent(out)              :: jpvt(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02jx
    end interface
    public :: mb02jx
    
    interface
        subroutine mb02kd(ldblk, trans, k, l, m, n, r, alpha, &
                       beta, tc, ldtc, tr, ldtr, b, ldb, c, &
                       ldc, dwork, ldwork, info)
            character, intent(in)             :: ldblk
            character, intent(in)             :: trans
            integer, intent(in)               :: k
            integer, intent(in)               :: l
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: r
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(in)      :: tc(ldtc, *)
            integer, intent(in)               :: ldtc
            double precision, intent(in)      :: tr(ldtr, *)
            integer, intent(in)               :: ldtr
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02kd
    end interface
    public :: mb02kd
    
    interface
        subroutine mb02md(job, m, n, l, rank, c, ldc, s, &
                       x, ldx, tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: job
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: l
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: s(*)
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mb02md
    end interface
    public :: mb02md
    
    interface
        subroutine mb02nd(m, n, l, rank, theta, c, ldc, x, &
                       ldx, q, inul, tol, reltol, iwork, dwork, ldwork, &
                       bwork, iwarn, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: l
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: theta
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: q(*)
            logical, intent(out)              :: inul(*)
            double precision, intent(in)      :: tol
            double precision, intent(in)      :: reltol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mb02nd
    end interface
    public :: mb02nd
    
    interface
        subroutine mb02ny(updatu, updatv, m, n, i, k, q, e, &
                       u, ldu, v, ldv, dwork)
            logical, intent(in)               :: updatu
            logical, intent(in)               :: updatv
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: i
            integer, intent(in)               :: k
            double precision, intent(inout)   :: q(*)
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(in)               :: ldv
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb02ny
    end interface
    public :: mb02ny
    
    interface
        subroutine mb02od(side, uplo, trans, diag, norm, m, n, alpha, &
                       a, lda, b, ldb, rcond, tol, iwork, dwork, &
                       info)
            character, intent(in)             :: side
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            character, intent(in)             :: diag
            character, intent(in)             :: norm
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: rcond
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb02od
    end interface
    public :: mb02od
    
    interface
        subroutine mb02pd(fact, trans, n, nrhs, a, lda, af, ldaf, &
                       ipiv, equed, r, c, b, ldb, x, ldx, &
                       rcond, ferr, berr, iwork, dwork, info)
            character, intent(in)             :: fact
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: nrhs
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: af(ldaf, *)
            integer, intent(in)               :: ldaf
            integer, intent(inout)            :: ipiv(*)
            character, intent(inout)          :: equed
            double precision, intent(inout)   :: r(*)
            double precision, intent(inout)   :: c(*)
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: rcond
            double precision, intent(out)     :: ferr(*)
            double precision, intent(out)     :: berr(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb02pd
    end interface
    public :: mb02pd
    
    interface
        subroutine mb02qd(job, iniper, m, n, nrhs, rcond, svlmax, a, &
                       lda, b, ldb, y, jpvt, rank, sval, dwork, &
                       ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: iniper
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: nrhs
            double precision, intent(in)      :: rcond
            double precision, intent(in)      :: svlmax
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: y(*)
            integer, intent(inout)            :: jpvt(*)
            integer, intent(out)              :: rank
            double precision, intent(out)     :: sval(3)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02qd
    end interface
    public :: mb02qd
    
    interface
        subroutine mb02qy(m, n, nrhs, rank, a, lda, jpvt, b, &
                       ldb, tau, dwork, ldwork, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: nrhs
            integer, intent(in)               :: rank
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            integer, intent(in)               :: jpvt(*)
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02qy
    end interface
    public :: mb02qy
    
    interface
        subroutine mb02rd(trans, n, nrhs, h, ldh, ipiv, b, ldb, &
                       info)
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: nrhs
            double precision, intent(in)      :: h(ldh, *)
            integer, intent(in)               :: ldh
            integer, intent(in)               :: ipiv(*)
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            integer, intent(out)              :: info
        end subroutine mb02rd
    end interface
    public :: mb02rd
    
    interface
        subroutine mb02rz(trans, n, nrhs, h, ldh, ipiv, b, ldb, &
                       info)
            character, intent(in)       :: trans
            integer, intent(in)         :: n
            integer, intent(in)         :: nrhs
            complex*16, intent(in)      :: h(ldh, *)
            integer, intent(in)         :: ldh
            integer, intent(in)         :: ipiv(*)
            complex*16, intent(inout)   :: b(ldb, *)
            integer, intent(in)         :: ldb
            integer, intent(out)        :: info
        end subroutine mb02rz
    end interface
    public :: mb02rz
    
    interface
        subroutine mb02sd(n, h, ldh, ipiv, info)
            integer, intent(in)               :: n
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(in)               :: ldh
            integer, intent(out)              :: ipiv(*)
            integer, intent(out)              :: info
        end subroutine mb02sd
    end interface
    public :: mb02sd
    
    interface
        subroutine mb02sz(n, h, ldh, ipiv, info)
            integer, intent(in)         :: n
            complex*16, intent(inout)   :: h(ldh, *)
            integer, intent(in)         :: ldh
            integer, intent(out)        :: ipiv(*)
            integer, intent(out)        :: info
        end subroutine mb02sz
    end interface
    public :: mb02sz
    
    interface
        subroutine mb02td(norm, n, hnorm, h, ldh, ipiv, rcond, iwork, &
                       dwork, info)
            character, intent(in)             :: norm
            integer, intent(in)               :: n
            double precision, intent(in)      :: hnorm
            double precision, intent(in)      :: h(ldh, *)
            integer, intent(in)               :: ldh
            integer, intent(in)               :: ipiv(*)
            double precision, intent(out)     :: rcond
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb02td
    end interface
    public :: mb02td
    
    interface
        subroutine mb02tz(norm, n, hnorm, h, ldh, ipiv, rcond, dwork, &
                       zwork, info)
            character, intent(in)             :: norm
            integer, intent(in)               :: n
            double precision, intent(in)      :: hnorm
            complex*16, intent(in)            :: h(ldh, *)
            integer, intent(in)               :: ldh
            integer, intent(in)               :: ipiv(*)
            double precision, intent(out)     :: rcond
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(out)              :: info
        end subroutine mb02tz
    end interface
    public :: mb02tz
    
    interface
        subroutine mb02ud(fact, side, trans, jobp, m, n, alpha, rcond, &
                       rank, r, ldr, q, ldq, sv, b, ldb, &
                       rp, ldrp, dwork, ldwork, info)
            character, intent(in)             :: fact
            character, intent(in)             :: side
            character, intent(in)             :: trans
            character, intent(in)             :: jobp
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: rcond
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: sv(*)
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: rp(ldrp, *)
            integer, intent(in)               :: ldrp
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02ud
    end interface
    public :: mb02ud
    
    interface
        subroutine mb02uu(n, a, lda, rhs, ipiv, jpiv, scale)
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: rhs(*)
            integer, intent(in)               :: ipiv(*)
            integer, intent(in)               :: jpiv(*)
            double precision, intent(out)     :: scale
        end subroutine mb02uu
    end interface
    public :: mb02uu
    
    interface
        subroutine mb02uv(n, a, lda, ipiv, jpiv, info)
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            integer, intent(out)              :: ipiv(*)
            integer, intent(out)              :: jpiv(*)
            integer, intent(out)              :: info
        end subroutine mb02uv
    end interface
    public :: mb02uv
    
    interface
        subroutine mb02uw(ltrans, n, m, par, a, lda, b, ldb, &
                       scale, iwarn)
            logical, intent(in)               :: ltrans
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(in)      :: par(*)
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: scale
            integer, intent(out)              :: iwarn
        end subroutine mb02uw
    end interface
    public :: mb02uw
    
    interface
        subroutine mb02vd(trans, m, n, a, lda, ipiv, b, ldb, &
                       info)
            character, intent(in)             :: trans
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            integer, intent(out)              :: ipiv(*)
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            integer, intent(out)              :: info
        end subroutine mb02vd
    end interface
    public :: mb02vd
    
    interface
        subroutine mb02wd(form, f, n, ipar, lipar, dpar, ldpar, itmax, &
                       a, lda, b, incb, x, incx, tol, dwork, &
                       ldwork, iwarn, info)
            character, intent(in)             :: form
            external                :: f
            integer, intent(in)               :: n
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(in)      :: dpar(*)
            integer, intent(in)               :: ldpar
            integer, intent(in)               :: itmax
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(*)
            integer, intent(in)               :: incb
            double precision, intent(inout)   :: x(*)
            integer, intent(in)               :: incx
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mb02wd
    end interface
    public :: mb02wd
    
    interface
        subroutine mb02xd(form, stor, uplo, f, m, n, nrhs, ipar, &
                       lipar, dpar, ldpar, a, lda, b, ldb, ata, &
                       ldata, dwork, ldwork, info)
            character, intent(in)             :: form
            character, intent(in)             :: stor
            character, intent(in)             :: uplo
            external                :: f
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: nrhs
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(in)      :: dpar(*)
            integer, intent(in)               :: ldpar
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: ata(*)
            integer, intent(in)               :: ldata
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02xd
    end interface
    public :: mb02xd
    
    interface
        subroutine mb02yd(cond, n, r, ldr, ipvt, diag, qtb, rank, &
                       x, tol, dwork, ldwork, info)
            character, intent(in)             :: cond
            integer, intent(in)               :: n
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            integer, intent(in)               :: ipvt(*)
            double precision, intent(in)      :: diag(*)
            double precision, intent(in)      :: qtb(*)
            integer, intent(inout)            :: rank
            double precision, intent(out)     :: x(*)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb02yd
    end interface
    public :: mb02yd
    
    interface
        subroutine mb03ab(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, w1, w2, c1, s1, c2, s2)
            character, intent(in)           :: shft
            integer, intent(in)             :: k
            integer, intent(in)             :: n
            integer, intent(in)             :: amap(*)
            integer, intent(in)             :: s(*)
            integer, intent(in)             :: sinv
            double precision, intent(in)    :: a(lda1, lda2, *)
            integer, intent(in)             :: lda1
            integer, intent(in)             :: lda2
            double precision, intent(in)    :: w1
            double precision, intent(in)    :: w2
            double precision, intent(out)   :: c1
            double precision, intent(out)   :: s1
            double precision, intent(out)   :: c2
            double precision, intent(out)   :: s2
        end subroutine mb03ab
    end interface
    public :: mb03ab
    
    interface
        subroutine mb03ad(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, c1, s1, c2, s2)
            character, intent(in)           :: shft
            integer, intent(in)             :: k
            integer, intent(in)             :: n
            integer, intent(in)             :: amap(*)
            integer, intent(in)             :: s(*)
            integer, intent(in)             :: sinv
            double precision, intent(in)    :: a(lda1, lda2, *)
            integer, intent(in)             :: lda1
            integer, intent(in)             :: lda2
            double precision, intent(out)   :: c1
            double precision, intent(out)   :: s1
            double precision, intent(out)   :: c2
            double precision, intent(out)   :: s2
        end subroutine mb03ad
    end interface
    public :: mb03ad
    
    interface
        subroutine mb03ae(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, c1, s1, c2, s2)
            character, intent(in)           :: shft
            integer, intent(in)             :: k
            integer, intent(in)             :: n
            integer, intent(in)             :: amap(*)
            integer, intent(in)             :: s(*)
            integer, intent(in)             :: sinv
            double precision, intent(in)    :: a(lda1, lda2, *)
            integer, intent(in)             :: lda1
            integer, intent(in)             :: lda2
            double precision, intent(out)   :: c1
            double precision, intent(out)   :: s1
            double precision, intent(out)   :: c2
            double precision, intent(out)   :: s2
        end subroutine mb03ae
    end interface
    public :: mb03ae
    
    interface
        subroutine mb03af(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, c1, s1, c2, s2)
            character, intent(in)           :: shft
            integer, intent(in)             :: k
            integer, intent(in)             :: n
            integer, intent(in)             :: amap(*)
            integer, intent(in)             :: s(*)
            integer, intent(in)             :: sinv
            double precision, intent(in)    :: a(lda1, lda2, *)
            integer, intent(in)             :: lda1
            integer, intent(in)             :: lda2
            double precision, intent(out)   :: c1
            double precision, intent(out)   :: s1
            double precision, intent(out)   :: c2
            double precision, intent(out)   :: s2
        end subroutine mb03af
    end interface
    public :: mb03af
    
    interface
        subroutine mb03ag(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, c1, s1, c2, s2, iwork, dwork)
            character, intent(in)             :: shft
            integer, intent(in)               :: k
            integer, intent(in)               :: n
            integer, intent(in)               :: amap(*)
            integer, intent(in)               :: s(*)
            integer, intent(in)               :: sinv
            double precision, intent(in)      :: a(lda1, lda2, *)
            integer, intent(in)               :: lda1
            integer, intent(in)               :: lda2
            double precision, intent(out)     :: c1
            double precision, intent(out)     :: s1
            double precision, intent(out)     :: c2
            double precision, intent(out)     :: s2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb03ag
    end interface
    public :: mb03ag
    
    interface
        subroutine mb03ah(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, c1, s1, c2, s2)
            character, intent(in)           :: shft
            integer, intent(in)             :: k
            integer, intent(in)             :: n
            integer, intent(in)             :: amap(*)
            integer, intent(in)             :: s(*)
            integer, intent(in)             :: sinv
            double precision, intent(in)    :: a(lda1, lda2, *)
            integer, intent(in)             :: lda1
            integer, intent(in)             :: lda2
            double precision, intent(out)   :: c1
            double precision, intent(out)   :: s1
            double precision, intent(out)   :: c2
            double precision, intent(out)   :: s2
        end subroutine mb03ah
    end interface
    public :: mb03ah
    
    interface
        subroutine mb03ai(shft, k, n, amap, s, sinv, a, lda1, &
                       lda2, c1, s1, c2, s2, dwork)
            character, intent(in)             :: shft
            integer, intent(in)               :: k
            integer, intent(in)               :: n
            integer, intent(in)               :: amap(*)
            integer, intent(in)               :: s(*)
            integer, intent(in)               :: sinv
            double precision, intent(in)      :: a(lda1, lda2, *)
            integer, intent(in)               :: lda1
            integer, intent(in)               :: lda2
            double precision, intent(out)     :: c1
            double precision, intent(out)     :: s1
            double precision, intent(out)     :: c2
            double precision, intent(out)     :: s2
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb03ai
    end interface
    public :: mb03ai
    
    interface
        subroutine mb03ba(k, h, s, smult, amap, qmap)
            integer, intent(in)    :: k
            integer, intent(in)    :: h
            integer, intent(in)    :: s(*)
            integer, intent(out)   :: smult
            integer, intent(out)   :: amap(*)
            integer, intent(out)   :: qmap(*)
        end subroutine mb03ba
    end interface
    public :: mb03ba
    
    interface
        subroutine mb03bb(base, lgbas, ulp, k, amap, s, sinv, a, &
                       lda1, lda2, alphar, alphai, beta, scal, dwork, info)
            double precision, intent(in)      :: base
            double precision, intent(in)      :: lgbas
            double precision, intent(in)      :: ulp
            integer, intent(in)               :: k
            integer, intent(in)               :: amap(*)
            integer, intent(in)               :: s(*)
            integer, intent(in)               :: sinv
            double precision, intent(in)      :: a(lda1, lda2, *)
            integer, intent(in)               :: lda1
            integer, intent(in)               :: lda2
            double precision, intent(out)     :: alphar(2)
            double precision, intent(out)     :: alphai(2)
            double precision, intent(out)     :: beta(2)
            integer, intent(out)              :: scal(2)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb03bb
    end interface
    public :: mb03bb
    
    interface
        subroutine mb03bc(k, amap, s, sinv, a, lda1, lda2, macpar, &
                       cv, sv, dwork)
            integer, intent(in)               :: k
            integer, intent(in)               :: amap(*)
            integer, intent(in)               :: s(*)
            integer, intent(in)               :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(in)               :: lda1
            integer, intent(in)               :: lda2
            double precision, intent(in)      :: macpar(*)
            double precision, intent(out)     :: cv(*)
            double precision, intent(out)     :: sv(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb03bc
    end interface
    public :: mb03bc
    
    interface
        subroutine mb03bd(job, defl, compq, qind, k, n, h, ilo, &
                       ihi, s, a, lda1, lda2, q, ldq1, ldq2, &
                       alphar, alphai, beta, scal, iwork, liwork, dwork, ldwork, &
                       iwarn, info)
            character, intent(in)             :: job
            character, intent(in)             :: defl
            character, intent(in)             :: compq
            integer, intent(in)               :: qind(*)
            integer, intent(in)               :: k
            integer, intent(in)               :: n
            integer, intent(in)               :: h
            integer, intent(in)               :: ilo
            integer, intent(in)               :: ihi
            integer, intent(in)               :: s(*)
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(in)               :: lda1
            integer, intent(in)               :: lda2
            double precision, intent(inout)   :: q(ldq1, ldq2, *)
            integer, intent(in)               :: ldq1
            integer, intent(in)               :: ldq2
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(out)              :: scal(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mb03bd
    end interface
    public :: mb03bd
    
    interface
        subroutine mb03be(k, amap, s, sinv, a, lda1, lda2)
            integer, intent(in)               :: k
            integer, intent(in)               :: amap(*)
            integer, intent(in)               :: s(*)
            integer, intent(in)               :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(in)               :: lda1
            integer, intent(in)               :: lda2
        end subroutine mb03be
    end interface
    public :: mb03be
    
    interface
        subroutine mb03bf(k, amap, s, sinv, a, lda1, lda2, ulp)
            integer, intent(in)               :: k
            integer, intent(in)               :: amap(*)
            integer, intent(in)               :: s(*)
            integer, intent(in)               :: sinv
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(in)               :: lda1
            integer, intent(in)               :: lda2
            double precision, intent(in)      :: ulp
        end subroutine mb03bf
    end interface
    public :: mb03bf
    
    interface
        subroutine mb03bg(k, n, amap, s, sinv, a, lda1, lda2, &
                       wr, wi)
            integer, intent(in)             :: k
            integer, intent(in)             :: n
            integer, intent(in)             :: amap(*)
            integer, intent(in)             :: s(*)
            integer, intent(in)             :: sinv
            double precision, intent(in)    :: a(lda1, lda2, *)
            integer, intent(in)             :: lda1
            integer, intent(in)             :: lda2
            double precision, intent(out)   :: wr(*)
            double precision, intent(out)   :: wi(*)
        end subroutine mb03bg
    end interface
    public :: mb03bg
    
    interface
        subroutine mb03bz(job, compq, k, n, ilo, ihi, s, a, &
                       lda1, lda2, q, ldq1, ldq2, alpha, beta, scal, &
                       dwork, ldwork, zwork, lzwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: compq
            integer, intent(in)               :: k
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            integer, intent(in)               :: ihi
            integer, intent(in)               :: s(*)
            complex*16, intent(inout)         :: a(lda1, lda2, *)
            integer, intent(in)               :: lda1
            integer, intent(in)               :: lda2
            complex*16, intent(inout)         :: q(ldq1, ldq2, *)
            integer, intent(in)               :: ldq1
            integer, intent(in)               :: ldq2
            complex*16, intent(out)           :: alpha(*)
            complex*16, intent(out)           :: beta(*)
            integer, intent(out)              :: scal(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine mb03bz
    end interface
    public :: mb03bz
    
    interface
        subroutine mb03cd(uplo, n1, n2, prec, a, lda, b, ldb, &
                       d, ldd, q1, ldq1, q2, ldq2, q3, ldq3, &
                       dwork, ldwork, info)
            character, intent(in)             :: uplo
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: n2
            double precision, intent(in)      :: prec
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: q1(ldq1, *)
            integer, intent(in)               :: ldq1
            double precision, intent(out)     :: q2(ldq2, *)
            integer, intent(in)               :: ldq2
            double precision, intent(out)     :: q3(ldq3, *)
            integer, intent(in)               :: ldq3
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03cd
    end interface
    public :: mb03cd
    
    interface
        subroutine mb03cz(a, lda, b, ldb, d, ldd, co1, si1, &
                       co2, si2, co3, si3)
            complex*16, intent(in)          :: a(lda, *)
            integer, intent(in)             :: lda
            complex*16, intent(in)          :: b(ldb, *)
            integer, intent(in)             :: ldb
            complex*16, intent(in)          :: d(ldd, *)
            integer, intent(in)             :: ldd
            double precision, intent(out)   :: co1
            complex*16, intent(out)         :: si1
            double precision, intent(out)   :: co2
            complex*16, intent(out)         :: si2
            double precision, intent(out)   :: co3
            complex*16, intent(out)         :: si3
        end subroutine mb03cz
    end interface
    public :: mb03cz
    
    interface
        subroutine mb03dd(uplo, n1, n2, prec, a, lda, b, ldb, &
                       q1, ldq1, q2, ldq2, dwork, ldwork, info)
            character, intent(in)             :: uplo
            integer, intent(inout)            :: n1
            integer, intent(inout)            :: n2
            double precision, intent(in)      :: prec
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: q1(ldq1, *)
            integer, intent(in)               :: ldq1
            double precision, intent(out)     :: q2(ldq2, *)
            integer, intent(in)               :: ldq2
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03dd
    end interface
    public :: mb03dd
    
    interface
        subroutine mb03dz(a, lda, b, ldb, co1, si1, co2, si2)
            complex*16, intent(in)          :: a(lda, *)
            integer, intent(in)             :: lda
            complex*16, intent(in)          :: b(ldb, *)
            integer, intent(in)             :: ldb
            double precision, intent(out)   :: co1
            complex*16, intent(out)         :: si1
            double precision, intent(out)   :: co2
            complex*16, intent(out)         :: si2
        end subroutine mb03dz
    end interface
    public :: mb03dz
    
    interface
        subroutine mb03ed(n, prec, a, lda, b, ldb, d, ldd, &
                       q1, ldq1, q2, ldq2, q3, ldq3, dwork, ldwork, &
                       info)
            integer, intent(in)               :: n
            double precision, intent(in)      :: prec
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: q1(ldq1, *)
            integer, intent(in)               :: ldq1
            double precision, intent(out)     :: q2(ldq2, *)
            integer, intent(in)               :: ldq2
            double precision, intent(out)     :: q3(ldq3, *)
            integer, intent(in)               :: ldq3
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03ed
    end interface
    public :: mb03ed
    
    interface
        subroutine mb03fd(n, prec, a, lda, b, ldb, q1, ldq1, &
                       q2, ldq2, dwork, ldwork, info)
            integer, intent(in)               :: n
            double precision, intent(in)      :: prec
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: q1(ldq1, *)
            integer, intent(in)               :: ldq1
            double precision, intent(out)     :: q2(ldq2, *)
            integer, intent(in)               :: ldq2
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03fd
    end interface
    public :: mb03fd
    
    interface
        subroutine mb03fz(compq, compu, orth, n, z, ldz, b, ldb, &
                       fg, ldfg, neig, d, ldd, c, ldc, q, &
                       ldq, u, ldu, alphar, alphai, beta, iwork, liwork, &
                       dwork, ldwork, zwork, lzwork, bwork, info)
            character, intent(in)             :: compq
            character, intent(in)             :: compu
            character, intent(in)             :: orth
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: z(ldz, *)
            integer, intent(in)               :: ldz
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: fg(ldfg, *)
            integer, intent(in)               :: ldfg
            integer, intent(out)              :: neig
            complex*16, intent(out)           :: d(ldd, *)
            integer, intent(in)               :: ldd
            complex*16, intent(out)           :: c(ldc, *)
            integer, intent(in)               :: ldc
            complex*16, intent(out)           :: q(ldq, *)
            integer, intent(in)               :: ldq
            complex*16, intent(out)           :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine mb03fz
    end interface
    public :: mb03fz
    
    interface
        subroutine mb03gd(n, b, ldb, d, ldd, macpar, q, ldq, &
                       u, ldu, dwork, ldwork, info)
            integer, intent(in)               :: n
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: macpar(*)
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03gd
    end interface
    public :: mb03gd
    
    interface
        subroutine mb03gz(z11, z12, z22, h11, h12, co1, si1, co2, &
                       si2)
            complex*16, intent(in)          :: z11
            complex*16, intent(in)          :: z12
            complex*16, intent(in)          :: z22
            complex*16, intent(in)          :: h11
            complex*16, intent(in)          :: h12
            double precision, intent(out)   :: co1
            complex*16, intent(out)         :: si1
            double precision, intent(out)   :: co2
            complex*16, intent(out)         :: si2
        end subroutine mb03gz
    end interface
    public :: mb03gz
    
    interface
        subroutine mb03hd(n, a, lda, b, ldb, macpar, q, ldq, &
                       dwork, info)
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: macpar(*)
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb03hd
    end interface
    public :: mb03hd
    
    interface
        subroutine mb03hz(s11, s12, h11, h12, co, si)
            complex*16, intent(in)          :: s11
            complex*16, intent(in)          :: s12
            complex*16, intent(in)          :: h11
            complex*16, intent(in)          :: h12
            double precision, intent(out)   :: co
            complex*16, intent(out)         :: si
        end subroutine mb03hz
    end interface
    public :: mb03hz
    
    interface
        subroutine mb03id(compq, compu, n, a, lda, c, ldc, d, &
                       ldd, b, ldb, f, ldf, q, ldq, u1, &
                       ldu1, u2, ldu2, neig, iwork, liwork, dwork, ldwork, &
                       info)
            character, intent(in)             :: compq
            character, intent(in)             :: compu
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(in)               :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(in)               :: ldu2
            integer, intent(out)              :: neig
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03id
    end interface
    public :: mb03id
    
    interface
        subroutine mb03iz(compq, compu, n, a, lda, c, ldc, d, &
                       ldd, b, ldb, f, ldf, q, ldq, u1, &
                       ldu1, u2, ldu2, neig, tol, info)
            character, intent(in)             :: compq
            character, intent(in)             :: compu
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(in)               :: ldc
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(in)               :: ldd
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: f(ldf, *)
            integer, intent(in)               :: ldf
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(in)               :: ldq
            complex*16, intent(inout)         :: u1(ldu1, *)
            integer, intent(in)               :: ldu1
            complex*16, intent(inout)         :: u2(ldu2, *)
            integer, intent(in)               :: ldu2
            integer, intent(out)              :: neig
            double precision, intent(in)      :: tol
            integer, intent(out)              :: info
        end subroutine mb03iz
    end interface
    public :: mb03iz
    
    interface
        subroutine mb03jd(compq, n, a, lda, d, ldd, b, ldb, &
                       f, ldf, q, ldq, neig, iwork, liwork, dwork, &
                       ldwork, info)
            character, intent(in)             :: compq
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            integer, intent(out)              :: neig
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03jd
    end interface
    public :: mb03jd
    
    interface
        subroutine mb03jp(compq, n, a, lda, d, ldd, b, ldb, &
                       f, ldf, q, ldq, neig, iwork, liwork, dwork, &
                       ldwork, info)
            character, intent(in)             :: compq
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            integer, intent(out)              :: neig
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03jp
    end interface
    public :: mb03jp
    
    interface
        subroutine mb03jz(compq, n, a, lda, d, ldd, b, ldb, &
                       f, ldf, q, ldq, neig, tol, info)
            character, intent(in)             :: compq
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(in)               :: ldd
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: f(ldf, *)
            integer, intent(in)               :: ldf
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(in)               :: ldq
            integer, intent(out)              :: neig
            double precision, intent(in)      :: tol
            integer, intent(out)              :: info
        end subroutine mb03jz
    end interface
    public :: mb03jz
    
    interface
        subroutine mb03ka(compq, whichq, ws, k, nc, kschur, ifst, ilst, &
                       n, ni, s, t, ldt, ixt, q, ldq, &
                       ixq, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: compq
            integer, intent(in)               :: whichq(*)
            logical, intent(in)               :: ws
            integer, intent(in)               :: k
            integer, intent(in)               :: nc
            integer, intent(in)               :: kschur
            integer, intent(inout)            :: ifst
            integer, intent(inout)            :: ilst
            integer, intent(in)               :: n(*)
            integer, intent(in)               :: ni(*)
            integer, intent(in)               :: s(*)
            double precision, intent(inout)   :: t(*)
            integer, intent(in)               :: ldt(*)
            integer, intent(in)               :: ixt(*)
            double precision, intent(inout)   :: q(*)
            integer, intent(in)               :: ldq(*)
            integer, intent(in)               :: ixq(*)
            double precision, intent(in)      :: tol(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03ka
    end interface
    public :: mb03ka
    
    interface
        subroutine mb03kb(compq, whichq, ws, k, nc, kschur, j1, n1, &
                       n2, n, ni, s, t, ldt, ixt, q, &
                       ldq, ixq, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: compq
            integer, intent(in)               :: whichq(*)
            logical, intent(in)               :: ws
            integer, intent(in)               :: k
            integer, intent(in)               :: nc
            integer, intent(in)               :: kschur
            integer, intent(in)               :: j1
            integer, intent(in)               :: n1
            integer, intent(in)               :: n2
            integer, intent(in)               :: n(*)
            integer, intent(in)               :: ni(*)
            integer, intent(in)               :: s(*)
            double precision, intent(inout)   :: t(*)
            integer, intent(in)               :: ldt(*)
            integer, intent(in)               :: ixt(*)
            double precision, intent(inout)   :: q(*)
            integer, intent(in)               :: ldq(*)
            integer, intent(in)               :: ixq(*)
            double precision, intent(in)      :: tol(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03kb
    end interface
    public :: mb03kb
    
    interface
        subroutine mb03kc(k, khess, n, r, s, a, lda, v, &
                       tau)
            integer, intent(in)               :: k
            integer, intent(in)               :: khess
            integer, intent(in)               :: n
            integer, intent(in)               :: r
            integer, intent(in)               :: s(*)
            double precision, intent(inout)   :: a(*)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: v(*)
            double precision, intent(out)     :: tau(*)
        end subroutine mb03kc
    end interface
    public :: mb03kc
    
    interface
        subroutine mb03kd(compq, whichq, strong, k, nc, kschur, n, ni, &
                       s, select, t, ldt, ixt, q, ldq, ixq, &
                       m, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: compq
            integer, intent(in)               :: whichq(*)
            character, intent(in)             :: strong
            integer, intent(in)               :: k
            integer, intent(in)               :: nc
            integer, intent(in)               :: kschur
            integer, intent(in)               :: n(*)
            integer, intent(in)               :: ni(*)
            integer, intent(in)               :: s(*)
            logical, intent(in)               :: select(*)
            double precision, intent(inout)   :: t(*)
            integer, intent(in)               :: ldt(*)
            integer, intent(in)               :: ixt(*)
            double precision, intent(inout)   :: q(*)
            integer, intent(in)               :: ldq(*)
            integer, intent(in)               :: ixq(*)
            integer, intent(out)              :: m
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03kd
    end interface
    public :: mb03kd
    
    interface
        subroutine mb03ke(trana, tranb, isgn, k, m, n, prec, smin, &
                       s, a, b, c, scale, dwork, ldwork, info)
            logical, intent(in)               :: trana
            logical, intent(in)               :: tranb
            integer, intent(in)               :: isgn
            integer, intent(in)               :: k
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: prec
            double precision, intent(in)      :: smin
            integer, intent(in)               :: s(*)
            double precision, intent(in)      :: a(*)
            double precision, intent(in)      :: b(*)
            double precision, intent(inout)   :: c(*)
            double precision, intent(out)     :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03ke
    end interface
    public :: mb03ke
    
    interface
        subroutine mb03ld(compq, orth, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, neig, q, ldq, alphar, alphai, &
                       beta, iwork, liwork, dwork, ldwork, bwork, info)
            character, intent(in)             :: compq
            character, intent(in)             :: orth
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(in)               :: ldde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: fg(ldfg, *)
            integer, intent(in)               :: ldfg
            integer, intent(out)              :: neig
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine mb03ld
    end interface
    public :: mb03ld
    
    interface
        subroutine mb03lf(compq, compu, orth, n, z, ldz, b, ldb, &
                       fg, ldfg, neig, q, ldq, u, ldu, alphar, &
                       alphai, beta, iwork, liwork, dwork, ldwork, bwork, iwarn, &
                       info)
            character, intent(in)             :: compq
            character, intent(in)             :: compu
            character, intent(in)             :: orth
            integer, intent(in)               :: n
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: fg(ldfg, *)
            integer, intent(in)               :: ldfg
            integer, intent(out)              :: neig
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mb03lf
    end interface
    public :: mb03lf
    
    interface
        subroutine mb03lp(compq, orth, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, neig, q, ldq, alphar, alphai, &
                       beta, iwork, liwork, dwork, ldwork, bwork, info)
            character, intent(in)             :: compq
            character, intent(in)             :: orth
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(in)               :: ldde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: fg(ldfg, *)
            integer, intent(in)               :: ldfg
            integer, intent(out)              :: neig
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine mb03lp
    end interface
    public :: mb03lp
    
    interface
        subroutine mb03lz(compq, orth, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, neig, q, ldq, alphar, alphai, &
                       beta, iwork, dwork, ldwork, zwork, lzwork, bwork, info)
            character, intent(in)             :: compq
            character, intent(in)             :: orth
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: de(ldde, *)
            integer, intent(in)               :: ldde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: fg(ldfg, *)
            integer, intent(in)               :: ldfg
            integer, intent(out)              :: neig
            complex*16, intent(out)           :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine mb03lz
    end interface
    public :: mb03lz
    
    interface
        subroutine mb03md(n, l, theta, q, e, q2, e2, pivmin, &
                       tol, reltol, iwarn, info)
            integer, intent(in)               :: n
            integer, intent(inout)            :: l
            double precision, intent(inout)   :: theta
            double precision, intent(in)      :: q(*)
            double precision, intent(in)      :: e(*)
            double precision, intent(in)      :: q2(*)
            double precision, intent(in)      :: e2(*)
            double precision, intent(in)      :: pivmin
            double precision, intent(in)      :: tol
            double precision, intent(in)      :: reltol
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mb03md
    end interface
    public :: mb03md
    
    interface
        double precision function mb03my (nx,x,incx)
            integer, intent(in)            :: nx
            double precision, intent(in)   :: x(*)
            integer, intent(in)            :: incx
        end function mb03my
    end interface
    public :: mb03my
    
    interface
        integer function mb03nd (n,theta,q2,e2,pivmin,info)
            integer, intent(in)             :: n
            double precision, intent(in)    :: theta
            double precision, intent(in)    :: q2(*)
            double precision, intent(in)    :: e2(*)
            double precision, intent(in)    :: pivmin
            integer, intent(out)            :: info
        end function mb03nd
    end interface
    public :: mb03nd
    
    interface
        double precision function mb03ny (n,omega,a,lda,s,dwork,ldwork,zwork, &
                       lzwork,info)
            integer, intent(in)               :: n
            double precision, intent(in)      :: omega
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: s(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end function mb03ny
    end interface
    public :: mb03ny
    
    interface
        subroutine mb03od(jobqr, m, n, a, lda, jpvt, rcond, svlmax, &
                       tau, rank, sval, dwork, ldwork, info)
            character, intent(in)             :: jobqr
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            integer, intent(inout)            :: jpvt(*)
            double precision, intent(in)      :: rcond
            double precision, intent(in)      :: svlmax
            double precision, intent(out)     :: tau(*)
            integer, intent(out)              :: rank
            double precision, intent(out)     :: sval(3)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03od
    end interface
    public :: mb03od
    
    interface
        subroutine mb03oy(m, n, a, lda, rcond, svlmax, rank, sval, &
                       jpvt, tau, dwork, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: rcond
            double precision, intent(in)      :: svlmax
            integer, intent(out)              :: rank
            double precision, intent(out)     :: sval(3)
            integer, intent(out)              :: jpvt(*)
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb03oy
    end interface
    public :: mb03oy
    
    interface
        subroutine mb03pd(jobrq, m, n, a, lda, jpvt, rcond, svlmax, &
                       tau, rank, sval, dwork, info)
            character, intent(in)             :: jobrq
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            integer, intent(inout)            :: jpvt(*)
            double precision, intent(in)      :: rcond
            double precision, intent(in)      :: svlmax
            double precision, intent(out)     :: tau(*)
            integer, intent(out)              :: rank
            double precision, intent(out)     :: sval(3)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb03pd
    end interface
    public :: mb03pd
    
    interface
        subroutine mb03py(m, n, a, lda, rcond, svlmax, rank, sval, &
                       jpvt, tau, dwork, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: rcond
            double precision, intent(in)      :: svlmax
            integer, intent(out)              :: rank
            double precision, intent(out)     :: sval(3)
            integer, intent(out)              :: jpvt(*)
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb03py
    end interface
    public :: mb03py
    
    interface
        subroutine mb03qd(dico, stdom, jobu, n, nlow, nsup, alpha, a, &
                       lda, u, ldu, ndim, dwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: stdom
            character, intent(in)             :: jobu
            integer, intent(in)               :: n
            integer, intent(in)               :: nlow
            integer, intent(in)               :: nsup
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            integer, intent(out)              :: ndim
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb03qd
    end interface
    public :: mb03qd
    
    interface
        subroutine mb03qg(dico, stdom, jobu, jobv, n, nlow, nsup, alpha, &
                       a, lda, e, lde, u, ldu, v, ldv, &
                       ndim, dwork, ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: stdom
            character, intent(in)             :: jobu
            character, intent(in)             :: jobv
            integer, intent(in)               :: n
            integer, intent(in)               :: nlow
            integer, intent(in)               :: nsup
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(in)               :: ldv
            integer, intent(out)              :: ndim
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03qg
    end interface
    public :: mb03qg
    
    interface
        subroutine mb03qv(n, s, lds, t, ldt, alphar, alphai, beta, &
                       info)
            integer, intent(in)             :: n
            double precision, intent(in)    :: s(lds, *)
            integer, intent(in)             :: lds
            double precision, intent(in)    :: t(ldt, *)
            integer, intent(in)             :: ldt
            double precision, intent(out)   :: alphar(*)
            double precision, intent(out)   :: alphai(*)
            double precision, intent(out)   :: beta(*)
            integer, intent(out)            :: info
        end subroutine mb03qv
    end interface
    public :: mb03qv
    
    interface
        subroutine mb03qw(n, l, a, lda, e, lde, u, ldu, &
                       v, ldv, alphar, alphai, beta, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: l
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(in)               :: ldv
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(out)              :: info
        end subroutine mb03qw
    end interface
    public :: mb03qw
    
    interface
        subroutine mb03qx(n, t, ldt, wr, wi, info)
            integer, intent(in)             :: n
            double precision, intent(in)    :: t(ldt, *)
            integer, intent(in)             :: ldt
            double precision, intent(out)   :: wr(*)
            double precision, intent(out)   :: wi(*)
            integer, intent(out)            :: info
        end subroutine mb03qx
    end interface
    public :: mb03qx
    
    interface
        subroutine mb03qy(n, l, a, lda, u, ldu, e1, e2, &
                       info)
            integer, intent(in)               :: n
            integer, intent(in)               :: l
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: e1
            double precision, intent(out)     :: e2
            integer, intent(out)              :: info
        end subroutine mb03qy
    end interface
    public :: mb03qy
    
    interface
        subroutine mb03rd(jobx, sort, n, pmax, a, lda, x, ldx, &
                       nblcks, blsize, wr, wi, tol, dwork, info)
            character, intent(in)             :: jobx
            character, intent(in)             :: sort
            integer, intent(in)               :: n
            double precision, intent(in)      :: pmax
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            integer, intent(out)              :: nblcks
            integer, intent(out)              :: blsize(*)
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb03rd
    end interface
    public :: mb03rd
    
    interface
        subroutine mb03rw(m, n, pmax, a, lda, b, ldb, c, &
                       ldc, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: pmax
            complex*16, intent(in)            :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(in)            :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: info
        end subroutine mb03rw
    end interface
    public :: mb03rw
    
    interface
        subroutine mb03rx(jobv, n, kl, ku, a, lda, x, ldx, &
                       wr, wi, dwork)
            character, intent(in)             :: jobv
            integer, intent(in)               :: n
            integer, intent(in)               :: kl
            integer, intent(inout)            :: ku
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb03rx
    end interface
    public :: mb03rx
    
    interface
        subroutine mb03ry(m, n, pmax, a, lda, b, ldb, c, &
                       ldc, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: pmax
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: info
        end subroutine mb03ry
    end interface
    public :: mb03ry
    
    interface
        subroutine mb03rz(jobx, sort, n, pmax, a, lda, x, ldx, &
                       nblcks, blsize, w, tol, info)
            character, intent(in)             :: jobx
            character, intent(in)             :: sort
            integer, intent(in)               :: n
            double precision, intent(in)      :: pmax
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: x(ldx, *)
            integer, intent(in)               :: ldx
            integer, intent(out)              :: nblcks
            integer, intent(out)              :: blsize(*)
            complex*16, intent(out)           :: w(*)
            double precision, intent(in)      :: tol
            integer, intent(out)              :: info
        end subroutine mb03rz
    end interface
    public :: mb03rz
    
    interface
        subroutine mb03sd(jobscl, n, a, lda, qg, ldqg, wr, wi, &
                       dwork, ldwork, info)
            character, intent(in)             :: jobscl
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03sd
    end interface
    public :: mb03sd
    
    interface
        subroutine mb03td(typ, compu, select, lower, n, a, lda, g, &
                       ldg, u1, ldu1, u2, ldu2, wr, wi, m, &
                       dwork, ldwork, info)
            character, intent(in)             :: typ
            character, intent(in)             :: compu
            logical, intent(inout)            :: select(*)
            logical, intent(inout)            :: lower(*)
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(in)               :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(in)               :: ldu2
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            integer, intent(out)              :: m
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03td
    end interface
    public :: mb03td
    
    interface
        subroutine mb03ts(isham, wantu, n, a, lda, g, ldg, u1, &
                       ldu1, u2, ldu2, j1, n1, n2, dwork, info)
            logical, intent(in)               :: isham
            logical, intent(in)               :: wantu
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(in)               :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(in)               :: ldu2
            integer, intent(in)               :: j1
            integer, intent(in)               :: n1
            integer, intent(in)               :: n2
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb03ts
    end interface
    public :: mb03ts
    
    interface
        subroutine mb03ud(jobq, jobp, n, a, lda, q, ldq, sv, &
                       dwork, ldwork, info)
            character, intent(in)             :: jobq
            character, intent(in)             :: jobp
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: sv(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03ud
    end interface
    public :: mb03ud
    
    interface
        subroutine mb03vd(n, p, ilo, ihi, a, lda1, lda2, tau, &
                       ldtau, dwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: p
            integer, intent(in)               :: ilo
            integer, intent(in)               :: ihi
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(in)               :: lda1
            integer, intent(in)               :: lda2
            double precision, intent(out)     :: tau(ldtau, *)
            integer, intent(in)               :: ldtau
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb03vd
    end interface
    public :: mb03vd
    
    interface
        subroutine mb03vw(compq, qind, triu, n, k, h, ilo, ihi, &
                       s, a, lda1, lda2, q, ldq1, ldq2, iwork, &
                       liwork, dwork, ldwork, info)
            character, intent(in)             :: compq
            integer, intent(in)               :: qind(*)
            character, intent(in)             :: triu
            integer, intent(in)               :: n
            integer, intent(in)               :: k
            integer, intent(inout)            :: h
            integer, intent(in)               :: ilo
            integer, intent(in)               :: ihi
            integer, intent(in)               :: s(*)
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(in)               :: lda1
            integer, intent(in)               :: lda2
            double precision, intent(inout)   :: q(ldq1, ldq2, *)
            integer, intent(in)               :: ldq1
            integer, intent(in)               :: ldq2
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(ldwork)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03vw
    end interface
    public :: mb03vw
    
    interface
        subroutine mb03vy(n, p, ilo, ihi, a, lda1, lda2, tau, &
                       ldtau, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: p
            integer, intent(in)               :: ilo
            integer, intent(in)               :: ihi
            double precision, intent(inout)   :: a(lda1, lda2, *)
            integer, intent(in)               :: lda1
            integer, intent(in)               :: lda2
            double precision, intent(in)      :: tau(ldtau, *)
            integer, intent(in)               :: ldtau
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03vy
    end interface
    public :: mb03vy
    
    interface
        subroutine mb03wa(wantq, wantz, n1, n2, a, lda, b, ldb, &
                       q, ldq, z, ldz, info)
            logical, intent(in)               :: wantq
            logical, intent(in)               :: wantz
            integer, intent(in)               :: n1
            integer, intent(in)               :: n2
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: info
        end subroutine mb03wa
    end interface
    public :: mb03wa
    
    interface
        subroutine mb03wd(job, compz, n, p, ilo, ihi, iloz, ihiz, &
                       h, ldh1, ldh2, z, ldz1, ldz2, wr, wi, &
                       dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: compz
            integer, intent(in)               :: n
            integer, intent(in)               :: p
            integer, intent(in)               :: ilo
            integer, intent(in)               :: ihi
            integer, intent(in)               :: iloz
            integer, intent(in)               :: ihiz
            double precision, intent(inout)   :: h(ldh1, ldh2, *)
            integer, intent(in)               :: ldh1
            integer, intent(in)               :: ldh2
            double precision, intent(inout)   :: z(ldz1, ldz2, *)
            integer, intent(in)               :: ldz1
            integer, intent(in)               :: ldz2
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03wd
    end interface
    public :: mb03wd
    
    interface
        subroutine mb03wx(n, p, t, ldt1, ldt2, wr, wi, info)
            integer, intent(in)             :: n
            integer, intent(in)             :: p
            double precision, intent(in)    :: t(ldt1, ldt2, *)
            integer, intent(in)             :: ldt1
            integer, intent(in)             :: ldt2
            double precision, intent(out)   :: wr(*)
            double precision, intent(out)   :: wi(*)
            integer, intent(out)            :: info
        end subroutine mb03wx
    end interface
    public :: mb03wx
    
    interface
        subroutine mb03xd(balanc, job, jobu, jobv, n, a, lda, qg, &
                       ldqg, t, ldt, u1, ldu1, u2, ldu2, v1, &
                       ldv1, v2, ldv2, wr, wi, ilo, scale, dwork, &
                       ldwork, info)
            character, intent(in)             :: balanc
            character, intent(in)             :: job
            character, intent(in)             :: jobu
            character, intent(in)             :: jobv
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            double precision, intent(out)     :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(out)     :: u1(ldu1, *)
            integer, intent(in)               :: ldu1
            double precision, intent(out)     :: u2(ldu2, *)
            integer, intent(in)               :: ldu2
            double precision, intent(out)     :: v1(ldv1, *)
            integer, intent(in)               :: ldv1
            double precision, intent(out)     :: v2(ldv2, *)
            integer, intent(in)               :: ldv2
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            integer, intent(out)              :: ilo
            double precision, intent(out)     :: scale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03xd
    end interface
    public :: mb03xd
    
    interface
        subroutine mb03xp(job, compq, compz, n, ilo, ihi, a, lda, &
                       b, ldb, q, ldq, z, ldz, alphar, alphai, &
                       beta, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: compq
            character, intent(in)             :: compz
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            integer, intent(in)               :: ihi
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03xp
    end interface
    public :: mb03xp
    
    interface
        subroutine mb03xs(jobu, n, a, lda, qg, ldqg, u1, ldu1, &
                       u2, ldu2, wr, wi, dwork, ldwork, info)
            character, intent(in)             :: jobu
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            double precision, intent(out)     :: u1(ldu1, *)
            integer, intent(in)               :: ldu1
            double precision, intent(out)     :: u2(ldu2, *)
            integer, intent(in)               :: ldu2
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03xs
    end interface
    public :: mb03xs
    
    interface
        subroutine mb03xu(ltra, ltrb, n, k, nb, a, lda, b, &
                       ldb, g, ldg, q, ldq, xa, ldxa, xb, &
                       ldxb, xg, ldxg, xq, ldxq, ya, ldya, yb, &
                       ldyb, yg, ldyg, yq, ldyq, csl, csr, taul, &
                       taur, dwork)
            logical, intent(in)               :: ltra
            logical, intent(in)               :: ltrb
            integer, intent(in)               :: n
            integer, intent(in)               :: k
            integer, intent(in)               :: nb
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: xa(ldxa, *)
            integer, intent(in)               :: ldxa
            double precision, intent(out)     :: xb(ldxb, *)
            integer, intent(in)               :: ldxb
            double precision, intent(out)     :: xg(ldxg, *)
            integer, intent(in)               :: ldxg
            double precision, intent(out)     :: xq(ldxq, *)
            integer, intent(in)               :: ldxq
            double precision, intent(out)     :: ya(ldya, *)
            integer, intent(in)               :: ldya
            double precision, intent(out)     :: yb(ldyb, *)
            integer, intent(in)               :: ldyb
            double precision, intent(out)     :: yg(ldyg, *)
            integer, intent(in)               :: ldyg
            double precision, intent(out)     :: yq(ldyq, *)
            integer, intent(in)               :: ldyq
            double precision, intent(out)     :: csl(*)
            double precision, intent(out)     :: csr(*)
            double precision, intent(out)     :: taul(*)
            double precision, intent(out)     :: taur(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb03xu
    end interface
    public :: mb03xu
    
    interface
        subroutine mb03xz(balanc, job, jobu, n, a, lda, qg, ldqg, &
                       u1, ldu1, u2, ldu2, wr, wi, ilo, scale, &
                       dwork, ldwork, zwork, lzwork, bwork, info)
            character, intent(in)             :: balanc
            character, intent(in)             :: job
            character, intent(in)             :: jobu
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            complex*16, intent(out)           :: u1(ldu1, *)
            integer, intent(in)               :: ldu1
            complex*16, intent(out)           :: u2(ldu2, *)
            integer, intent(in)               :: ldu2
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            integer, intent(out)              :: ilo
            double precision, intent(out)     :: scale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine mb03xz
    end interface
    public :: mb03xz
    
    interface
        subroutine mb03ya(wantt, wantq, wantz, n, ilo, ihi, iloq, ihiq, &
                       pos, a, lda, b, ldb, q, ldq, z, &
                       ldz, info)
            logical, intent(in)               :: wantt
            logical, intent(in)               :: wantq
            logical, intent(in)               :: wantz
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            integer, intent(in)               :: ihi
            integer, intent(in)               :: iloq
            integer, intent(in)               :: ihiq
            integer, intent(in)               :: pos
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: info
        end subroutine mb03ya
    end interface
    public :: mb03ya
    
    interface
        subroutine mb03yd(wantt, wantq, wantz, n, ilo, ihi, iloq, ihiq, &
                       a, lda, b, ldb, q, ldq, z, ldz, &
                       alphar, alphai, beta, dwork, ldwork, info)
            logical, intent(in)               :: wantt
            logical, intent(in)               :: wantq
            logical, intent(in)               :: wantz
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            integer, intent(in)               :: ihi
            integer, intent(in)               :: iloq
            integer, intent(in)               :: ihiq
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03yd
    end interface
    public :: mb03yd
    
    interface
        subroutine mb03yt(a, lda, b, ldb, alphar, alphai, beta, csl, &
                       snl, csr, snr)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: alphar(2)
            double precision, intent(out)     :: alphai(2)
            double precision, intent(out)     :: beta(2)
            double precision, intent(out)     :: csl
            double precision, intent(out)     :: snl
            double precision, intent(out)     :: csr
            double precision, intent(out)     :: snr
        end subroutine mb03yt
    end interface
    public :: mb03yt
    
    interface
        subroutine mb03za(compc, compu, compv, compw, which, select, n, a, &
                       lda, b, ldb, c, ldc, u1, ldu1, u2, &
                       ldu2, v1, ldv1, v2, ldv2, w, ldw, wr, &
                       wi, m, dwork, ldwork, info)
            character, intent(in)             :: compc
            character, intent(in)             :: compu
            character, intent(in)             :: compv
            character, intent(in)             :: compw
            character, intent(in)             :: which
            logical, intent(in)               :: select(*)
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(in)               :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(in)               :: ldu2
            double precision, intent(inout)   :: v1(ldv1, *)
            integer, intent(in)               :: ldv1
            double precision, intent(inout)   :: v2(ldv2, *)
            integer, intent(in)               :: ldv2
            double precision, intent(inout)   :: w(ldw, *)
            integer, intent(in)               :: ldw
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            integer, intent(out)              :: m
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03za
    end interface
    public :: mb03za
    
    interface
        subroutine mb03zd(which, meth, stab, balanc, ortbal, select, n, mm, &
                       ilo, scale, s, lds, t, ldt, g, ldg, &
                       u1, ldu1, u2, ldu2, v1, ldv1, v2, ldv2, &
                       m, wr, wi, us, ldus, uu, lduu, lwork, &
                       iwork, dwork, ldwork, info)
            character, intent(in)             :: which
            character, intent(in)             :: meth
            character, intent(in)             :: stab
            character, intent(in)             :: balanc
            character, intent(in)             :: ortbal
            logical, intent(in)               :: select(*)
            integer, intent(in)               :: n
            integer, intent(in)               :: mm
            integer, intent(in)               :: ilo
            double precision, intent(in)      :: scale(*)
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(in)               :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(in)               :: ldu2
            double precision, intent(inout)   :: v1(ldv1, *)
            integer, intent(in)               :: ldv1
            double precision, intent(inout)   :: v2(ldv2, *)
            integer, intent(in)               :: ldv2
            integer, intent(out)              :: m
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            double precision, intent(out)     :: us(ldus, *)
            integer, intent(in)               :: ldus
            double precision, intent(out)     :: uu(lduu, *)
            integer, intent(in)               :: lduu
            logical, intent(inout)            :: lwork(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb03zd
    end interface
    public :: mb03zd
    
    interface
        subroutine mb04ad(job, compq1, compq2, compu1, compu2, n, z, ldz, &
                       h, ldh, q1, ldq1, q2, ldq2, u11, ldu11, &
                       u12, ldu12, u21, ldu21, u22, ldu22, t, ldt, &
                       alphar, alphai, beta, iwork, liwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: compq1
            character, intent(in)             :: compq2
            character, intent(in)             :: compu1
            character, intent(in)             :: compu2
            integer, intent(in)               :: n
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(inout)   :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(in)               :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(in)               :: ldq2
            double precision, intent(inout)   :: u11(ldu11, *)
            integer, intent(in)               :: ldu11
            double precision, intent(inout)   :: u12(ldu12, *)
            integer, intent(in)               :: ldu12
            double precision, intent(inout)   :: u21(ldu21, *)
            integer, intent(in)               :: ldu21
            double precision, intent(inout)   :: u22(ldu22, *)
            integer, intent(in)               :: ldu22
            double precision, intent(out)     :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04ad
    end interface
    public :: mb04ad
    
    interface
        subroutine mb04az(job, compq, compu, n, z, ldz, b, ldb, &
                       fg, ldfg, d, ldd, c, ldc, q, ldq, &
                       u, ldu, alphar, alphai, beta, iwork, liwork, dwork, &
                       ldwork, zwork, lzwork, bwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: compq
            character, intent(in)             :: compu
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: z(ldz, *)
            integer, intent(in)               :: ldz
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: fg(ldfg, *)
            integer, intent(in)               :: ldfg
            complex*16, intent(out)           :: d(ldd, *)
            integer, intent(in)               :: ldd
            complex*16, intent(out)           :: c(ldc, *)
            integer, intent(in)               :: ldc
            complex*16, intent(out)           :: q(ldq, *)
            integer, intent(in)               :: ldq
            complex*16, intent(out)           :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine mb04az
    end interface
    public :: mb04az
    
    interface
        subroutine mb04bd(job, compq1, compq2, n, a, lda, de, ldde, &
                       c1, ldc1, vw, ldvw, q1, ldq1, q2, ldq2, &
                       b, ldb, f, ldf, c2, ldc2, alphar, alphai, &
                       beta, iwork, liwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: compq1
            character, intent(in)             :: compq2
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(in)               :: ldde
            double precision, intent(inout)   :: c1(ldc1, *)
            integer, intent(in)               :: ldc1
            double precision, intent(inout)   :: vw(ldvw, *)
            integer, intent(in)               :: ldvw
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(in)               :: ldq1
            double precision, intent(out)     :: q2(ldq2, *)
            integer, intent(in)               :: ldq2
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(out)     :: c2(ldc2, *)
            integer, intent(in)               :: ldc2
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04bd
    end interface
    public :: mb04bd
    
    interface
        subroutine mb04bp(job, compq1, compq2, n, a, lda, de, ldde, &
                       c1, ldc1, vw, ldvw, q1, ldq1, q2, ldq2, &
                       b, ldb, f, ldf, c2, ldc2, alphar, alphai, &
                       beta, iwork, liwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: compq1
            character, intent(in)             :: compq2
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(in)               :: ldde
            double precision, intent(inout)   :: c1(ldc1, *)
            integer, intent(in)               :: ldc1
            double precision, intent(inout)   :: vw(ldvw, *)
            integer, intent(in)               :: ldvw
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(in)               :: ldq1
            double precision, intent(out)     :: q2(ldq2, *)
            integer, intent(in)               :: ldq2
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(out)     :: c2(ldc2, *)
            integer, intent(in)               :: ldc2
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04bp
    end interface
    public :: mb04bp
    
    interface
        subroutine mb04bz(job, compq, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, q, ldq, alphar, alphai, beta, &
                       iwork, dwork, ldwork, zwork, lzwork, bwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: compq
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: de(ldde, *)
            integer, intent(in)               :: ldde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: fg(ldfg, *)
            integer, intent(in)               :: ldfg
            complex*16, intent(out)           :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine mb04bz
    end interface
    public :: mb04bz
    
    interface
        subroutine mb04cd(compq1, compq2, compq3, n, a, lda, b, ldb, &
                       d, ldd, q1, ldq1, q2, ldq2, q3, ldq3, &
                       iwork, liwork, dwork, ldwork, bwork, info)
            character, intent(in)             :: compq1
            character, intent(in)             :: compq2
            character, intent(in)             :: compq3
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(in)               :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(in)               :: ldq2
            double precision, intent(inout)   :: q3(ldq3, *)
            integer, intent(in)               :: ldq3
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine mb04cd
    end interface
    public :: mb04cd
    
    interface
        subroutine mb04db(job, sgn, n, ilo, lscale, rscale, m, v1, &
                       ldv1, v2, ldv2, info)
            character, intent(in)             :: job
            character, intent(in)             :: sgn
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            double precision, intent(in)      :: lscale(*)
            double precision, intent(in)      :: rscale(*)
            integer, intent(in)               :: m
            double precision, intent(inout)   :: v1(ldv1, *)
            integer, intent(in)               :: ldv1
            double precision, intent(inout)   :: v2(ldv2, *)
            integer, intent(in)               :: ldv2
            integer, intent(out)              :: info
        end subroutine mb04db
    end interface
    public :: mb04db
    
    interface
        subroutine mb04dd(job, n, a, lda, qg, ldqg, ilo, scale, &
                       info)
            character, intent(in)             :: job
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            integer, intent(out)              :: ilo
            double precision, intent(out)     :: scale(*)
            integer, intent(out)              :: info
        end subroutine mb04dd
    end interface
    public :: mb04dd
    
    interface
        subroutine mb04di(job, sgn, n, ilo, scale, m, v1, ldv1, &
                       v2, ldv2, info)
            character, intent(in)             :: job
            character, intent(in)             :: sgn
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            double precision, intent(in)      :: scale(*)
            integer, intent(in)               :: m
            double precision, intent(inout)   :: v1(ldv1, *)
            integer, intent(in)               :: ldv1
            double precision, intent(inout)   :: v2(ldv2, *)
            integer, intent(in)               :: ldv2
            integer, intent(out)              :: info
        end subroutine mb04di
    end interface
    public :: mb04di
    
    interface
        subroutine mb04dl(job, n, thresh, a, lda, b, ldb, ilo, &
                       ihi, lscale, rscale, dwork, iwarn, info)
            character, intent(in)             :: job
            integer, intent(in)               :: n
            double precision, intent(in)      :: thresh
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            integer, intent(out)              :: ilo
            integer, intent(out)              :: ihi
            double precision, intent(out)     :: lscale(*)
            double precision, intent(out)     :: rscale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mb04dl
    end interface
    public :: mb04dl
    
    interface
        subroutine mb04dp(job, n, thresh, a, lda, de, ldde, c, &
                       ldc, vw, ldvw, ilo, lscale, rscale, dwork, iwarn, &
                       info)
            character, intent(in)             :: job
            integer, intent(in)               :: n
            double precision, intent(in)      :: thresh
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(in)               :: ldde
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: vw(ldvw, *)
            integer, intent(in)               :: ldvw
            integer, intent(out)              :: ilo
            double precision, intent(out)     :: lscale(*)
            double precision, intent(out)     :: rscale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mb04dp
    end interface
    public :: mb04dp
    
    interface
        subroutine mb04ds(job, n, a, lda, qg, ldqg, ilo, scale, &
                       info)
            character, intent(in)             :: job
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            integer, intent(out)              :: ilo
            double precision, intent(out)     :: scale(*)
            integer, intent(out)              :: info
        end subroutine mb04ds
    end interface
    public :: mb04ds
    
    interface
        subroutine mb04dy(jobscl, n, a, lda, qg, ldqg, d, dwork, &
                       info)
            character, intent(in)             :: jobscl
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            double precision, intent(out)     :: d(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb04dy
    end interface
    public :: mb04dy
    
    interface
        subroutine mb04dz(job, n, a, lda, qg, ldqg, ilo, scale, &
                       info)
            character, intent(in)             :: job
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            integer, intent(out)              :: ilo
            double precision, intent(out)     :: scale(*)
            integer, intent(out)              :: info
        end subroutine mb04dz
    end interface
    public :: mb04dz
    
    interface
        subroutine mb04ed(job, compq, compu, n, z, ldz, b, ldb, &
                       fg, ldfg, q, ldq, u1, ldu1, u2, ldu2, &
                       alphar, alphai, beta, iwork, liwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: compq
            character, intent(in)             :: compu
            integer, intent(in)               :: n
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: fg(ldfg, *)
            integer, intent(in)               :: ldfg
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(in)               :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(in)               :: ldu2
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04ed
    end interface
    public :: mb04ed
    
    interface
        subroutine mb04fd(job, compq, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, q, ldq, alphar, alphai, beta, &
                       iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: compq
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(in)               :: ldde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: fg(ldfg, *)
            integer, intent(in)               :: ldfg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04fd
    end interface
    public :: mb04fd
    
    interface
        subroutine mb04fp(job, compq, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, q, ldq, alphar, alphai, beta, &
                       iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: compq
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: de(ldde, *)
            integer, intent(in)               :: ldde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: fg(ldfg, *)
            integer, intent(in)               :: ldfg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04fp
    end interface
    public :: mb04fp
    
    interface
        subroutine mb04gd(m, n, a, lda, jpvt, tau, dwork, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            integer, intent(inout)            :: jpvt(*)
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb04gd
    end interface
    public :: mb04gd
    
    interface
        subroutine mb04hd(compq1, compq2, n, a, lda, b, ldb, q1, &
                       ldq1, q2, ldq2, iwork, liwork, dwork, ldwork, bwork, &
                       info)
            character, intent(in)             :: compq1
            character, intent(in)             :: compq2
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(in)               :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(in)               :: ldq2
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine mb04hd
    end interface
    public :: mb04hd
    
    interface
        subroutine mb04id(n, m, p, l, a, lda, b, ldb, &
                       tau, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: l
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04id
    end interface
    public :: mb04id
    
    interface
        subroutine mb04iy(side, trans, n, m, k, p, a, lda, &
                       tau, c, ldc, dwork, ldwork, info)
            character, intent(in)             :: side
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: k
            integer, intent(in)               :: p
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: tau(*)
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04iy
    end interface
    public :: mb04iy
    
    interface
        subroutine mb04iz(n, m, p, l, a, lda, b, ldb, &
                       tau, zwork, lzwork, info)
            integer, intent(in)         :: n
            integer, intent(in)         :: m
            integer, intent(in)         :: p
            integer, intent(in)         :: l
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(in)         :: lda
            complex*16, intent(inout)   :: b(ldb, *)
            integer, intent(in)         :: ldb
            complex*16, intent(out)     :: tau(*)
            complex*16, intent(inout)   :: zwork(*)
            integer, intent(in)         :: lzwork
            integer, intent(out)        :: info
        end subroutine mb04iz
    end interface
    public :: mb04iz
    
    interface
        subroutine mb04jd(n, m, p, l, a, lda, b, ldb, &
                       tau, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: l
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04jd
    end interface
    public :: mb04jd
    
    interface
        subroutine mb04kd(uplo, n, m, p, r, ldr, a, lda, &
                       b, ldb, c, ldc, tau, dwork)
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04kd
    end interface
    public :: mb04kd
    
    interface
        subroutine mb04ld(uplo, n, m, p, l, ldl, a, lda, &
                       b, ldb, c, ldc, tau, dwork)
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: l(ldl, *)
            integer, intent(in)               :: ldl
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04ld
    end interface
    public :: mb04ld
    
    interface
        subroutine mb04md(n, maxred, a, lda, scale, info)
            integer, intent(in)               :: n
            double precision, intent(inout)   :: maxred
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: scale(*)
            integer, intent(out)              :: info
        end subroutine mb04md
    end interface
    public :: mb04md
    
    interface
        subroutine mb04nd(uplo, n, m, p, r, ldr, a, lda, &
                       b, ldb, c, ldc, tau, dwork)
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04nd
    end interface
    public :: mb04nd
    
    interface
        subroutine mb04ny(m, n, v, incv, tau, a, lda, b, &
                       ldb, dwork)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: v(*)
            integer, intent(in)               :: incv
            double precision, intent(in)      :: tau
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04ny
    end interface
    public :: mb04ny
    
    interface
        subroutine mb04od(uplo, n, m, p, r, ldr, a, lda, &
                       b, ldb, c, ldc, tau, dwork)
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04od
    end interface
    public :: mb04od
    
    interface
        subroutine mb04ow(m, n, p, a, lda, t, ldt, x, &
                       incx, b, ldb, c, ldc, d, incd)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: x(*)
            integer, intent(in)               :: incx
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(*)
            integer, intent(in)               :: incd
        end subroutine mb04ow
    end interface
    public :: mb04ow
    
    interface
        subroutine mb04ox(n, a, lda, x, incx)
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: x(*)
            integer, intent(in)               :: incx
        end subroutine mb04ox
    end interface
    public :: mb04ox
    
    interface
        subroutine mb04oy(m, n, v, tau, a, lda, b, ldb, &
                       dwork)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: v(*)
            double precision, intent(in)      :: tau
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04oy
    end interface
    public :: mb04oy
    
    interface
        subroutine mb04pa(lham, n, k, nb, a, lda, qg, ldqg, &
                       xa, ldxa, xg, ldxg, xq, ldxq, ya, ldya, &
                       cs, tau, dwork)
            logical, intent(in)               :: lham
            integer, intent(in)               :: n
            integer, intent(in)               :: k
            integer, intent(in)               :: nb
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            double precision, intent(out)     :: xa(ldxa, *)
            integer, intent(in)               :: ldxa
            double precision, intent(out)     :: xg(ldxg, *)
            integer, intent(in)               :: ldxg
            double precision, intent(out)     :: xq(ldxq, *)
            integer, intent(in)               :: ldxq
            double precision, intent(out)     :: ya(ldya, *)
            integer, intent(in)               :: ldya
            double precision, intent(out)     :: cs(*)
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04pa
    end interface
    public :: mb04pa
    
    interface
        subroutine mb04pb(n, ilo, a, lda, qg, ldqg, cs, tau, &
                       dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            double precision, intent(out)     :: cs(*)
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04pb
    end interface
    public :: mb04pb
    
    interface
        subroutine mb04pu(n, ilo, a, lda, qg, ldqg, cs, tau, &
                       dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            double precision, intent(out)     :: cs(*)
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04pu
    end interface
    public :: mb04pu
    
    interface
        subroutine mb04py(side, m, n, v, tau, c, ldc, dwork)
            character, intent(in)             :: side
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: v(*)
            double precision, intent(in)      :: tau
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04py
    end interface
    public :: mb04py
    
    interface
        subroutine mb04qb(tranc, trand, tranq, storev, storew, m, n, k, &
                       v, ldv, w, ldw, c, ldc, d, ldd, &
                       cs, tau, dwork, ldwork, info)
            character, intent(in)             :: tranc
            character, intent(in)             :: trand
            character, intent(in)             :: tranq
            character, intent(in)             :: storev
            character, intent(in)             :: storew
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: k
            double precision, intent(in)      :: v(ldv, *)
            integer, intent(in)               :: ldv
            double precision, intent(in)      :: w(ldw, *)
            integer, intent(in)               :: ldw
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: cs(*)
            double precision, intent(in)      :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04qb
    end interface
    public :: mb04qb
    
    interface
        subroutine mb04qc(strab, trana, tranb, tranq, direct, storev, storew, m, &
                       n, k, v, ldv, w, ldw, rs, ldrs, &
                       t, ldt, a, lda, b, ldb, dwork)
            character, intent(in)             :: strab
            character, intent(in)             :: trana
            character, intent(in)             :: tranb
            character, intent(in)             :: tranq
            character, intent(in)             :: direct
            character, intent(in)             :: storev
            character, intent(in)             :: storew
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: k
            double precision, intent(in)      :: v(ldv, *)
            integer, intent(in)               :: ldv
            double precision, intent(in)      :: w(ldw, *)
            integer, intent(in)               :: ldw
            double precision, intent(in)      :: rs(ldrs, *)
            integer, intent(in)               :: ldrs
            double precision, intent(in)      :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04qc
    end interface
    public :: mb04qc
    
    interface
        subroutine mb04qf(direct, storev, storew, n, k, v, ldv, w, &
                       ldw, cs, tau, rs, ldrs, t, ldt, dwork)
            character, intent(in)             :: direct
            character, intent(in)             :: storev
            character, intent(in)             :: storew
            integer, intent(in)               :: n
            integer, intent(in)               :: k
            double precision, intent(in)      :: v(ldv, *)
            integer, intent(in)               :: ldv
            double precision, intent(in)      :: w(ldw, *)
            integer, intent(in)               :: ldw
            double precision, intent(in)      :: cs(*)
            double precision, intent(in)      :: tau(*)
            double precision, intent(out)     :: rs(ldrs, *)
            integer, intent(in)               :: ldrs
            double precision, intent(out)     :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04qf
    end interface
    public :: mb04qf
    
    interface
        subroutine mb04qs(tranc, trand, tranu, m, n, ilo, v, ldv, &
                       w, ldw, c, ldc, d, ldd, cs, tau, &
                       dwork, ldwork, info)
            character, intent(in)             :: tranc
            character, intent(in)             :: trand
            character, intent(in)             :: tranu
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            double precision, intent(in)      :: v(ldv, *)
            integer, intent(in)               :: ldv
            double precision, intent(in)      :: w(ldw, *)
            integer, intent(in)               :: ldw
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: cs(*)
            double precision, intent(in)      :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04qs
    end interface
    public :: mb04qs
    
    interface
        subroutine mb04qu(tranc, trand, tranq, storev, storew, m, n, k, &
                       v, ldv, w, ldw, c, ldc, d, ldd, &
                       cs, tau, dwork, ldwork, info)
            character, intent(in)             :: tranc
            character, intent(in)             :: trand
            character, intent(in)             :: tranq
            character, intent(in)             :: storev
            character, intent(in)             :: storew
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: k
            double precision, intent(in)      :: v(ldv, *)
            integer, intent(in)               :: ldv
            double precision, intent(in)      :: w(ldw, *)
            integer, intent(in)               :: ldw
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: cs(*)
            double precision, intent(in)      :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04qu
    end interface
    public :: mb04qu
    
    interface
        subroutine mb04rb(n, ilo, a, lda, qg, ldqg, cs, tau, &
                       dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            double precision, intent(out)     :: cs(*)
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04rb
    end interface
    public :: mb04rb
    
    interface
        subroutine mb04rd(jobx, joby, sort, n, pmax, a, lda, b, &
                       ldb, x, ldx, y, ldy, nblcks, blsize, alphar, &
                       alphai, beta, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: jobx
            character, intent(in)             :: joby
            character, intent(in)             :: sort
            integer, intent(in)               :: n
            double precision, intent(in)      :: pmax
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(in)               :: ldy
            integer, intent(out)              :: nblcks
            integer, intent(out)              :: blsize(*)
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04rd
    end interface
    public :: mb04rd
    
    interface
        subroutine mb04rs(m, n, pmax, a, lda, b, ldb, c, &
                       ldc, d, ldd, e, lde, f, ldf, scale, &
                       iwork, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: pmax
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(out)     :: scale
            integer, intent(inout)            :: iwork(*)
            integer, intent(out)              :: info
        end subroutine mb04rs
    end interface
    public :: mb04rs
    
    interface
        subroutine mb04rt(m, n, pmax, a, lda, b, ldb, c, &
                       ldc, d, ldd, e, lde, f, ldf, scale, &
                       iwork, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: pmax
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(out)     :: scale
            integer, intent(inout)            :: iwork(*)
            integer, intent(out)              :: info
        end subroutine mb04rt
    end interface
    public :: mb04rt
    
    interface
        subroutine mb04ru(n, ilo, a, lda, qg, ldqg, cs, tau, &
                       dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            double precision, intent(out)     :: cs(*)
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04ru
    end interface
    public :: mb04ru
    
    interface
        subroutine mb04rv(m, n, pmax, a, lda, b, ldb, c, &
                       ldc, d, ldd, e, lde, f, ldf, scale, &
                       info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: pmax
            complex*16, intent(in)            :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(in)            :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(in)               :: ldc
            complex*16, intent(in)            :: d(ldd, *)
            integer, intent(in)               :: ldd
            complex*16, intent(in)            :: e(lde, *)
            integer, intent(in)               :: lde
            complex*16, intent(inout)         :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(out)     :: scale
            integer, intent(out)              :: info
        end subroutine mb04rv
    end interface
    public :: mb04rv
    
    interface
        subroutine mb04rw(m, n, pmax, a, lda, b, ldb, c, &
                       ldc, d, ldd, e, lde, f, ldf, scale, &
                       iwork, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: pmax
            complex*16, intent(in)            :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(in)            :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(in)               :: ldc
            complex*16, intent(in)            :: d(ldd, *)
            integer, intent(in)               :: ldd
            complex*16, intent(in)            :: e(lde, *)
            integer, intent(in)               :: lde
            complex*16, intent(inout)         :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(out)     :: scale
            integer, intent(inout)            :: iwork(*)
            integer, intent(out)              :: info
        end subroutine mb04rw
    end interface
    public :: mb04rw
    
    interface
        subroutine mb04rz(jobx, joby, sort, n, pmax, a, lda, b, &
                       ldb, x, ldx, y, ldy, nblcks, blsize, alpha, &
                       beta, tol, iwork, info)
            character, intent(in)             :: jobx
            character, intent(in)             :: joby
            character, intent(in)             :: sort
            integer, intent(in)               :: n
            double precision, intent(in)      :: pmax
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: x(ldx, *)
            integer, intent(in)               :: ldx
            complex*16, intent(inout)         :: y(ldy, *)
            integer, intent(in)               :: ldy
            integer, intent(out)              :: nblcks
            integer, intent(out)              :: blsize(*)
            complex*16, intent(out)           :: alpha(*)
            complex*16, intent(out)           :: beta(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            integer, intent(out)              :: info
        end subroutine mb04rz
    end interface
    public :: mb04rz
    
    interface
        subroutine mb04su(m, n, a, lda, b, ldb, cs, tau, &
                       dwork, ldwork, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: cs(*)
            double precision, intent(out)     :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04su
    end interface
    public :: mb04su
    
    interface
        subroutine mb04tb(trana, tranb, n, ilo, a, lda, b, ldb, &
                       g, ldg, q, ldq, csl, csr, taul, taur, &
                       dwork, ldwork, info)
            character, intent(in)             :: trana
            character, intent(in)             :: tranb
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: csl(*)
            double precision, intent(out)     :: csr(*)
            double precision, intent(out)     :: taul(*)
            double precision, intent(out)     :: taur(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04tb
    end interface
    public :: mb04tb
    
    interface
        subroutine mb04ts(trana, tranb, n, ilo, a, lda, b, ldb, &
                       g, ldg, q, ldq, csl, csr, taul, taur, &
                       dwork, ldwork, info)
            character, intent(in)             :: trana
            character, intent(in)             :: tranb
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: csl(*)
            double precision, intent(out)     :: csr(*)
            double precision, intent(out)     :: taul(*)
            double precision, intent(out)     :: taur(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04ts
    end interface
    public :: mb04ts
    
    interface
        subroutine mb04tt(updatq, updatz, m, n, ifira, ifica, nca, a, &
                       lda, e, lde, q, ldq, z, ldz, istair, &
                       rank, tol, iwork)
            logical, intent(in)               :: updatq
            logical, intent(in)               :: updatz
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: ifira
            integer, intent(in)               :: ifica
            integer, intent(in)               :: nca
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(inout)            :: istair(*)
            integer, intent(out)              :: rank
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
        end subroutine mb04tt
    end interface
    public :: mb04tt
    
    interface
        subroutine mb04tu(n, x, incx, y, incy, c, s)
            integer, intent(in)               :: n
            double precision, intent(inout)   :: x(*)
            integer, intent(in)               :: incx
            double precision, intent(inout)   :: y(*)
            integer, intent(in)               :: incy
            double precision, intent(in)      :: c
            double precision, intent(in)      :: s
        end subroutine mb04tu
    end interface
    public :: mb04tu
    
    interface
        subroutine mb04tv(updatz, n, nra, nca, ifira, ifica, a, lda, &
                       e, lde, z, ldz)
            logical, intent(in)               :: updatz
            integer, intent(in)               :: n
            integer, intent(in)               :: nra
            integer, intent(in)               :: nca
            integer, intent(in)               :: ifira
            integer, intent(in)               :: ifica
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
        end subroutine mb04tv
    end interface
    public :: mb04tv
    
    interface
        subroutine mb04tw(updatq, m, n, nre, nce, ifire, ifice, ifica, &
                       a, lda, e, lde, q, ldq)
            logical, intent(in)               :: updatq
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: nre
            integer, intent(in)               :: nce
            integer, intent(in)               :: ifire
            integer, intent(in)               :: ifice
            integer, intent(in)               :: ifica
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
        end subroutine mb04tw
    end interface
    public :: mb04tw
    
    interface
        subroutine mb04tx(updatq, updatz, m, n, nblcks, inuk, imuk, a, &
                       lda, e, lde, q, ldq, z, ldz, mnei)
            logical, intent(in)               :: updatq
            logical, intent(in)               :: updatz
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(inout)            :: nblcks
            integer, intent(inout)            :: inuk(*)
            integer, intent(inout)            :: imuk(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: mnei(4)
        end subroutine mb04tx
    end interface
    public :: mb04tx
    
    interface
        subroutine mb04ty(updatq, updatz, m, n, nblcks, inuk, imuk, a, &
                       lda, e, lde, q, ldq, z, ldz, info)
            logical, intent(in)               :: updatq
            logical, intent(in)               :: updatz
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: nblcks
            integer, intent(in)               :: inuk(*)
            integer, intent(in)               :: imuk(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: info
        end subroutine mb04ty
    end interface
    public :: mb04ty
    
    interface
        subroutine mb04ud(jobq, jobz, m, n, a, lda, e, lde, &
                       q, ldq, z, ldz, ranke, istair, tol, dwork, &
                       info)
            character, intent(in)             :: jobq
            character, intent(in)             :: jobz
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: ranke
            integer, intent(out)              :: istair(*)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb04ud
    end interface
    public :: mb04ud
    
    interface
        subroutine mb04vd(mode, jobq, jobz, m, n, ranke, a, lda, &
                       e, lde, q, ldq, z, ldz, istair, nblcks, &
                       nblcki, imuk, inuk, imuk0, mnei, tol, iwork, info)
            character, intent(in)             :: mode
            character, intent(in)             :: jobq
            character, intent(in)             :: jobz
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: ranke
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(inout)            :: istair(*)
            integer, intent(out)              :: nblcks
            integer, intent(out)              :: nblcki
            integer, intent(out)              :: imuk(*)
            integer, intent(out)              :: inuk(*)
            integer, intent(out)              :: imuk0(*)
            integer, intent(out)              :: mnei(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            integer, intent(out)              :: info
        end subroutine mb04vd
    end interface
    public :: mb04vd
    
    interface
        subroutine mb04vx(updatq, updatz, m, n, nblcks, inuk, imuk, a, &
                       lda, e, lde, q, ldq, z, ldz, mnei)
            logical, intent(in)               :: updatq
            logical, intent(in)               :: updatz
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: nblcks
            integer, intent(inout)            :: inuk(*)
            integer, intent(inout)            :: imuk(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: mnei(3)
        end subroutine mb04vx
    end interface
    public :: mb04vx
    
    interface
        subroutine mb04wd(tranq1, tranq2, m, n, k, q1, ldq1, q2, &
                       ldq2, cs, tau, dwork, ldwork, info)
            character, intent(in)             :: tranq1
            character, intent(in)             :: tranq2
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: k
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(in)               :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(in)               :: ldq2
            double precision, intent(in)      :: cs(*)
            double precision, intent(in)      :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04wd
    end interface
    public :: mb04wd
    
    interface
        subroutine mb04wp(n, ilo, u1, ldu1, u2, ldu2, cs, tau, &
                       dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            double precision, intent(inout)   :: u1(ldu1, *)
            integer, intent(in)               :: ldu1
            double precision, intent(inout)   :: u2(ldu2, *)
            integer, intent(in)               :: ldu2
            double precision, intent(in)      :: cs(*)
            double precision, intent(in)      :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04wp
    end interface
    public :: mb04wp
    
    interface
        subroutine mb04wr(job, trans, n, ilo, q1, ldq1, q2, ldq2, &
                       cs, tau, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(in)               :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(in)               :: ldq2
            double precision, intent(in)      :: cs(*)
            double precision, intent(in)      :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04wr
    end interface
    public :: mb04wr
    
    interface
        subroutine mb04wu(tranq1, tranq2, m, n, k, q1, ldq1, q2, &
                       ldq2, cs, tau, dwork, ldwork, info)
            character, intent(in)             :: tranq1
            character, intent(in)             :: tranq2
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: k
            double precision, intent(inout)   :: q1(ldq1, *)
            integer, intent(in)               :: ldq1
            double precision, intent(inout)   :: q2(ldq2, *)
            integer, intent(in)               :: ldq2
            double precision, intent(in)      :: cs(*)
            double precision, intent(in)      :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb04wu
    end interface
    public :: mb04wu
    
    interface
        subroutine mb04xd(jobu, jobv, m, n, rank, theta, a, lda, &
                       u, ldu, v, ldv, q, inul, tol, reltol, &
                       dwork, ldwork, iwarn, info)
            character, intent(in)             :: jobu
            character, intent(in)             :: jobv
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: theta
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: v(ldv, *)
            integer, intent(in)               :: ldv
            double precision, intent(out)     :: q(*)
            logical, intent(out)              :: inul(*)
            double precision, intent(in)      :: tol
            double precision, intent(in)      :: reltol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mb04xd
    end interface
    public :: mb04xd
    
    interface
        subroutine mb04xy(jobu, jobv, m, n, x, ldx, taup, tauq, &
                       u, ldu, v, ldv, inul, info)
            character, intent(in)             :: jobu
            character, intent(in)             :: jobv
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(in)      :: taup(*)
            double precision, intent(in)      :: tauq(*)
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(in)               :: ldv
            logical, intent(in)               :: inul(*)
            integer, intent(out)              :: info
        end subroutine mb04xy
    end interface
    public :: mb04xy
    
    interface
        subroutine mb04yd(jobu, jobv, m, n, rank, theta, q, e, &
                       u, ldu, v, ldv, inul, tol, reltol, dwork, &
                       ldwork, iwarn, info)
            character, intent(in)             :: jobu
            character, intent(in)             :: jobv
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(inout)            :: rank
            double precision, intent(inout)   :: theta
            double precision, intent(inout)   :: q(*)
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(in)               :: ldv
            logical, intent(inout)            :: inul(*)
            double precision, intent(in)      :: tol
            double precision, intent(in)      :: reltol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mb04yd
    end interface
    public :: mb04yd
    
    interface
        subroutine mb04yw(qrit, updatu, updatv, m, n, l, k, shift, &
                       d, e, u, ldu, v, ldv, dwork)
            logical, intent(in)               :: qrit
            logical, intent(in)               :: updatu
            logical, intent(in)               :: updatv
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: l
            integer, intent(in)               :: k
            double precision, intent(in)      :: shift
            double precision, intent(inout)   :: d(*)
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(in)               :: ldv
            double precision, intent(inout)   :: dwork(*)
        end subroutine mb04yw
    end interface
    public :: mb04yw
    
    interface
        subroutine mb04zd(compu, n, a, lda, qg, ldqg, u, ldu, &
                       dwork, info)
            character, intent(in)             :: compu
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: qg(ldqg, *)
            integer, intent(in)               :: ldqg
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mb04zd
    end interface
    public :: mb04zd
    
    interface
        subroutine mb05md(balanc, n, delta, a, lda, v, ldv, y, &
                       ldy, valr, vali, iwork, dwork, ldwork, info)
            character, intent(in)             :: balanc
            integer, intent(in)               :: n
            double precision, intent(in)      :: delta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: v(ldv, *)
            integer, intent(in)               :: ldv
            double precision, intent(out)     :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(out)     :: valr(*)
            double precision, intent(out)     :: vali(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb05md
    end interface
    public :: mb05md
    
    interface
        subroutine mb05my(balanc, n, a, lda, wr, wi, r, ldr, &
                       q, ldq, dwork, ldwork, info)
            character, intent(in)             :: balanc
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            double precision, intent(out)     :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb05my
    end interface
    public :: mb05my
    
    interface
        subroutine mb05nd(n, delta, a, lda, ex, ldex, exint, ldexin, &
                       tol, iwork, dwork, ldwork, info)
            integer, intent(in)               :: n
            double precision, intent(in)      :: delta
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: ex(ldex, *)
            integer, intent(in)               :: ldex
            double precision, intent(out)     :: exint(ldexin, *)
            integer, intent(in)               :: ldexin
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mb05nd
    end interface
    public :: mb05nd
    
    interface
        subroutine mb05od(balanc, n, ndiag, delta, a, lda, mdig, idig, &
                       iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: balanc
            integer, intent(in)               :: n
            integer, intent(in)               :: ndiag
            double precision, intent(in)      :: delta
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            integer, intent(out)              :: mdig
            integer, intent(out)              :: idig
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mb05od
    end interface
    public :: mb05od
    
    interface
        subroutine mb05oy(job, n, low, igh, a, lda, scale, info)
            character, intent(in)             :: job
            integer, intent(in)               :: n
            integer, intent(in)               :: low
            integer, intent(in)               :: igh
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: scale(*)
            integer, intent(out)              :: info
        end subroutine mb05oy
    end interface
    public :: mb05oy
    
    interface
        subroutine mb3jzp(compq, n, a, lda, d, ldd, b, ldb, &
                       f, ldf, q, ldq, neig, tol, dwork, zwork, &
                       info)
            character, intent(in)             :: compq
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: d(ldd, *)
            integer, intent(in)               :: ldd
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: f(ldf, *)
            integer, intent(in)               :: ldf
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(in)               :: ldq
            integer, intent(out)              :: neig
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(out)              :: info
        end subroutine mb3jzp
    end interface
    public :: mb3jzp
    
    interface
        subroutine mb3lzp(compq, orth, n, a, lda, de, ldde, b, &
                       ldb, fg, ldfg, neig, q, ldq, alphar, alphai, &
                       beta, iwork, dwork, ldwork, zwork, lzwork, bwork, info)
            character, intent(in)             :: compq
            character, intent(in)             :: orth
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: de(ldde, *)
            integer, intent(in)               :: ldde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: fg(ldfg, *)
            integer, intent(in)               :: ldfg
            integer, intent(out)              :: neig
            complex*16, intent(out)           :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine mb3lzp
    end interface
    public :: mb3lzp
    
    interface
        subroutine mb3oyz(m, n, a, lda, rcond, svlmax, rank, sval, &
                       jpvt, tau, dwork, zwork, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: rcond
            double precision, intent(in)      :: svlmax
            integer, intent(out)              :: rank
            double precision, intent(out)     :: sval(3)
            integer, intent(out)              :: jpvt(*)
            complex*16, intent(out)           :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(out)              :: info
        end subroutine mb3oyz
    end interface
    public :: mb3oyz
    
    interface
        subroutine mb3pyz(m, n, a, lda, rcond, svlmax, rank, sval, &
                       jpvt, tau, dwork, zwork, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: rcond
            double precision, intent(in)      :: svlmax
            integer, intent(out)              :: rank
            double precision, intent(out)     :: sval(3)
            integer, intent(out)              :: jpvt(*)
            complex*16, intent(out)           :: tau(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(out)              :: info
        end subroutine mb3pyz
    end interface
    public :: mb3pyz
    
    interface
        subroutine mb4dbz(job, sgn, n, ilo, lscale, rscale, m, v1, &
                       ldv1, v2, ldv2, info)
            character, intent(in)             :: job
            character, intent(in)             :: sgn
            integer, intent(in)               :: n
            integer, intent(in)               :: ilo
            double precision, intent(in)      :: lscale(*)
            double precision, intent(in)      :: rscale(*)
            integer, intent(in)               :: m
            complex*16, intent(inout)         :: v1(ldv1, *)
            integer, intent(in)               :: ldv1
            complex*16, intent(inout)         :: v2(ldv2, *)
            integer, intent(in)               :: ldv2
            integer, intent(out)              :: info
        end subroutine mb4dbz
    end interface
    public :: mb4dbz
    
    interface
        subroutine mb4dlz(job, n, thresh, a, lda, b, ldb, ilo, &
                       ihi, lscale, rscale, dwork, iwarn, info)
            character, intent(in)             :: job
            integer, intent(in)               :: n
            double precision, intent(in)      :: thresh
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            integer, intent(out)              :: ilo
            integer, intent(out)              :: ihi
            double precision, intent(out)     :: lscale(*)
            double precision, intent(out)     :: rscale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mb4dlz
    end interface
    public :: mb4dlz
    
    interface
        subroutine mb4dpz(job, n, thresh, a, lda, de, ldde, c, &
                       ldc, vw, ldvw, ilo, lscale, rscale, dwork, iwarn, &
                       info)
            character, intent(in)             :: job
            integer, intent(in)               :: n
            double precision, intent(in)      :: thresh
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: de(ldde, *)
            integer, intent(in)               :: ldde
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(in)               :: ldc
            complex*16, intent(inout)         :: vw(ldvw, *)
            integer, intent(in)               :: ldvw
            integer, intent(out)              :: ilo
            double precision, intent(out)     :: lscale(*)
            double precision, intent(out)     :: rscale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mb4dpz
    end interface
    public :: mb4dpz
    
    interface
        subroutine mc01md(dp, alpha, k, p, q, info)
            integer, intent(in)             :: dp
            double precision, intent(in)    :: alpha
            integer, intent(in)             :: k
            double precision, intent(in)    :: p(*)
            double precision, intent(out)   :: q(*)
            integer, intent(out)            :: info
        end subroutine mc01md
    end interface
    public :: mc01md
    
    interface
        subroutine mc01nd(dp, xr, xi, p, vr, vi, info)
            integer, intent(in)             :: dp
            double precision, intent(in)    :: xr
            double precision, intent(in)    :: xi
            double precision, intent(in)    :: p(*)
            double precision, intent(out)   :: vr
            double precision, intent(out)   :: vi
            integer, intent(out)            :: info
        end subroutine mc01nd
    end interface
    public :: mc01nd
    
    interface
        subroutine mc01od(k, rez, imz, rep, imp, dwork, info)
            integer, intent(in)               :: k
            double precision, intent(in)      :: rez(*)
            double precision, intent(in)      :: imz(*)
            double precision, intent(out)     :: rep(*)
            double precision, intent(out)     :: imp(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mc01od
    end interface
    public :: mc01od
    
    interface
        subroutine mc01pd(k, rez, imz, p, dwork, info)
            integer, intent(in)               :: k
            double precision, intent(in)      :: rez(*)
            double precision, intent(in)      :: imz(*)
            double precision, intent(out)     :: p(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mc01pd
    end interface
    public :: mc01pd
    
    interface
        subroutine mc01py(k, rez, imz, p, dwork, info)
            integer, intent(in)               :: k
            double precision, intent(in)      :: rez(*)
            double precision, intent(in)      :: imz(*)
            double precision, intent(out)     :: p(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mc01py
    end interface
    public :: mc01py
    
    interface
        subroutine mc01qd(da, db, a, b, rq, iwarn, info)
            integer, intent(in)               :: da
            integer, intent(inout)            :: db
            double precision, intent(in)      :: a(*)
            double precision, intent(in)      :: b(*)
            double precision, intent(out)     :: rq(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mc01qd
    end interface
    public :: mc01qd
    
    interface
        subroutine mc01rd(dp1, dp2, dp3, alpha, p1, p2, p3, info)
            integer, intent(in)               :: dp1
            integer, intent(in)               :: dp2
            integer, intent(inout)            :: dp3
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: p1(*)
            double precision, intent(in)      :: p2(*)
            double precision, intent(inout)   :: p3(*)
            integer, intent(out)              :: info
        end subroutine mc01rd
    end interface
    public :: mc01rd
    
    interface
        subroutine mc01sd(dp, p, s, t, mant, e, iwork, info)
            integer, intent(in)               :: dp
            double precision, intent(inout)   :: p(*)
            integer, intent(out)              :: s
            integer, intent(out)              :: t
            double precision, intent(out)     :: mant(*)
            integer, intent(out)              :: e(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(out)              :: info
        end subroutine mc01sd
    end interface
    public :: mc01sd
    
    interface
        subroutine mc01sw(a, b, m, e)
            double precision, intent(in)    :: a
            integer, intent(in)             :: b
            double precision, intent(out)   :: m
            integer, intent(out)            :: e
        end subroutine mc01sw
    end interface
    public :: mc01sw
    
    interface
        integer function mc01sx (lb,ub,e,mant)
            integer, intent(in)            :: lb
            integer, intent(in)            :: ub
            integer, intent(in)            :: e(*)
            double precision, intent(in)   :: mant(*)
        end function mc01sx
    end interface
    public :: mc01sx
    
    interface
        subroutine mc01sy(m, e, b, a, ovflow)
            double precision, intent(in)    :: m
            integer, intent(in)             :: e
            integer, intent(in)             :: b
            double precision, intent(out)   :: a
            logical, intent(out)            :: ovflow
        end subroutine mc01sy
    end interface
    public :: mc01sy
    
    interface
        subroutine mc01td(dico, dp, p, stable, nz, dwork, iwarn, info)
            character, intent(in)             :: dico
            integer, intent(inout)            :: dp
            double precision, intent(in)      :: p(*)
            logical, intent(out)              :: stable
            integer, intent(out)              :: nz
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine mc01td
    end interface
    public :: mc01td
    
    interface
        subroutine mc01vd(a, b, c, z1re, z1im, z2re, z2im, info)
            double precision, intent(in)    :: a
            double precision, intent(in)    :: b
            double precision, intent(in)    :: c
            double precision, intent(out)   :: z1re
            double precision, intent(out)   :: z1im
            double precision, intent(out)   :: z2re
            double precision, intent(out)   :: z2im
            integer, intent(out)            :: info
        end subroutine mc01vd
    end interface
    public :: mc01vd
    
    interface
        subroutine mc01wd(dp, p, u1, u2, q, info)
            integer, intent(in)             :: dp
            double precision, intent(in)    :: p(*)
            double precision, intent(in)    :: u1
            double precision, intent(in)    :: u2
            double precision, intent(out)   :: q(*)
            integer, intent(out)            :: info
        end subroutine mc01wd
    end interface
    public :: mc01wd
    
    interface
        subroutine mc01xd(alpha, beta, gamma, delta, evr, evi, evq, dwork, &
                       ldwork, info)
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: beta
            double precision, intent(in)      :: gamma
            double precision, intent(in)      :: delta
            double precision, intent(out)     :: evr(3)
            double precision, intent(out)     :: evi(3)
            double precision, intent(out)     :: evq(3)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mc01xd
    end interface
    public :: mc01xd
    
    interface
        subroutine mc03md(rp1, cp1, cp2, dp1, dp2, dp3, alpha, p1, &
                       ldp11, ldp12, p2, ldp21, ldp22, p3, ldp31, ldp32, &
                       dwork, info)
            integer, intent(in)               :: rp1
            integer, intent(in)               :: cp1
            integer, intent(in)               :: cp2
            integer, intent(in)               :: dp1
            integer, intent(in)               :: dp2
            integer, intent(inout)            :: dp3
            double precision, intent(in)      :: alpha
            double precision, intent(in)      :: p1(ldp11, ldp12, *)
            integer, intent(in)               :: ldp11
            integer, intent(in)               :: ldp12
            double precision, intent(in)      :: p2(ldp21, ldp22, *)
            integer, intent(in)               :: ldp21
            integer, intent(in)               :: ldp22
            double precision, intent(inout)   :: p3(ldp31, ldp32, *)
            integer, intent(in)               :: ldp31
            integer, intent(in)               :: ldp32
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine mc03md
    end interface
    public :: mc03md
    
    interface
        subroutine mc03nd(mp, np, dp, p, ldp1, ldp2, dk, gam, &
                       nullsp, ldnull, ker, ldker1, ldker2, tol, iwork, dwork, &
                       ldwork, info)
            integer, intent(in)               :: mp
            integer, intent(in)               :: np
            integer, intent(in)               :: dp
            double precision, intent(in)      :: p(ldp1, ldp2, *)
            integer, intent(in)               :: ldp1
            integer, intent(in)               :: ldp2
            integer, intent(out)              :: dk
            integer, intent(out)              :: gam(*)
            double precision, intent(out)     :: nullsp(ldnull, *)
            integer, intent(in)               :: ldnull
            double precision, intent(out)     :: ker(ldker1, ldker2, *)
            integer, intent(in)               :: ldker1
            integer, intent(in)               :: ldker2
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine mc03nd
    end interface
    public :: mc03nd
    
    interface
        subroutine mc03nx(mp, np, dp, p, ldp1, ldp2, a, lda, &
                       e, lde)
            integer, intent(in)             :: mp
            integer, intent(in)             :: np
            integer, intent(in)             :: dp
            double precision, intent(in)    :: p(ldp1, ldp2, *)
            integer, intent(in)             :: ldp1
            integer, intent(in)             :: ldp2
            double precision, intent(out)   :: a(lda, *)
            integer, intent(in)             :: lda
            double precision, intent(out)   :: e(lde, *)
            integer, intent(in)             :: lde
        end subroutine mc03nx
    end interface
    public :: mc03nx
    
    interface
        subroutine mc03ny(nblcks, nra, nca, a, lda, e, lde, imuk, &
                       inuk, veps, ldveps, info)
            integer, intent(in)               :: nblcks
            integer, intent(in)               :: nra
            integer, intent(in)               :: nca
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            integer, intent(in)               :: imuk(*)
            integer, intent(in)               :: inuk(*)
            double precision, intent(out)     :: veps(ldveps, *)
            integer, intent(in)               :: ldveps
            integer, intent(out)              :: info
        end subroutine mc03ny
    end interface
    public :: mc03ny
    
    interface
        subroutine md03ad(xinit, alg, stor, uplo, fcn, jpj, m, n, &
                       itmax, nprint, ipar, lipar, dpar1, ldpar1, dpar2, ldpar2, &
                       x, nfev, njev, tol, cgtol, dwork, ldwork, iwarn, &
                       info)
            character, intent(in)             :: xinit
            character, intent(in)             :: alg
            character, intent(in)             :: stor
            character, intent(in)             :: uplo
            external                :: fcn
            external                :: jpj
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: itmax
            integer, intent(in)               :: nprint
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(inout)   :: dpar1(ldpar1, *)
            integer, intent(in)               :: ldpar1
            double precision, intent(inout)   :: dpar2(ldpar2, *)
            integer, intent(in)               :: ldpar2
            double precision, intent(inout)   :: x(*)
            integer, intent(out)              :: nfev
            integer, intent(out)              :: njev
            double precision, intent(in)      :: tol
            double precision, intent(in)      :: cgtol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine md03ad
    end interface
    public :: md03ad
    
    interface
        subroutine md03ba(n, ipar, lipar, fnorm, j, ldj, e, jnorms, &
                       gnorm, ipvt, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(in)      :: fnorm
            double precision, intent(inout)   :: j(*)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: e(*)
            double precision, intent(out)     :: jnorms(*)
            double precision, intent(out)     :: gnorm
            integer, intent(out)              :: ipvt(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine md03ba
    end interface
    public :: md03ba
    
    interface
        subroutine md03bb(cond, n, ipar, lipar, r, ldr, ipvt, diag, &
                       qtb, delta, par, ranks, x, rx, tol, dwork, &
                       ldwork, info)
            character, intent(in)             :: cond
            integer, intent(in)               :: n
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            integer, intent(in)               :: ipvt(*)
            double precision, intent(in)      :: diag(*)
            double precision, intent(in)      :: qtb(*)
            double precision, intent(in)      :: delta
            double precision, intent(inout)   :: par
            integer, intent(inout)            :: ranks(*)
            double precision, intent(out)     :: x(*)
            double precision, intent(out)     :: rx(*)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine md03bb
    end interface
    public :: md03bb
    
    interface
        subroutine md03bd(xinit, scale, cond, fcn, qrfact, lmparm, m, n, &
                       itmax, factor, nprint, ipar, lipar, dpar1, ldpar1, dpar2, &
                       ldpar2, x, diag, nfev, njev, ftol, xtol, gtol, &
                       tol, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: xinit
            character, intent(in)             :: scale
            character, intent(in)             :: cond
            external                :: fcn
            external                :: qrfact
            external                :: lmparm
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(in)               :: itmax
            double precision, intent(in)      :: factor
            integer, intent(in)               :: nprint
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(inout)   :: dpar1(*)
            integer, intent(in)               :: ldpar1
            double precision, intent(inout)   :: dpar2(*)
            integer, intent(in)               :: ldpar2
            double precision, intent(inout)   :: x(*)
            double precision, intent(inout)   :: diag(*)
            integer, intent(out)              :: nfev
            integer, intent(out)              :: njev
            double precision, intent(in)      :: ftol
            double precision, intent(in)      :: xtol
            double precision, intent(in)      :: gtol
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine md03bd
    end interface
    public :: md03bd
    
    interface
        subroutine md03bf(iflag, m, n, ipar, lipar, dpar1, ldpar1, dpar2, &
                       ldpar2, x, nfevl, e, j, ldj, dwork, ldwork, &
                       info)
            integer, intent(inout)            :: iflag
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(inout)   :: dpar1(*)
            integer, intent(in)               :: ldpar1
            double precision, intent(inout)   :: dpar2(*)
            integer, intent(in)               :: ldpar2
            double precision, intent(in)      :: x(*)
            integer, intent(inout)            :: nfevl
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine md03bf
    end interface
    public :: md03bf
    
    interface
        subroutine md03bx(m, n, fnorm, j, ldj, e, jnorms, gnorm, &
                       ipvt, dwork, ldwork, info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: fnorm
            double precision, intent(inout)   :: j(*)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: e(*)
            double precision, intent(out)     :: jnorms(*)
            double precision, intent(out)     :: gnorm
            integer, intent(out)              :: ipvt(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine md03bx
    end interface
    public :: md03bx
    
    interface
        subroutine md03by(cond, n, r, ldr, ipvt, diag, qtb, delta, &
                       par, rank, x, rx, tol, dwork, ldwork, info)
            character, intent(in)             :: cond
            integer, intent(in)               :: n
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            integer, intent(in)               :: ipvt(*)
            double precision, intent(in)      :: diag(*)
            double precision, intent(in)      :: qtb(*)
            double precision, intent(in)      :: delta
            double precision, intent(inout)   :: par
            integer, intent(inout)            :: rank
            double precision, intent(out)     :: x(*)
            double precision, intent(out)     :: rx(*)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine md03by
    end interface
    public :: md03by
    
    interface
        subroutine nf01ad(nsmp, m, l, ipar, lipar, x, lx, u, &
                       ldu, y, ldy, dwork, ldwork, info)
            integer, intent(in)               :: nsmp
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(in)      :: x(*)
            integer, intent(in)               :: lx
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01ad
    end interface
    public :: nf01ad
    
    interface
        subroutine nf01ay(nsmp, nz, l, ipar, lipar, wb, lwb, z, &
                       ldz, y, ldy, dwork, ldwork, info)
            integer, intent(in)               :: nsmp
            integer, intent(in)               :: nz
            integer, intent(in)               :: l
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(in)      :: wb(*)
            integer, intent(in)               :: lwb
            double precision, intent(in)      :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(out)     :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01ay
    end interface
    public :: nf01ay
    
    interface
        subroutine nf01ba(iflag, nsmp, n, ipar, lipar, z, ldz, y, &
                       ldy, x, nfevl, e, j, ldj, jte, dwork, &
                       ldwork, info)
            integer, intent(inout)            :: iflag
            integer, intent(in)               :: nsmp
            integer, intent(in)               :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(in)      :: x(*)
            integer, intent(inout)            :: nfevl
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(out)     :: jte(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01ba
    end interface
    public :: nf01ba
    
    interface
        subroutine nf01bb(iflag, nfun, lx, ipar, lipar, u, ldu, y, &
                       ldy, x, nfevl, e, j, ldj, jte, dwork, &
                       ldwork, info)
            integer, intent(inout)            :: iflag
            integer, intent(in)               :: nfun
            integer, intent(in)               :: lx
            integer, intent(inout)            :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(in)      :: x(*)
            integer, intent(inout)            :: nfevl
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(out)     :: jte(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01bb
    end interface
    public :: nf01bb
    
    interface
        subroutine nf01bd(cjte, nsmp, m, l, ipar, lipar, x, lx, &
                       u, ldu, e, j, ldj, jte, dwork, ldwork, &
                       info)
            character, intent(in)             :: cjte
            integer, intent(in)               :: nsmp
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            integer, intent(inout)            :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(in)      :: x(*)
            integer, intent(in)               :: lx
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: e(*)
            double precision, intent(out)     :: j(ldj, *)
            integer, intent(in)               :: ldj
            double precision, intent(out)     :: jte(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01bd
    end interface
    public :: nf01bd
    
    interface
        subroutine nf01be(iflag, nsmp, n, ipar, lipar, z, ldz, y, &
                       ldy, x, nfevl, e, j, ldj, dwork, ldwork, &
                       info)
            integer, intent(inout)            :: iflag
            integer, intent(in)               :: nsmp
            integer, intent(in)               :: n
            integer, intent(inout)            :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(in)      :: x(*)
            integer, intent(inout)            :: nfevl
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01be
    end interface
    public :: nf01be
    
    interface
        subroutine nf01bf(iflag, nfun, lx, ipar, lipar, u, ldu, y, &
                       ldy, x, nfevl, e, j, ldj, dwork, ldwork, &
                       info)
            integer, intent(inout)            :: iflag
            integer, intent(in)               :: nfun
            integer, intent(in)               :: lx
            integer, intent(inout)            :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(in)      :: x(*)
            integer, intent(inout)            :: nfevl
            double precision, intent(inout)   :: e(*)
            double precision, intent(inout)   :: j(ldj, *)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01bf
    end interface
    public :: nf01bf
    
    interface
        subroutine nf01bp(cond, n, ipar, lipar, r, ldr, ipvt, diag, &
                       qtb, delta, par, ranks, x, rx, tol, dwork, &
                       ldwork, info)
            character, intent(in)             :: cond
            integer, intent(in)               :: n
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            integer, intent(in)               :: ipvt(*)
            double precision, intent(in)      :: diag(*)
            double precision, intent(in)      :: qtb(*)
            double precision, intent(in)      :: delta
            double precision, intent(inout)   :: par
            integer, intent(inout)            :: ranks(*)
            double precision, intent(out)     :: x(*)
            double precision, intent(out)     :: rx(*)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01bp
    end interface
    public :: nf01bp
    
    interface
        subroutine nf01bq(cond, n, ipar, lipar, r, ldr, ipvt, diag, &
                       qtb, ranks, x, tol, dwork, ldwork, info)
            character, intent(in)             :: cond
            integer, intent(in)               :: n
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            integer, intent(in)               :: ipvt(*)
            double precision, intent(in)      :: diag(*)
            double precision, intent(in)      :: qtb(*)
            integer, intent(inout)            :: ranks(*)
            double precision, intent(out)     :: x(*)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01bq
    end interface
    public :: nf01bq
    
    interface
        subroutine nf01br(cond, uplo, trans, n, ipar, lipar, r, ldr, &
                       sdiag, s, lds, b, ranks, tol, dwork, ldwork, &
                       info)
            character, intent(in)             :: cond
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(in)      :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: sdiag(*)
            double precision, intent(in)      :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(inout)   :: b(*)
            integer, intent(inout)            :: ranks(*)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01br
    end interface
    public :: nf01br
    
    interface
        subroutine nf01bs(n, ipar, lipar, fnorm, j, ldj, e, jnorms, &
                       gnorm, ipvt, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(in)      :: fnorm
            double precision, intent(inout)   :: j(*)
            integer, intent(inout)            :: ldj
            double precision, intent(inout)   :: e(*)
            double precision, intent(out)     :: jnorms(*)
            double precision, intent(out)     :: gnorm
            integer, intent(out)              :: ipvt(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01bs
    end interface
    public :: nf01bs
    
    interface
        subroutine nf01bu(stor, uplo, n, ipar, lipar, dpar, ldpar, j, &
                       ldj, jtj, ldjtj, dwork, ldwork, info)
            character, intent(in)             :: stor
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(in)      :: dpar(*)
            integer, intent(in)               :: ldpar
            double precision, intent(in)      :: j(ldj, *)
            integer, intent(in)               :: ldj
            double precision, intent(out)     :: jtj(*)
            integer, intent(in)               :: ldjtj
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01bu
    end interface
    public :: nf01bu
    
    interface
        subroutine nf01bv(stor, uplo, n, ipar, lipar, dpar, ldpar, j, &
                       ldj, jtj, ldjtj, dwork, ldwork, info)
            character, intent(in)             :: stor
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(in)      :: dpar(*)
            integer, intent(in)               :: ldpar
            double precision, intent(in)      :: j(ldj, *)
            integer, intent(in)               :: ldj
            double precision, intent(out)     :: jtj(*)
            integer, intent(in)               :: ldjtj
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01bv
    end interface
    public :: nf01bv
    
    interface
        subroutine nf01bw(n, ipar, lipar, dpar, ldpar, j, ldj, x, &
                       incx, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(in)      :: dpar(*)
            integer, intent(in)               :: ldpar
            double precision, intent(in)      :: j(ldj, *)
            integer, intent(in)               :: ldj
            double precision, intent(inout)   :: x(*)
            integer, intent(in)               :: incx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01bw
    end interface
    public :: nf01bw
    
    interface
        subroutine nf01bx(n, ipar, lipar, dpar, ldpar, j, ldj, x, &
                       incx, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(in)      :: dpar(*)
            integer, intent(in)               :: ldpar
            double precision, intent(in)      :: j(ldj, *)
            integer, intent(in)               :: ldj
            double precision, intent(inout)   :: x(*)
            integer, intent(in)               :: incx
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01bx
    end interface
    public :: nf01bx
    
    interface
        subroutine nf01by(cjte, nsmp, nz, l, ipar, lipar, wb, lwb, &
                       z, ldz, e, j, ldj, jte, dwork, ldwork, &
                       info)
            character, intent(in)             :: cjte
            integer, intent(in)               :: nsmp
            integer, intent(in)               :: nz
            integer, intent(in)               :: l
            integer, intent(inout)            :: ipar(*)
            integer, intent(in)               :: lipar
            double precision, intent(in)      :: wb(*)
            integer, intent(in)               :: lwb
            double precision, intent(in)      :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(in)      :: e(*)
            double precision, intent(out)     :: j(ldj, *)
            integer, intent(in)               :: ldj
            double precision, intent(out)     :: jte(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine nf01by
    end interface
    public :: nf01by
    
    interface
        subroutine sb01bd(dico, n, m, np, alpha, a, lda, b, &
                       ldb, wr, wi, nfp, nap, nup, f, ldf, &
                       z, ldz, tol, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            integer, intent(out)              :: nfp
            integer, intent(out)              :: nap
            integer, intent(out)              :: nup
            double precision, intent(out)     :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine sb01bd
    end interface
    public :: sb01bd
    
    interface
        subroutine sb01bx(reig, n, xr, xi, wr, wi, s, p)
            logical, intent(in)               :: reig
            integer, intent(in)               :: n
            double precision, intent(in)      :: xr
            double precision, intent(in)      :: xi
            double precision, intent(inout)   :: wr(*)
            double precision, intent(inout)   :: wi(*)
            double precision, intent(out)     :: s
            double precision, intent(out)     :: p
        end subroutine sb01bx
    end interface
    public :: sb01bx
    
    interface
        subroutine sb01by(n, m, s, p, a, b, f, tol, &
                       dwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(in)      :: s
            double precision, intent(in)      :: p
            double precision, intent(inout)   :: a(n, *)
            double precision, intent(inout)   :: b(n, *)
            double precision, intent(out)     :: f(m, *)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine sb01by
    end interface
    public :: sb01by
    
    interface
        subroutine sb01dd(n, m, indcon, a, lda, b, ldb, nblk, &
                       wr, wi, z, ldz, y, count, g, ldg, &
                       tol, iwork, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: indcon
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            integer, intent(in)               :: nblk(*)
            double precision, intent(in)      :: wr(*)
            double precision, intent(in)      :: wi(*)
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(in)      :: y(*)
            integer, intent(out)              :: count
            double precision, intent(out)     :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb01dd
    end interface
    public :: sb01dd
    
    interface
        subroutine sb01fy(discr, n, m, a, lda, b, ldb, f, &
                       ldf, v, ldv, info)
            logical, intent(in)             :: discr
            integer, intent(in)             :: n
            integer, intent(in)             :: m
            double precision, intent(in)    :: a(lda, *)
            integer, intent(in)             :: lda
            double precision, intent(in)    :: b(ldb, *)
            integer, intent(in)             :: ldb
            double precision, intent(out)   :: f(ldf, *)
            integer, intent(in)             :: ldf
            double precision, intent(out)   :: v(ldv, *)
            integer, intent(in)             :: ldv
            integer, intent(out)            :: info
        end subroutine sb01fy
    end interface
    public :: sb01fy
    
    interface
        subroutine sb01md(ncont, n, a, lda, b, wr, wi, z, &
                       ldz, g, dwork, info)
            integer, intent(in)               :: ncont
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(*)
            double precision, intent(in)      :: wr(*)
            double precision, intent(in)      :: wi(*)
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(out)     :: g(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine sb01md
    end interface
    public :: sb01md
    
    interface
        logical function sb02cx (reig,ieig)
            double precision, intent(in)   :: reig
            double precision, intent(in)   :: ieig
        end function sb02cx
    end interface
    public :: sb02cx
    
    interface
        subroutine sb02md(dico, hinv, uplo, scal, sort, n, a, lda, &
                       g, ldg, q, ldq, rcond, wr, wi, s, &
                       lds, u, ldu, iwork, dwork, ldwork, bwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: hinv
            character, intent(in)             :: uplo
            character, intent(in)             :: scal
            character, intent(in)             :: sort
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: rcond
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            double precision, intent(out)     :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(out)     :: u(ldu, *)
            integer, intent(in)               :: ldu
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine sb02md
    end interface
    public :: sb02md
    
    interface
        logical function sb02mr (reig,ieig)
            double precision, intent(in)   :: reig
            double precision, intent(in)   :: ieig
        end function sb02mr
    end interface
    public :: sb02mr
    
    interface
        logical function sb02ms (reig,ieig)
            double precision, intent(in)   :: reig
            double precision, intent(in)   :: ieig
        end function sb02ms
    end interface
    public :: sb02ms
    
    interface
        subroutine sb02mt(jobg, jobl, fact, uplo, n, m, a, lda, &
                       b, ldb, q, ldq, r, ldr, l, ldl, &
                       ipiv, oufact, g, ldg, iwork, dwork, ldwork, info)
            character, intent(in)             :: jobg
            character, intent(in)             :: jobl
            character, intent(in)             :: fact
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(inout)   :: l(ldl, *)
            integer, intent(in)               :: ldl
            integer, intent(inout)            :: ipiv(*)
            integer, intent(out)              :: oufact
            double precision, intent(out)     :: g(ldg, *)
            integer, intent(in)               :: ldg
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb02mt
    end interface
    public :: sb02mt
    
    interface
        subroutine sb02mu(dico, hinv, uplo, n, a, lda, g, ldg, &
                       q, ldq, s, lds, iwork, dwork, ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: hinv
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(in)      :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: s(lds, *)
            integer, intent(in)               :: lds
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb02mu
    end interface
    public :: sb02mu
    
    interface
        logical function sb02mv (reig,ieig)
            double precision, intent(in)   :: reig
            double precision, intent(in)   :: ieig
        end function sb02mv
    end interface
    public :: sb02mv
    
    interface
        logical function sb02mw (reig,ieig)
            double precision, intent(in)   :: reig
            double precision, intent(in)   :: ieig
        end function sb02mw
    end interface
    public :: sb02mw
    
    interface
        subroutine sb02mx(jobg, jobl, fact, uplo, trans, flag, def, n, &
                       m, a, lda, b, ldb, q, ldq, r, &
                       ldr, l, ldl, ipiv, oufact, g, ldg, iwork, &
                       dwork, ldwork, info)
            character, intent(in)             :: jobg
            character, intent(in)             :: jobl
            character, intent(in)             :: fact
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            character, intent(in)             :: flag
            character, intent(in)             :: def
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(inout)   :: l(ldl, *)
            integer, intent(in)               :: ldl
            integer, intent(inout)            :: ipiv(*)
            integer, intent(out)              :: oufact
            double precision, intent(out)     :: g(ldg, *)
            integer, intent(in)               :: ldg
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb02mx
    end interface
    public :: sb02mx
    
    interface
        subroutine sb02nd(dico, fact, uplo, jobl, n, m, p, a, &
                       lda, b, ldb, r, ldr, ipiv, l, ldl, &
                       x, ldx, rnorm, f, ldf, oufact, iwork, dwork, &
                       ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: fact
            character, intent(in)             :: uplo
            character, intent(in)             :: jobl
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            integer, intent(inout)            :: ipiv(*)
            double precision, intent(in)      :: l(ldl, *)
            integer, intent(in)               :: ldl
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(in)      :: rnorm
            double precision, intent(out)     :: f(ldf, *)
            integer, intent(in)               :: ldf
            integer, intent(out)              :: oufact(2)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb02nd
    end interface
    public :: sb02nd
    
    interface
        subroutine sb02od(dico, jobb, fact, uplo, jobl, sort, n, m, &
                       p, a, lda, b, ldb, q, ldq, r, &
                       ldr, l, ldl, rcond, x, ldx, alfar, alfai, &
                       beta, s, lds, t, ldt, u, ldu, tol, &
                       iwork, dwork, ldwork, bwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobb
            character, intent(in)             :: fact
            character, intent(in)             :: uplo
            character, intent(in)             :: jobl
            character, intent(in)             :: sort
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(in)      :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: l(ldl, *)
            integer, intent(in)               :: ldl
            double precision, intent(out)     :: rcond
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: alfar(*)
            double precision, intent(out)     :: alfai(*)
            double precision, intent(out)     :: beta(*)
            double precision, intent(out)     :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(out)     :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(out)     :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine sb02od
    end interface
    public :: sb02od
    
    interface
        logical function sb02ou (alphar,alphai,beta)
            double precision, intent(in)   :: alphar
            double precision, intent(in)   :: alphai
            double precision, intent(in)   :: beta
        end function sb02ou
    end interface
    public :: sb02ou
    
    interface
        logical function sb02ov (alphar,alphai,beta)
            double precision, intent(in)   :: alphar
            double precision, intent(in)   :: alphai
            double precision, intent(in)   :: beta
        end function sb02ov
    end interface
    public :: sb02ov
    
    interface
        logical function sb02ow (alphar,alphai,beta)
            double precision, intent(in)   :: alphar
            double precision, intent(in)   :: alphai
            double precision, intent(in)   :: beta
        end function sb02ow
    end interface
    public :: sb02ow
    
    interface
        logical function sb02ox (alphar,alphai,beta)
            double precision, intent(in)   :: alphar
            double precision, intent(in)   :: alphai
            double precision, intent(in)   :: beta
        end function sb02ox
    end interface
    public :: sb02ox
    
    interface
        subroutine sb02oy(type, dico, jobb, fact, uplo, jobl, jobe, n, &
                       m, p, a, lda, b, ldb, q, ldq, &
                       r, ldr, l, ldl, e, lde, af, ldaf, &
                       bf, ldbf, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: type
            character, intent(in)             :: dico
            character, intent(in)             :: jobb
            character, intent(in)             :: fact
            character, intent(in)             :: uplo
            character, intent(in)             :: jobl
            character, intent(in)             :: jobe
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(in)      :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: l(ldl, *)
            integer, intent(in)               :: ldl
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(out)     :: af(ldaf, *)
            integer, intent(in)               :: ldaf
            double precision, intent(out)     :: bf(ldbf, *)
            integer, intent(in)               :: ldbf
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb02oy
    end interface
    public :: sb02oy
    
    interface
        subroutine sb02pd(job, trana, uplo, n, a, lda, g, ldg, &
                       q, ldq, x, ldx, rcond, ferr, wr, wi, &
                       iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: trana
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(in)      :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: rcond
            double precision, intent(out)     :: ferr
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb02pd
    end interface
    public :: sb02pd
    
    interface
        subroutine sb02qd(job, fact, trana, uplo, lyapun, n, a, lda, &
                       t, ldt, u, ldu, g, ldg, q, ldq, &
                       x, ldx, sep, rcond, ferr, iwork, dwork, ldwork, &
                       info)
            character, intent(in)             :: job
            character, intent(in)             :: fact
            character, intent(in)             :: trana
            character, intent(in)             :: uplo
            character, intent(in)             :: lyapun
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(in)      :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(in)      :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: sep
            double precision, intent(out)     :: rcond
            double precision, intent(out)     :: ferr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb02qd
    end interface
    public :: sb02qd
    
    interface
        subroutine sb02rd(job, dico, hinv, trana, uplo, scal, sort, fact, &
                       lyapun, n, a, lda, t, ldt, v, ldv, &
                       g, ldg, q, ldq, x, ldx, sep, rcond, &
                       ferr, wr, wi, s, lds, iwork, dwork, ldwork, &
                       bwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: dico
            character, intent(in)             :: hinv
            character, intent(in)             :: trana
            character, intent(in)             :: uplo
            character, intent(in)             :: scal
            character, intent(in)             :: sort
            character, intent(in)             :: fact
            character, intent(in)             :: lyapun
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(in)               :: ldv
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: sep
            double precision, intent(out)     :: rcond
            double precision, intent(out)     :: ferr
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            double precision, intent(out)     :: s(lds, *)
            integer, intent(in)               :: lds
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine sb02rd
    end interface
    public :: sb02rd
    
    interface
        subroutine sb02ru(dico, hinv, trana, uplo, n, a, lda, g, &
                       ldg, q, ldq, s, lds, iwork, dwork, ldwork, &
                       info)
            character, intent(in)             :: dico
            character, intent(in)             :: hinv
            character, intent(in)             :: trana
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: s(lds, *)
            integer, intent(in)               :: lds
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb02ru
    end interface
    public :: sb02ru
    
    interface
        subroutine sb02sd(job, fact, trana, uplo, lyapun, n, a, lda, &
                       t, ldt, u, ldu, g, ldg, q, ldq, &
                       x, ldx, sepd, rcond, ferr, iwork, dwork, ldwork, &
                       info)
            character, intent(in)             :: job
            character, intent(in)             :: fact
            character, intent(in)             :: trana
            character, intent(in)             :: uplo
            character, intent(in)             :: lyapun
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(in)      :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(in)      :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: sepd
            double precision, intent(out)     :: rcond
            double precision, intent(out)     :: ferr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb02sd
    end interface
    public :: sb02sd
    
    interface
        subroutine sb03md(dico, job, fact, trana, n, a, lda, u, &
                       ldu, c, ldc, scale, sep, ferr, wr, wi, &
                       iwork, dwork, ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: job
            character, intent(in)             :: fact
            character, intent(in)             :: trana
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: scale
            double precision, intent(out)     :: sep
            double precision, intent(out)     :: ferr
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb03md
    end interface
    public :: sb03md
    
    interface
        subroutine sb03mu(ltranl, ltranr, isgn, n1, n2, tl, ldtl, tr, &
                       ldtr, b, ldb, scale, x, ldx, xnorm, info)
            logical, intent(in)             :: ltranl
            logical, intent(in)             :: ltranr
            integer, intent(in)             :: isgn
            integer, intent(in)             :: n1
            integer, intent(in)             :: n2
            double precision, intent(in)    :: tl(ldtl, *)
            integer, intent(in)             :: ldtl
            double precision, intent(in)    :: tr(ldtr, *)
            integer, intent(in)             :: ldtr
            double precision, intent(in)    :: b(ldb, *)
            integer, intent(in)             :: ldb
            double precision, intent(out)   :: scale
            double precision, intent(out)   :: x(ldx, *)
            integer, intent(in)             :: ldx
            double precision, intent(out)   :: xnorm
            integer, intent(out)            :: info
        end subroutine sb03mu
    end interface
    public :: sb03mu
    
    interface
        subroutine sb03mv(ltran, lupper, t, ldt, b, ldb, scale, x, &
                       ldx, xnorm, info)
            logical, intent(in)             :: ltran
            logical, intent(in)             :: lupper
            double precision, intent(in)    :: t(ldt, *)
            integer, intent(in)             :: ldt
            double precision, intent(in)    :: b(ldb, *)
            integer, intent(in)             :: ldb
            double precision, intent(out)   :: scale
            double precision, intent(out)   :: x(ldx, *)
            integer, intent(in)             :: ldx
            double precision, intent(out)   :: xnorm
            integer, intent(out)            :: info
        end subroutine sb03mv
    end interface
    public :: sb03mv
    
    interface
        subroutine sb03mw(ltran, lupper, t, ldt, b, ldb, scale, x, &
                       ldx, xnorm, info)
            logical, intent(in)             :: ltran
            logical, intent(in)             :: lupper
            double precision, intent(in)    :: t(ldt, *)
            integer, intent(in)             :: ldt
            double precision, intent(in)    :: b(ldb, *)
            integer, intent(in)             :: ldb
            double precision, intent(out)   :: scale
            double precision, intent(out)   :: x(ldx, *)
            integer, intent(in)             :: ldx
            double precision, intent(out)   :: xnorm
            integer, intent(out)            :: info
        end subroutine sb03mw
    end interface
    public :: sb03mw
    
    interface
        subroutine sb03mx(trana, n, a, lda, c, ldc, scale, dwork, &
                       info)
            character, intent(in)             :: trana
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine sb03mx
    end interface
    public :: sb03mx
    
    interface
        subroutine sb03my(trana, n, a, lda, c, ldc, scale, info)
            character, intent(in)             :: trana
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: scale
            integer, intent(out)              :: info
        end subroutine sb03my
    end interface
    public :: sb03my
    
    interface
        subroutine sb03od(dico, fact, trans, n, m, a, lda, q, &
                       ldq, b, ldb, scale, wr, wi, dwork, ldwork, &
                       info)
            character, intent(in)             :: dico
            character, intent(in)             :: fact
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: scale
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb03od
    end interface
    public :: sb03od
    
    interface
        subroutine sb03or(discr, ltrans, n, m, s, lds, a, lda, &
                       c, ldc, scale, info)
            logical, intent(in)               :: discr
            logical, intent(in)               :: ltrans
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(in)      :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: scale
            integer, intent(out)              :: info
        end subroutine sb03or
    end interface
    public :: sb03or
    
    interface
        subroutine sb03os(discr, ltrans, n, s, lds, r, ldr, scale, &
                       dwork, zwork, info)
            logical, intent(in)               :: discr
            logical, intent(in)               :: ltrans
            integer, intent(in)               :: n
            complex*16, intent(in)            :: s(lds, *)
            integer, intent(in)               :: lds
            complex*16, intent(inout)         :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: scale
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(out)              :: info
        end subroutine sb03os
    end interface
    public :: sb03os
    
    interface
        subroutine sb03ot(discr, ltrans, n, s, lds, r, ldr, scale, &
                       dwork, info)
            logical, intent(in)               :: discr
            logical, intent(in)               :: ltrans
            integer, intent(in)               :: n
            double precision, intent(in)      :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine sb03ot
    end interface
    public :: sb03ot
    
    interface
        subroutine sb03ou(discr, ltrans, n, m, a, lda, b, ldb, &
                       tau, u, ldu, scale, dwork, ldwork, info)
            logical, intent(in)               :: discr
            logical, intent(in)               :: ltrans
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: tau(*)
            double precision, intent(out)     :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb03ou
    end interface
    public :: sb03ou
    
    interface
        subroutine sb03ov(a, b, small, c, s)
            double precision, intent(inout)   :: a(2)
            double precision, intent(in)      :: b
            double precision, intent(in)      :: small
            double precision, intent(out)     :: c(2)
            double precision, intent(out)     :: s
        end subroutine sb03ov
    end interface
    public :: sb03ov
    
    interface
        subroutine sb03oy(discr, ltrans, isgn, s, lds, r, ldr, a, &
                       lda, scale, info)
            logical, intent(in)               :: discr
            logical, intent(in)               :: ltrans
            integer, intent(in)               :: isgn
            double precision, intent(inout)   :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: scale
            integer, intent(out)              :: info
        end subroutine sb03oy
    end interface
    public :: sb03oy
    
    interface
        subroutine sb03oz(dico, fact, trans, n, m, a, lda, q, &
                       ldq, b, ldb, scale, w, dwork, zwork, lzwork, &
                       info)
            character, intent(in)             :: dico
            character, intent(in)             :: fact
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(in)               :: ldq
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: scale
            complex*16, intent(out)           :: w(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine sb03oz
    end interface
    public :: sb03oz
    
    interface
        subroutine sb03pd(job, fact, trana, n, a, lda, u, ldu, &
                       c, ldc, scale, sepd, ferr, wr, wi, iwork, &
                       dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: fact
            character, intent(in)             :: trana
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: scale
            double precision, intent(out)     :: sepd
            double precision, intent(out)     :: ferr
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb03pd
    end interface
    public :: sb03pd
    
    interface
        subroutine sb03qd(job, fact, trana, uplo, lyapun, n, scale, a, &
                       lda, t, ldt, u, ldu, c, ldc, x, &
                       ldx, sep, rcond, ferr, iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: fact
            character, intent(in)             :: trana
            character, intent(in)             :: uplo
            character, intent(in)             :: lyapun
            integer, intent(in)               :: n
            double precision, intent(in)      :: scale
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: sep
            double precision, intent(out)     :: rcond
            double precision, intent(out)     :: ferr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb03qd
    end interface
    public :: sb03qd
    
    interface
        subroutine sb03qx(trana, uplo, lyapun, n, xanorm, t, ldt, u, &
                       ldu, r, ldr, ferr, iwork, dwork, ldwork, info)
            character, intent(in)             :: trana
            character, intent(in)             :: uplo
            character, intent(in)             :: lyapun
            integer, intent(in)               :: n
            double precision, intent(in)      :: xanorm
            double precision, intent(in)      :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: ferr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb03qx
    end interface
    public :: sb03qx
    
    interface
        subroutine sb03qy(job, trana, lyapun, n, t, ldt, u, ldu, &
                       x, ldx, sep, thnorm, iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: trana
            character, intent(in)             :: lyapun
            integer, intent(in)               :: n
            double precision, intent(in)      :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: sep
            double precision, intent(out)     :: thnorm
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb03qy
    end interface
    public :: sb03qy
    
    interface
        subroutine sb03rd(job, fact, trana, n, a, lda, u, ldu, &
                       c, ldc, scale, sep, ferr, wr, wi, iwork, &
                       dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: fact
            character, intent(in)             :: trana
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: scale
            double precision, intent(out)     :: sep
            double precision, intent(out)     :: ferr
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb03rd
    end interface
    public :: sb03rd
    
    interface
        subroutine sb03sd(job, fact, trana, uplo, lyapun, n, scale, a, &
                       lda, t, ldt, u, ldu, c, ldc, x, &
                       ldx, sepd, rcond, ferr, iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: fact
            character, intent(in)             :: trana
            character, intent(in)             :: uplo
            character, intent(in)             :: lyapun
            integer, intent(in)               :: n
            double precision, intent(in)      :: scale
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: sepd
            double precision, intent(out)     :: rcond
            double precision, intent(out)     :: ferr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb03sd
    end interface
    public :: sb03sd
    
    interface
        subroutine sb03sx(trana, uplo, lyapun, n, xanorm, t, ldt, u, &
                       ldu, r, ldr, ferr, iwork, dwork, ldwork, info)
            character, intent(in)             :: trana
            character, intent(in)             :: uplo
            character, intent(in)             :: lyapun
            integer, intent(in)               :: n
            double precision, intent(in)      :: xanorm
            double precision, intent(in)      :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: ferr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb03sx
    end interface
    public :: sb03sx
    
    interface
        subroutine sb03sy(job, trana, lyapun, n, t, ldt, u, ldu, &
                       xa, ldxa, sepd, thnorm, iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: trana
            character, intent(in)             :: lyapun
            integer, intent(in)               :: n
            double precision, intent(in)      :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: xa(ldxa, *)
            integer, intent(in)               :: ldxa
            double precision, intent(out)     :: sepd
            double precision, intent(out)     :: thnorm
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb03sy
    end interface
    public :: sb03sy
    
    interface
        subroutine sb03td(job, fact, trana, uplo, lyapun, n, scale, a, &
                       lda, t, ldt, u, ldu, c, ldc, x, &
                       ldx, sep, rcond, ferr, wr, wi, iwork, dwork, &
                       ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: fact
            character, intent(in)             :: trana
            character, intent(in)             :: uplo
            character, intent(in)             :: lyapun
            integer, intent(in)               :: n
            double precision, intent(inout)   :: scale
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: sep
            double precision, intent(out)     :: rcond
            double precision, intent(out)     :: ferr
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb03td
    end interface
    public :: sb03td
    
    interface
        subroutine sb03ud(job, fact, trana, uplo, lyapun, n, scale, a, &
                       lda, t, ldt, u, ldu, c, ldc, x, &
                       ldx, sepd, rcond, ferr, wr, wi, iwork, dwork, &
                       ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: fact
            character, intent(in)             :: trana
            character, intent(in)             :: uplo
            character, intent(in)             :: lyapun
            integer, intent(in)               :: n
            double precision, intent(inout)   :: scale
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: sepd
            double precision, intent(out)     :: rcond
            double precision, intent(out)     :: ferr
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb03ud
    end interface
    public :: sb03ud
    
    interface
        subroutine sb04md(n, m, a, lda, b, ldb, c, ldc, &
                       z, ldz, iwork, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb04md
    end interface
    public :: sb04md
    
    interface
        subroutine sb04mr(m, d, ipr, info)
            integer, intent(in)               :: m
            double precision, intent(inout)   :: d(*)
            integer, intent(out)              :: ipr(*)
            integer, intent(out)              :: info
        end subroutine sb04mr
    end interface
    public :: sb04mr
    
    interface
        subroutine sb04mu(n, m, ind, a, lda, b, ldb, c, &
                       ldc, d, ipr, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: ind
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: ipr(*)
            integer, intent(out)              :: info
        end subroutine sb04mu
    end interface
    public :: sb04mu
    
    interface
        subroutine sb04mw(m, d, ipr, info)
            integer, intent(in)               :: m
            double precision, intent(inout)   :: d(*)
            integer, intent(out)              :: ipr(*)
            integer, intent(out)              :: info
        end subroutine sb04mw
    end interface
    public :: sb04mw
    
    interface
        subroutine sb04my(n, m, ind, a, lda, b, ldb, c, &
                       ldc, d, ipr, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: ind
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: ipr(*)
            integer, intent(out)              :: info
        end subroutine sb04my
    end interface
    public :: sb04my
    
    interface
        subroutine sb04nd(abschu, ula, ulb, n, m, a, lda, b, &
                       ldb, c, ldc, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: abschu
            character, intent(in)             :: ula
            character, intent(in)             :: ulb
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb04nd
    end interface
    public :: sb04nd
    
    interface
        subroutine sb04nv(abschr, ul, n, m, c, ldc, indx, ab, &
                       ldab, d)
            character, intent(in)           :: abschr
            character, intent(in)           :: ul
            integer, intent(in)             :: n
            integer, intent(in)             :: m
            double precision, intent(in)    :: c(ldc, *)
            integer, intent(in)             :: ldc
            integer, intent(in)             :: indx
            double precision, intent(in)    :: ab(ldab, *)
            integer, intent(in)             :: ldab
            double precision, intent(out)   :: d(*)
        end subroutine sb04nv
    end interface
    public :: sb04nv
    
    interface
        subroutine sb04nw(abschr, ul, n, m, c, ldc, indx, ab, &
                       ldab, d)
            character, intent(in)           :: abschr
            character, intent(in)           :: ul
            integer, intent(in)             :: n
            integer, intent(in)             :: m
            double precision, intent(in)    :: c(ldc, *)
            integer, intent(in)             :: ldc
            integer, intent(in)             :: indx
            double precision, intent(in)    :: ab(ldab, *)
            integer, intent(in)             :: ldab
            double precision, intent(out)   :: d(*)
        end subroutine sb04nw
    end interface
    public :: sb04nw
    
    interface
        subroutine sb04nx(rc, ul, m, a, lda, lambd1, lambd2, lambd3, &
                       lambd4, d, tol, iwork, dwork, lddwor, info)
            character, intent(in)             :: rc
            character, intent(in)             :: ul
            integer, intent(in)               :: m
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: lambd1
            double precision, intent(in)      :: lambd2
            double precision, intent(in)      :: lambd3
            double precision, intent(in)      :: lambd4
            double precision, intent(inout)   :: d(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(lddwor, *)
            integer, intent(in)               :: lddwor
            integer, intent(out)              :: info
        end subroutine sb04nx
    end interface
    public :: sb04nx
    
    interface
        subroutine sb04ny(rc, ul, m, a, lda, lambda, d, tol, &
                       iwork, dwork, lddwor, info)
            character, intent(in)             :: rc
            character, intent(in)             :: ul
            integer, intent(in)               :: m
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: lambda
            double precision, intent(inout)   :: d(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(lddwor, *)
            integer, intent(in)               :: lddwor
            integer, intent(out)              :: info
        end subroutine sb04ny
    end interface
    public :: sb04ny
    
    interface
        subroutine sb04od(reduce, trans, jobd, m, n, a, lda, b, &
                       ldb, c, ldc, d, ldd, e, lde, f, &
                       ldf, scale, dif, p, ldp, q, ldq, u, &
                       ldu, v, ldv, iwork, dwork, ldwork, info)
            character, intent(in)             :: reduce
            character, intent(in)             :: trans
            character, intent(in)             :: jobd
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(out)     :: scale
            double precision, intent(out)     :: dif
            double precision, intent(out)     :: p(ldp, *)
            integer, intent(in)               :: ldp
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: v(ldv, *)
            integer, intent(in)               :: ldv
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb04od
    end interface
    public :: sb04od
    
    interface
        subroutine sb04ow(m, n, a, lda, b, ldb, c, ldc, &
                       d, ldd, e, lde, f, ldf, scale, iwork, &
                       info)
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(out)     :: scale
            integer, intent(inout)            :: iwork(*)
            integer, intent(out)              :: info
        end subroutine sb04ow
    end interface
    public :: sb04ow
    
    interface
        subroutine sb04pd(dico, facta, factb, trana, tranb, isgn, m, n, &
                       a, lda, u, ldu, b, ldb, v, ldv, &
                       c, ldc, scale, dwork, ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: facta
            character, intent(in)             :: factb
            character, intent(in)             :: trana
            character, intent(in)             :: tranb
            integer, intent(in)               :: isgn
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: v(ldv, *)
            integer, intent(in)               :: ldv
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb04pd
    end interface
    public :: sb04pd
    
    interface
        subroutine sb04px(ltranl, ltranr, isgn, n1, n2, tl, ldtl, tr, &
                       ldtr, b, ldb, scale, x, ldx, xnorm, info)
            logical, intent(in)             :: ltranl
            logical, intent(in)             :: ltranr
            integer, intent(in)             :: isgn
            integer, intent(in)             :: n1
            integer, intent(in)             :: n2
            double precision, intent(in)    :: tl(ldtl, *)
            integer, intent(in)             :: ldtl
            double precision, intent(in)    :: tr(ldtr, *)
            integer, intent(in)             :: ldtr
            double precision, intent(in)    :: b(ldb, *)
            integer, intent(in)             :: ldb
            double precision, intent(out)   :: scale
            double precision, intent(out)   :: x(ldx, *)
            integer, intent(in)             :: ldx
            double precision, intent(out)   :: xnorm
            integer, intent(out)            :: info
        end subroutine sb04px
    end interface
    public :: sb04px
    
    interface
        subroutine sb04py(trana, tranb, isgn, m, n, a, lda, b, &
                       ldb, c, ldc, scale, dwork, info)
            character, intent(in)             :: trana
            character, intent(in)             :: tranb
            integer, intent(in)               :: isgn
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine sb04py
    end interface
    public :: sb04py
    
    interface
        subroutine sb04qd(n, m, a, lda, b, ldb, c, ldc, &
                       z, ldz, iwork, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb04qd
    end interface
    public :: sb04qd
    
    interface
        subroutine sb04qr(m, d, ipr, info)
            integer, intent(in)               :: m
            double precision, intent(inout)   :: d(*)
            integer, intent(out)              :: ipr(*)
            integer, intent(out)              :: info
        end subroutine sb04qr
    end interface
    public :: sb04qr
    
    interface
        subroutine sb04qu(n, m, ind, a, lda, b, ldb, c, &
                       ldc, d, ipr, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: ind
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: ipr(*)
            integer, intent(out)              :: info
        end subroutine sb04qu
    end interface
    public :: sb04qu
    
    interface
        subroutine sb04qy(n, m, ind, a, lda, b, ldb, c, &
                       ldc, d, ipr, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: ind
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: ipr(*)
            integer, intent(out)              :: info
        end subroutine sb04qy
    end interface
    public :: sb04qy
    
    interface
        subroutine sb04rd(abschu, ula, ulb, n, m, a, lda, b, &
                       ldb, c, ldc, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: abschu
            character, intent(in)             :: ula
            character, intent(in)             :: ulb
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb04rd
    end interface
    public :: sb04rd
    
    interface
        subroutine sb04rv(abschr, ul, n, m, c, ldc, indx, ab, &
                       ldab, ba, ldba, d, dwork)
            character, intent(in)             :: abschr
            character, intent(in)             :: ul
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(in)               :: indx
            double precision, intent(in)      :: ab(ldab, *)
            integer, intent(in)               :: ldab
            double precision, intent(in)      :: ba(ldba, *)
            integer, intent(in)               :: ldba
            double precision, intent(out)     :: d(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine sb04rv
    end interface
    public :: sb04rv
    
    interface
        subroutine sb04rw(abschr, ul, n, m, c, ldc, indx, ab, &
                       ldab, ba, ldba, d, dwork)
            character, intent(in)             :: abschr
            character, intent(in)             :: ul
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(in)               :: indx
            double precision, intent(in)      :: ab(ldab, *)
            integer, intent(in)               :: ldab
            double precision, intent(in)      :: ba(ldba, *)
            integer, intent(in)               :: ldba
            double precision, intent(out)     :: d(*)
            double precision, intent(inout)   :: dwork(*)
        end subroutine sb04rw
    end interface
    public :: sb04rw
    
    interface
        subroutine sb04rx(rc, ul, m, a, lda, lambd1, lambd2, lambd3, &
                       lambd4, d, tol, iwork, dwork, lddwor, info)
            character, intent(in)             :: rc
            character, intent(in)             :: ul
            integer, intent(in)               :: m
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: lambd1
            double precision, intent(in)      :: lambd2
            double precision, intent(in)      :: lambd3
            double precision, intent(in)      :: lambd4
            double precision, intent(inout)   :: d(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(lddwor, *)
            integer, intent(in)               :: lddwor
            integer, intent(out)              :: info
        end subroutine sb04rx
    end interface
    public :: sb04rx
    
    interface
        subroutine sb04ry(rc, ul, m, a, lda, lambda, d, tol, &
                       iwork, dwork, lddwor, info)
            character, intent(in)             :: rc
            character, intent(in)             :: ul
            integer, intent(in)               :: m
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: lambda
            double precision, intent(inout)   :: d(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(lddwor, *)
            integer, intent(in)               :: lddwor
            integer, intent(out)              :: info
        end subroutine sb04ry
    end interface
    public :: sb04ry
    
    interface
        subroutine sb06nd(n, m, kmax, a, lda, b, ldb, kstair, &
                       u, ldu, f, ldf, dwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: kmax
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            integer, intent(in)               :: kstair(*)
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine sb06nd
    end interface
    public :: sb06nd
    
    interface
        subroutine sb08cd(dico, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, nq, nr, br, ldbr, &
                       dr, lddr, tol, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nq
            integer, intent(out)              :: nr
            double precision, intent(out)     :: br(ldbr, *)
            integer, intent(in)               :: ldbr
            double precision, intent(out)     :: dr(lddr, *)
            integer, intent(in)               :: lddr
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine sb08cd
    end interface
    public :: sb08cd
    
    interface
        subroutine sb08dd(dico, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, nq, nr, cr, ldcr, &
                       dr, lddr, tol, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nq
            integer, intent(out)              :: nr
            double precision, intent(out)     :: cr(ldcr, *)
            integer, intent(in)               :: ldcr
            double precision, intent(out)     :: dr(lddr, *)
            integer, intent(in)               :: lddr
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine sb08dd
    end interface
    public :: sb08dd
    
    interface
        subroutine sb08ed(dico, n, m, p, alpha, a, lda, b, &
                       ldb, c, ldc, d, ldd, nq, nr, br, &
                       ldbr, dr, lddr, tol, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: alpha(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nq
            integer, intent(out)              :: nr
            double precision, intent(out)     :: br(ldbr, *)
            integer, intent(in)               :: ldbr
            double precision, intent(out)     :: dr(lddr, *)
            integer, intent(in)               :: lddr
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine sb08ed
    end interface
    public :: sb08ed
    
    interface
        subroutine sb08fd(dico, n, m, p, alpha, a, lda, b, &
                       ldb, c, ldc, d, ldd, nq, nr, cr, &
                       ldcr, dr, lddr, tol, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: alpha(*)
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nq
            integer, intent(out)              :: nr
            double precision, intent(out)     :: cr(ldcr, *)
            integer, intent(in)               :: ldcr
            double precision, intent(out)     :: dr(lddr, *)
            integer, intent(in)               :: lddr
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine sb08fd
    end interface
    public :: sb08fd
    
    interface
        subroutine sb08gd(n, m, p, a, lda, b, ldb, c, &
                       ldc, d, ldd, br, ldbr, dr, lddr, iwork, &
                       dwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: br(ldbr, *)
            integer, intent(in)               :: ldbr
            double precision, intent(inout)   :: dr(lddr, *)
            integer, intent(in)               :: lddr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine sb08gd
    end interface
    public :: sb08gd
    
    interface
        subroutine sb08hd(n, m, p, a, lda, b, ldb, c, &
                       ldc, d, ldd, cr, ldcr, dr, lddr, iwork, &
                       dwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: cr(ldcr, *)
            integer, intent(in)               :: ldcr
            double precision, intent(inout)   :: dr(lddr, *)
            integer, intent(in)               :: lddr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine sb08hd
    end interface
    public :: sb08hd
    
    interface
        subroutine sb08md(acona, da, a, res, e, dwork, ldwork, info)
            character, intent(in)             :: acona
            integer, intent(in)               :: da
            double precision, intent(inout)   :: a(*)
            double precision, intent(out)     :: res
            double precision, intent(out)     :: e(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb08md
    end interface
    public :: sb08md
    
    interface
        subroutine sb08my(da, a, b, epsb)
            integer, intent(in)               :: da
            double precision, intent(in)      :: a(*)
            double precision, intent(out)     :: b(*)
            double precision, intent(inout)   :: epsb
        end subroutine sb08my
    end interface
    public :: sb08my
    
    interface
        subroutine sb08nd(acona, da, a, res, e, dwork, ldwork, info)
            character, intent(in)             :: acona
            integer, intent(inout)            :: da
            double precision, intent(inout)   :: a(*)
            double precision, intent(out)     :: res
            double precision, intent(out)     :: e(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb08nd
    end interface
    public :: sb08nd
    
    interface
        subroutine sb08ny(da, a, b, epsb)
            integer, intent(in)             :: da
            double precision, intent(in)    :: a(*)
            double precision, intent(out)   :: b(*)
            double precision, intent(out)   :: epsb
        end subroutine sb08ny
    end interface
    public :: sb08ny
    
    interface
        subroutine sb09md(n, nc, nb, h1, ldh1, h2, ldh2, ss, &
                       ldss, se, ldse, pre, ldpre, tol, info)
            integer, intent(in)             :: n
            integer, intent(in)             :: nc
            integer, intent(in)             :: nb
            double precision, intent(in)    :: h1(ldh1, *)
            integer, intent(in)             :: ldh1
            double precision, intent(in)    :: h2(ldh2, *)
            integer, intent(in)             :: ldh2
            double precision, intent(out)   :: ss(ldss, *)
            integer, intent(in)             :: ldss
            double precision, intent(out)   :: se(ldse, *)
            integer, intent(in)             :: ldse
            double precision, intent(out)   :: pre(ldpre, *)
            integer, intent(in)             :: ldpre
            double precision, intent(in)    :: tol
            integer, intent(out)            :: info
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
            integer, intent(in)               :: job
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            integer, intent(in)               :: ncon
            integer, intent(in)               :: nmeas
            double precision, intent(inout)   :: gamma
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: ak(ldak, *)
            integer, intent(in)               :: ldak
            double precision, intent(out)     :: bk(ldbk, *)
            integer, intent(in)               :: ldbk
            double precision, intent(out)     :: ck(ldck, *)
            integer, intent(in)               :: ldck
            double precision, intent(out)     :: dk(lddk, *)
            integer, intent(in)               :: lddk
            double precision, intent(out)     :: ac(ldac, *)
            integer, intent(in)               :: ldac
            double precision, intent(out)     :: bc(ldbc, *)
            integer, intent(in)               :: ldbc
            double precision, intent(out)     :: cc(ldcc, *)
            integer, intent(in)               :: ldcc
            double precision, intent(out)     :: dc(lddc, *)
            integer, intent(in)               :: lddc
            double precision, intent(out)     :: rcond(4)
            double precision, intent(in)      :: gtol
            double precision, intent(in)      :: actol
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(in)               :: lbwork
            integer, intent(out)              :: info
        end subroutine sb10ad
    end interface
    public :: sb10ad
    
    interface
        subroutine sb10dd(n, m, np, ncon, nmeas, gamma, a, lda, &
                       b, ldb, c, ldc, d, ldd, ak, ldak, &
                       bk, ldbk, ck, ldck, dk, lddk, x, ldx, &
                       z, ldz, rcond, tol, iwork, dwork, ldwork, bwork, &
                       info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            integer, intent(in)               :: ncon
            integer, intent(in)               :: nmeas
            double precision, intent(in)      :: gamma
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: ak(ldak, *)
            integer, intent(in)               :: ldak
            double precision, intent(out)     :: bk(ldbk, *)
            integer, intent(in)               :: ldbk
            double precision, intent(out)     :: ck(ldck, *)
            integer, intent(in)               :: ldck
            double precision, intent(out)     :: dk(lddk, *)
            integer, intent(in)               :: lddk
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(out)     :: rcond(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine sb10dd
    end interface
    public :: sb10dd
    
    interface
        subroutine sb10ed(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, d, ldd, ak, ldak, bk, &
                       ldbk, ck, ldck, dk, lddk, rcond, tol, iwork, &
                       dwork, ldwork, bwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            integer, intent(in)               :: ncon
            integer, intent(in)               :: nmeas
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: ak(ldak, *)
            integer, intent(in)               :: ldak
            double precision, intent(out)     :: bk(ldbk, *)
            integer, intent(in)               :: ldbk
            double precision, intent(out)     :: ck(ldck, *)
            integer, intent(in)               :: ldck
            double precision, intent(out)     :: dk(lddk, *)
            integer, intent(in)               :: lddk
            double precision, intent(out)     :: rcond(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine sb10ed
    end interface
    public :: sb10ed
    
    interface
        subroutine sb10fd(n, m, np, ncon, nmeas, gamma, a, lda, &
                       b, ldb, c, ldc, d, ldd, ak, ldak, &
                       bk, ldbk, ck, ldck, dk, lddk, rcond, tol, &
                       iwork, dwork, ldwork, bwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            integer, intent(in)               :: ncon
            integer, intent(in)               :: nmeas
            double precision, intent(in)      :: gamma
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: ak(ldak, *)
            integer, intent(in)               :: ldak
            double precision, intent(out)     :: bk(ldbk, *)
            integer, intent(in)               :: ldbk
            double precision, intent(out)     :: ck(ldck, *)
            integer, intent(in)               :: ldck
            double precision, intent(out)     :: dk(lddk, *)
            integer, intent(in)               :: lddk
            double precision, intent(out)     :: rcond(4)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine sb10fd
    end interface
    public :: sb10fd
    
    interface
        subroutine sb10hd(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, d, ldd, ak, ldak, bk, &
                       ldbk, ck, ldck, dk, lddk, rcond, tol, iwork, &
                       dwork, ldwork, bwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            integer, intent(in)               :: ncon
            integer, intent(in)               :: nmeas
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: ak(ldak, *)
            integer, intent(in)               :: ldak
            double precision, intent(out)     :: bk(ldbk, *)
            integer, intent(in)               :: ldbk
            double precision, intent(out)     :: ck(ldck, *)
            integer, intent(in)               :: ldck
            double precision, intent(out)     :: dk(lddk, *)
            integer, intent(in)               :: lddk
            double precision, intent(out)     :: rcond(4)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine sb10hd
    end interface
    public :: sb10hd
    
    interface
        subroutine sb10id(n, m, np, a, lda, b, ldb, c, &
                       ldc, d, ldd, factor, nk, ak, ldak, bk, &
                       ldbk, ck, ldck, dk, lddk, rcond, iwork, dwork, &
                       ldwork, bwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: factor
            integer, intent(out)              :: nk
            double precision, intent(out)     :: ak(ldak, *)
            integer, intent(in)               :: ldak
            double precision, intent(out)     :: bk(ldbk, *)
            integer, intent(in)               :: ldbk
            double precision, intent(out)     :: ck(ldck, *)
            integer, intent(in)               :: ldck
            double precision, intent(out)     :: dk(lddk, *)
            integer, intent(in)               :: lddk
            double precision, intent(out)     :: rcond(2)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine sb10id
    end interface
    public :: sb10id
    
    interface
        subroutine sb10jd(n, m, np, a, lda, b, ldb, c, &
                       ldc, d, ldd, e, lde, nsys, dwork, ldwork, &
                       info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            integer, intent(out)              :: nsys
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb10jd
    end interface
    public :: sb10jd
    
    interface
        subroutine sb10kd(n, m, np, a, lda, b, ldb, c, &
                       ldc, factor, ak, ldak, bk, ldbk, ck, ldck, &
                       dk, lddk, rcond, iwork, dwork, ldwork, bwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: factor
            double precision, intent(out)     :: ak(ldak, *)
            integer, intent(in)               :: ldak
            double precision, intent(out)     :: bk(ldbk, *)
            integer, intent(in)               :: ldbk
            double precision, intent(out)     :: ck(ldck, *)
            integer, intent(in)               :: ldck
            double precision, intent(out)     :: dk(lddk, *)
            integer, intent(in)               :: lddk
            double precision, intent(out)     :: rcond(4)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine sb10kd
    end interface
    public :: sb10kd
    
    interface
        subroutine sb10ld(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, d, ldd, ak, ldak, bk, &
                       ldbk, ck, ldck, dk, lddk, ac, ldac, bc, &
                       ldbc, cc, ldcc, dc, lddc, iwork, dwork, ldwork, &
                       info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            integer, intent(in)               :: ncon
            integer, intent(in)               :: nmeas
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: ak(ldak, *)
            integer, intent(in)               :: ldak
            double precision, intent(in)      :: bk(ldbk, *)
            integer, intent(in)               :: ldbk
            double precision, intent(in)      :: ck(ldck, *)
            integer, intent(in)               :: ldck
            double precision, intent(in)      :: dk(lddk, *)
            integer, intent(in)               :: lddk
            double precision, intent(out)     :: ac(ldac, *)
            integer, intent(in)               :: ldac
            double precision, intent(out)     :: bc(ldbc, *)
            integer, intent(in)               :: ldbc
            double precision, intent(out)     :: cc(ldcc, *)
            integer, intent(in)               :: ldcc
            double precision, intent(out)     :: dc(lddc, *)
            integer, intent(in)               :: lddc
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb10ld
    end interface
    public :: sb10ld
    
    interface
        subroutine sb10md(nc, mp, lendat, f, ord, mnb, nblock, itype, &
                       qutol, a, lda, b, ldb, c, ldc, d, &
                       ldd, omega, totord, ad, ldad, bd, ldbd, cd, &
                       ldcd, dd, lddd, mju, iwork, liwork, dwork, ldwork, &
                       zwork, lzwork, info)
            integer, intent(in)               :: nc
            integer, intent(in)               :: mp
            integer, intent(in)               :: lendat
            integer, intent(in)               :: f
            integer, intent(inout)            :: ord
            integer, intent(in)               :: mnb
            integer, intent(in)               :: nblock(*)
            integer, intent(in)               :: itype(*)
            double precision, intent(in)      :: qutol
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: omega(*)
            integer, intent(out)              :: totord
            double precision, intent(out)     :: ad(ldad, *)
            integer, intent(in)               :: ldad
            double precision, intent(out)     :: bd(ldbd, *)
            integer, intent(in)               :: ldbd
            double precision, intent(out)     :: cd(ldcd, *)
            integer, intent(in)               :: ldcd
            double precision, intent(out)     :: dd(lddd, *)
            integer, intent(in)               :: lddd
            double precision, intent(out)     :: mju(*)
            integer, intent(inout)            :: iwork(*)
            integer, intent(in)               :: liwork
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine sb10md
    end interface
    public :: sb10md
    
    interface
        subroutine sb10pd(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, d, ldd, tu, ldtu, ty, &
                       ldty, rcond, tol, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            integer, intent(in)               :: ncon
            integer, intent(in)               :: nmeas
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: tu(ldtu, *)
            integer, intent(in)               :: ldtu
            double precision, intent(out)     :: ty(ldty, *)
            integer, intent(in)               :: ldty
            double precision, intent(out)     :: rcond(2)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb10pd
    end interface
    public :: sb10pd
    
    interface
        subroutine sb10qd(n, m, np, ncon, nmeas, gamma, a, lda, &
                       b, ldb, c, ldc, d, ldd, f, ldf, &
                       h, ldh, x, ldx, y, ldy, xycond, iwork, &
                       dwork, ldwork, bwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            integer, intent(in)               :: ncon
            integer, intent(in)               :: nmeas
            double precision, intent(in)      :: gamma
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(out)     :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(out)     :: xycond(2)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine sb10qd
    end interface
    public :: sb10qd
    
    interface
        subroutine sb10rd(n, m, np, ncon, nmeas, gamma, a, lda, &
                       b, ldb, c, ldc, d, ldd, f, ldf, &
                       h, ldh, tu, ldtu, ty, ldty, x, ldx, &
                       y, ldy, ak, ldak, bk, ldbk, ck, ldck, &
                       dk, lddk, iwork, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            integer, intent(in)               :: ncon
            integer, intent(in)               :: nmeas
            double precision, intent(in)      :: gamma
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(in)      :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(in)      :: tu(ldtu, *)
            integer, intent(in)               :: ldtu
            double precision, intent(in)      :: ty(ldty, *)
            integer, intent(in)               :: ldty
            double precision, intent(in)      :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(in)      :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(out)     :: ak(ldak, *)
            integer, intent(in)               :: ldak
            double precision, intent(out)     :: bk(ldbk, *)
            integer, intent(in)               :: ldbk
            double precision, intent(out)     :: ck(ldck, *)
            integer, intent(in)               :: ldck
            double precision, intent(out)     :: dk(lddk, *)
            integer, intent(in)               :: lddk
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb10rd
    end interface
    public :: sb10rd
    
    interface
        subroutine sb10sd(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, d, ldd, ak, ldak, bk, &
                       ldbk, ck, ldck, dk, lddk, x, ldx, y, &
                       ldy, rcond, tol, iwork, dwork, ldwork, bwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            integer, intent(in)               :: ncon
            integer, intent(in)               :: nmeas
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: ak(ldak, *)
            integer, intent(in)               :: ldak
            double precision, intent(out)     :: bk(ldbk, *)
            integer, intent(in)               :: ldbk
            double precision, intent(out)     :: ck(ldck, *)
            integer, intent(in)               :: ldck
            double precision, intent(out)     :: dk(lddk, *)
            integer, intent(in)               :: lddk
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(out)     :: rcond(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine sb10sd
    end interface
    public :: sb10sd
    
    interface
        subroutine sb10td(n, m, np, ncon, nmeas, d, ldd, tu, &
                       ldtu, ty, ldty, ak, ldak, bk, ldbk, ck, &
                       ldck, dk, lddk, rcond, tol, iwork, dwork, ldwork, &
                       info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            integer, intent(in)               :: ncon
            integer, intent(in)               :: nmeas
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: tu(ldtu, *)
            integer, intent(in)               :: ldtu
            double precision, intent(in)      :: ty(ldty, *)
            integer, intent(in)               :: ldty
            double precision, intent(inout)   :: ak(ldak, *)
            integer, intent(in)               :: ldak
            double precision, intent(inout)   :: bk(ldbk, *)
            integer, intent(in)               :: ldbk
            double precision, intent(inout)   :: ck(ldck, *)
            integer, intent(in)               :: ldck
            double precision, intent(inout)   :: dk(lddk, *)
            integer, intent(in)               :: lddk
            double precision, intent(out)     :: rcond
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb10td
    end interface
    public :: sb10td
    
    interface
        subroutine sb10ud(n, m, np, ncon, nmeas, b, ldb, c, &
                       ldc, d, ldd, tu, ldtu, ty, ldty, rcond, &
                       tol, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            integer, intent(in)               :: ncon
            integer, intent(in)               :: nmeas
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: tu(ldtu, *)
            integer, intent(in)               :: ldtu
            double precision, intent(out)     :: ty(ldty, *)
            integer, intent(in)               :: ldty
            double precision, intent(out)     :: rcond(2)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb10ud
    end interface
    public :: sb10ud
    
    interface
        subroutine sb10vd(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, f, ldf, h, ldh, x, &
                       ldx, y, ldy, xycond, iwork, dwork, ldwork, bwork, &
                       info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            integer, intent(in)               :: ncon
            integer, intent(in)               :: nmeas
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(out)     :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(out)     :: xycond(2)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine sb10vd
    end interface
    public :: sb10vd
    
    interface
        subroutine sb10wd(n, m, np, ncon, nmeas, a, lda, b, &
                       ldb, c, ldc, d, ldd, f, ldf, h, &
                       ldh, tu, ldtu, ty, ldty, ak, ldak, bk, &
                       ldbk, ck, ldck, dk, lddk, info)
            integer, intent(in)             :: n
            integer, intent(in)             :: m
            integer, intent(in)             :: np
            integer, intent(in)             :: ncon
            integer, intent(in)             :: nmeas
            double precision, intent(in)    :: a(lda, *)
            integer, intent(in)             :: lda
            double precision, intent(in)    :: b(ldb, *)
            integer, intent(in)             :: ldb
            double precision, intent(in)    :: c(ldc, *)
            integer, intent(in)             :: ldc
            double precision, intent(in)    :: d(ldd, *)
            integer, intent(in)             :: ldd
            double precision, intent(in)    :: f(ldf, *)
            integer, intent(in)             :: ldf
            double precision, intent(in)    :: h(ldh, *)
            integer, intent(in)             :: ldh
            double precision, intent(in)    :: tu(ldtu, *)
            integer, intent(in)             :: ldtu
            double precision, intent(in)    :: ty(ldty, *)
            integer, intent(in)             :: ldty
            double precision, intent(out)   :: ak(ldak, *)
            integer, intent(in)             :: ldak
            double precision, intent(out)   :: bk(ldbk, *)
            integer, intent(in)             :: ldbk
            double precision, intent(out)   :: ck(ldck, *)
            integer, intent(in)             :: ldck
            double precision, intent(out)   :: dk(lddk, *)
            integer, intent(in)             :: lddk
            integer, intent(out)            :: info
        end subroutine sb10wd
    end interface
    public :: sb10wd
    
    interface
        subroutine sb10yd(discfl, flag, lendat, rfrdat, ifrdat, omega, n, a, &
                       lda, b, c, d, tol, iwork, dwork, ldwork, &
                       zwork, lzwork, info)
            integer, intent(in)               :: discfl
            integer, intent(in)               :: flag
            integer, intent(in)               :: lendat
            double precision, intent(in)      :: rfrdat(*)
            double precision, intent(in)      :: ifrdat(*)
            double precision, intent(in)      :: omega(*)
            integer, intent(inout)            :: n
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: b(*)
            double precision, intent(out)     :: c(*)
            double precision, intent(out)     :: d(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine sb10yd
    end interface
    public :: sb10yd
    
    interface
        subroutine sb10zd(n, m, np, a, lda, b, ldb, c, &
                       ldc, d, ldd, factor, ak, ldak, bk, ldbk, &
                       ck, ldck, dk, lddk, rcond, tol, iwork, dwork, &
                       ldwork, bwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: np
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: factor
            double precision, intent(out)     :: ak(ldak, *)
            integer, intent(in)               :: ldak
            double precision, intent(out)     :: bk(ldbk, *)
            integer, intent(in)               :: ldbk
            double precision, intent(out)     :: ck(ldck, *)
            integer, intent(in)               :: ldck
            double precision, intent(out)     :: dk(lddk, *)
            integer, intent(in)               :: lddk
            double precision, intent(out)     :: rcond(6)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: info
        end subroutine sb10zd
    end interface
    public :: sb10zd
    
    interface
        subroutine sb10zp(discfl, n, a, lda, b, c, d, iwork, &
                       dwork, ldwork, info)
            integer, intent(in)               :: discfl
            integer, intent(inout)            :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: c(*)
            double precision, intent(inout)   :: d(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb10zp
    end interface
    public :: sb10zp
    
    interface
        subroutine sb16ad(dico, jobc, jobo, jobmr, weight, equil, ordsel, n, &
                       m, p, nc, ncr, alpha, a, lda, b, &
                       ldb, c, ldc, d, ldd, ac, ldac, bc, &
                       ldbc, cc, ldcc, dc, lddc, ncs, hsvc, tol1, &
                       tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobc
            character, intent(in)             :: jobo
            character, intent(in)             :: jobmr
            character, intent(in)             :: weight
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: nc
            integer, intent(inout)            :: ncr
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: ac(ldac, *)
            integer, intent(in)               :: ldac
            double precision, intent(inout)   :: bc(ldbc, *)
            integer, intent(in)               :: ldbc
            double precision, intent(inout)   :: cc(ldcc, *)
            integer, intent(in)               :: ldcc
            double precision, intent(inout)   :: dc(lddc, *)
            integer, intent(in)               :: lddc
            integer, intent(out)              :: ncs
            double precision, intent(out)     :: hsvc(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine sb16ad
    end interface
    public :: sb16ad
    
    interface
        subroutine sb16ay(dico, jobc, jobo, weight, n, m, p, nc, &
                       ncs, a, lda, b, ldb, c, ldc, d, &
                       ldd, ac, ldac, bc, ldbc, cc, ldcc, dc, &
                       lddc, scalec, scaleo, s, lds, r, ldr, iwork, &
                       dwork, ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobc
            character, intent(in)             :: jobo
            character, intent(in)             :: weight
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: nc
            integer, intent(in)               :: ncs
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: ac(ldac, *)
            integer, intent(in)               :: ldac
            double precision, intent(in)      :: bc(ldbc, *)
            integer, intent(in)               :: ldbc
            double precision, intent(in)      :: cc(ldcc, *)
            integer, intent(in)               :: ldcc
            double precision, intent(in)      :: dc(lddc, *)
            integer, intent(in)               :: lddc
            double precision, intent(out)     :: scalec
            double precision, intent(out)     :: scaleo
            double precision, intent(out)     :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(out)     :: r(ldr, *)
            integer, intent(in)               :: ldr
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb16ay
    end interface
    public :: sb16ay
    
    interface
        subroutine sb16bd(dico, jobd, jobmr, jobcf, equil, ordsel, n, m, &
                       p, ncr, a, lda, b, ldb, c, ldc, &
                       d, ldd, f, ldf, g, ldg, dc, lddc, &
                       hsv, tol1, tol2, iwork, dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobd
            character, intent(in)             :: jobmr
            character, intent(in)             :: jobcf
            character, intent(in)             :: equil
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: ncr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(out)     :: dc(lddc, *)
            integer, intent(in)               :: lddc
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine sb16bd
    end interface
    public :: sb16bd
    
    interface
        subroutine sb16cd(dico, jobd, jobmr, jobcf, ordsel, n, m, p, &
                       ncr, a, lda, b, ldb, c, ldc, d, &
                       ldd, f, ldf, g, ldg, hsv, tol, iwork, &
                       dwork, ldwork, iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobd
            character, intent(in)             :: jobmr
            character, intent(in)             :: jobcf
            character, intent(in)             :: ordsel
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(inout)            :: ncr
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(out)     :: hsv(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine sb16cd
    end interface
    public :: sb16cd
    
    interface
        subroutine sb16cy(dico, jobcf, n, m, p, a, lda, b, &
                       ldb, c, ldc, f, ldf, g, ldg, scalec, &
                       scaleo, s, lds, r, ldr, dwork, ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobcf
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(in)      :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(out)     :: scalec
            double precision, intent(out)     :: scaleo
            double precision, intent(out)     :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(out)     :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sb16cy
    end interface
    public :: sb16cy
    
    interface
        subroutine sg02ad(dico, jobb, fact, uplo, jobl, scal, sort, acc, &
                       n, m, p, a, lda, e, lde, b, &
                       ldb, q, ldq, r, ldr, l, ldl, rcondu, &
                       x, ldx, alfar, alfai, beta, s, lds, t, &
                       ldt, u, ldu, tol, iwork, dwork, ldwork, bwork, &
                       iwarn, info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobb
            character, intent(in)             :: fact
            character, intent(in)             :: uplo
            character, intent(in)             :: jobl
            character, intent(in)             :: scal
            character, intent(in)             :: sort
            character, intent(in)             :: acc
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(in)      :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: l(ldl, *)
            integer, intent(in)               :: ldl
            double precision, intent(out)     :: rcondu
            double precision, intent(out)     :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: alfar(*)
            double precision, intent(out)     :: alfai(*)
            double precision, intent(out)     :: beta(*)
            double precision, intent(out)     :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(out)     :: t(ldt, *)
            integer, intent(in)               :: ldt
            double precision, intent(out)     :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            logical, intent(inout)            :: bwork(*)
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine sg02ad
    end interface
    public :: sg02ad
    
    interface
        subroutine sg02cv(dico, job, jobe, uplo, trans, n, a, lda, &
                       e, lde, x, ldx, r, ldr, norms, dwork, &
                       ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: job
            character, intent(in)             :: jobe
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: norms(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sg02cv
    end interface
    public :: sg02cv
    
    interface
        subroutine sg02cw(dico, job, jobe, flag, jobg, uplo, trans, n, &
                       m, a, lda, e, lde, g, ldg, x, &
                       ldx, f, ldf, k, ldk, xe, ldxe, r, &
                       ldr, c, ldc, norms, dwork, ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: job
            character, intent(in)             :: jobe
            character, intent(in)             :: flag
            character, intent(in)             :: jobg
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(in)      :: f(ldf, *)
            integer, intent(in)               :: ldf
            double precision, intent(in)      :: k(ldk, *)
            integer, intent(in)               :: ldk
            double precision, intent(in)      :: xe(ldxe, *)
            integer, intent(in)               :: ldxe
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(out)     :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: norms(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sg02cw
    end interface
    public :: sg02cw
    
    interface
        subroutine sg02cx(jobe, flag, jobg, uplo, trans, n, m, e, &
                       lde, r, ldr, s, lds, g, ldg, alpha, &
                       rnorm, dwork, ldwork, iwarn, info)
            character, intent(in)             :: jobe
            character, intent(in)             :: flag
            character, intent(in)             :: jobg
            character, intent(in)             :: uplo
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(in)      :: r(ldr, *)
            integer, intent(in)               :: ldr
            double precision, intent(in)      :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(inout)   :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(out)     :: alpha
            double precision, intent(out)     :: rnorm
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: iwarn
            integer, intent(out)              :: info
        end subroutine sg02cx
    end interface
    public :: sg02cx
    
    interface
        subroutine sg02nd(dico, jobe, job, jobx, fact, uplo, jobl, trans, &
                       n, m, p, a, lda, e, lde, b, &
                       ldb, r, ldr, ipiv, l, ldl, x, ldx, &
                       rnorm, k, ldk, h, ldh, xe, ldxe, oufact, &
                       iwork, dwork, ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: jobe
            character, intent(in)             :: job
            character, intent(in)             :: jobx
            character, intent(in)             :: fact
            character, intent(in)             :: uplo
            character, intent(in)             :: jobl
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: r(ldr, *)
            integer, intent(in)               :: ldr
            integer, intent(inout)            :: ipiv(*)
            double precision, intent(in)      :: l(ldl, *)
            integer, intent(in)               :: ldl
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(in)      :: rnorm
            double precision, intent(out)     :: k(ldk, *)
            integer, intent(in)               :: ldk
            double precision, intent(out)     :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(out)     :: xe(ldxe, *)
            integer, intent(in)               :: ldxe
            integer, intent(out)              :: oufact(2)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sg02nd
    end interface
    public :: sg02nd
    
    interface
        subroutine sg03ad(dico, job, fact, trans, uplo, n, a, lda, &
                       e, lde, q, ldq, z, ldz, x, ldx, &
                       scale, sep, ferr, alphar, alphai, beta, iwork, dwork, &
                       ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: job
            character, intent(in)             :: fact
            character, intent(in)             :: trans
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: scale
            double precision, intent(out)     :: sep
            double precision, intent(out)     :: ferr
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sg03ad
    end interface
    public :: sg03ad
    
    interface
        subroutine sg03ax(trans, n, a, lda, e, lde, x, ldx, &
                       scale, info)
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: scale
            integer, intent(out)              :: info
        end subroutine sg03ax
    end interface
    public :: sg03ax
    
    interface
        subroutine sg03ay(trans, n, a, lda, e, lde, x, ldx, &
                       scale, info)
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: scale
            integer, intent(out)              :: info
        end subroutine sg03ay
    end interface
    public :: sg03ay
    
    interface
        subroutine sg03bd(dico, fact, trans, n, m, a, lda, e, &
                       lde, q, ldq, z, ldz, b, ldb, scale, &
                       alphar, alphai, beta, dwork, ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: fact
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: scale
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine sg03bd
    end interface
    public :: sg03bd
    
    interface
        subroutine sg03br(xr, xi, yr, yi, c, sr, si, zr, &
                       zi)
            double precision, intent(in)    :: xr
            double precision, intent(in)    :: xi
            double precision, intent(in)    :: yr
            double precision, intent(in)    :: yi
            double precision, intent(out)   :: c
            double precision, intent(out)   :: sr
            double precision, intent(out)   :: si
            double precision, intent(out)   :: zr
            double precision, intent(out)   :: zi
        end subroutine sg03br
    end interface
    public :: sg03br
    
    interface
        subroutine sg03bs(trans, n, a, lda, e, lde, b, ldb, &
                       scale, dwork, zwork, info)
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(in)               :: lde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: scale
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(out)              :: info
        end subroutine sg03bs
    end interface
    public :: sg03bs
    
    interface
        subroutine sg03bt(trans, n, a, lda, e, lde, b, ldb, &
                       scale, dwork, zwork, info)
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(in)               :: lde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: scale
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(out)              :: info
        end subroutine sg03bt
    end interface
    public :: sg03bt
    
    interface
        subroutine sg03bu(trans, n, a, lda, e, lde, b, ldb, &
                       scale, dwork, info)
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine sg03bu
    end interface
    public :: sg03bu
    
    interface
        subroutine sg03bv(trans, n, a, lda, e, lde, b, ldb, &
                       scale, dwork, info)
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: scale
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine sg03bv
    end interface
    public :: sg03bv
    
    interface
        subroutine sg03bw(trans, m, n, a, lda, c, ldc, e, &
                       lde, d, ldd, x, ldx, scale, info)
            character, intent(in)             :: trans
            integer, intent(in)               :: m
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(out)     :: scale
            integer, intent(out)              :: info
        end subroutine sg03bw
    end interface
    public :: sg03bw
    
    interface
        subroutine sg03bx(dico, trans, a, lda, e, lde, b, ldb, &
                       u, ldu, scale, m1, ldm1, m2, ldm2, info)
            character, intent(in)           :: dico
            character, intent(in)           :: trans
            double precision, intent(in)    :: a(lda, *)
            integer, intent(in)             :: lda
            double precision, intent(in)    :: e(lde, *)
            integer, intent(in)             :: lde
            double precision, intent(in)    :: b(ldb, *)
            integer, intent(in)             :: ldb
            double precision, intent(out)   :: u(ldu, *)
            integer, intent(in)             :: ldu
            double precision, intent(out)   :: scale
            double precision, intent(out)   :: m1(ldm1, *)
            integer, intent(in)             :: ldm1
            double precision, intent(out)   :: m2(ldm2, *)
            integer, intent(in)             :: ldm2
            integer, intent(out)            :: info
        end subroutine sg03bx
    end interface
    public :: sg03bx
    
    interface
        subroutine sg03by(xr, xi, yr, yi, cr, ci, sr, si, &
                       z)
            double precision, intent(in)    :: xr
            double precision, intent(in)    :: xi
            double precision, intent(in)    :: yr
            double precision, intent(in)    :: yi
            double precision, intent(out)   :: cr
            double precision, intent(out)   :: ci
            double precision, intent(out)   :: sr
            double precision, intent(out)   :: si
            double precision, intent(out)   :: z
        end subroutine sg03by
    end interface
    public :: sg03by
    
    interface
        subroutine sg03bz(dico, fact, trans, n, m, a, lda, e, &
                       lde, q, ldq, z, ldz, b, ldb, scale, &
                       alpha, beta, dwork, zwork, lzwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: fact
            character, intent(in)             :: trans
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(in)               :: lde
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(in)               :: ldq
            complex*16, intent(inout)         :: z(ldz, *)
            integer, intent(in)               :: ldz
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: scale
            complex*16, intent(out)           :: alpha(*)
            complex*16, intent(out)           :: beta(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine sg03bz
    end interface
    public :: sg03bz
    
    interface
        subroutine tb01id(job, n, m, p, maxred, a, lda, b, &
                       ldb, c, ldc, scale, info)
            character, intent(in)             :: job
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: maxred
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: scale(*)
            integer, intent(out)              :: info
        end subroutine tb01id
    end interface
    public :: tb01id
    
    interface
        subroutine tb01iz(job, n, m, p, maxred, a, lda, b, &
                       ldb, c, ldc, scale, info)
            character, intent(in)             :: job
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: maxred
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: scale(*)
            integer, intent(out)              :: info
        end subroutine tb01iz
    end interface
    public :: tb01iz
    
    interface
        subroutine tb01kd(dico, stdom, joba, n, m, p, alpha, a, &
                       lda, b, ldb, c, ldc, ndim, u, ldu, &
                       wr, wi, dwork, ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: stdom
            character, intent(in)             :: joba
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: ndim
            double precision, intent(out)     :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb01kd
    end interface
    public :: tb01kd
    
    interface
        subroutine tb01kx(n, m, p, ndim, a, lda, b, ldb, &
                       c, ldc, u, ldu, v, ldv, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: ndim
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: v(ldv, *)
            integer, intent(in)               :: ldv
            integer, intent(out)              :: info
        end subroutine tb01kx
    end interface
    public :: tb01kx
    
    interface
        subroutine tb01ld(dico, stdom, joba, n, m, p, alpha, a, &
                       lda, b, ldb, c, ldc, ndim, u, ldu, &
                       wr, wi, dwork, ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: stdom
            character, intent(in)             :: joba
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: ndim
            double precision, intent(out)     :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb01ld
    end interface
    public :: tb01ld
    
    interface
        subroutine tb01md(jobu, uplo, n, m, a, lda, b, ldb, &
                       u, ldu, dwork, info)
            character, intent(in)             :: jobu
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine tb01md
    end interface
    public :: tb01md
    
    interface
        subroutine tb01nd(jobu, uplo, n, p, a, lda, c, ldc, &
                       u, ldu, dwork, info)
            character, intent(in)             :: jobu
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine tb01nd
    end interface
    public :: tb01nd
    
    interface
        subroutine tb01pd(job, equil, n, m, p, a, lda, b, &
                       ldb, c, ldc, nr, tol, iwork, dwork, ldwork, &
                       info)
            character, intent(in)             :: job
            character, intent(in)             :: equil
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: nr
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb01pd
    end interface
    public :: tb01pd
    
    interface
        subroutine tb01px(job, equil, n, m, p, a, lda, b, &
                       ldb, c, ldc, nr, infred, tol, iwork, dwork, &
                       ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: equil
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: nr
            integer, intent(out)              :: infred(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb01px
    end interface
    public :: tb01px
    
    interface
        subroutine tb01td(n, m, p, a, lda, b, ldb, c, &
                       ldc, d, ldd, low, igh, scstat, scin, scout, &
                       dwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: low
            integer, intent(out)              :: igh
            double precision, intent(out)     :: scstat(*)
            double precision, intent(out)     :: scin(*)
            double precision, intent(out)     :: scout(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine tb01td
    end interface
    public :: tb01td
    
    interface
        subroutine tb01ty(mode, ioff, joff, nrow, ncol, size, x, ldx, &
                       bvect)
            integer, intent(in)               :: mode
            integer, intent(in)               :: ioff
            integer, intent(in)               :: joff
            integer, intent(in)               :: nrow
            integer, intent(in)               :: ncol
            double precision, intent(in)      :: size
            double precision, intent(inout)   :: x(ldx, *)
            integer, intent(in)               :: ldx
            double precision, intent(inout)   :: bvect(*)
        end subroutine tb01ty
    end interface
    public :: tb01ty
    
    interface
        subroutine tb01ud(jobz, n, m, p, a, lda, b, ldb, &
                       c, ldc, ncont, indcon, nblk, z, ldz, tau, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: jobz
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: ncont
            integer, intent(out)              :: indcon
            integer, intent(out)              :: nblk(*)
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(out)     :: tau(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb01ud
    end interface
    public :: tb01ud
    
    interface
        subroutine tb01ux(compz, n, m, p, a, lda, b, ldb, &
                       c, ldc, z, ldz, nobsv, nlblck, ctau, tol, &
                       iwork, dwork, info)
            character, intent(in)             :: compz
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: nobsv
            integer, intent(out)              :: nlblck
            integer, intent(out)              :: ctau(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine tb01ux
    end interface
    public :: tb01ux
    
    interface
        subroutine tb01uy(jobz, n, m1, m2, p, a, lda, b, &
                       ldb, c, ldc, ncont, indcon, nblk, z, ldz, &
                       tau, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: jobz
            integer, intent(in)               :: n
            integer, intent(in)               :: m1
            integer, intent(in)               :: m2
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: ncont
            integer, intent(out)              :: indcon
            integer, intent(out)              :: nblk(*)
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(out)     :: tau(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb01uy
    end interface
    public :: tb01uy
    
    interface
        subroutine tb01vd(apply, n, m, l, a, lda, b, ldb, &
                       c, ldc, d, ldd, x0, theta, ltheta, dwork, &
                       ldwork, info)
            character, intent(in)             :: apply
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(inout)   :: x0(*)
            double precision, intent(out)     :: theta(*)
            integer, intent(in)               :: ltheta
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb01vd
    end interface
    public :: tb01vd
    
    interface
        subroutine tb01vy(apply, n, m, l, theta, ltheta, a, lda, &
                       b, ldb, c, ldc, d, ldd, x0, dwork, &
                       ldwork, info)
            character, intent(in)             :: apply
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: l
            double precision, intent(in)      :: theta(*)
            integer, intent(in)               :: ltheta
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(out)     :: x0(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb01vy
    end interface
    public :: tb01vy
    
    interface
        subroutine tb01wd(n, m, p, a, lda, b, ldb, c, &
                       ldc, u, ldu, wr, wi, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(out)     :: wr(*)
            double precision, intent(out)     :: wi(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb01wd
    end interface
    public :: tb01wd
    
    interface
        subroutine tb01wx(compu, n, m, p, a, lda, b, ldb, &
                       c, ldc, u, ldu, dwork, ldwork, info)
            character, intent(in)             :: compu
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb01wx
    end interface
    public :: tb01wx
    
    interface
        subroutine tb01xd(jobd, n, m, p, kl, ku, a, lda, &
                       b, ldb, c, ldc, d, ldd, info)
            character, intent(in)             :: jobd
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: kl
            integer, intent(in)               :: ku
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: info
        end subroutine tb01xd
    end interface
    public :: tb01xd
    
    interface
        subroutine tb01xz(jobd, n, m, p, kl, ku, a, lda, &
                       b, ldb, c, ldc, d, ldd, info)
            character, intent(in)       :: jobd
            integer, intent(in)         :: n
            integer, intent(in)         :: m
            integer, intent(in)         :: p
            integer, intent(in)         :: kl
            integer, intent(in)         :: ku
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(in)         :: lda
            complex*16, intent(inout)   :: b(ldb, *)
            integer, intent(in)         :: ldb
            complex*16, intent(inout)   :: c(ldc, *)
            integer, intent(in)         :: ldc
            complex*16, intent(inout)   :: d(ldd, *)
            integer, intent(in)         :: ldd
            integer, intent(out)        :: info
        end subroutine tb01xz
    end interface
    public :: tb01xz
    
    interface
        subroutine tb01yd(n, m, p, a, lda, b, ldb, c, &
                       ldc, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: info
        end subroutine tb01yd
    end interface
    public :: tb01yd
    
    interface
        subroutine tb01zd(jobz, n, p, a, lda, b, c, ldc, &
                       ncont, z, ldz, tau, tol, dwork, ldwork, info)
            character, intent(in)             :: jobz
            integer, intent(in)               :: n
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: ncont
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(out)     :: tau(*)
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb01zd
    end interface
    public :: tb01zd
    
    interface
        subroutine tb03ad(leri, equil, n, m, p, a, lda, b, &
                       ldb, c, ldc, d, ldd, nr, index, pcoeff, &
                       ldpco1, ldpco2, qcoeff, ldqco1, ldqco2, vcoeff, ldvco1, ldvco2, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: leri
            character, intent(in)             :: equil
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nr
            integer, intent(out)              :: index(*)
            double precision, intent(out)     :: pcoeff(ldpco1, ldpco2, *)
            integer, intent(in)               :: ldpco1
            integer, intent(in)               :: ldpco2
            double precision, intent(out)     :: qcoeff(ldqco1, ldqco2, *)
            integer, intent(in)               :: ldqco1
            integer, intent(in)               :: ldqco2
            double precision, intent(out)     :: vcoeff(ldvco1, ldvco2, *)
            integer, intent(in)               :: ldvco1
            integer, intent(in)               :: ldvco2
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb03ad
    end interface
    public :: tb03ad
    
    interface
        subroutine tb03ay(nr, a, lda, indblk, nblk, vcoeff, ldvco1, ldvco2, &
                       pcoeff, ldpco1, ldpco2, info)
            integer, intent(in)               :: nr
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            integer, intent(in)               :: indblk
            integer, intent(in)               :: nblk(*)
            double precision, intent(inout)   :: vcoeff(ldvco1, ldvco2, *)
            integer, intent(in)               :: ldvco1
            integer, intent(in)               :: ldvco2
            double precision, intent(inout)   :: pcoeff(ldpco1, ldpco2, *)
            integer, intent(in)               :: ldpco1
            integer, intent(in)               :: ldpco2
            integer, intent(inout)            :: info
        end subroutine tb03ay
    end interface
    public :: tb03ay
    
    interface
        subroutine tb04ad(rowcol, n, m, p, a, lda, b, ldb, &
                       c, ldc, d, ldd, nr, index, dcoeff, lddcoe, &
                       ucoeff, lduco1, lduco2, tol1, tol2, iwork, dwork, ldwork, &
                       info)
            character, intent(in)             :: rowcol
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nr
            integer, intent(out)              :: index(*)
            double precision, intent(out)     :: dcoeff(lddcoe, *)
            integer, intent(in)               :: lddcoe
            double precision, intent(out)     :: ucoeff(lduco1, lduco2, *)
            integer, intent(in)               :: lduco1
            integer, intent(in)               :: lduco2
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb04ad
    end interface
    public :: tb04ad
    
    interface
        subroutine tb04ay(n, mwork, pwork, a, lda, b, ldb, c, &
                       ldc, d, ldd, ncont, indexd, dcoeff, lddcoe, ucoeff, &
                       lduco1, lduco2, at, n1, tau, tol1, tol2, iwork, &
                       dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: mwork
            integer, intent(in)               :: pwork
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(in)               :: ncont
            integer, intent(in)               :: indexd(*)
            double precision, intent(in)      :: dcoeff(lddcoe, *)
            integer, intent(in)               :: lddcoe
            double precision, intent(inout)   :: ucoeff(lduco1, lduco2, *)
            integer, intent(in)               :: lduco1
            integer, intent(in)               :: lduco2
            double precision, intent(inout)   :: at(n1, *)
            integer, intent(in)               :: n1
            double precision, intent(in)      :: tau(*)
            double precision, intent(in)      :: tol1
            double precision, intent(in)      :: tol2
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(inout)            :: info
        end subroutine tb04ay
    end interface
    public :: tb04ay
    
    interface
        subroutine tb04bd(jobd, order, equil, n, m, p, md, a, &
                       lda, b, ldb, c, ldc, d, ldd, ign, &
                       ldign, igd, ldigd, gn, gd, tol, iwork, dwork, &
                       ldwork, info)
            character, intent(in)             :: jobd
            character, intent(in)             :: order
            character, intent(in)             :: equil
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: md
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: ign(ldign, *)
            integer, intent(in)               :: ldign
            integer, intent(out)              :: igd(ldigd, *)
            integer, intent(in)               :: ldigd
            double precision, intent(out)     :: gn(*)
            double precision, intent(out)     :: gd(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb04bd
    end interface
    public :: tb04bd
    
    interface
        subroutine tb04bv(order, p, m, md, ign, ldign, igd, ldigd, &
                       gn, gd, d, ldd, tol, info)
            character, intent(in)             :: order
            integer, intent(in)               :: p
            integer, intent(in)               :: m
            integer, intent(in)               :: md
            integer, intent(inout)            :: ign(ldign, *)
            integer, intent(in)               :: ldign
            integer, intent(in)               :: igd(ldigd, *)
            integer, intent(in)               :: ldigd
            double precision, intent(inout)   :: gn(*)
            double precision, intent(in)      :: gd(*)
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: tol
            integer, intent(out)              :: info
        end subroutine tb04bv
    end interface
    public :: tb04bv
    
    interface
        subroutine tb04bw(order, p, m, md, ign, ldign, igd, ldigd, &
                       gn, gd, d, ldd, info)
            character, intent(in)             :: order
            integer, intent(in)               :: p
            integer, intent(in)               :: m
            integer, intent(in)               :: md
            integer, intent(inout)            :: ign(ldign, *)
            integer, intent(in)               :: ldign
            integer, intent(in)               :: igd(ldigd, *)
            integer, intent(in)               :: ldigd
            double precision, intent(inout)   :: gn(*)
            double precision, intent(in)      :: gd(*)
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: info
        end subroutine tb04bw
    end interface
    public :: tb04bw
    
    interface
        subroutine tb04bx(ip, iz, a, lda, b, c, d, pr, &
                       pi, zr, zi, gain, iwork)
            integer, intent(in)               :: ip
            integer, intent(in)               :: iz
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(*)
            double precision, intent(in)      :: c(*)
            double precision, intent(in)      :: d
            double precision, intent(in)      :: pr(*)
            double precision, intent(in)      :: pi(*)
            double precision, intent(in)      :: zr(*)
            double precision, intent(in)      :: zi(*)
            double precision, intent(out)     :: gain
            integer, intent(inout)            :: iwork(*)
        end subroutine tb04bx
    end interface
    public :: tb04bx
    
    interface
        subroutine tb04cd(jobd, equil, n, m, p, npz, a, lda, &
                       b, ldb, c, ldc, d, ldd, nz, ldnz, &
                       np, ldnp, zerosr, zerosi, polesr, polesi, gains, ldgain, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: jobd
            character, intent(in)             :: equil
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: npz
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: nz(ldnz, *)
            integer, intent(in)               :: ldnz
            integer, intent(out)              :: np(ldnp, *)
            integer, intent(in)               :: ldnp
            double precision, intent(out)     :: zerosr(*)
            double precision, intent(out)     :: zerosi(*)
            double precision, intent(out)     :: polesr(*)
            double precision, intent(out)     :: polesi(*)
            double precision, intent(out)     :: gains(ldgain, *)
            integer, intent(in)               :: ldgain
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tb04cd
    end interface
    public :: tb04cd
    
    interface
        subroutine tb05ad(baleig, inita, n, m, p, freq, a, lda, &
                       b, ldb, c, ldc, rcond, g, ldg, evre, &
                       evim, hinvb, ldhinv, iwork, dwork, ldwork, zwork, lzwork, &
                       info)
            character, intent(in)             :: baleig
            character, intent(in)             :: inita
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            complex*16, intent(in)            :: freq
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: rcond
            complex*16, intent(out)           :: g(ldg, *)
            integer, intent(in)               :: ldg
            double precision, intent(out)     :: evre(*)
            double precision, intent(out)     :: evim(*)
            complex*16, intent(out)           :: hinvb(ldhinv, *)
            integer, intent(in)               :: ldhinv
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine tb05ad
    end interface
    public :: tb05ad
    
    interface
        subroutine tc01od(leri, m, p, indlim, pcoeff, ldpco1, ldpco2, qcoeff, &
                       ldqco1, ldqco2, info)
            character, intent(in)             :: leri
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: indlim
            double precision, intent(inout)   :: pcoeff(ldpco1, ldpco2, *)
            integer, intent(in)               :: ldpco1
            integer, intent(in)               :: ldpco2
            double precision, intent(inout)   :: qcoeff(ldqco1, ldqco2, *)
            integer, intent(in)               :: ldqco1
            integer, intent(in)               :: ldqco2
            integer, intent(out)              :: info
        end subroutine tc01od
    end interface
    public :: tc01od
    
    interface
        subroutine tc04ad(leri, m, p, index, pcoeff, ldpco1, ldpco2, qcoeff, &
                       ldqco1, ldqco2, n, rcond, a, lda, b, ldb, &
                       c, ldc, d, ldd, iwork, dwork, ldwork, info)
            character, intent(in)             :: leri
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: index(*)
            double precision, intent(in)      :: pcoeff(ldpco1, ldpco2, *)
            integer, intent(in)               :: ldpco1
            integer, intent(in)               :: ldpco2
            double precision, intent(in)      :: qcoeff(ldqco1, ldqco2, *)
            integer, intent(in)               :: ldqco1
            integer, intent(in)               :: ldqco2
            integer, intent(out)              :: n
            double precision, intent(out)     :: rcond
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tc04ad
    end interface
    public :: tc04ad
    
    interface
        subroutine tc05ad(leri, m, p, sval, index, pcoeff, ldpco1, ldpco2, &
                       qcoeff, ldqco1, ldqco2, rcond, cfreqr, ldcfre, iwork, dwork, &
                       zwork, info)
            character, intent(in)             :: leri
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            complex*16, intent(in)            :: sval
            integer, intent(in)               :: index(*)
            double precision, intent(in)      :: pcoeff(ldpco1, ldpco2, *)
            integer, intent(in)               :: ldpco1
            integer, intent(in)               :: ldpco2
            double precision, intent(in)      :: qcoeff(ldqco1, ldqco2, *)
            integer, intent(in)               :: ldqco1
            integer, intent(in)               :: ldqco2
            double precision, intent(out)     :: rcond
            complex*16, intent(out)           :: cfreqr(ldcfre, *)
            integer, intent(in)               :: ldcfre
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(out)              :: info
        end subroutine tc05ad
    end interface
    public :: tc05ad
    
    interface
        subroutine td03ad(rowcol, leri, equil, m, p, indexd, dcoeff, lddcoe, &
                       ucoeff, lduco1, lduco2, nr, a, lda, b, ldb, &
                       c, ldc, d, ldd, indexp, pcoeff, ldpco1, ldpco2, &
                       qcoeff, ldqco1, ldqco2, vcoeff, ldvco1, ldvco2, tol, iwork, &
                       dwork, ldwork, info)
            character, intent(in)             :: rowcol
            character, intent(in)             :: leri
            character, intent(in)             :: equil
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: indexd(*)
            double precision, intent(in)      :: dcoeff(lddcoe, *)
            integer, intent(in)               :: lddcoe
            double precision, intent(in)      :: ucoeff(lduco1, lduco2, *)
            integer, intent(in)               :: lduco1
            integer, intent(in)               :: lduco2
            integer, intent(out)              :: nr
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: indexp(*)
            double precision, intent(out)     :: pcoeff(ldpco1, ldpco2, *)
            integer, intent(in)               :: ldpco1
            integer, intent(in)               :: ldpco2
            double precision, intent(out)     :: qcoeff(ldqco1, ldqco2, *)
            integer, intent(in)               :: ldqco1
            integer, intent(in)               :: ldqco2
            double precision, intent(out)     :: vcoeff(ldvco1, ldvco2, *)
            integer, intent(in)               :: ldvco1
            integer, intent(in)               :: ldvco2
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine td03ad
    end interface
    public :: td03ad
    
    interface
        subroutine td03ay(mwork, pwork, index, dcoeff, lddcoe, ucoeff, lduco1, lduco2, &
                       n, a, lda, b, ldb, c, ldc, d, &
                       ldd, info)
            integer, intent(in)               :: mwork
            integer, intent(in)               :: pwork
            integer, intent(in)               :: index(*)
            double precision, intent(in)      :: dcoeff(lddcoe, *)
            integer, intent(in)               :: lddcoe
            double precision, intent(in)      :: ucoeff(lduco1, lduco2, *)
            integer, intent(in)               :: lduco1
            integer, intent(in)               :: lduco2
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(inout)            :: info
        end subroutine td03ay
    end interface
    public :: td03ay
    
    interface
        subroutine td04ad(rowcol, m, p, index, dcoeff, lddcoe, ucoeff, lduco1, &
                       lduco2, nr, a, lda, b, ldb, c, ldc, &
                       d, ldd, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: rowcol
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: index(*)
            double precision, intent(in)      :: dcoeff(lddcoe, *)
            integer, intent(in)               :: lddcoe
            double precision, intent(in)      :: ucoeff(lduco1, lduco2, *)
            integer, intent(in)               :: lduco1
            integer, intent(in)               :: lduco2
            integer, intent(out)              :: nr
            double precision, intent(out)     :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(out)     :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(out)     :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine td04ad
    end interface
    public :: td04ad
    
    interface
        subroutine td05ad(unitf, output, np1, mp1, w, a, b, valr, &
                       vali, info)
            character, intent(in)           :: unitf
            character, intent(in)           :: output
            integer, intent(in)             :: np1
            integer, intent(in)             :: mp1
            double precision, intent(in)    :: w
            double precision, intent(in)    :: a(*)
            double precision, intent(in)    :: b(*)
            double precision, intent(out)   :: valr
            double precision, intent(out)   :: vali
            integer, intent(out)            :: info
        end subroutine td05ad
    end interface
    public :: td05ad
    
    interface
        subroutine tf01md(n, m, p, ny, a, lda, b, ldb, &
                       c, ldc, d, ldd, u, ldu, x, y, &
                       ldy, dwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: ny
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: x(*)
            double precision, intent(out)     :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine tf01md
    end interface
    public :: tf01md
    
    interface
        subroutine tf01mx(n, m, p, ny, s, lds, u, ldu, &
                       x, y, ldy, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: ny
            double precision, intent(in)      :: s(lds, *)
            integer, intent(in)               :: lds
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: x(*)
            double precision, intent(out)     :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tf01mx
    end interface
    public :: tf01mx
    
    interface
        subroutine tf01my(n, m, p, ny, a, lda, b, ldb, &
                       c, ldc, d, ldd, u, ldu, x, y, &
                       ldy, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: ny
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: x(*)
            double precision, intent(out)     :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tf01my
    end interface
    public :: tf01my
    
    interface
        subroutine tf01nd(uplo, n, m, p, ny, a, lda, b, &
                       ldb, c, ldc, d, ldd, u, ldu, x, &
                       y, ldy, dwork, info)
            character, intent(in)             :: uplo
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: ny
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(in)      :: d(ldd, *)
            integer, intent(in)               :: ldd
            double precision, intent(in)      :: u(ldu, *)
            integer, intent(in)               :: ldu
            double precision, intent(inout)   :: x(*)
            double precision, intent(out)     :: y(ldy, *)
            integer, intent(in)               :: ldy
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine tf01nd
    end interface
    public :: tf01nd
    
    interface
        subroutine tf01od(nh1, nh2, nr, nc, h, ldh, t, ldt, &
                       info)
            integer, intent(in)             :: nh1
            integer, intent(in)             :: nh2
            integer, intent(in)             :: nr
            integer, intent(in)             :: nc
            double precision, intent(in)    :: h(ldh, *)
            integer, intent(in)             :: ldh
            double precision, intent(out)   :: t(ldt, *)
            integer, intent(in)             :: ldt
            integer, intent(out)            :: info
        end subroutine tf01od
    end interface
    public :: tf01od
    
    interface
        subroutine tf01pd(nh1, nh2, nr, nc, h, ldh, t, ldt, &
                       info)
            integer, intent(in)             :: nh1
            integer, intent(in)             :: nh2
            integer, intent(in)             :: nr
            integer, intent(in)             :: nc
            double precision, intent(in)    :: h(ldh, *)
            integer, intent(in)             :: ldh
            double precision, intent(out)   :: t(ldt, *)
            integer, intent(in)             :: ldt
            integer, intent(out)            :: info
        end subroutine tf01pd
    end interface
    public :: tf01pd
    
    interface
        subroutine tf01qd(nc, nb, n, iord, ar, ma, h, ldh, &
                       info)
            integer, intent(in)             :: nc
            integer, intent(in)             :: nb
            integer, intent(in)             :: n
            integer, intent(in)             :: iord(*)
            double precision, intent(in)    :: ar(*)
            double precision, intent(in)    :: ma(*)
            double precision, intent(out)   :: h(ldh, *)
            integer, intent(in)             :: ldh
            integer, intent(out)            :: info
        end subroutine tf01qd
    end interface
    public :: tf01qd
    
    interface
        subroutine tf01rd(na, nb, nc, n, a, lda, b, ldb, &
                       c, ldc, h, ldh, dwork, ldwork, info)
            integer, intent(in)               :: na
            integer, intent(in)               :: nb
            integer, intent(in)               :: nc
            integer, intent(in)               :: n
            double precision, intent(in)      :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(in)      :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(in)      :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: h(ldh, *)
            integer, intent(in)               :: ldh
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tf01rd
    end interface
    public :: tf01rd
    
    interface
        subroutine tg01ad(job, l, n, m, p, thresh, a, lda, &
                       e, lde, b, ldb, c, ldc, lscale, rscale, &
                       dwork, info)
            character, intent(in)             :: job
            integer, intent(in)               :: l
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: thresh
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: lscale(*)
            double precision, intent(out)     :: rscale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine tg01ad
    end interface
    public :: tg01ad
    
    interface
        subroutine tg01az(job, l, n, m, p, thresh, a, lda, &
                       e, lde, b, ldb, c, ldc, lscale, rscale, &
                       dwork, info)
            character, intent(in)             :: job
            integer, intent(in)               :: l
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: thresh
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(in)               :: lde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: lscale(*)
            double precision, intent(out)     :: rscale(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine tg01az
    end interface
    public :: tg01az
    
    interface
        subroutine tg01bd(jobe, compq, compz, n, m, p, ilo, ihi, &
                       a, lda, e, lde, b, ldb, c, ldc, &
                       q, ldq, z, ldz, dwork, ldwork, info)
            character, intent(in)             :: jobe
            character, intent(in)             :: compq
            character, intent(in)             :: compz
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: ilo
            integer, intent(in)               :: ihi
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01bd
    end interface
    public :: tg01bd
    
    interface
        subroutine tg01cd(compq, l, n, m, a, lda, e, lde, &
                       b, ldb, q, ldq, dwork, ldwork, info)
            character, intent(in)             :: compq
            integer, intent(in)               :: l
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01cd
    end interface
    public :: tg01cd
    
    interface
        subroutine tg01dd(compz, l, n, p, a, lda, e, lde, &
                       c, ldc, z, ldz, dwork, ldwork, info)
            character, intent(in)             :: compz
            integer, intent(in)               :: l
            integer, intent(in)               :: n
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01dd
    end interface
    public :: tg01dd
    
    interface
        subroutine tg01ed(joba, l, n, m, p, a, lda, e, &
                       lde, b, ldb, c, ldc, q, ldq, z, &
                       ldz, ranke, rnka22, tol, dwork, ldwork, info)
            character, intent(in)             :: joba
            integer, intent(in)               :: l
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: ranke
            integer, intent(out)              :: rnka22
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01ed
    end interface
    public :: tg01ed
    
    interface
        subroutine tg01fd(compq, compz, joba, l, n, m, p, a, &
                       lda, e, lde, b, ldb, c, ldc, q, &
                       ldq, z, ldz, ranke, rnka22, tol, iwork, dwork, &
                       ldwork, info)
            character, intent(in)             :: compq
            character, intent(in)             :: compz
            character, intent(in)             :: joba
            integer, intent(in)               :: l
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: ranke
            integer, intent(out)              :: rnka22
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01fd
    end interface
    public :: tg01fd
    
    interface
        subroutine tg01fz(compq, compz, joba, l, n, m, p, a, &
                       lda, e, lde, b, ldb, c, ldc, q, &
                       ldq, z, ldz, ranke, rnka22, tol, iwork, dwork, &
                       zwork, lzwork, info)
            character, intent(in)             :: compq
            character, intent(in)             :: compz
            character, intent(in)             :: joba
            integer, intent(in)               :: l
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            complex*16, intent(inout)         :: a(lda, *)
            integer, intent(in)               :: lda
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(in)               :: lde
            complex*16, intent(inout)         :: b(ldb, *)
            integer, intent(in)               :: ldb
            complex*16, intent(inout)         :: c(ldc, *)
            integer, intent(in)               :: ldc
            complex*16, intent(inout)         :: q(ldq, *)
            integer, intent(in)               :: ldq
            complex*16, intent(inout)         :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: ranke
            integer, intent(out)              :: rnka22
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine tg01fz
    end interface
    public :: tg01fz
    
    interface
        subroutine tg01gd(jobs, l, n, m, p, a, lda, e, &
                       lde, b, ldb, c, ldc, d, ldd, lr, &
                       nr, ranke, infred, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: jobs
            integer, intent(in)               :: l
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: d(ldd, *)
            integer, intent(in)               :: ldd
            integer, intent(out)              :: lr
            integer, intent(out)              :: nr
            integer, intent(out)              :: ranke
            integer, intent(out)              :: infred
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01gd
    end interface
    public :: tg01gd
    
    interface
        subroutine tg01hd(jobcon, compq, compz, n, m, p, a, lda, &
                       e, lde, b, ldb, c, ldc, q, ldq, &
                       z, ldz, ncont, niucon, nrblck, rtau, tol, iwork, &
                       dwork, info)
            character, intent(in)             :: jobcon
            character, intent(in)             :: compq
            character, intent(in)             :: compz
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: ncont
            integer, intent(out)              :: niucon
            integer, intent(out)              :: nrblck
            integer, intent(out)              :: rtau(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine tg01hd
    end interface
    public :: tg01hd
    
    interface
        subroutine tg01hu(compq, compz, l, n, m1, m2, p, n1, &
                       lbe, a, lda, e, lde, b, ldb, c, &
                       ldc, q, ldq, z, ldz, nr, nrblck, rtau, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: compq
            character, intent(in)             :: compz
            integer, intent(in)               :: l
            integer, intent(in)               :: n
            integer, intent(in)               :: m1
            integer, intent(in)               :: m2
            integer, intent(in)               :: p
            integer, intent(in)               :: n1
            integer, intent(in)               :: lbe
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: nr
            integer, intent(out)              :: nrblck
            integer, intent(out)              :: rtau(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01hu
    end interface
    public :: tg01hu
    
    interface
        subroutine tg01hx(compq, compz, l, n, m, p, n1, lbe, &
                       a, lda, e, lde, b, ldb, c, ldc, &
                       q, ldq, z, ldz, nr, nrblck, rtau, tol, &
                       iwork, dwork, info)
            character, intent(in)             :: compq
            character, intent(in)             :: compz
            integer, intent(in)               :: l
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: n1
            integer, intent(in)               :: lbe
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: nr
            integer, intent(out)              :: nrblck
            integer, intent(out)              :: rtau(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine tg01hx
    end interface
    public :: tg01hx
    
    interface
        subroutine tg01hy(compq, compz, l, n, m, p, n1, lbe, &
                       a, lda, e, lde, b, ldb, c, ldc, &
                       q, ldq, z, ldz, nr, nrblck, rtau, tol, &
                       iwork, dwork, ldwork, info)
            character, intent(in)             :: compq
            character, intent(in)             :: compz
            integer, intent(in)               :: l
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: n1
            integer, intent(in)               :: lbe
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: nr
            integer, intent(out)              :: nrblck
            integer, intent(out)              :: rtau(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01hy
    end interface
    public :: tg01hy
    
    interface
        subroutine tg01id(jobobs, compq, compz, n, m, p, a, lda, &
                       e, lde, b, ldb, c, ldc, q, ldq, &
                       z, ldz, nobsv, niuobs, nlblck, ctau, tol, iwork, &
                       dwork, info)
            character, intent(in)             :: jobobs
            character, intent(in)             :: compq
            character, intent(in)             :: compz
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: nobsv
            integer, intent(out)              :: niuobs
            integer, intent(out)              :: nlblck
            integer, intent(out)              :: ctau(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(out)              :: info
        end subroutine tg01id
    end interface
    public :: tg01id
    
    interface
        subroutine tg01jd(job, systyp, equil, n, m, p, a, lda, &
                       e, lde, b, ldb, c, ldc, nr, infred, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: systyp
            character, intent(in)             :: equil
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: nr
            integer, intent(out)              :: infred(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01jd
    end interface
    public :: tg01jd
    
    interface
        subroutine tg01jy(job, systyp, equil, cksing, restor, n, m, p, &
                       a, lda, e, lde, b, ldb, c, ldc, &
                       nr, infred, tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: systyp
            character, intent(in)             :: equil
            character, intent(in)             :: cksing
            character, intent(in)             :: restor
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: nr
            integer, intent(out)              :: infred(*)
            double precision, intent(in)      :: tol(*)
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01jy
    end interface
    public :: tg01jy
    
    interface
        subroutine tg01kd(jobe, compc, compq, compz, n, a, lda, e, &
                       lde, b, c, incc, q, ldq, z, ldz, &
                       info)
            character, intent(in)             :: jobe
            character, intent(in)             :: compc
            character, intent(in)             :: compq
            character, intent(in)             :: compz
            integer, intent(in)               :: n
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(*)
            double precision, intent(inout)   :: c(*)
            integer, intent(in)               :: incc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: info
        end subroutine tg01kd
    end interface
    public :: tg01kd
    
    interface
        subroutine tg01kz(jobe, compc, compq, compz, n, a, lda, e, &
                       lde, b, c, incc, q, ldq, z, ldz, &
                       info)
            character, intent(in)       :: jobe
            character, intent(in)       :: compc
            character, intent(in)       :: compq
            character, intent(in)       :: compz
            integer, intent(in)         :: n
            complex*16, intent(inout)   :: a(lda, *)
            integer, intent(in)         :: lda
            complex*16, intent(inout)   :: e(lde, *)
            integer, intent(in)         :: lde
            complex*16, intent(inout)   :: b(*)
            complex*16, intent(inout)   :: c(*)
            integer, intent(in)         :: incc
            complex*16, intent(inout)   :: q(ldq, *)
            integer, intent(in)         :: ldq
            complex*16, intent(inout)   :: z(ldz, *)
            integer, intent(in)         :: ldz
            integer, intent(out)        :: info
        end subroutine tg01kz
    end interface
    public :: tg01kz
    
    interface
        subroutine tg01ld(job, joba, compq, compz, n, m, p, a, &
                       lda, e, lde, b, ldb, c, ldc, q, &
                       ldq, z, ldz, nf, nd, niblck, iblck, tol, &
                       iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: joba
            character, intent(in)             :: compq
            character, intent(in)             :: compz
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: nf
            integer, intent(out)              :: nd
            integer, intent(out)              :: niblck
            integer, intent(out)              :: iblck(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01ld
    end interface
    public :: tg01ld
    
    interface
        subroutine tg01ly(compq, compz, n, m, p, ranke, rnka22, a, &
                       lda, e, lde, b, ldb, c, ldc, q, &
                       ldq, z, ldz, nf, niblck, iblck, tol, iwork, &
                       dwork, ldwork, info)
            logical, intent(in)               :: compq
            logical, intent(in)               :: compz
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: ranke
            integer, intent(in)               :: rnka22
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: nf
            integer, intent(out)              :: niblck
            integer, intent(out)              :: iblck(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01ly
    end interface
    public :: tg01ly
    
    interface
        subroutine tg01md(job, n, m, p, a, lda, e, lde, &
                       b, ldb, c, ldc, alphar, alphai, beta, q, &
                       ldq, z, ldz, nf, nd, niblck, iblck, tol, &
                       iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: nf
            integer, intent(out)              :: nd
            integer, intent(out)              :: niblck
            integer, intent(out)              :: iblck(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01md
    end interface
    public :: tg01md
    
    interface
        subroutine tg01nd(job, jobt, n, m, p, a, lda, e, &
                       lde, b, ldb, c, ldc, alphar, alphai, beta, &
                       q, ldq, z, ldz, nf, nd, niblck, iblck, &
                       tol, iwork, dwork, ldwork, info)
            character, intent(in)             :: job
            character, intent(in)             :: jobt
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: nf
            integer, intent(out)              :: nd
            integer, intent(out)              :: niblck
            integer, intent(out)              :: iblck(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01nd
    end interface
    public :: tg01nd
    
    interface
        subroutine tg01nx(jobt, n, m, p, ndim, a, lda, e, &
                       lde, b, ldb, c, ldc, q, ldq, z, &
                       ldz, iwork, info)
            character, intent(in)             :: jobt
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: ndim
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(inout)            :: iwork(*)
            integer, intent(out)              :: info
        end subroutine tg01nx
    end interface
    public :: tg01nx
    
    interface
        subroutine tg01oa(jobe, n, dcba, lddcba, e, lde, info)
            character, intent(in)             :: jobe
            integer, intent(in)               :: n
            double precision, intent(inout)   :: dcba(lddcba, *)
            integer, intent(in)               :: lddcba
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            integer, intent(out)              :: info
        end subroutine tg01oa
    end interface
    public :: tg01oa
    
    interface
        subroutine tg01ob(jobe, n, dcba, lddcba, e, lde, info)
            character, intent(in)       :: jobe
            integer, intent(in)         :: n
            complex*16, intent(inout)   :: dcba(lddcba, *)
            integer, intent(in)         :: lddcba
            complex*16, intent(inout)   :: e(lde, *)
            integer, intent(in)         :: lde
            integer, intent(out)        :: info
        end subroutine tg01ob
    end interface
    public :: tg01ob
    
    interface
        subroutine tg01od(jobe, n, dcba, lddcba, e, lde, nz, g, &
                       tol, dwork, ldwork, info)
            character, intent(in)             :: jobe
            integer, intent(in)               :: n
            double precision, intent(inout)   :: dcba(lddcba, *)
            integer, intent(in)               :: lddcba
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            integer, intent(out)              :: nz
            double precision, intent(out)     :: g
            double precision, intent(in)      :: tol
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01od
    end interface
    public :: tg01od
    
    interface
        subroutine tg01oz(jobe, n, dcba, lddcba, e, lde, nz, g, &
                       tol, zwork, lzwork, info)
            character, intent(in)             :: jobe
            integer, intent(in)               :: n
            complex*16, intent(inout)         :: dcba(lddcba, *)
            integer, intent(in)               :: lddcba
            complex*16, intent(inout)         :: e(lde, *)
            integer, intent(in)               :: lde
            integer, intent(out)              :: nz
            complex*16, intent(out)           :: g
            double precision, intent(in)      :: tol
            complex*16, intent(inout)         :: zwork(*)
            integer, intent(in)               :: lzwork
            integer, intent(out)              :: info
        end subroutine tg01oz
    end interface
    public :: tg01oz
    
    interface
        subroutine tg01pd(dico, stdom, jobae, compq, compz, n, m, p, &
                       nlow, nsup, alpha, a, lda, e, lde, b, &
                       ldb, c, ldc, q, ldq, z, ldz, ndim, &
                       alphar, alphai, beta, dwork, ldwork, info)
            character, intent(in)             :: dico
            character, intent(in)             :: stdom
            character, intent(in)             :: jobae
            character, intent(in)             :: compq
            character, intent(in)             :: compz
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            integer, intent(in)               :: nlow
            integer, intent(in)               :: nsup
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(inout)   :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(inout)   :: z(ldz, *)
            integer, intent(in)               :: ldz
            integer, intent(out)              :: ndim
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01pd
    end interface
    public :: tg01pd
    
    interface
        subroutine tg01qd(dico, stdom, jobfi, n, m, p, alpha, a, &
                       lda, e, lde, b, ldb, c, ldc, n1, &
                       n2, n3, nd, niblck, iblck, q, ldq, z, &
                       ldz, alphar, alphai, beta, tol, iwork, dwork, ldwork, &
                       info)
            character, intent(in)             :: dico
            character, intent(in)             :: stdom
            character, intent(in)             :: jobfi
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(in)      :: alpha
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            integer, intent(out)              :: n1
            integer, intent(out)              :: n2
            integer, intent(out)              :: n3
            integer, intent(out)              :: nd
            integer, intent(out)              :: niblck
            integer, intent(out)              :: iblck(*)
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            double precision, intent(in)      :: tol
            integer, intent(inout)            :: iwork(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01qd
    end interface
    public :: tg01qd
    
    interface
        subroutine tg01wd(n, m, p, a, lda, e, lde, b, &
                       ldb, c, ldc, q, ldq, z, ldz, alphar, &
                       alphai, beta, dwork, ldwork, info)
            integer, intent(in)               :: n
            integer, intent(in)               :: m
            integer, intent(in)               :: p
            double precision, intent(inout)   :: a(lda, *)
            integer, intent(in)               :: lda
            double precision, intent(inout)   :: e(lde, *)
            integer, intent(in)               :: lde
            double precision, intent(inout)   :: b(ldb, *)
            integer, intent(in)               :: ldb
            double precision, intent(inout)   :: c(ldc, *)
            integer, intent(in)               :: ldc
            double precision, intent(out)     :: q(ldq, *)
            integer, intent(in)               :: ldq
            double precision, intent(out)     :: z(ldz, *)
            integer, intent(in)               :: ldz
            double precision, intent(out)     :: alphar(*)
            double precision, intent(out)     :: alphai(*)
            double precision, intent(out)     :: beta(*)
            double precision, intent(inout)   :: dwork(*)
            integer, intent(in)               :: ldwork
            integer, intent(out)              :: info
        end subroutine tg01wd
    end interface
    public :: tg01wd
    
    interface
        subroutine ud01bd(mp, np, dp, nin, p, ldp1, ldp2, info)
            integer, intent(in)             :: mp
            integer, intent(in)             :: np
            integer, intent(in)             :: dp
            integer, intent(in)             :: nin
            double precision, intent(out)   :: p(ldp1, ldp2, *)
            integer, intent(in)             :: ldp1
            integer, intent(in)             :: ldp2
            integer, intent(out)            :: info
        end subroutine ud01bd
    end interface
    public :: ud01bd
    
    interface
        subroutine ud01cd(mp, np, dp, nin, p, ldp1, ldp2, info)
            integer, intent(in)               :: mp
            integer, intent(in)               :: np
            integer, intent(in)               :: dp
            integer, intent(in)               :: nin
            double precision, intent(inout)   :: p(ldp1, ldp2, *)
            integer, intent(in)               :: ldp1
            integer, intent(in)               :: ldp2
            integer, intent(out)              :: info
        end subroutine ud01cd
    end interface
    public :: ud01cd
    
    interface
        subroutine ud01dd(m, n, nin, a, lda, info)
            integer, intent(in)             :: m
            integer, intent(in)             :: n
            integer, intent(in)             :: nin
            double precision, intent(out)   :: a(lda, *)
            integer, intent(in)             :: lda
            integer, intent(out)            :: info
        end subroutine ud01dd
    end interface
    public :: ud01dd
    
    interface
        subroutine ud01md(m, n, l, nout, a, lda, text, info)
            integer, intent(in)             :: m
            integer, intent(in)             :: n
            integer, intent(in)             :: l
            integer, intent(in)             :: nout
            double precision, intent(in)    :: a(lda, *)
            integer, intent(in)             :: lda
            character*(*), intent(in)       :: text
            integer, intent(out)            :: info
        end subroutine ud01md
    end interface
    public :: ud01md
    
    interface
        subroutine ud01mz(m, n, l, nout, a, lda, text, info)
            integer, intent(in)          :: m
            integer, intent(in)          :: n
            integer, intent(in)          :: l
            integer, intent(in)          :: nout
            complex*16, intent(in)       :: a(lda, *)
            integer, intent(in)          :: lda
            character*(*), intent(in)    :: text
            integer, intent(out)         :: info
        end subroutine ud01mz
    end interface
    public :: ud01mz
    
    interface
        subroutine ud01nd(mp, np, dp, l, nout, p, ldp1, ldp2, &
                       text, info)
            integer, intent(in)             :: mp
            integer, intent(in)             :: np
            integer, intent(in)             :: dp
            integer, intent(in)             :: l
            integer, intent(in)             :: nout
            double precision, intent(in)    :: p(ldp1, ldp2, *)
            integer, intent(in)             :: ldp1
            integer, intent(in)             :: ldp2
            character*(*), intent(in)       :: text
            integer, intent(out)            :: info
        end subroutine ud01nd
    end interface
    public :: ud01nd
    
    interface
        integer function ue01md (ispec,name,opts,n1,n2,n3)
            integer, intent(in)         :: ispec
            character*(*), intent(in)   :: name
            character*(*), intent(in)   :: opts
            integer, intent(in)         :: n1
            integer, intent(in)         :: n2
            integer, intent(in)         :: n3
        end function ue01md
    end interface
    public :: ue01md
    
end module slicot