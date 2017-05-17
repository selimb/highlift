program main
    use globals, only: ni, nj, nk, X2D, X3D, nblocks, kmax, &
        set_sizes
    use utils
    implicit none
    integer :: i, j, k, m, n
    real*8 :: bgn, fin
    integer :: num
    integer, parameter :: AXI = 1
    integer, parameter :: AXJ = 2
    ! Input
    open(unit=10, form='formatted', file='modded2D.x')
    read(10,*) nblocks
    allocate(ni(nblocks))
    allocate(nj(nblocks))
    allocate(nk(nblocks))
    do m = 1, nblocks
       read(10,*) ni(m), nj(m)
    end do
    call set_sizes
    do m = 1, nblocks
        do n = 1, 2
            read(10,*) (( X2D(n,i,j,m), i=1,ni(m)), j=1,nj(m))
        end do
    end do

    call read_conn

    ! Identify blocks that can't be multigridded
    call output("0")

    ! Fix blocks
    call resize_blocks( &
        (/ 73, 57, 97, 101, 72, 53, 56, 98, 90, 49, 52, 89, 78, &
           33, 71, 82, 48, 70 /), &
        201, 2)

!   call dup_1(6, AXJ, 2)
!   call dup_1(37, AXI, 1)
!   call dup_1(52, AXI, 1)
!   call dup_1(35, AXJ, 2)
!   call dup_1(34, AXJ, 2)
!   call sync(9, 1)
!   call sync(47, 1)
!   call dup_1(36, AXI, 1)
!   call sync(47, 3)
!   call sync(4, 3)
!   call dup_1(50, AXJ, 1)
!   call dup_1(78, AXI, 2)
    call dup_propagate(6, AXJ, 2)
    call sync(9, 1)
    call sync(47, 1)
    call sync(47, 3)
    call sync(4, 3)

!   call dup_1(82, AXI, 2)
!   call dup_1(62, AXJ, 1)
!   call dup_1(61, AXJ, 1)
!   call dup_1(60, AXJ, 1)
!   call dup_1(59, AXJ, 1)
!   call dup_1(19, AXJ, 2)
!   call dup_1(38, AXI, 1)
!   call dup_1(39, AXI, 1)
!   call dup_1(26, AXJ, 2)
!   call dup_1(67, AXJ, 1)
!   call dup_1(68, AXJ, 1)
!   call dup_1(86, AXI, 1)
!   call dup_1(97, AXI, 1)
    call dup_propagate(82, AXI, 2)

!   call dup_1(72, AXI, 2)
!   call dup_1(29, AXI, 2)
!   call dup_1(28, AXJ, 2)
!   call dup_1(27, AXJ, 2)
!   call dup_1(66, AXJ, 1)
!   call dup_1(40, AXI, 1)
!   call dup_1(58, AXJ, 1)
!   call dup_1(20, AXJ, 2)
!   call dup_1(21, AXJ, 2)
!   call dup_1(22, AXJ, 2)
!   call dup_1(23, AXJ, 2)
!   call dup_1(71, AXI, 1)
    call dup_propagate(72, AXI, 2)

!   call dup_1(90, AXI, 2)
!   call dup_1(14, AXJ, 2)
!   call dup_1(13, AXJ, 2)
!   call dup_1(51, AXJ, 1)
!   call dup_1(45, AXI, 1)
!   call sync(1, 44)
!   call dup_1(44, AXJ, 2)
!   call sync(1, 45)
!   call dup_1(76, AXJ, 2)
!   call dup_1(43, AXJ, 2)
!   call sync(47, 3)
!   call sync(8, 3)
!   call dup_1(42, AXJ, 2)
!   call dup_1(41, AXJ, 2)
!   call dup_1(46, AXI, 1)
!   call dup_1(10, AXJ, 2)
!   call dup_1(54, AXJ, 1)
!   call dup_1(55, AXJ, 1)
!   call dup_1(56, AXI, 1)
    call dup_propagate(90, AXI, 2)
    call sync(1, 44)
    call sync(1, 45)
    call sync(47, 3)
    call sync(8, 3)

    call grow_blocks(AXJ, 1, 20, 2, &
        (/44, 1, 9, 11, 12, 49/))
    call grow_blocks(AXJ, 1, 14, 2, &
        (/42, 7, 58, 19, 30, 63/))
    call grow_blocks(AXI, 2, 11, 1, &
        (/75, 84, 85, 95, 101/))
    call grow_blocks(AXI, 2, 22, 1, &
        (/75, 84, 85, 95, 101/))
    call grow_blocks(AXI, 2, 11, 1, &
        (/77, 79, 80, 98/))
    call grow_blocks(AXI, 2, 22, 1, &
        (/77, 79, 80, 98/))

    call rem_1(34, AXI, 30)
    call rem_1(34, AXI, 28)
    call rem_1(47, AXJ, 30)
    call rem_1(47, AXJ, 28)
    call rem_1(76, AXI, 30)
    call rem_1(76, AXI, 28)

    call extend

    call output("1")

    ! Output
    open(unit=20, form='formatted', file='new2D.x')
    write(20,*) nblocks
    do m = 1, nblocks
       write(20,*) ni(m), nj(m)
    end do
    do m = 1, nblocks
        do n = 1, 2
            write(20,*) (( X2D(n,i,j,m), i=1,ni(m)), j=1,nj(m))
        end do
    end do

    ! Make sure connectivity is intact
    if (conn_intact()) then
    else
        write(*,*) "BROKE CONN"
        return
    end if

    bgn = 0
    fin = 80
    num = kmax
    open(unit=30, form='formatted', file='modded3D.x')
    write(30,*), nblocks
    do m = 1, nblocks
        write(30,*) ni(m), nj(m), num
    end do
    do m = 1, nblocks
        do n = 1, 3
            call extrude(m, bgn, fin, num)
            write(30,*) ((( X3D(n,i,j,k), i=1,ni(m)), j=1,nj(m)), k=1,nk(m))
        end do
    end do
end program
