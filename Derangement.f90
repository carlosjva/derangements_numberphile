
!
!A code made to simulate the game from the Numberphile video:
!YouTube: Derangements - Numberphile
!https://www.youtube.com/watch?v=pbXg5EI5t4c
!
!Author: Carlos Jose Vargas Aguero
!email: carlos.vargasaguero@gmail.com
!
!This part of the program is generate the ratio wins generated in a text file

program DerangementNp
  implicit none

  integer :: i,j,k,n_tries
  integer, allocatable, dimension (:) :: permut_list
  real :: n_wins
  character(len=16) :: print_format
  ! To avoid integer division equals to 0
  print_format = '(i8, 7x, f10.6)'
  n_tries = 50000


! I know this is not elegant, I'll later pass this logarithm count to a subroutine
open(unit=10,file="wins_ratio.txt")
do i = 1,9
    allocate ( permut_list(1:i) )
    !Restarting the the counter of wins
    n_wins = 0.0
    permut_list =  [ ( j , j=1,i ) ]
    do j = 1,n_tries
            call Shuffle(permut_list)
            do k = 1,i
                !Search if there is any equal to its position
                if (k == permut_list(k)) then
                    n_wins = n_wins + 1
                    exit
                end if
            end do
    end do
    write(unit=10, fmt =print_format) i, (n_wins/n_tries)
    deallocate(permut_list)
end do

do i = 10,90,10
    allocate ( permut_list(1:i) )
    !Restarting the the counter of wins
    n_wins = 0.0
    permut_list =  [ ( j , j=1,i ) ]
    do j = 1,n_tries
            call Shuffle(permut_list)
            do k = 1,i
                !Search if there is any equal to its position
                if (k == permut_list(k)) then
                    n_wins = n_wins + 1
                    exit
                end if
            end do
    end do
    write(unit=10, fmt =print_format) i, (n_wins/n_tries)
    deallocate( permut_list)
end do


do i = 100,900,100
    allocate ( permut_list(1:i) )
    !Restarting the the counter of wins
    n_wins = 0.0
    permut_list =  [ ( j , j=1,i ) ]
    do j = 1,n_tries
            call Shuffle(permut_list)
            do k = 1,i
                !Search if there is any equal to its position
                if (k == permut_list(k)) then
                    n_wins = n_wins + 1
                    exit
                end if
            end do
    end do
    write(unit=10, fmt = print_format) i, (n_wins/n_tries)
    deallocate( permut_list)
end do


do i = 1000,9000,1000
    allocate ( permut_list(1:i) )
    !Restarting the the counter of wins
    n_wins = 0.0
    permut_list =  [ ( j , j=1,i ) ]
    do j = 1,n_tries
            call Shuffle(permut_list)
            do k = 1,i
                !Search if there is any equal to its position
                if (k == permut_list(k)) then
                    n_wins = n_wins + 1
                    exit
                end if
            end do
    end do
    write(unit=10, fmt =print_format) i, (n_wins/n_tries)
    deallocate( permut_list)
end do



do i = 10000,90000,10000
    allocate ( permut_list(1:i) )
    !Restarting the the counter of wins
    n_wins = 0.0
    permut_list =  [ ( j , j=1,i ) ]
    do j = 1,n_tries
            call Shuffle(permut_list)
            do k = 1,i
                !Search if there is any equal to its position
                if (k == permut_list(k)) then
                    n_wins = n_wins + 1
                    exit
                end if
            end do
    end do
    write(unit=10, fmt =print_format) i, (n_wins/n_tries)
    deallocate( permut_list)
end do

close(unit=10)

! Knuth Shuffle taken from
! https://www.rosettacode.org/wiki/Knuth_shuffle#Fortran


contains

subroutine Shuffle(a)
  integer, intent(inout) :: a(:)
  integer :: i, randpos, temp
  real :: r

  do i = size(a), 2, -1
    call random_number(r)
    randpos = int(r * i) + 1
    temp = a(randpos)
    a(randpos) = a(i)
    a(i) = temp
  end do

end subroutine Shuffle

!subroutine(i,n_wins,n_tries,print_format)
!    integer :: i,j,k,n_tries
!    integer, allocatable, dimension (:) :: permut_list
!    real :: n_wins

!    do i = 10000,90000,10000
!        allocate ( permut_list(1:i) )
        !Restarting the the counter of wins
!        n_wins = 0.0
!        permut_list =  [ ( j , j=1,i ) ]
!        do j = 1,n_tries
!                call Shuffle(permut_list)
!                do k = 1,i
                    !Search if there is any equal to its position
!                    if (k == permut_list(k)) then
!                        n_wins = n_wins + 1
!                        exit
!                    end if
!                end do
!        end do
!        write(unit=10, fmt =print_format) i, (n_wins/n_tries)
!        deallocate( permut_list)
!    end do




end program DerangementNp
