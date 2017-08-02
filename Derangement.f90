!A code made to simulate the game from the Numberphile video:
!YouTube: Derangements - Numberphile
!https://www.youtube.com/watch?v=pbXg5EI5t4c
!
!Author: Carlos Jose Vargas Aguero
!email: carlos.vargasaguero@gmail.com
!
!This part of the program is to generate the ratio wins on a
!textfile "wins_ratio.txt"

program DerangementNp
implicit none

integer :: i,j,k,n_tries
integer, allocatable, dimension (:) :: permut_list
real :: n_wins
character(len=16) :: print_format

print_format = '(i8, 7x, f10.6)'
n_tries = 10000

open(unit=10,file="wins_ratio.txt")

call Play(1,n_tries,print_format)
call Play(10,n_tries,print_format)
call Play(100,n_tries,print_format)
call Play(1000,n_tries,print_format)
call Play(10000,n_tries,print_format)

close(unit=10)

contains

! Knuth Shuffle taken from
! https://www.rosettacode.org/wiki/Knuth_shuffle#Fortran
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

subroutine Play(cards_ord_mag,n_tries,print_format)
    integer :: i,j,k,n_tries,cards_ord_mag !Cards: Order of magnitude
    integer, allocatable, dimension (:) :: permut_list
    real :: n_wins !Reals to avoid problems dividing integer/integer, and need a real as the ratio
    character(len=16) :: print_format

    do i = cards_ord_mag,9*cards_ord_mag,cards_ord_mag
        !Have to change the size of the permutation list
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
    write(*,'(a, i8)') "Finished the order of magnitude: ", cards_ord_mag
end subroutine Play

end program DerangementNp
