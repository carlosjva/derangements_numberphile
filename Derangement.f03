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

!Creating the list of all cards
allocate( permut_list(1:n_tries) )
permut_list =  [ (j, j=1, 90000) ]
call init_random_seed()

open(unit=10,file="wins_ratio.txt")

call Play(1    ,n_tries, print_format, permut_list)
call Play(10   ,n_tries, print_format, permut_list)
call Play(100  ,n_tries, print_format, permut_list)
call Play(1000 ,n_tries, print_format, permut_list)
call Play(10000,n_tries, print_format, permut_list)

close(unit=10)
deallocate(permut_list)

write(*,'(a)') 'Program finished.'

contains

subroutine Play(cards_ord_mag,n_tries,print_format,permut_list)
    integer :: i,j,k,n_tries,cards_ord_mag !cards_ord_mag: Order of magnitude
    integer, intent(inout) :: permut_list(:)
    real :: n_wins !Real to avoid problems dividing integer/integer, and need a real as the ratio
    character(len=16) :: print_format

    !Running through the order of magnitudes of number of cards to play with.
    do i = cards_ord_mag, 9*cards_ord_mag, cards_ord_mag
        !Restarting the the counter of wins
        n_wins = 0.0

        do j = 1, n_tries
            call Shuffle(permut_list,i)
            do k = 1, i
            !Search if there is any equal to its position
                if (k == permut_list(k)) then
                    n_wins = n_wins + 1
                    exit
                end if
            end do
        end do
    write(unit=10, fmt =print_format) i, (n_wins/n_tries)
    write(*,'(a, i8)') "Finished: ", i
    end do
end subroutine Play

subroutine Shuffle(a,j)
    ! Knuth Shuffle taken from
    ! https://www.rosettacode.org/wiki/Knuth_shuffle#Fortran
    integer, intent(inout) :: a(:)
    integer :: i, randpos, temp,j
    real :: r
    !The permut_list is initialized with the 90000 cards, so we only
    !include the number to the size of the cards we want to play with
    !the variable j
    do i = j, 2, -1
        call random_number(r)
        randpos = int(r * i) + 1
        temp = a(randpos)
        a(randpos) = a(i)
    a(i) = temp
    end do
end subroutine Shuffle

subroutine init_random_seed()
    !Generate random seed
    !Taken from Physical Mathematics - K Cahill Page 540-541
    implicit none
    integer i, n, clock
    integer, dimension(:), allocatable :: seed
    call random_seed(size = n) ! find size of seed
    allocate(seed(n))
    call system_clock(count=clock)!get time of processor clock
    seed = clock + 37 * (/ (i-1, i=1, n) /) ! make seed
    call random_seed(put=seed) ! set seed
    deallocate(seed)
end subroutine init_random_seed

end program DerangementNp
