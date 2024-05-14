program array
    implicit none
    integer, dimension(5) :: a1
    integer :: num,i


    do i = 1, 5
        print *, "enter a number:"
        read *, num
        a1(i) = num
    end do

    print *, a1


end program array