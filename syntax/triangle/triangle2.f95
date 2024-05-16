
! 倒三角
program triangle
    implicit none
    integer :: num, i, j

    print *, "enter a number:"
    read *, num


    do i = 1, num

        do j = num,i,-1
            write(*, '(A)', advance='no') "*"
        end do
        
        print * 

    end do

end program triangle
