program bubble

    integer :: i, j, m, sort_num
    integer, allocatable :: a(:)
    write(*, '(A)', advance='no') "enter the number of numbers you want to sort:"
    read *, sort_num
    
    allocate(a(sort_num))

    do i = 1,sort_num
        write(*, '(A)', advance='no') "enter the number"
        write(*, '(A)', advance='no') i
        write(*, '(A)', advance='no') ":"
        read *, a(i)
    end do

    m = size(a)

    do l = 1, m-1
        do i = 1, m-1
            if (a(i) > a(i+1)) then
                j = a(i)
                a(i) = a(i+1)
                a(i+1) = j
            end if
        end do
    end do

    print *, a

end program bubble
