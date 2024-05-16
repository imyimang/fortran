program bubble

    implicit none
    integer :: i, j, m, l, sort_num
    integer, allocatable :: a(:)
    logical :: bool
    bool = .true.

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
        if (.not. bool) exit !檢測上一輪是否有任何交換，沒有就代表已經排序完成可以提早結束程式
        bool = .false.
        do i = 1, m-1
            if (a(i) > a(i+1)) then
                j = a(i)
                a(i) = a(i+1)
                a(i+1) = j
                bool = .true.
            end if
        end do
    end do
    print *, a


end program bubble
