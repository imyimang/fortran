program bubble

    implicit none
    integer :: i, j, m, l, sort_num,times
    integer, allocatable :: a(:)
    logical :: bool
    bool = .true.
    times = 0

    write(*, '(A)', advance='no') "enter the number of numbers you want to sort:"
    read *, sort_num
    
    allocate(a(sort_num))

    do i = 1,sort_num
        write(*, '(A, I0, A)', advance='no') "enter the number", i, ": "
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
                times = times+1
            end if
        end do
    end do
    print *, a
    write(*, '(A, I0)', advance='no') "how much times bubble try: ", times


end program bubble
