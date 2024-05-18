program bogosort
    implicit none
    integer :: n ! 要排序的元素數量
    integer, allocatable :: array(:)
    integer :: attempts ! 嘗試的次數
    
    ! 從使用者讀取陣列大小
    write(*, '(A)', advance='no') "Please enter the size of the array to be sorted:"
    read *, n
    
    ! 分配動態陣列
    allocate(array(n))
    
    attempts = 0 ! 初始化嘗試次數
    

    call initialize_array(array)
    

    print *, "numbers before sort:"
    call print_array(array)
    

    call bogo_sort(array, attempts)
    

    print *, "numbers after sort:"
    call print_array(array)
    

    write(*, '(A)', advance='no') "how much times bogo try:"
    print *, attempts
    

    deallocate(array)

contains

    subroutine initialize_array(arr)
        integer, intent(inout) :: arr(:)
        integer :: i
        
        ! 用亂數初始化陣列
        do i = 1, size(arr)
            arr(i) = i
        end do
        call shuffle_array(arr)
    end subroutine initialize_array

    subroutine shuffle_array(arr)
        integer, intent(inout) :: arr(:)
        integer :: i, j, temp
        call random_seed()
        
        ! 隨機打亂陣列
        do i = size(arr), 2, -1
            j = randint(1, i)
            temp = arr(i)
            arr(i) = arr(j)
            arr(j) = temp
        end do
    end subroutine shuffle_array

    subroutine bogo_sort(arr, attempts)
        integer, intent(inout) :: arr(:)
        integer, intent(inout) :: attempts
        logical :: sorted
        sorted = .false.
        do while (.not. sorted)
            call shuffle_array(arr)
            sorted = is_sorted(arr)
            attempts = attempts + 1 ! 嘗試次數加一
        end do
    end subroutine bogo_sort
    
    logical function is_sorted(arr)
        integer, intent(in) :: arr(:)
        integer :: i
        do i = 1, size(arr) - 1
            if (arr(i) > arr(i+1)) then
                is_sorted = .false.
                return
            end if
        end do
        is_sorted = .true.
    end function is_sorted

    subroutine print_array(arr)
        integer, intent(in) :: arr(:)
        integer :: i
        do i = 1, size(arr)
            print *, arr(i)
        end do
    end subroutine print_array
    
    integer function randint(lower, upper)
        integer, intent(in) :: lower, upper
        integer :: range
        real :: r

        range = upper - lower + 1
        call random_number(r)
        randint = lower + int(r * range)
    end function randint

end program bogosort
