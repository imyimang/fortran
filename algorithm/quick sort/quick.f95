module sort_module
    implicit none
    
contains

    recursive subroutine quick_sort(arr, left, right,attempts)
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: left, right
        integer :: i, j, pivot, temp,attempts

        if (left < right) then
            pivot = arr(left)
            i = left
            j = right

            do while (i <= j)
                do while (arr(i) < pivot)
                    i = i + 1
                end do
                do while (arr(j) > pivot)
                    j = j - 1
                end do

                if (i <= j) then
                    temp = arr(i)
                    arr(i) = arr(j)
                    arr(j) = temp
                    i = i + 1
                    j = j - 1
                    attempts = attempts + 1
                end if
            end do

            ! Recursive calls
            call quick_sort(arr, left, j, attempts)
            call quick_sort(arr, i, right, attempts)
        end if
    end subroutine quick_sort

end module sort_module



program test_quick_sort
    use sort_module
    implicit none
    integer, allocatable :: array(:)
    integer :: i,n,attempts

    write(*, '(A)', advance='no') "Please enter the size of the array to be sorted:"
    read *, n
    
    allocate(array(n))
    do i = 1,n
        write(*, '(A, I0, A)', advance='no') "enter the number", i, ": "
        read *, array(i)
    end do
    attempts = 0
    call quick_sort(array, 1, size(array), attempts)
    print *, "numbers after sort:"
    do i = 1, size(array)
        print *, array(i)
    end do
    write(*, '(A, I0)', advance='no') "how much times quick sort try: ", attempts

end program test_quick_sort




