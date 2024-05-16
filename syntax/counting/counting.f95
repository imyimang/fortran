program counting

    implicit none
    integer :: number,i,j
    character(len = 20) :: str_number,str_j

    print *, "enter a number:"
    read *, number

    i = 0
    j = i

    write(str_number, '(I0)') number
    write(str_j, '(I0)') j
    print *, "start counting from "//trim(adjustl(str_j))//" to "//adjustl(str_number)

    do while(number > i) 
        i = i+1
        print *, i

    end do
    print *, "Successfully count from "//trim(adjustl(str_j))//" to "//adjustl(str_number)



end program counting

