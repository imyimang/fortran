program checkscore
    implicit none
    integer :: score
    character(len=3) :: str_score
    print *, "Write you score:"
    read *, score    

    if ((score>=60) .and. (score <= 100)) then
        write(str_score, '(I0)') score
        print *, "you are pass! your score is:"//str_score
    else if((score<60) .and. (score>=0)) then
        write(str_score, '(I0)') score
        print *, "you are fail.. your score is:"//str_score
    else 
        print *, "please enter a number between 0~100"
    end if


end program checkscore