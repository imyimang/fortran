program bmi
    implicit none
    character(len = 4) :: bmi_str
    integer :: height, weight
    real :: bmi_value
    
    write(*, '(A)', advance='no') "enter your height(cm):"
    read *, height
    write(*, '(A)', advance='no') "enter your weight(kg):"
    read *, weight

    
    bmi_value = weight / ((real(height) / 100.0)**2)
    if ((bmi_value<18.5) .and. (bmi_value>0)) then
        write(*, '(A)', advance='no') "You are too thin!,your bmi is:"

    else if ((bmi_value >= 18.5) .and. (bmi_value<24)) then
        write(*, '(A)', advance='no') "Your bmi is normal,your bmi is:"

    else if (bmi_value>=24) then
        write(*, '(A)', advance='no') "Your are too fat!,your bmi is:"
    
    else
        print *, "please the correct height and weight"

    end if


    bmi_value = anint(bmi_value*10.0)/10.0
    write(*, '(A,F0.1)') "", bmi_value

    
end program bmi
