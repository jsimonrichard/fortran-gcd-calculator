program gcd_lin
    implicit none
    character(10) a_str,b_str;
    integer :: a,b;
    integer, dimension(3,2) :: M
    integer :: x,y,gcd;

    ! Get Command line arguments
    if (command_argument_count().ne.2) then
        write(*,*)'ERROR, TWO COMMAND-LINE ARGUMENTS REQUIRED, STOPPING'
        stop
    endif

    call get_command_argument(1, a_str)
    call get_command_argument(2, b_str)

    ! Convert to integers
    READ(a_str,*) a
    READ(b_str,*) b

    ! Create matrix
    M(:,1) = (/1,0,a/)
    M(:,2) = (/0,1,b/)

    ! Solve matrix
    do
        ! Handle end conditions
        if (M(3,1) == 0) then
            x=M(1,2)
            y=M(2,2)
            gcd=M(3,2)
            exit
        else if (M(3,2) == 0) then
            x=M(1,1)
            y=M(2,1)
            gcd=M(3,1)          
            exit

        ! Do a row op.
        else if (M(3,2) > M(3,1)) then
            M(:,2) = rowop3(M(:,2), M(:,1), M(3,2)/M(3,1))
        else
            M(:,1) = rowop3(M(:,1), M(:,2), M(3,1)/M(3,2))
        end if
    end do

    print "(i10,a5,i10,a5,i10,a5,i10,a5,i10)", a,"  *  ",x,"  +  ",b,"  *  ",y,"  =  ",gcd

contains
    function rowop3(r1, r2, k) result(row)
        implicit none
        integer, dimension(3), intent(in) :: r1,r2
        integer, intent(in) :: k
        integer, dimension(3) :: row;

        row = r1 - k*r2
    end function rowop3

end program gcd_lin