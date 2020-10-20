program SORT
    implicit none
    
    real :: A(1000), TEMP
    integer :: N, I, J

    write(*,*) "What's the length of the list"
    read(*,*) N
    write(*,*) "Enter the elements of the list to be sorted"
    read(*,*) (A(I), I = 1, N)

 

    do J = 1, N
        do I = 1, N-1
                    if ( A(I) .ge. A(I+1) ) then
                        TEMP = A(I)
                        A(I) = A(I+1)
                            A(I+1) = TEMP
                    end if 
        end do
    end do    

    write(*,*) "Sorted list: "
    write(*,*) (A(I), I = 1, N)
    
end program SORT