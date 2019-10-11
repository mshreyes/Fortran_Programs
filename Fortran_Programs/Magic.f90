program MAGIC
    implicit none
    
    integer :: A(100,100), NCUR , NSTART, NEND, N, M, I, J, JOLD, IOLD

    do 
        write(*,*) "Enter an odd positive integer (order of the magic square)"
        read (*,*) M 
        if (mod(M,2) .ne. 0) then
            N = M
            exit
        end if
    end do
    
    write(*,*) "What's the initial number?"
    read(*,*) NSTART

    NEND = NSTART + N**2 - 1

    do I = 1, N 
        do J = 1, N 
            A(I,J) = -9999
        end do
    end do

    I = (N+1)/2
    J = N 

    do NCUR = NSTART, NEND
        IOLD = I 
        JOLD = J 

        A(I,J) = NCUR
        I = I+1
        J = J+1

        if (I .gt. N) then
            I = 1
        end if

        if (J .gt. N) then
            J = 1
        end if

        if (A(i,J) .ne. -9999) then
            I = IOLD
            J = JOLD -1
        end if
    end do

    do I = 1, N 
        write(*,*) (A(I,J), J = 1, N)
    end do 


end program MAGIC
