!Matrix Operations

program MATOP
    implicit none

    real, dimension(100,100) :: A, B, C 
    integer :: I, J, K, L, Z1, Z2, N1, N2, M1, M2

    write(*,*) "Select the matrix operation"
    write(*,*) "1. Matrix addition"
    write(*,*) "2. Matrix multiplication"
    write(*,*) "3. Transpose of a matrix"
    read(*,*) Z2

    Z1 = int(Z2)

    if (Z1 == 1) then

        write(*,*) "Dimension of A and B"
        read(*,*) I, J

        write(*,*) "Enter the elements of first matrix"
        do K = 1,I
            read(*,*) (A(K,L), L = 1,J)
        end do

        write(*,*) "Enter the elements of second matrix"
        do K = 1,I
            read(*,*) (B(K,L), L = 1,J)
        end do

        call MATADD(I, J, A, B, C)

        write(*,*) "Addition of the matrices A and B is "
        do K = 1,I
            write(*,12) (C(K,L), L = 1,J)
        end do

    elseif (Z1 == 2) then

        write(*,*) "Dimension of A"
        read(*,*) N1, M1

        write(*,*) "Dimension of B"
        read(*,*) N2, M2

        if (M1 == N2) then
            write(*,*) "Enter the elements of matrix A"
            do K = 1,N1
                read(*,*) (A(K,L), L = 1,M1)
            end do

            write(*,*) "Enter the elements of matrix B"
            do K = 1,N2
                read(*,*) (B(K,L), L = 1,M2)
            end do

            call MATMULT(N1, M2, M1, A, B, C)

            write(*,*) "Product of the matrices A and B is "
            do K = 1,N1
                write(*,12) (C(K,L), L = 1,M2)
            end do  
        else
            write(*,*) "Provided matrices cannot be multiplied as they fail & the matrix multiplication condition"
        end if 

    elseif (Z1 == 3) then

        write(*,*) "Dimension of A"
        read(*,*) N1, M1

        write(*,*) "Enter the elements of matrix A"
        do K = 1,N1
            read(*,*) (A(K,L), L = 1,M1)
        end do

        write(*,*) "Enter the elements of matrix A"
        do L = 1, M1
            write(*,12) (A(K,L), K = 1,N1)
        end do
        
    end if

    12 format(100(f9.3,3x))
end program MATOP

!subroutine for matrix addition operation
subroutine MATADD(N, M, A, B, C)
    
    implicit none
    
    real, dimension(100,100) :: A, B, C
    integer :: I, J, N, M
    intent(out) :: C

    do I = 1,N 
        do J = 1,M 
            C(I,J) = A(I,J) + B(I,J)
        end do
    end do


return
end subroutine MATADD 

!subprogram for matrix multiplication
subroutine MATMULT(X1, X2, Y1, A, B, C1)
        
    real, dimension(100,100) :: A, B, C1
    integer :: I, J, X1, X2, Y1
    intent(out) :: C1

    do I = 1,X1
        do J = 1,X2
            C1(I,J) = 0

            do K = 1,Y1
             C1(I,J) = C1(I,J) + A(I,K)*B(K,J)
            end do  

        end do
    end do


return
end subroutine MATMULT
