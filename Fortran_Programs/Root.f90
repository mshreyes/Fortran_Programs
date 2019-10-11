!Matrix Operations

program ROOT
    implicit none

    real :: X, Y1, ZERO, A, B
    integer :: Z1, Z2, N1, S1

    ZERO = 0.5E-6

    write(*,*) "Select the method for finding the root of the following function"
    write(*,*) "F(X) = X^3 - 2X^2 - 5"
    write(*,*) "1. Newton-Raphson method"
    write(*,*) "2. Bisection method"
    write(*,*) "3. Secant method"
    read(*,*) Z2

    Z1 = int(Z2)

    if (Z1 == 1) then

        write(*,*) "Enter the guess value"
        read(*,*) X 

        call NR(X, Y1, S1, N1)

        if (S1 == 1) then
            write(*,12) Y1
            write(*,13) N1
        else
            write(*,*) "Try different guess value"
        end if
        
    elseif (Z1 == 2) then

        write(*,*) "Enter the values of A and B (bracketing interval)"
        read(*,*) A, B

        call BIS(A, B, Y1, S1, N1)

        if (S1 == 1) then
            write(*,12) Y1
            write(*,13) N1
        else
            write(*,*) "Try different values of A and B"
        end if
        
    elseif (Z1 == 3) then

        write(*,*) "Enter the values of A and B (bracketing interval)"
        read(*,*) A, B
    
        call SEC(A, B, Y1, S1, N1)

        if (S1 == 1) then
            write(*,12) Y1
            write(*,13) N1
        else
            write(*,*) "Try different values of A and B"
        end if

       
    end if
    12 format("The root of given equation is ", f9.4)
    13 format("Number of iterations ", I5)
end program ROOT

!Newton-Raphson
subroutine NR(A,Y,S,N)
    implicit none
    
    real :: X(0:1000), ZERO
    real :: Y, A, R, D1, F1
    integer :: S, I, N

    S = 0
    ZERO = 0.5E-6
    X(0) = A

    do  I = 0, 1000
        call F(X(I), F1)
        call DF(X(I), D1)

        X(I+1) = X(I) - (F1/D1)

        if (abs((X(I+1)-X(I))/X(I)) .le. ZERO) then
            R = X(I+1)
            N = I
            S = 1
            exit
        end if

        if (abs(F1) .le. ZERO) then
            R = X(I+1)
            N = I
            S = 1
            exit
        end if
    
        if ( abs(D1) .le. ZERO) then
            S = 2
            exit
        end if
    end do

    Y = R

return
end subroutine NR 

!Bisection method
subroutine BIS(A, B, Y, S, N)  
    implicit none

    real :: A, B, C, ZERO, FA, FB, FC, R, Y, K
    integer :: S, I, N

    S = 0
    ZERO = 0.5E-6

    call F(A, FA)
    call F(B, FB)

    if (FA*FB .lt. 0) then
        do  I = 1, 1000
            C = (A+B)/2
            call F(C, FC)

            if (FA*FC .lt. 0) then
                B = C 
                K = A 
                call F(B, FB)
            elseif (FB*FC .lt. 0) then
                A = C 
                K = B 
                call F(A, FA)
            end if
            
       
            if ((abs((C-K)/K) .le. ZERO) .or. (abs(FC) .le. ZERO)) then
                R = C
                N = I
                S = 1
                exit
            end if
        end do
    end if

    Y = R

return
end subroutine BIS

!Secant method
subroutine SEC(A, B, Y, S, N)  
    implicit none

    real :: X(0:1000), ZERO
    real :: Y, A, R, B, F1, F2
    integer :: S, I, N

    S = 0
    ZERO = 0.5E-6
    X(0) = A
    X(1) = B

    do  I = 0, 1000
        call F(X(I), F1)
        call F(X(I+1), F2)

        X(I+2) = X(I+1) - F2*((X(I+1) - X(I))/(F2 - F1))

        if (abs((X(I+2)-X(I+1))/X(I+1)) .le. ZERO) then
            R = X(I+2)
            N = I
            S = 1
            exit
        end if

        if (abs(F1-F2) .le. ZERO) then 
            S = 2
        end if  

        if (abs(F1) .le. ZERO) then
            R = X(I+2)
            N = I
            S = 1
            exit
        end if
    end do

    Y = R

return
end subroutine SEC


!Function
subroutine F(X, Y) 
    implicit none
    real :: X, Y 

    Y = X**3 - 2*(X**2) - 5
return    
end subroutine F

!Derivative of the provided function
subroutine DF(X, V) 
    implicit none
    real :: X, F1, F2, V
    call F(X, F2)
    call F(X + 1.0E-6, F1)
    
    V = (F1 - F2)/(1.0E-6)
return    
end subroutine DF