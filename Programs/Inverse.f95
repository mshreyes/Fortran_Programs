!Inverse of a matrix by Gauss-Jordan elimination
!Shreyes Madgaonkar

program INVERSE
implicit none

real :: A(50,50), B(50,50), TEMP1, TEMP2, C1, C2, C4
integer :: I, J, N, M, L, K, P, R


write(*,*) "Enter the order of matrix"
read(*,*) L

write(*,*) "Enter the elements of the matrix"
do M=1,L
	read(*,*)(A(M,N), N=1,L)
end do

write(*,*) "Given matrix is A = "
do M=1,L
	write(*,*)(A(M,N), N=1,L)
end do

!Identity matrix
do M=1,L
	do N=1,L
		if (M==N) then
			B(M,N) = 1
		else 
			B(M,N) = 0
		end if
	end do        
end do

!********************************************************
!Gaussian elimination
do I = 1, L

	if (abs(A(I,I)) .le. 1.0E-10) then

		do P = 1, L
			TEMP1 = A(I, P)
			A(I,P) = A(I+1,P)
			A(I+1,P) = TEMP1

			TEMP2 = B(I, P)
			B(I,P) = B(I+1,P)	
			B(I+1,P) = TEMP2
		end do
	end if

	C1 = A(I,I)

	do J=1,L
		A(I,J) = A(I,J)/C1
		B(I,J) = B(I,J)/C1
	end do

	do J = I+1, L
		C2 = A(J,I)
		do K = 1, L
			A(J,K) = A(J,K) - C2*A(I,K)
			B(J,K) = B(J,K) - C2*B(I,K)
		end do
	end do      	
end do

!*********************************************************
!To determine the rank of the reduced matrix
R = 0

do J = 1, L
	if (A(J,J) == 1.0) then
		R = R + 1
	end if
end do

!**********************************************************
!To further reduce the matrices (if the inverse exists)
if (R==L) then

	do I = 1, L-1
		do J = I+1, L
			C4 = A(I,J)
			do K = 1, L 
				A(I,K) = A(I,K) - C4*A(J,K)
				B(I,K) = B(I,K) - C4*B(J,K)
			end do
		end do 
	end do	

	do I = 1, L
		do J = 1, L
			if (abs(A(I,J)) .le. 1.0E-10) then
				A(I,J) = 0.0
			end if 

			if (abs(B(I,J)) .le. 1.0E-10) then
				B(I,J) = 0.0
			end if
		end do

	end do

	write(*,*) "Inverse of matrix A is"

	do M=1,L
		write(*,*)(B(M,N), N=1,L)
	end do

else 

	write(*,*) "Inverse doesn't exist for provided matrix"

end if

end program INVERSE
