program RW2D
implicit none

integer :: I, N 
real :: R, X, Y

N = 1000000
X = 0
Y = 0

open(unit = 10, file = "rdwalk3.dat")

    write(10,*) 0, 0
    do I = 1, N 
        R = rand()

        if (R > 0 .and. R <= 0.25) then
            X = X + 1
            Y = Y
        else if (R > 0.25 .and. R <= 0.5) then
            X = X
            Y = Y + 1
        else if (R > 0.5 .and. R <= 0.75) then
            X = X - 1
            Y = Y
        else 
            X = X
            Y = Y - 1
        end if 

        write(10,*) X, Y
    end do

close(10)

end program RW2D