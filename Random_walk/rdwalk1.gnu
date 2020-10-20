set title "Random walk (for 10000 steps)"
set autoscale
set xtics auto
set ytics auto

plot "rdwalk1.dat" using 1:2 with lines title "path"
pause -1