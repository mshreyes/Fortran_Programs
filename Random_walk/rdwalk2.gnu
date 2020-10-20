set title "Random walk (for 100000 steps)"
set autoscale
set xtics auto
set ytics auto

plot "rdwalk2.dat" using 1:2 with lines title "path"
pause -1