set xrange [1:140]
set xlabel "Number of workers"
set yrange [1:4000]
set ylabel "Wall time (in seconds)"
set title "Wall time spent solving the N-Queens problem versus number of workers"
set logscale
set terminal png
set output "scaling.png"
plot 'N_19' u 1:2 lc 1 title 'N=19 actual', x >= 4 && x <= 128 ? 1700*4/x : 0/0 lc 1 title 'N=19 ideal', \
     'N_18' u 1:2 lc 3 title 'N=18 actual', x >= 1 && x <= 128 ? 840/x    : 0/0 lc 3 title 'N=18 ideal', \
     'N_17' u 1:2 lc 4 title 'N=17 actual', x >= 1 && x <=  32 ? 110/x    : 0/0 lc 4 title 'N=17 ideal'
