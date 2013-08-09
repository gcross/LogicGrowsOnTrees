set xrange [1:140]
set xlabel "Number of workers"
set yrange [1:2000]
set ylabel "Wall time (in seconds)"
set title "Results of scaling experiment"
set logscale
  plot 'N_19' u 1:2 lc 1 title 'N=19 actual', x >= 4 && x <= 128 ? 1700*4/x : 0/0 lc 1 title 'N=19 ideal'
replot 'N_18' u 1:2 lc 3 title 'N=18 actual', x >= 1 && x <= 128 ? 840/x    : 0/0 lc 3 title 'N=18 ideal'
replot 'N_17' u 1:2 lc 4 title 'N=17 actual', x >= 1 && x <=  32 ? 110/x    : 0/0 lc 4 title 'N=17 ideal'
