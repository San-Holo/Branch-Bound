# script_gnuplot.p
set   autoscale                        # scale axes automatically
unset log                              # remove any log-scaling
unset label                            # remove any previous labels
set xtic auto                          # set xtics automatically
set ytic auto                          # set ytics automatically
set title "One Born exact's algorithm execution times "
set xlabel "Trial's length"
set ylabel "Execution time (s)"
plot "exact_one_gnuplot.txt" using 1:2 title 'A_class' with linespoints , \
   "exact_one_gnuplot.txt" using 1:3 title 'B_class' with linespoints, \
      "exact_one_gnuplot.txt" using 1:4 title 'C_class' with linespoints
