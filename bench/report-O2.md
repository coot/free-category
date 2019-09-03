```
cabal run -O2 bench-cats -- -o bench/raport-O2.html
Up to date
benchmarking main/Cat/right right
time                 310.9 μs   (309.4 μs .. 312.5 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 311.7 μs   (309.9 μs .. 316.8 μs)
std dev              9.062 μs   (3.470 μs .. 17.57 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking main/Cat/right left
time                 1.696 ms   (1.678 ms .. 1.713 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 1.705 ms   (1.693 ms .. 1.741 ms)
std dev              59.40 μs   (25.74 μs .. 111.9 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking main/Cat/left left
time                 1.695 ms   (1.674 ms .. 1.714 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.711 ms   (1.699 ms .. 1.741 ms)
std dev              60.22 μs   (27.32 μs .. 121.2 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking main/Cat/left right
time                 1.684 ms   (1.666 ms .. 1.701 ms)
                     0.996 R²   (0.989 R² .. 0.999 R²)
mean                 1.733 ms   (1.709 ms .. 1.792 ms)
std dev              119.3 μs   (48.20 μs .. 235.0 μs)
variance introduced by outliers: 52% (severely inflated)

benchmarking main/Cat/alternate
time                 1.463 ms   (1.448 ms .. 1.477 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 1.494 ms   (1.477 ms .. 1.529 ms)
std dev              83.76 μs   (42.07 μs .. 149.1 μs)
variance introduced by outliers: 43% (moderately inflated)

benchmarking main/CatL/right right
time                 1.656 ms   (1.641 ms .. 1.669 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 1.668 ms   (1.662 ms .. 1.685 ms)
std dev              32.42 μs   (9.361 μs .. 66.77 μs)

benchmarking main/CatL/right left
time                 794.6 μs   (781.0 μs .. 809.5 μs)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 791.0 μs   (784.6 μs .. 803.2 μs)
std dev              29.21 μs   (14.98 μs .. 45.44 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking main/CatL/left left
time                 782.6 μs   (778.6 μs .. 786.8 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 789.7 μs   (785.6 μs .. 801.2 μs)
std dev              20.84 μs   (7.754 μs .. 40.62 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking main/CatL/left right
time                 780.8 μs   (776.2 μs .. 786.3 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 788.0 μs   (782.4 μs .. 805.9 μs)
std dev              28.81 μs   (11.03 μs .. 56.41 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking main/CatL/alternate
time                 1.631 ms   (1.621 ms .. 1.642 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.633 ms   (1.623 ms .. 1.656 ms)
std dev              49.15 μs   (26.49 μs .. 87.26 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking main/ListTr/right right
time                 615.5 μs   (612.1 μs .. 619.1 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 618.6 μs   (613.2 μs .. 637.3 μs)
std dev              27.93 μs   (8.673 μs .. 59.40 μs)
variance introduced by outliers: 37% (moderately inflated)

benchmarking main/ListTr/right left
time                 613.6 μs   (610.8 μs .. 617.4 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 615.1 μs   (612.4 μs .. 621.3 μs)
std dev              13.04 μs   (4.667 μs .. 24.51 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking main/ListTr/left left 
time                 612.8 μs   (610.2 μs .. 616.0 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 614.2 μs   (611.7 μs .. 622.8 μs)
std dev              13.33 μs   (3.828 μs .. 25.56 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking main/ListTr/left right
time                 612.6 μs   (610.2 μs .. 615.0 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 614.7 μs   (612.2 μs .. 622.7 μs)
std dev              12.50 μs   (3.636 μs .. 25.31 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking main/ListTr/alternate 
time                 611.9 μs   (609.8 μs .. 614.1 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 613.4 μs   (611.0 μs .. 618.4 μs)
std dev              11.06 μs   (3.249 μs .. 18.49 μs)

benchmarking main/C/right right
time                 701.7 μs   (699.1 μs .. 705.5 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 702.1 μs   (699.5 μs .. 708.0 μs)
std dev              12.82 μs   (5.261 μs .. 21.17 μs)

benchmarking main/C/right left
time                 628.0 μs   (623.0 μs .. 633.4 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 637.4 μs   (630.6 μs .. 653.0 μs)
std dev              33.51 μs   (19.90 μs .. 51.63 μs)
variance introduced by outliers: 45% (moderately inflated)

benchmarking main/C/left left
time                 724.9 μs   (721.4 μs .. 728.9 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 731.5 μs   (725.3 μs .. 751.5 μs)
std dev              36.12 μs   (11.50 μs .. 72.16 μs)
variance introduced by outliers: 41% (moderately inflated)

benchmarking main/C/left right
time                 639.5 μs   (626.3 μs .. 667.2 μs)
                     0.965 R²   (0.908 R² .. 0.998 R²)
mean                 654.0 μs   (631.3 μs .. 708.1 μs)
std dev              106.6 μs   (32.29 μs .. 219.6 μs)
variance introduced by outliers: 89% (severely inflated)

benchmarking main/C/alternate
time                 985.5 μs   (973.1 μs .. 1.007 ms)
                     0.957 R²   (0.880 R² .. 0.998 R²)
mean                 1.072 ms   (1.022 ms .. 1.188 ms)
std dev              241.4 μs   (78.27 μs .. 465.3 μs)
variance introduced by outliers: 94% (severely inflated)
```
