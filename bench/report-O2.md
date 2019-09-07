```
cabal run -O2 bench-cats
benchmarking main/Cat/right right
time                 417.7 μs   (415.7 μs .. 419.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 418.3 μs   (416.6 μs .. 420.0 μs)
std dev              5.762 μs   (4.583 μs .. 7.364 μs)

benchmarking main/Cat/right left
time                 1.968 ms   (1.953 ms .. 1.990 ms)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 1.977 ms   (1.959 ms .. 2.000 ms)
std dev              68.00 μs   (53.52 μs .. 84.49 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking main/Cat/left left
time                 1.958 ms   (1.942 ms .. 1.975 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.969 ms   (1.959 ms .. 1.983 ms)
std dev              41.30 μs   (31.26 μs .. 57.77 μs)

benchmarking main/Cat/left right
time                 1.958 ms   (1.934 ms .. 1.982 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.953 ms   (1.943 ms .. 1.962 ms)
std dev              31.57 μs   (24.84 μs .. 42.80 μs)

benchmarking main/Cat/alternate
time                 1.655 ms   (1.641 ms .. 1.670 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.648 ms   (1.638 ms .. 1.656 ms)
std dev              30.06 μs   (24.24 μs .. 37.57 μs)

benchmarking main/Queue/right right
time                 359.4 μs   (357.7 μs .. 361.4 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 361.4 μs   (360.2 μs .. 363.5 μs)
std dev              5.338 μs   (3.727 μs .. 9.423 μs)

benchmarking main/Queue/right left
time                 359.6 μs   (357.8 μs .. 361.7 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 359.4 μs   (357.1 μs .. 363.2 μs)
std dev              9.314 μs   (6.026 μs .. 16.40 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking main/Queue/left left 
time                 364.9 μs   (361.2 μs .. 368.5 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 362.4 μs   (359.7 μs .. 366.1 μs)
std dev              10.49 μs   (8.487 μs .. 14.03 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking main/Queue/left right
time                 364.9 μs   (359.4 μs .. 372.7 μs)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 364.3 μs   (361.8 μs .. 367.9 μs)
std dev              10.03 μs   (7.286 μs .. 14.05 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking main/Queue/alternate 
time                 348.9 μs   (346.4 μs .. 351.5 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 351.0 μs   (349.2 μs .. 353.1 μs)
std dev              6.883 μs   (5.020 μs .. 9.645 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking main/ListTr/right right
time                 699.2 μs   (695.2 μs .. 702.4 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 695.1 μs   (692.3 μs .. 698.8 μs)
std dev              10.80 μs   (8.155 μs .. 15.39 μs)

benchmarking main/ListTr/right left
time                 707.9 μs   (696.8 μs .. 723.4 μs)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 700.7 μs   (692.7 μs .. 713.5 μs)
std dev              31.72 μs   (19.64 μs .. 51.81 μs)
variance introduced by outliers: 37% (moderately inflated)

benchmarking main/ListTr/left left 
time                 700.4 μs   (695.8 μs .. 706.9 μs)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 702.7 μs   (698.0 μs .. 715.4 μs)
std dev              24.32 μs   (11.51 μs .. 46.45 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking main/ListTr/left right
time                 703.8 μs   (691.8 μs .. 714.6 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 695.4 μs   (691.5 μs .. 699.8 μs)
std dev              13.95 μs   (10.12 μs .. 18.83 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking main/ListTr/alternate 
time                 706.3 μs   (700.2 μs .. 713.1 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 694.8 μs   (690.7 μs .. 699.6 μs)
std dev              14.73 μs   (12.14 μs .. 19.85 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking main/C/right right
time                 764.5 μs   (758.7 μs .. 769.3 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 756.4 μs   (753.1 μs .. 760.1 μs)
std dev              11.93 μs   (9.724 μs .. 14.72 μs)

benchmarking main/C/right left
time                 722.2 μs   (703.6 μs .. 741.4 μs)
                     0.996 R²   (0.994 R² .. 0.999 R²)
mean                 701.7 μs   (695.8 μs .. 712.3 μs)
std dev              26.03 μs   (17.36 μs .. 41.34 μs)
variance introduced by outliers: 29% (moderately inflated)

benchmarking main/C/left left
time                 811.6 μs   (799.5 μs .. 828.3 μs)
                     0.974 R²   (0.949 R² .. 0.994 R²)
mean                 897.2 μs   (857.3 μs .. 966.8 μs)
std dev              177.5 μs   (114.9 μs .. 283.1 μs)
variance introduced by outliers: 93% (severely inflated)

benchmarking main/C/left right
time                 720.7 μs   (707.1 μs .. 741.5 μs)
                     0.990 R²   (0.980 R² .. 0.998 R²)
mean                 733.5 μs   (719.8 μs .. 755.1 μs)
std dev              58.97 μs   (37.58 μs .. 86.10 μs)
variance introduced by outliers: 65% (severely inflated)

benchmarking main/C/alternate
time                 1.088 ms   (1.079 ms .. 1.096 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.094 ms   (1.088 ms .. 1.103 ms)
std dev              23.88 μs   (16.62 μs .. 38.52 μs)
variance introduced by outliers: 11% (moderately inflated)
```
