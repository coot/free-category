```
cabal run -O2 bench-cats                                               
benchmarking main/Cat 100/right right
time                 387.7 μs   (382.0 μs .. 396.1 μs)
                     0.997 R²   (0.992 R² .. 1.000 R²)
mean                 382.3 μs   (378.8 μs .. 389.3 μs)
std dev              15.61 μs   (9.528 μs .. 27.64 μs)
variance introduced by outliers: 35% (moderately inflated)

benchmarking main/Cat 100/right left
time                 1.902 ms   (1.885 ms .. 1.919 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.926 ms   (1.911 ms .. 1.943 ms)
std dev              55.12 μs   (41.26 μs .. 90.08 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking main/Cat 100/left left
time                 1.941 ms   (1.924 ms .. 1.958 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.937 ms   (1.929 ms .. 1.947 ms)
std dev              30.60 μs   (23.82 μs .. 41.19 μs)

benchmarking main/Cat 100/left right
time                 1.946 ms   (1.895 ms .. 2.034 ms)
                     0.994 R²   (0.988 R² .. 0.999 R²)
mean                 1.982 ms   (1.957 ms .. 2.009 ms)
std dev              86.70 μs   (66.65 μs .. 112.2 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking main/Cat 100/alternate
time                 1.700 ms   (1.605 ms .. 1.902 ms)
                     0.954 R²   (0.904 R² .. 1.000 R²)
mean                 1.641 ms   (1.614 ms .. 1.740 ms)
std dev              159.2 μs   (28.75 μs .. 334.0 μs)
variance introduced by outliers: 69% (severely inflated)

benchmarking main/Queue 100/right right
time                 341.9 μs   (341.2 μs .. 342.8 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 340.9 μs   (340.0 μs .. 341.9 μs)
std dev              3.115 μs   (2.201 μs .. 4.603 μs)

benchmarking main/Queue 100/right left
time                 342.2 μs   (341.6 μs .. 342.8 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 341.5 μs   (340.9 μs .. 342.2 μs)
std dev              2.108 μs   (1.700 μs .. 2.556 μs)

benchmarking main/Queue 100/left left 
time                 341.5 μs   (341.1 μs .. 342.2 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 341.9 μs   (341.2 μs .. 343.3 μs)
std dev              3.117 μs   (1.865 μs .. 5.348 μs)

benchmarking main/Queue 100/left right
time                 344.0 μs   (342.7 μs .. 345.8 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 344.5 μs   (343.2 μs .. 346.5 μs)
std dev              5.442 μs   (4.365 μs .. 7.546 μs)

benchmarking main/Queue 100/alternate 
time                 359.1 μs   (343.2 μs .. 378.1 μs)
                     0.989 R²   (0.979 R² .. 0.999 R²)
mean                 347.1 μs   (343.3 μs .. 357.3 μs)
std dev              19.80 μs   (10.27 μs .. 35.11 μs)
variance introduced by outliers: 53% (severely inflated)

benchmarking main/ListTr 100/right right
time                 175.0 μs   (174.3 μs .. 176.0 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 175.8 μs   (174.8 μs .. 178.3 μs)
std dev              5.004 μs   (2.051 μs .. 9.617 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking main/ListTr 100/right left
time                 175.2 μs   (174.2 μs .. 176.4 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 175.3 μs   (174.6 μs .. 176.2 μs)
std dev              2.866 μs   (2.014 μs .. 4.405 μs)

benchmarking main/ListTr 100/left left 
time                 178.8 μs   (175.7 μs .. 182.2 μs)
                     0.998 R²   (0.998 R² .. 1.000 R²)
mean                 177.0 μs   (175.6 μs .. 179.9 μs)
std dev              6.350 μs   (3.620 μs .. 11.78 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking main/ListTr 100/left right
time                 174.2 μs   (173.3 μs .. 175.6 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 180.9 μs   (178.4 μs .. 184.9 μs)
std dev              10.83 μs   (7.423 μs .. 16.29 μs)
variance introduced by outliers: 59% (severely inflated)

benchmarking main/ListTr 100/alternate 
time                 173.9 μs   (173.0 μs .. 175.0 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 173.5 μs   (173.0 μs .. 174.1 μs)
std dev              1.951 μs   (1.514 μs .. 2.633 μs)

benchmarking main/C 100/right right
time                 798.5 μs   (792.7 μs .. 803.5 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 797.5 μs   (793.5 μs .. 800.9 μs)
std dev              11.98 μs   (9.948 μs .. 15.01 μs)

benchmarking main/C 100/right left
time                 679.3 μs   (670.8 μs .. 691.7 μs)
                     0.998 R²   (0.997 R² .. 0.998 R²)
mean                 686.6 μs   (678.4 μs .. 695.5 μs)
std dev              27.59 μs   (25.17 μs .. 30.52 μs)
variance introduced by outliers: 32% (moderately inflated)

benchmarking main/C 100/left left
time                 780.6 μs   (776.9 μs .. 783.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 775.6 μs   (772.1 μs .. 778.7 μs)
std dev              10.92 μs   (8.476 μs .. 15.08 μs)

benchmarking main/C 100/left right
time                 601.6 μs   (598.7 μs .. 604.4 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 607.9 μs   (602.8 μs .. 616.0 μs)
std dev              20.49 μs   (15.07 μs .. 26.09 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking main/C 100/alternate
time                 1.045 ms   (987.6 μs .. 1.161 ms)
                     0.940 R²   (0.867 R² .. 0.999 R²)
mean                 1.016 ms   (986.8 μs .. 1.107 ms)
std dev              152.4 μs   (57.42 μs .. 296.9 μs)
variance introduced by outliers: 86% (severely inflated)

benchmarking main/Queue 250/right right
time                 2.733 ms   (2.700 ms .. 2.764 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.694 ms   (2.676 ms .. 2.710 ms)
std dev              56.66 μs   (45.91 μs .. 76.23 μs)

benchmarking main/ListTr 250/right right
time                 3.356 ms   (3.175 ms .. 3.551 ms)
                     0.979 R²   (0.968 R² .. 0.989 R²)
mean                 3.425 ms   (3.315 ms .. 3.587 ms)
std dev              422.6 μs   (296.3 μs .. 610.7 μs)
variance introduced by outliers: 74% (severely inflated)

benchmarking main/Queue 500/right right
time                 11.27 ms   (10.84 ms .. 11.78 ms)
                     0.985 R²   (0.973 R² .. 0.993 R²)
mean                 12.36 ms   (11.94 ms .. 12.99 ms)
std dev              1.344 ms   (995.0 μs .. 1.928 ms)
variance introduced by outliers: 56% (severely inflated)

benchmarking main/ListTr 500/right right
time                 18.27 ms   (17.13 ms .. 19.35 ms)
                     0.986 R²   (0.975 R² .. 0.998 R²)
mean                 18.14 ms   (17.70 ms .. 18.98 ms)
std dev              1.466 ms   (866.9 μs .. 2.203 ms)
variance introduced by outliers: 35% (moderately inflated)
```
