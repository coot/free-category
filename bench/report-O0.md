```
cabal run -O0 bench-cats -- -o bench/raport-O0.html 
Up to date
benchmarking main/Cat/right right
time                 1.901 ms   (1.883 ms .. 1.916 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 1.887 ms   (1.874 ms .. 1.909 ms)
std dev              58.98 μs   (37.42 μs .. 98.37 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking main/Cat/right left
time                 2.789 ms   (2.750 ms .. 2.826 ms)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 2.843 ms   (2.818 ms .. 2.898 ms)
std dev              120.6 μs   (78.67 μs .. 183.1 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking main/Cat/left left
time                 2.790 ms   (2.750 ms .. 2.825 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 2.826 ms   (2.802 ms .. 2.884 ms)
std dev              120.4 μs   (48.42 μs .. 227.2 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking main/Cat/left right
time                 2.804 ms   (2.768 ms .. 2.836 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 2.815 ms   (2.801 ms .. 2.845 ms)
std dev              68.45 μs   (40.50 μs .. 116.4 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking main/Cat/alternate
time                 2.198 ms   (2.166 ms .. 2.224 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 2.211 ms   (2.199 ms .. 2.238 ms)
std dev              59.55 μs   (30.48 μs .. 103.4 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/CatL/right right
time                 1.970 ms   (1.951 ms .. 1.988 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.991 ms   (1.981 ms .. 2.017 ms)
std dev              50.05 μs   (24.37 μs .. 103.6 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking main/CatL/right left
time                 1.484 ms   (1.469 ms .. 1.496 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 1.496 ms   (1.488 ms .. 1.521 ms)
std dev              41.49 μs   (20.36 μs .. 85.75 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking main/CatL/left left
time                 1.490 ms   (1.476 ms .. 1.503 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.497 ms   (1.487 ms .. 1.525 ms)
std dev              47.63 μs   (22.53 μs .. 101.3 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking main/CatL/left right
time                 1.491 ms   (1.480 ms .. 1.501 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.512 ms   (1.502 ms .. 1.533 ms)
std dev              48.56 μs   (25.46 μs .. 86.16 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking main/CatL/alternate
time                 2.173 ms   (2.152 ms .. 2.194 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 2.183 ms   (2.174 ms .. 2.202 ms)
std dev              43.44 μs   (24.81 μs .. 78.66 μs)

benchmarking main/ListTr/right right
time                 659.5 μs   (655.9 μs .. 662.6 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 666.0 μs   (661.0 μs .. 676.3 μs)
std dev              23.17 μs   (11.53 μs .. 38.04 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking main/ListTr/right left
time                 659.1 μs   (655.2 μs .. 663.4 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 666.2 μs   (661.6 μs .. 677.2 μs)
std dev              22.93 μs   (10.85 μs .. 42.19 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking main/ListTr/left left 
time                 677.1 μs   (662.6 μs .. 694.4 μs)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 668.2 μs   (662.3 μs .. 677.1 μs)
std dev              24.46 μs   (13.89 μs .. 39.53 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking main/ListTr/left right
time                 665.3 μs   (659.6 μs .. 671.9 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 677.0 μs   (668.0 μs .. 693.8 μs)
std dev              39.44 μs   (24.39 μs .. 60.81 μs)
variance introduced by outliers: 50% (moderately inflated)

benchmarking main/ListTr/alternate 
time                 682.4 μs   (670.1 μs .. 692.7 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 669.5 μs   (664.9 μs .. 677.9 μs)
std dev              20.84 μs   (14.55 μs .. 32.31 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking main/C/right right
time                 908.5 μs   (798.8 μs .. 992.7 μs)
                     0.962 R²   (0.955 R² .. 0.997 R²)
mean                 827.5 μs   (810.2 μs .. 860.2 μs)
std dev              78.59 μs   (42.99 μs .. 126.5 μs)
variance introduced by outliers: 72% (severely inflated)

benchmarking main/C/right left
time                 845.5 μs   (840.0 μs .. 851.1 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 850.9 μs   (846.6 μs .. 856.8 μs)
std dev              17.43 μs   (12.64 μs .. 25.54 μs)
variance introduced by outliers: 10% (moderately inflated)

benchmarking main/C/left left
time                 1.030 ms   (1.023 ms .. 1.040 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.036 ms   (1.029 ms .. 1.052 ms)
std dev              33.36 μs   (16.74 μs .. 62.02 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking main/C/left right
time                 792.2 μs   (787.8 μs .. 797.6 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 801.8 μs   (794.9 μs .. 821.8 μs)
std dev              36.91 μs   (14.44 μs .. 76.60 μs)
variance introduced by outliers: 37% (moderately inflated)

benchmarking main/C/alternate
time                 1.032 ms   (1.013 ms .. 1.056 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 1.030 ms   (1.020 ms .. 1.056 ms)
std dev              51.65 μs   (24.41 μs .. 95.44 μs)
variance introduced by outliers: 40% (moderately inflated)
```
