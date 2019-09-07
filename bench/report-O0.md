```
cabal run -O0 bench-cats
benchmarking main/Cat/right right
time                 2.568 ms   (2.498 ms .. 2.641 ms)
                     0.995 R²   (0.990 R² .. 0.997 R²)
mean                 2.541 ms   (2.503 ms .. 2.586 ms)
std dev              138.7 μs   (109.0 μs .. 205.2 μs)
variance introduced by outliers: 38% (moderately inflated)

benchmarking main/Cat/right left
time                 3.660 ms   (3.528 ms .. 3.775 ms)
                     0.995 R²   (0.993 R² .. 0.999 R²)
mean                 3.587 ms   (3.553 ms .. 3.628 ms)
std dev              115.2 μs   (86.37 μs .. 159.1 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking main/Cat/left left
time                 3.711 ms   (3.648 ms .. 3.779 ms)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 3.780 ms   (3.715 ms .. 3.855 ms)
std dev              227.0 μs   (167.3 μs .. 377.7 μs)
variance introduced by outliers: 38% (moderately inflated)

benchmarking main/Cat/left right
time                 3.603 ms   (3.562 ms .. 3.651 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 3.643 ms   (3.606 ms .. 3.689 ms)
std dev              141.1 μs   (109.3 μs .. 185.6 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking main/Cat/alternate
time                 2.893 ms   (2.812 ms .. 2.981 ms)
                     0.993 R²   (0.989 R² .. 0.996 R²)
mean                 2.914 ms   (2.866 ms .. 2.979 ms)
std dev              183.2 μs   (136.0 μs .. 273.8 μs)
variance introduced by outliers: 42% (moderately inflated)

benchmarking main/Queue/right right
time                 1.260 ms   (1.237 ms .. 1.277 ms)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 1.257 ms   (1.246 ms .. 1.272 ms)
std dev              43.07 μs   (32.11 μs .. 62.56 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking main/Queue/right left
time                 1.250 ms   (1.237 ms .. 1.260 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 1.275 ms   (1.261 ms .. 1.301 ms)
std dev              62.84 μs   (42.45 μs .. 93.51 μs)
variance introduced by outliers: 38% (moderately inflated)

benchmarking main/Queue/left left 
time                 1.273 ms   (1.252 ms .. 1.289 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.240 ms   (1.231 ms .. 1.251 ms)
std dev              32.58 μs   (26.50 μs .. 42.39 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/Queue/left right
time                 1.255 ms   (1.243 ms .. 1.267 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.242 ms   (1.231 ms .. 1.256 ms)
std dev              41.50 μs   (31.77 μs .. 52.84 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking main/Queue/alternate 
time                 1.286 ms   (1.246 ms .. 1.343 ms)
                     0.994 R²   (0.989 R² .. 0.999 R²)
mean                 1.256 ms   (1.243 ms .. 1.275 ms)
std dev              52.21 μs   (35.03 μs .. 86.99 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking main/ListTr/right right
time                 800.8 μs   (782.0 μs .. 823.7 μs)
                     0.995 R²   (0.991 R² .. 0.998 R²)
mean                 812.7 μs   (802.9 μs .. 825.7 μs)
std dev              38.06 μs   (27.65 μs .. 54.52 μs)
variance introduced by outliers: 38% (moderately inflated)

benchmarking main/ListTr/right left
time                 773.4 μs   (769.0 μs .. 780.1 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 775.4 μs   (769.8 μs .. 783.1 μs)
std dev              22.93 μs   (16.51 μs .. 34.17 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking main/ListTr/left left 
time                 778.0 μs   (768.5 μs .. 786.4 μs)
                     0.998 R²   (0.994 R² .. 0.999 R²)
mean                 791.7 μs   (772.1 μs .. 878.3 μs)
std dev              117.7 μs   (19.43 μs .. 266.4 μs)
variance introduced by outliers: 87% (severely inflated)

benchmarking main/ListTr/left right
time                 785.7 μs   (772.9 μs .. 800.5 μs)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 778.5 μs   (770.0 μs .. 792.1 μs)
std dev              36.55 μs   (25.62 μs .. 51.10 μs)
variance introduced by outliers: 38% (moderately inflated)

benchmarking main/ListTr/alternate 
time                 778.9 μs   (770.4 μs .. 786.6 μs)
                     0.996 R²   (0.990 R² .. 0.999 R²)
mean                 782.0 μs   (771.5 μs .. 808.8 μs)
std dev              51.49 μs   (20.02 μs .. 96.60 μs)
variance introduced by outliers: 54% (severely inflated)

benchmarking main/C/right right
time                 949.8 μs   (915.4 μs .. 995.7 μs)
                     0.989 R²   (0.984 R² .. 0.995 R²)
mean                 952.7 μs   (934.5 μs .. 981.1 μs)
std dev              71.30 μs   (53.14 μs .. 109.1 μs)
variance introduced by outliers: 61% (severely inflated)

benchmarking main/C/right left
time                 949.7 μs   (922.3 μs .. 988.8 μs)
                     0.993 R²   (0.987 R² .. 0.998 R²)
mean                 937.1 μs   (925.7 μs .. 956.9 μs)
std dev              52.58 μs   (36.13 μs .. 75.02 μs)
variance introduced by outliers: 46% (moderately inflated)

benchmarking main/C/left left
time                 1.208 ms   (1.181 ms .. 1.230 ms)
                     0.996 R²   (0.995 R² .. 0.998 R²)
mean                 1.205 ms   (1.192 ms .. 1.223 ms)
std dev              53.93 μs   (43.99 μs .. 72.16 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking main/C/left right
time                 973.2 μs   (951.1 μs .. 996.9 μs)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 947.8 μs   (933.1 μs .. 967.3 μs)
std dev              55.03 μs   (45.51 μs .. 68.47 μs)
variance introduced by outliers: 48% (moderately inflated)

benchmarking main/C/alternate
time                 1.207 ms   (1.187 ms .. 1.234 ms)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 1.239 ms   (1.224 ms .. 1.256 ms)
std dev              57.62 μs   (48.24 μs .. 71.42 μs)
variance introduced by outliers: 35% (moderately inflated)
```
