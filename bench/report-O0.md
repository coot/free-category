```
cabal run -O0 bench-cats  
benchmarking main/Queue 100/right right
time                 1.141 ms   (1.138 ms .. 1.143 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.140 ms   (1.138 ms .. 1.143 ms)
std dev              8.646 μs   (6.948 μs .. 11.22 μs)

benchmarking main/Queue 100/right left
time                 1.141 ms   (1.136 ms .. 1.147 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.138 ms   (1.135 ms .. 1.142 ms)
std dev              10.92 μs   (8.393 μs .. 15.79 μs)

benchmarking main/Queue 100/left left 
time                 1.142 ms   (1.139 ms .. 1.146 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.142 ms   (1.138 ms .. 1.147 ms)
std dev              14.64 μs   (11.59 μs .. 19.91 μs)

benchmarking main/Queue 100/left right
time                 1.141 ms   (1.136 ms .. 1.147 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.141 ms   (1.137 ms .. 1.148 ms)
std dev              16.67 μs   (9.879 μs .. 29.72 μs)

benchmarking main/Queue 100/alternate 
time                 1.164 ms   (1.153 ms .. 1.185 ms)
                     0.983 R²   (0.951 R² .. 0.999 R²)
mean                 1.188 ms   (1.159 ms .. 1.290 ms)
std dev              158.9 μs   (71.22 μs .. 324.6 μs)
variance introduced by outliers: 83% (severely inflated)

benchmarking main/ListTr 100/right right
time                 700.0 μs   (691.4 μs .. 706.7 μs)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 756.8 μs   (718.7 μs .. 906.9 μs)
std dev              245.7 μs   (9.661 μs .. 523.4 μs)
variance introduced by outliers: 97% (severely inflated)

benchmarking main/ListTr 100/right left
time                 756.6 μs   (717.7 μs .. 812.2 μs)
                     0.980 R²   (0.959 R² .. 1.000 R²)
mean                 719.7 μs   (710.8 μs .. 755.0 μs)
std dev              48.82 μs   (9.722 μs .. 100.7 μs)
variance introduced by outliers: 57% (severely inflated)

benchmarking main/ListTr 100/left left 
time                 767.3 μs   (723.7 μs .. 813.2 μs)
                     0.983 R²   (0.968 R² .. 1.000 R²)
mean                 733.7 μs   (723.2 μs .. 762.2 μs)
std dev              51.61 μs   (22.86 μs .. 96.08 μs)
variance introduced by outliers: 59% (severely inflated)

benchmarking main/ListTr 100/left right
time                 717.4 μs   (715.5 μs .. 719.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 718.1 μs   (716.1 μs .. 721.1 μs)
std dev              7.838 μs   (5.645 μs .. 12.14 μs)

benchmarking main/ListTr 100/alternate 
time                 715.4 μs   (712.1 μs .. 720.6 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 715.6 μs   (713.4 μs .. 719.0 μs)
std dev              8.725 μs   (6.633 μs .. 11.88 μs)

benchmarking main/C 100/right right
time                 899.4 μs   (894.7 μs .. 905.2 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 902.1 μs   (898.5 μs .. 906.9 μs)
std dev              13.97 μs   (10.79 μs .. 19.73 μs)

benchmarking main/C 100/right left
time                 903.2 μs   (895.5 μs .. 911.5 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 900.8 μs   (896.2 μs .. 909.6 μs)
std dev              20.72 μs   (14.03 μs .. 33.73 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking main/C 100/left left
time                 1.213 ms   (1.199 ms .. 1.241 ms)
                     0.982 R²   (0.955 R² .. 0.999 R²)
mean                 1.236 ms   (1.204 ms .. 1.320 ms)
std dev              159.4 μs   (51.06 μs .. 269.7 μs)
variance introduced by outliers: 81% (severely inflated)

benchmarking main/C 100/left right
time                 898.3 μs   (854.1 μs .. 969.1 μs)
                     0.970 R²   (0.951 R² .. 0.992 R²)
mean                 925.6 μs   (903.9 μs .. 959.2 μs)
std dev              89.03 μs   (60.65 μs .. 138.8 μs)
variance introduced by outliers: 72% (severely inflated)

benchmarking main/C 100/alternate
time                 1.075 ms   (1.063 ms .. 1.093 ms)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 1.085 ms   (1.071 ms .. 1.109 ms)
std dev              58.94 μs   (35.22 μs .. 85.70 μs)
variance introduced by outliers: 43% (moderately inflated)

benchmarking main/Queue 250/right right
time                 8.210 ms   (8.158 ms .. 8.283 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 8.104 ms   (8.050 ms .. 8.148 ms)
std dev              141.8 μs   (100.5 μs .. 213.6 μs)

benchmarking main/ListTr 250/right right
time                 6.835 ms   (6.677 ms .. 7.059 ms)
                     0.997 R²   (0.994 R² .. 1.000 R²)
mean                 6.631 ms   (6.592 ms .. 6.697 ms)
std dev              151.1 μs   (73.51 μs .. 274.0 μs)

benchmarking main/Queue 500/right right
time                 33.14 ms   (32.12 ms .. 34.38 ms)
                     0.995 R²   (0.984 R² .. 0.999 R²)
mean                 33.84 ms   (33.06 ms .. 34.85 ms)
std dev              1.819 ms   (1.173 ms .. 2.736 ms)
variance introduced by outliers: 18% (moderately inflated)

benchmarking main/ListTr 500/right right
time                 28.88 ms   (27.92 ms .. 30.42 ms)
                     0.983 R²   (0.944 R² .. 0.999 R²)
mean                 29.04 ms   (28.51 ms .. 30.34 ms)
std dev              1.758 ms   (608.9 μs .. 3.384 ms)
variance introduced by outliers: 21% (moderately inflated)
```
