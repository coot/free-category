```
cabal run -O1 bench-cats  
benchmarking main/Cat 100/right right
time                 383.4 μs   (370.0 μs .. 412.2 μs)
                     0.928 R²   (0.846 R² .. 0.996 R²)
mean                 408.5 μs   (381.2 μs .. 466.5 μs)
std dev              131.4 μs   (59.97 μs .. 250.7 μs)
variance introduced by outliers: 98% (severely inflated)

benchmarking main/Cat 100/right left
time                 665.2 μs   (647.0 μs .. 681.1 μs)
                     0.996 R²   (0.994 R² .. 0.997 R²)
mean                 651.8 μs   (643.7 μs .. 660.0 μs)
std dev              27.99 μs   (23.84 μs .. 32.75 μs)
variance introduced by outliers: 35% (moderately inflated)

benchmarking main/Cat 100/left left
time                 682.9 μs   (671.9 μs .. 692.8 μs)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 694.2 μs   (683.5 μs .. 713.7 μs)
std dev              46.27 μs   (26.75 μs .. 75.32 μs)
variance introduced by outliers: 56% (severely inflated)

benchmarking main/Cat 100/left right
time                 682.0 μs   (664.8 μs .. 698.9 μs)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 673.7 μs   (666.1 μs .. 682.2 μs)
std dev              28.44 μs   (24.53 μs .. 33.41 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking main/Cat 100/alternate
ktime                 582.0 μs   (576.1 μs .. 587.5 μs)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 581.2 μs   (573.0 μs .. 597.4 μs)
std dev              37.40 μs   (21.44 μs .. 68.12 μs)
variance introduced by outliers: 56% (severely inflated)

benchmarking main/Queue 100/right right
time                 344.2 μs   (337.1 μs .. 362.0 μs)
                     0.974 R²   (0.923 R² .. 1.000 R²)
mean                 346.8 μs   (338.8 μs .. 375.4 μs)
std dev              47.04 μs   (4.662 μs .. 98.23 μs)
variance introduced by outliers: 87% (severely inflated)

benchmarking main/Queue 100/right left
time                 334.1 μs   (333.3 μs .. 334.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 333.7 μs   (333.1 μs .. 334.3 μs)
std dev              2.083 μs   (1.686 μs .. 2.609 μs)

benchmarking main/Queue 100/left left 
time                 336.6 μs   (334.3 μs .. 340.4 μs)
                     0.997 R²   (0.991 R² .. 1.000 R²)
mean                 339.0 μs   (335.2 μs .. 353.2 μs)
std dev              20.23 μs   (6.119 μs .. 45.16 μs)
variance introduced by outliers: 55% (severely inflated)

benchmarking main/Queue 100/left right
time                 334.4 μs   (333.9 μs .. 335.0 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 334.3 μs   (333.6 μs .. 335.3 μs)
std dev              2.840 μs   (2.002 μs .. 4.592 μs)

benchmarking main/Queue 100/alternate 
time                 333.4 μs   (332.9 μs .. 334.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 334.1 μs   (333.5 μs .. 334.7 μs)
std dev              1.989 μs   (1.597 μs .. 2.655 μs)

benchmarking main/ListTr 100/right right
time                 168.0 μs   (167.7 μs .. 168.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 168.4 μs   (168.1 μs .. 168.9 μs)
std dev              1.328 μs   (905.4 ns .. 2.041 μs)

benchmarking main/ListTr 100/right left
time                 176.4 μs   (173.7 μs .. 180.5 μs)
                     0.985 R²   (0.966 R² .. 0.999 R²)
mean                 180.4 μs   (174.5 μs .. 191.0 μs)
std dev              26.56 μs   (11.94 μs .. 40.58 μs)
variance introduced by outliers: 90% (severely inflated)

benchmarking main/ListTr 100/left left 
time                 181.4 μs   (171.8 μs .. 200.9 μs)
                     0.965 R²   (0.926 R² .. 1.000 R²)
mean                 174.4 μs   (171.5 μs .. 188.0 μs)
std dev              17.44 μs   (1.898 μs .. 39.71 μs)
variance introduced by outliers: 80% (severely inflated)

benchmarking main/ListTr 100/left right
time                 171.7 μs   (170.3 μs .. 173.4 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 175.3 μs   (173.9 μs .. 177.5 μs)
std dev              5.737 μs   (3.819 μs .. 10.35 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking main/ListTr 100/alternate 
time                 172.2 μs   (169.5 μs .. 174.3 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 170.3 μs   (169.6 μs .. 171.4 μs)
std dev              2.847 μs   (2.192 μs .. 3.745 μs)

benchmarking main/C 100/right right
time                 741.9 μs   (720.1 μs .. 769.8 μs)
                     0.996 R²   (0.993 R² .. 0.999 R²)
mean                 738.5 μs   (733.4 μs .. 746.4 μs)
std dev              21.90 μs   (14.74 μs .. 36.25 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking main/C 100/right left
time                 671.2 μs   (655.1 μs .. 693.1 μs)
                     0.974 R²   (0.924 R² .. 0.999 R²)
mean                 681.5 μs   (658.5 μs .. 777.3 μs)
std dev              127.4 μs   (26.16 μs .. 280.5 μs)
variance introduced by outliers: 92% (severely inflated)

benchmarking main/C 100/left left
time                 802.2 μs   (784.7 μs .. 824.5 μs)
                     0.996 R²   (0.996 R² .. 0.998 R²)
mean                 795.6 μs   (787.1 μs .. 805.5 μs)
std dev              30.72 μs   (26.91 μs .. 35.57 μs)
variance introduced by outliers: 29% (moderately inflated)

benchmarking main/C 100/left right
time                 657.8 μs   (650.4 μs .. 668.8 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 656.8 μs   (649.6 μs .. 665.1 μs)
std dev              26.63 μs   (20.92 μs .. 35.68 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking main/C 100/alternate
time                 1.065 ms   (1.053 ms .. 1.082 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 1.074 ms   (1.061 ms .. 1.093 ms)
std dev              53.90 μs   (39.80 μs .. 70.25 μs)
variance introduced by outliers: 39% (moderately inflated)

benchmarking main/Queue 250/right right
time                 3.203 ms   (2.938 ms .. 3.591 ms)
                     0.966 R²   (0.942 R² .. 0.998 R²)
mean                 2.963 ms   (2.915 ms .. 3.067 ms)
std dev              226.0 μs   (94.89 μs .. 422.7 μs)
variance introduced by outliers: 52% (severely inflated)

benchmarking main/ListTr 250/right right
time                 3.265 ms   (3.231 ms .. 3.295 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.261 ms   (3.239 ms .. 3.284 ms)
std dev              71.15 μs   (57.97 μs .. 86.47 μs)

benchmarking main/Queue 500/right right
time                 11.72 ms   (11.47 ms .. 11.94 ms)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 11.95 ms   (11.79 ms .. 12.20 ms)
std dev              521.0 μs   (333.3 μs .. 799.6 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking main/ListTr 500/right right
time                 17.55 ms   (15.86 ms .. 18.15 ms)
                     0.956 R²   (0.839 R² .. 0.999 R²)
mean                 18.91 ms   (18.14 ms .. 22.44 ms)
std dev              3.166 ms   (347.5 μs .. 6.963 ms)
variance introduced by outliers: 71% (severely inflated)
```
