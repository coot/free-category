```
cabal run -O1 bench-cats -- -o bench/raport-O1.html
Up to date
benchmarking main/Cat/right right
time                 358.2 μs   (355.8 μs .. 360.4 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 359.6 μs   (357.9 μs .. 363.8 μs)
std dev              8.317 μs   (3.710 μs .. 16.26 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking main/Cat/right left
time                 637.6 μs   (632.5 μs .. 642.7 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 641.5 μs   (637.4 μs .. 650.5 μs)
std dev              19.87 μs   (9.733 μs .. 36.06 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking main/Cat/left left
time                 631.7 μs   (626.4 μs .. 636.6 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 638.1 μs   (634.9 μs .. 647.3 μs)
std dev              16.83 μs   (3.914 μs .. 30.31 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking main/Cat/left right
time                 625.7 μs   (617.1 μs .. 636.1 μs)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 634.0 μs   (628.3 μs .. 644.6 μs)
std dev              25.87 μs   (17.92 μs .. 44.63 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking main/Cat/alternate
time                 556.2 μs   (551.9 μs .. 560.7 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 571.2 μs   (566.0 μs .. 584.0 μs)
std dev              25.78 μs   (11.37 μs .. 48.01 μs)
variance introduced by outliers: 39% (moderately inflated)

benchmarking main/CatL/right right
time                 1.218 ms   (1.207 ms .. 1.228 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 1.233 ms   (1.225 ms .. 1.257 ms)
std dev              40.28 μs   (14.29 μs .. 82.87 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking main/CatL/right left
time                 777.0 μs   (771.6 μs .. 782.4 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 788.1 μs   (782.2 μs .. 807.2 μs)
std dev              32.84 μs   (12.18 μs .. 65.60 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking main/CatL/left left
time                 781.6 μs   (775.8 μs .. 786.9 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 790.6 μs   (783.9 μs .. 805.9 μs)
std dev              32.26 μs   (13.08 μs .. 61.93 μs)
variance introduced by outliers: 32% (moderately inflated)

benchmarking main/CatL/left right
time                 773.5 μs   (767.6 μs .. 778.5 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 792.5 μs   (783.7 μs .. 823.0 μs)
std dev              47.82 μs   (16.00 μs .. 102.0 μs)
variance introduced by outliers: 50% (severely inflated)

benchmarking main/CatL/alternate
time                 1.252 ms   (1.239 ms .. 1.263 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 1.223 ms   (1.211 ms .. 1.236 ms)
std dev              44.68 μs   (35.08 μs .. 61.12 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking main/ListTr/right right
time                 637.6 μs   (634.0 μs .. 641.3 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 641.7 μs   (638.5 μs .. 647.2 μs)
std dev              13.55 μs   (6.722 μs .. 21.41 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking main/ListTr/right left
time                 637.6 μs   (634.7 μs .. 640.6 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 639.1 μs   (637.1 μs .. 643.8 μs)
std dev              9.983 μs   (4.158 μs .. 18.68 μs)

benchmarking main/ListTr/left left 
time                 635.3 μs   (631.8 μs .. 638.8 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 638.9 μs   (636.6 μs .. 644.1 μs)
std dev              11.19 μs   (6.435 μs .. 17.40 μs)

benchmarking main/ListTr/left right
time                 646.5 μs   (637.5 μs .. 663.3 μs)
                     0.996 R²   (0.991 R² .. 1.000 R²)
mean                 646.0 μs   (640.9 μs .. 656.5 μs)
std dev              24.70 μs   (13.22 μs .. 46.34 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking main/ListTr/alternate 
time                 627.3 μs   (593.9 μs .. 671.1 μs)
                     0.979 R²   (0.966 R² .. 0.992 R²)
mean                 690.3 μs   (641.1 μs .. 794.0 μs)
std dev              236.3 μs   (128.9 μs .. 397.8 μs)
variance introduced by outliers: 97% (severely inflated)

benchmarking main/C/right right
time                 727.4 μs   (689.4 μs .. 805.1 μs)
                     0.946 R²   (0.881 R² .. 0.996 R²)
mean                 716.9 μs   (696.6 μs .. 769.7 μs)
std dev              102.0 μs   (38.80 μs .. 200.5 μs)
variance introduced by outliers: 86% (severely inflated)

benchmarking main/C/right left
time                 685.7 μs   (634.4 μs .. 754.3 μs)
                     0.943 R²   (0.917 R² .. 0.977 R²)
mean                 651.2 μs   (623.6 μs .. 692.6 μs)
std dev              112.3 μs   (68.39 μs .. 161.4 μs)
variance introduced by outliers: 91% (severely inflated)

benchmarking main/C/left left
time                 690.5 μs   (680.8 μs .. 699.3 μs)
                     0.968 R²   (0.918 R² .. 0.998 R²)
mean                 758.7 μs   (716.2 μs .. 875.2 μs)
std dev              208.6 μs   (46.94 μs .. 355.4 μs)
variance introduced by outliers: 96% (severely inflated)

benchmarking main/C/left right
time                 595.8 μs   (586.2 μs .. 604.6 μs)
                     0.988 R²   (0.973 R² .. 0.997 R²)
mean                 683.9 μs   (640.8 μs .. 750.8 μs)
std dev              197.9 μs   (125.8 μs .. 288.3 μs)
variance introduced by outliers: 97% (severely inflated)

benchmarking main/C/alternate
time                 1.001 ms   (913.9 μs .. 1.163 ms)
                     0.908 R²   (0.825 R² .. 0.992 R²)
mean                 1.117 ms   (1.030 ms .. 1.255 ms)
std dev              401.4 μs   (281.8 μs .. 535.1 μs)
variance introduced by outliers: 98% (severely inflated)
```
