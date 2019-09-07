```
cabal run -O1 bench-cats
benchmarking main/Cat/right right
time                 354.9 μs   (353.0 μs .. 356.5 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 357.5 μs   (354.7 μs .. 362.6 μs)
std dev              12.79 μs   (6.005 μs .. 20.61 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking main/Cat/right left
time                 632.7 μs   (623.4 μs .. 641.3 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 628.1 μs   (623.8 μs .. 632.6 μs)
std dev              15.18 μs   (12.69 μs .. 18.40 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking main/Cat/left left
time                 630.7 μs   (625.5 μs .. 637.9 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 633.8 μs   (630.1 μs .. 638.2 μs)
std dev              14.06 μs   (11.70 μs .. 16.97 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking main/Cat/left right
time                 624.7 μs   (615.3 μs .. 631.6 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 626.0 μs   (620.8 μs .. 631.4 μs)
std dev              17.14 μs   (14.44 μs .. 20.91 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking main/Cat/alternate
time                 546.8 μs   (540.7 μs .. 554.8 μs)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 535.7 μs   (529.1 μs .. 543.0 μs)
std dev              22.14 μs   (16.93 μs .. 34.04 μs)
variance introduced by outliers: 35% (moderately inflated)

benchmarking main/Queue/right right
time                 337.8 μs   (336.2 μs .. 339.5 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 337.8 μs   (336.0 μs .. 341.4 μs)
std dev              8.226 μs   (4.189 μs .. 16.39 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking main/Queue/right left
time                 341.1 μs   (338.9 μs .. 344.1 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 340.4 μs   (339.0 μs .. 343.7 μs)
std dev              7.103 μs   (4.242 μs .. 12.45 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking main/Queue/left left 
time                 340.4 μs   (337.0 μs .. 344.6 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 338.5 μs   (336.9 μs .. 340.8 μs)
std dev              6.362 μs   (4.628 μs .. 9.175 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking main/Queue/left right
time                 342.2 μs   (337.0 μs .. 351.4 μs)
                     0.995 R²   (0.989 R² .. 1.000 R²)
mean                 341.2 μs   (337.0 μs .. 349.7 μs)
std dev              19.19 μs   (10.13 μs .. 31.45 μs)
variance introduced by outliers: 52% (severely inflated)

benchmarking main/Queue/alternate 
time                 338.6 μs   (336.6 μs .. 341.5 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 337.3 μs   (335.3 μs .. 341.6 μs)
std dev              9.044 μs   (4.670 μs .. 19.46 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking main/ListTr/right right
time                 619.4 μs   (611.0 μs .. 629.1 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 613.7 μs   (607.3 μs .. 621.7 μs)
std dev              24.35 μs   (17.22 μs .. 35.84 μs)
variance introduced by outliers: 32% (moderately inflated)

benchmarking main/ListTr/right left
time                 618.4 μs   (610.7 μs .. 626.5 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 608.5 μs   (604.6 μs .. 614.2 μs)
std dev              16.13 μs   (11.83 μs .. 27.07 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking main/ListTr/left left 
time                 606.6 μs   (603.3 μs .. 611.5 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 614.5 μs   (611.6 μs .. 618.0 μs)
std dev              10.79 μs   (8.979 μs .. 13.64 μs)

benchmarking main/ListTr/left right
time                 611.6 μs   (606.8 μs .. 616.9 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 615.7 μs   (613.0 μs .. 619.7 μs)
std dev              10.50 μs   (7.937 μs .. 15.47 μs)

benchmarking main/ListTr/alternate 
time                 618.0 μs   (607.9 μs .. 630.6 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 613.5 μs   (610.3 μs .. 617.8 μs)
std dev              12.60 μs   (9.273 μs .. 19.12 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking main/C/right right
time                 718.7 μs   (707.1 μs .. 730.6 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 704.9 μs   (699.3 μs .. 712.6 μs)
std dev              21.50 μs   (14.43 μs .. 28.80 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking main/C/right left
time                 626.2 μs   (621.0 μs .. 630.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 624.6 μs   (621.7 μs .. 627.2 μs)
std dev              9.159 μs   (7.975 μs .. 10.69 μs)

benchmarking main/C/left left
time                 717.7 μs   (709.6 μs .. 729.2 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 723.6 μs   (718.5 μs .. 732.4 μs)
std dev              22.18 μs   (13.44 μs .. 38.51 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking main/C/left right
time                 631.5 μs   (624.4 μs .. 641.3 μs)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 629.2 μs   (621.7 μs .. 638.3 μs)
std dev              26.65 μs   (17.77 μs .. 37.22 μs)
variance introduced by outliers: 35% (moderately inflated)

benchmarking main/C/alternate
time                 997.7 μs   (984.5 μs .. 1.010 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 995.6 μs   (989.0 μs .. 1.006 ms)
std dev              27.57 μs   (22.48 μs .. 34.18 μs)
variance introduced by outliers: 17% (moderately inflated)
```
