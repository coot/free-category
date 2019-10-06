# Changelog for free-category

## Version 0.0.4.0
- hoistOp
- Renamed `Control.Category.FreeEff` module as `Control.Category.FreeEffect`
  and renamed top level terms:
    - `EffCategory` type class to `EffectCategory`
    - `FreeEffCat` to `EffCat`
    - `FreeEffCat` constructor as `Effect` and `lift` as `effect`
    - `liftCat` to `liftEffect`
    - `foldNatLift` to `foldNatEffCat`
- Show instance of 'Cat' and 'C' via 'ListTr' (GHC >= 806)
- Performance optimisations: rewrite rules & inline pragmas
- Export ListTr from Control.Category.Free
- foldrL, foldlL and zipWithL

## Version 0.0.3.0
- Efficient 'Cat' and 'Aff' based on real time queues with scheduling
- Added Monoid instances 
- Added Op category
- added `arrArr`, `mapArr`, `foldArr` for `Arr` free arrow category
- added `arrCat`, `mapCat`, `fodlMap` for `Cat` free categroy

## Version 0.0.2.0

- EffCategory class and FreeEffCat category transformer
- Example usage of FreeEffCat

## Version 0.0.1.0
- free category (concrete and condensity transformed)
- free arrows (concrete and condensity transformed)
