# bad args

    Code
      bake(rec, new_data = sacr_te, composition = "dgCMatrix")
    Condition
      Error in `hardhat::recompose()`:
      ! `data` must only contain numeric columns.
      i These columns aren't numeric: "beds" and "type".

---

    Code
      juice(rec, composition = "dgCMatrix")
    Condition
      Error in `hardhat::recompose()`:
      ! `data` must only contain numeric columns.
      i These columns aren't numeric: "beds" and "type".

---

    Code
      recipe(~., data = ames) %>% prep() %>% bake(new_data = NULL, composition = "dgCMatrix")
    Condition
      Error in `hardhat::recompose()`:
      ! `data` must only contain numeric columns.
      i These columns aren't numeric: "MS_SubClass", "MS_Zoning", "Street", "Alley", "Lot_Shape", "Land_Contour", "Utilities", "Lot_Config", "Land_Slope", "Neighborhood", "Condition_1", "Condition_2", "Bldg_Type", "House_Style", "Overall_Cond", "Roof_Style", "Roof_Matl", "Exterior_1st", ..., "Sale_Type", and "Sale_Condition".

