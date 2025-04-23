# missing factors

    Code
      check_nominal_type(te, rec$orig_lvls)
    Condition
      Warning:
      ! There were 2 columns that were factors when the recipe was prepped:
      * `city` and `zip`
      i This may cause errors when processing new data.

# missing single factor

    Code
      check_nominal_type(te, rec$orig_lvls)
    Condition
      Warning:
      ! There was 1 column that was a factor when the recipe was prepped:
      * `city`
      i This may cause errors when processing new data.

# missing factors with skipping

    Code
      check_nominal_type(te, rec$orig_lvls)
    Condition
      Warning:
      ! There were 2 columns that were factors when the recipe was prepped:
      * `city` and `zip`
      i This may cause errors when processing new data.

---

    Code
      res <- bake(rec, mutate(te, city = as.character(city)))
    Condition
      Warning in `bake()`:
      ! There were 2 columns that were factors when the recipe was prepped:
      * `city` and `zip`
      i This may cause errors when processing new data.

