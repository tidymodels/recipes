# correct tables

    Code
      weighted_table(mtcars["vs"])
    Condition
      Error in `weighted_table()`:
      ! All columns in `.data` must be factors.

# is_unsupervised_weights works

    Code
      is_unsupervised_weights(3)
    Condition
      Error in `is_unsupervised_weights()`:
      ! Must be be a case_weights variable

