# change existing role

    Code
      add_role(rec, sample, new_role = "some other role")
    Condition
      Error in `add_role()`:
      ! No role currently exists for column(s): 'sample'. Please use `update_role()` instead.

# existing role is skipped

    Code
      rec <- add_role(rec, sample, new_role = "some other role")
    Condition
      Warning:
      Role, 'some other role', already exists for column(s): 'sample'. Skipping.

# existing role is skipped, but new one is added

    Code
      rec <- add_role(rec, sample, dataset, new_role = "some other role")
    Condition
      Warning:
      Role, 'some other role', already exists for column(s): 'sample'. Skipping.

# cannot add roles if the current one is `NA`

    Code
      add_role(rec, sample, sulfur)
    Condition
      Error in `add_role()`:
      ! No role currently exists for column(s): 'sample', 'sulfur'. Please use `update_role()` instead.

# `update_role()` cannot be ambiguous

    Code
      update_role(rec, sample, new_role = "y")
    Condition
      Error in `update_role()`:
      ! `old_role` can only be `NULL` when the variable(s) have a single existing role.

# `new_role` cannot be `NA_character_`

    Code
      add_role(rec, sample, new_role = NA_character_)
    Condition
      Error in `single_chr()`:
      ! `new_role` must not be `NA`.

---

    Code
      update_role(rec, sample, new_role = NA_character_)
    Condition
      Error in `single_chr()`:
      ! `new_role` must not be `NA`.

# remove roles

    Code
      rec <- remove_role(rec, sample, old_role = NA)
    Condition
      Error in `single_chr()`:
      ! `old_role` must be a character vector.

---

    Code
      rec <- remove_role(rec, sample)
    Condition
      Error in `single_chr()`:
      ! argument "old_role" is missing, with no default

---

    Code
      remove_role(rec, sample, old_role = "non-existant")
    Condition
      Warning:
      Column, 'sample', does not have role, 'non-existant'.
    Output
      Recipe
      
      Inputs:
      
        role #variables
       role1          1
      
        7 variables with undeclared roles

# empty dots and zero column selections return input with a warning

    Code
      rec2 <- add_role(rec)
    Condition
      Warning:
      No columns were selected in `add_role()`.

---

    Code
      rec2 <- update_role(rec)
    Condition
      Warning:
      No columns were selected in `update_role()`.

---

    Code
      rec2 <- remove_role(rec, old_role = "foo")
    Condition
      Warning:
      No columns were selected in `remove_role()`.

---

    Code
      rec2 <- add_role(rec, starts_with("foobar"))
    Condition
      Warning:
      No columns were selected in `add_role()`.

---

    Code
      rec2 <- update_role(rec, starts_with("foobar"))
    Condition
      Warning:
      No columns were selected in `update_role()`.

---

    Code
      rec2 <- remove_role(rec, starts_with("foobar"), old_role = "foo")
    Condition
      Warning:
      No columns were selected in `remove_role()`.

# bad args

    Code
      recipe(x = biomass) %>% add_role(carbon, new_role = letters[1:2])
    Condition
      Error in `single_chr()`:
      ! `new_role` must have length 1.

---

    Code
      recipe(x = biomass) %>% add_role(carbon, new_role = "a", new_type = letters[1:2])
    Condition
      Error in `add_role()`:
      ! `new_type` must have length 1.

---

    Code
      recipe(x = biomass) %>% update_role(carbon, new_role = c("a", "b"))
    Condition
      Error in `single_chr()`:
      ! `new_role` must have length 1.

---

    Code
      recipe(x = biomass) %>% update_role(carbon, old_role = c("a", "b"))
    Condition
      Error in `single_chr()`:
      ! `old_role` must have length 1.

# role functions handle case weights correctly

    Code
      recipe(mpg ~ ., data = mtcars) %>% update_role("disp", new_role = "case_weights")
    Condition
      Error in `update_role()`:
      ! Roles of "case_weights" cannot be set using `update_role()`.
      i Please use `frequency_weights()` or `importance_weights()` to specify case weights before the data is passed to `recipe()`.

---

    Code
      recipe(mpg ~ ., data = mtcars1) %>% remove_role(wt, old_role = "case_weights")
    Condition
      Error in `remove_role()`:
      ! Roles of "case_weights" cannot removed using `remove_role()`.

