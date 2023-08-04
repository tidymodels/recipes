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
      Error in `remove_role()`:
      ! argument "old_role" is missing, with no default.

---

    Code
      remove_role(rec, sample, old_role = "non-existant")
    Condition
      Warning:
      Column, 'sample', does not have role, 'non-existant'.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      role1:           1
      undeclared role: 7

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
      recipe(mpg ~ ., data = mtcars) %>% add_role("disp", new_role = "case_weights")
    Condition
      Error in `add_role()`:
      ! Roles of "case_weights" cannot be set using `add_role()`.
      i Please use `frequency_weights()` or `importance_weights()` to specify case weights before the data is passed to `recipe()`.

---

    Code
      recipe(mpg ~ ., data = mtcars1) %>% remove_role(wt, old_role = "case_weights")
    Condition
      Error in `remove_role()`:
      ! Roles of "case_weights" cannot removed using `remove_role()`.

---

    Code
      recipe(mpg ~ ., data = mtcars1) %>% update_role(wt)
    Condition
      Error in `update_role()`:
      ! `update_role()` cannot be used on variables with role "case_weights".

---

    Code
      recipe(mpg ~ ., data = mtcars1) %>% add_role(wt)
    Condition
      Error in `add_role()`:
      ! `add_role()` cannot be used on variables with role "case_weights".

# role information from summary()

    Code
      summary(rec_roles, original = TRUE)
    Output
      # A tibble: 11 x 5
         variable type      role      source   required_to_bake
         <chr>    <list>    <chr>     <chr>    <lgl>           
       1 mpg      <chr [2]> outcome   original FALSE           
       2 cyl      <chr [2]> predictor original TRUE            
       3 disp     <chr [2]> predictor original TRUE            
       4 hp       <chr [2]> predictor original TRUE            
       5 drat     <chr [2]> predictor original TRUE            
       6 wt       <chr [2]> predictor original TRUE            
       7 qsec     <chr [2]> predictor original TRUE            
       8 vs       <chr [2]> predictor original TRUE            
       9 am       <chr [2]> predictor original TRUE            
      10 gear     <chr [2]> id        original TRUE            
      11 carb     <chr [2]> important original TRUE            

---

    Code
      summary(req_roles, original = TRUE)
    Output
      # A tibble: 11 x 5
         variable type      role      source   required_to_bake
         <chr>    <list>    <chr>     <chr>    <lgl>           
       1 mpg      <chr [2]> outcome   original FALSE           
       2 cyl      <chr [2]> predictor original TRUE            
       3 disp     <chr [2]> predictor original TRUE            
       4 hp       <chr [2]> predictor original TRUE            
       5 drat     <chr [2]> predictor original TRUE            
       6 wt       <chr [2]> predictor original TRUE            
       7 qsec     <chr [2]> predictor original TRUE            
       8 vs       <chr [2]> predictor original TRUE            
       9 am       <chr [2]> predictor original TRUE            
      10 gear     <chr [2]> id        original TRUE            
      11 carb     <chr [2]> important original FALSE           

---

    Code
      summary(na_rec, original = TRUE)
    Output
      # A tibble: 11 x 5
         variable type      role      source   required_to_bake
         <chr>    <list>    <chr>     <chr>    <lgl>           
       1 mpg      <chr [2]> outcome   original FALSE           
       2 cyl      <chr [2]> <NA>      original TRUE            
       3 disp     <chr [2]> predictor original TRUE            
       4 hp       <chr [2]> <NA>      original TRUE            
       5 drat     <chr [2]> <NA>      original TRUE            
       6 wt       <chr [2]> predictor original TRUE            
       7 qsec     <chr [2]> <NA>      original TRUE            
       8 vs       <chr [2]> <NA>      original TRUE            
       9 am       <chr [2]> <NA>      original TRUE            
      10 gear     <chr [2]> <NA>      original TRUE            
      11 carb     <chr [2]> other     original TRUE            

---

    Code
      summary(na_req_rec, original = TRUE)
    Output
      # A tibble: 11 x 5
         variable type      role      source   required_to_bake
         <chr>    <list>    <chr>     <chr>    <lgl>           
       1 mpg      <chr [2]> outcome   original FALSE           
       2 cyl      <chr [2]> <NA>      original FALSE           
       3 disp     <chr [2]> predictor original TRUE            
       4 hp       <chr [2]> <NA>      original FALSE           
       5 drat     <chr [2]> <NA>      original FALSE           
       6 wt       <chr [2]> predictor original TRUE            
       7 qsec     <chr [2]> <NA>      original FALSE           
       8 vs       <chr [2]> <NA>      original FALSE           
       9 am       <chr [2]> <NA>      original FALSE           
      10 gear     <chr [2]> <NA>      original FALSE           
      11 carb     <chr [2]> other     original TRUE            

