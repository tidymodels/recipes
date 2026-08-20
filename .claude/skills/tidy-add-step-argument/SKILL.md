---
name: tidy-add-step-argument
description: Guide for adding a new argument to an existing recipes step or check. Use when a user asks to add an argument/parameter to a `step_*()` or `check_*()` function, covering the constructor, `prep()`, backwards compatibility with previously saved recipes, documentation, tests, and NEWS.
---

# Add an argument to an existing step

Use this skill when adding a new argument to an existing `step_*()` or `check_*()` function, either in recipes itself or in a recipes extension package.

## Overview

A step's state is stored in the object created by `step_*_new()`, so a new argument has to be threaded through several functions in order. Missing one of them produces a step that silently ignores the argument. Recipes are also frequently saved to disk and reloaded after a package upgrade, so a new argument must not break objects created by an older version.

Throughout, `step_name()` is used as a stand-in for the real step, and `new_arg` for the new argument.

1. Add the argument to `step_name()` and pass it to `step_name_new()`.
2. Add the argument to `step_name_new()` and pass it to `step()`.
3. Pass the argument through `prep.step_name()`.
4. Backfill a default in `prep.step_name()` so older recipes keep working.
5. Use the argument in `bake.step_name()` (or wherever it takes effect).
6. Document the argument.
7. Add a backwards-compatibility test and a test that the argument works.
8. Add a NEWS bullet.

## Workflow

### Step 1: Add the argument to `step_name()`

Add the argument to the user-facing constructor, with its default value, and pass it into the `step_name_new()` call inside `add_step()`.

Place it with the other step-specific arguments: after `...`, `role`, and `trained`, but before `skip` and `id`. `skip` and `id` stay last so that the argument order stays consistent across steps.

```r
step_name <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    new_arg = default_value,
    skip = FALSE,
    id = rand_id("name")
  ) {
    add_step(
      recipe,
      step_name_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        new_arg = new_arg,
        skip = skip,
        id = id
      )
    )
  }
```

If the argument needs validation, use the standard `check_*()` helpers (see the `tidy-argument-checking` skill). Validate in `prep()` rather than in the constructor, since the constructor runs before the data is available.

### Step 2: Add the argument to `step_name_new()`

The constructor is a plain function with no defaults; every argument must be supplied by every caller. Add `new_arg` to the formals and pass it to `step()`.

```r
step_name_new <-
  function(terms, role, trained, new_arg, skip, id) {
    step(
      subclass = "name",
      terms = terms,
      role = role,
      trained = trained,
      new_arg = new_arg,
      skip = skip,
      id = id
    )
  }
```

### Step 3: Pass the argument through `prep.step_name()`

`prep.step_name()` rebuilds the step by calling `step_name_new()` again, this time with `trained = TRUE`. Every field must be carried over from `x`, or it is lost when the recipe is prepped.

```r
prep.step_name <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  # ... estimation code ...

  step_name_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    new_arg = x$new_arg,
    skip = x$skip,
    id = x$id
  )
}
```

Search the whole file for other `step_name_new()` calls; some steps call it from more than one place.

### Step 4: Handle backwards compatibility

A recipe created and saved by an older version of the package has no `new_arg` field, so `x$new_arg` is `NULL`. Backfill the default at the top of `prep.step_name()`, before the field is used:

```r
prep.step_name <- function(x, training, info = NULL, ...) {
  if (is.null(x$new_arg)) {
    x$new_arg <- default_value
  }

  # rest of prep
}
```

Two things to keep in mind:

- The backfill value must be the same default used in `step_name()`, so an old recipe behaves exactly as it did before.
- An already-prepped old recipe never calls `prep()` again, so if `bake.step_name()` reads `object$new_arg` directly it also needs to tolerate `NULL`. Either guard there too, or use a helper that treats `NULL` as the default (this is how `sparse` works: `sparse_is_yes(NULL)` returns `FALSE`).

### Step 5: Document the argument

Add a `@param` entry to the step's roxygen block, wrapped at 80 characters. If the argument is shared across several steps, check `man-roxygen/` for an existing template to use with `@template` instead of writing it out again.

Then re-document:

```
Rscript -e "devtools::document()"
```

### Step 6: Add tests

Add both tests to `tests/testthat/test-name.R`, next to the existing tests for that step.

First, a backwards-compatibility test. It preps a recipe, deletes the new field to simulate an object made by an older version, and checks that baking still gives the same result:

```r
test_that("new_arg argument is backwards compatible", {
  rec <- recipe(~., data = data_set_of_your_choice) |>
    step_name(all_predictors()) |>
    prep()

  exp <- bake(rec, data_set_of_your_choice)

  # Simulate old recipe
  rec$steps[[1]]$new_arg <- NULL

  expect_identical(
    bake(rec, data_set_of_your_choice),
    exp
  )
})
```

Second, a test that the argument actually changes behavior. Bake with a non-default value and check the result differs from the default in the expected way. If the argument is validated, add an `expect_snapshot(error = TRUE)` test for an invalid value.

Run the tests:

```
Rscript -e "devtools::test_active_file('R/name.R')"
```

### Step 7: Add a NEWS bullet

Add a bullet to the development version section of `NEWS.md`, with the step name early in the bullet and the issue number in parentheses. Keep it on one line and keep bullets ordered alphabetically by function name.

```markdown
* `step_name()` gained the `new_arg` argument, which ... (#1234)
```

## Implementation checklist

- [ ] Add `new_arg` to `step_name()` and pass it to `step_name_new()` inside `add_step()`.
- [ ] Add `new_arg` to `step_name_new()` and pass it to `step()`.
- [ ] Pass `new_arg` to every `step_name_new()` call in `prep.step_name()`.
- [ ] Backfill `if (is.null(x$new_arg))` in `prep.step_name()` for older recipes.
- [ ] Make sure `bake.step_name()` tolerates a `NULL` value if it reads the field directly.
- [ ] Use the argument where it takes effect.
- [ ] Document the argument with `@param` (or an existing `@template`).
- [ ] Run `devtools::document()`.
- [ ] Add a backwards-compatibility test.
- [ ] Add a test that the new argument works.
- [ ] Run the tests.
- [ ] Add a bullet to `NEWS.md`.
- [ ] Run `air format .`.
