# recipes_pkg_check() works

    Code
      recipes_pkg_check(pkg = "missing_pkg", dependencies = NA)
    Message
      1 package (missing_pkg) is needed for this step but is not installed.
      To install run: `install.packages("missing_pkg", dependencies = NA)`

---

    Code
      recipes_pkg_check(pkg = c("missing_pkg_1", "missing_pkg_2"))
    Message
      2 packages (missing_pkg_1 and missing_pkg_2) are needed for this step but are
      not installed.
      To install run: `install.packages(c("missing_pkg_1", "missing_pkg_2"))`

---

    Code
      recipes_pkg_check(pkg = NULL)

