# dann

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/dann
* Date/Publication: 2023-09-23 15:50:03 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::revdep_details(, "dann")` for more info

</details>

## In both

*   checking whether package ‘dann’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/dann/new/dann.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dann’ ...
** package ‘dann’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.6)’
using SDK: ‘MacOSX15.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/new/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/dann/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/new/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/dann/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c internal_armadillo_helpers.cpp -o internal_armadillo_helpers.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/new/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/dann/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c internal_helpers_C.cpp -o internal_helpers_C.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -L/opt/R/arm64/lib -o dann.so RcppExports.o internal_armadillo_helpers.o internal_helpers_C.o -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.4-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [dann.so] Error 1
ERROR: compilation failed for package ‘dann’
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/dann/new/dann.Rcheck/dann’


```
### CRAN

```
* installing *source* package ‘dann’ ...
** package ‘dann’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.6)’
using SDK: ‘MacOSX15.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/old/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/dann/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/old/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/dann/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c internal_armadillo_helpers.cpp -o internal_armadillo_helpers.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/old/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/dann/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c internal_helpers_C.cpp -o internal_helpers_C.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -L/opt/R/arm64/lib -o dann.so RcppExports.o internal_armadillo_helpers.o internal_helpers_C.o -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.4-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [dann.so] Error 1
ERROR: compilation failed for package ‘dann’
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/dann/old/dann.Rcheck/dann’


```
# ldmppr

<details>

* Version: 1.0.3
* GitHub: https://github.com/lanedrew/ldmppr
* Source code: https://github.com/cran/ldmppr
* Date/Publication: 2024-12-02 12:41:19 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::revdep_details(, "ldmppr")` for more info

</details>

## In both

*   checking whether package ‘ldmppr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/ldmppr/new/ldmppr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ldmppr’ ...
** package ‘ldmppr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.6)’
using C++17
using SDK: ‘MacOSX15.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/new/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/ldmppr/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/new/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/ldmppr/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c self_correcting_model.cpp -o self_correcting_model.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -L/opt/R/arm64/lib -o ldmppr.so RcppExports.o self_correcting_model.o -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.4-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [ldmppr.so] Error 1
ERROR: compilation failed for package ‘ldmppr’
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/ldmppr/new/ldmppr.Rcheck/ldmppr’


```
### CRAN

```
* installing *source* package ‘ldmppr’ ...
** package ‘ldmppr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.6)’
using C++17
using SDK: ‘MacOSX15.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/old/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/ldmppr/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/old/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/ldmppr/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c self_correcting_model.cpp -o self_correcting_model.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -L/opt/R/arm64/lib -o ldmppr.so RcppExports.o self_correcting_model.o -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.4-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [ldmppr.so] Error 1
ERROR: compilation failed for package ‘ldmppr’
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/ldmppr/old/ldmppr.Rcheck/ldmppr’


```
# shapr

<details>

* Version: 1.0.2
* GitHub: https://github.com/NorskRegnesentral/shapr
* Source code: https://github.com/cran/shapr
* Date/Publication: 2025-02-07 00:40:02 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::revdep_details(, "shapr")` for more info

</details>

## In both

*   checking whether package ‘shapr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/shapr/new/shapr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘shapr’ ...
** package ‘shapr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.6)’
using SDK: ‘MacOSX15.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/new/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c AICc.cpp -o AICc.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/new/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c Copula.cpp -o Copula.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/new/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c Gaussian.cpp -o Gaussian.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/new/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
...
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/new/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c impute_data.cpp -o impute_data.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/new/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c weighted_matrix.cpp -o weighted_matrix.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -L/opt/R/arm64/lib -o shapr.so AICc.o Copula.o Gaussian.o RcppExports.o distance.o features.o impute_data.o weighted_matrix.o -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.4-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [shapr.so] Error 1
ERROR: compilation failed for package ‘shapr’
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/shapr/new/shapr.Rcheck/shapr’


```
### CRAN

```
* installing *source* package ‘shapr’ ...
** package ‘shapr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.6)’
using SDK: ‘MacOSX15.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/old/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c AICc.cpp -o AICc.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/old/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c Copula.cpp -o Copula.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/old/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c Gaussian.cpp -o Gaussian.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/old/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
...
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/old/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c impute_data.cpp -o impute_data.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/old/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c weighted_matrix.cpp -o weighted_matrix.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -L/opt/R/arm64/lib -o shapr.so AICc.o Copula.o Gaussian.o RcppExports.o distance.o features.o impute_data.o weighted_matrix.o -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.4-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [shapr.so] Error 1
ERROR: compilation failed for package ‘shapr’
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/shapr/old/shapr.Rcheck/shapr’


```
