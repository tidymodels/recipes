# D2MCS

<details>

* Version: 1.0.1
* GitHub: https://github.com/drordas/D2MCS
* Source code: https://github.com/cran/D2MCS
* Date/Publication: 2022-08-23 11:40:02 UTC
* Number of recursive dependencies: 181

Run `revdepcheck::revdep_details(, "D2MCS")` for more info

</details>

## In both

*   checking whether package ‘D2MCS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/D2MCS/new/D2MCS.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘D2MCS’ ...
** package ‘D2MCS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(jli, FALSE)
  error: unable to load shared object '/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib':
  dlopen(/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib, 0x000A): tried: '/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64')), '/System/Volumes/Preboot/Cryptexes/OS/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib' (no such file), '/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64'))
Execution halted
ERROR: lazy loading failed for package ‘D2MCS’
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/D2MCS/new/D2MCS.Rcheck/D2MCS’


```
### CRAN

```
* installing *source* package ‘D2MCS’ ...
** package ‘D2MCS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(jli, FALSE)
  error: unable to load shared object '/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib':
  dlopen(/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib, 0x000A): tried: '/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64')), '/System/Volumes/Preboot/Cryptexes/OS/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib' (no such file), '/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64'))
Execution halted
ERROR: lazy loading failed for package ‘D2MCS’
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/D2MCS/old/D2MCS.Rcheck/D2MCS’


```
# hydrorecipes

<details>

* Version: 0.0.3
* GitHub: https://github.com/jkennel/hydrorecipes
* Source code: https://github.com/cran/hydrorecipes
* Date/Publication: 2022-06-27 07:30:04 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::revdep_details(, "hydrorecipes")` for more info

</details>

## In both

*   checking whether package ‘hydrorecipes’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/hydrorecipes/new/hydrorecipes.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘hydrorecipes’ ...
** package ‘hydrorecipes’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.0.40.1)’
using C++11
using SDK: ‘MacOSX14.0.sdk’
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/new/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppParallel/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/new/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppParallel/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c lags.cpp -o lags.o
In file included from lags.cpp:7:
...
clang++ -arch arm64 -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib -L/opt/R/arm64/lib -o hydrorecipes.so RcppExports.o lags.o -L/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.3-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: -single_module is obsolete
ld: warning: -multiply_defined is obsolete
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [hydrorecipes.so] Error 1
ERROR: compilation failed for package ‘hydrorecipes’
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/hydrorecipes/new/hydrorecipes.Rcheck/hydrorecipes’


```
### CRAN

```
* installing *source* package ‘hydrorecipes’ ...
** package ‘hydrorecipes’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.0.40.1)’
using C++11
using SDK: ‘MacOSX14.0.sdk’
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/old/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppParallel/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/recipes/old/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppParallel/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c lags.cpp -o lags.o
In file included from lags.cpp:7:
...
clang++ -arch arm64 -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib -L/opt/R/arm64/lib -o hydrorecipes.so RcppExports.o lags.o -L/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.3-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: -single_module is obsolete
ld: warning: -multiply_defined is obsolete
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [hydrorecipes.so] Error 1
ERROR: compilation failed for package ‘hydrorecipes’
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/hydrorecipes/old/hydrorecipes.Rcheck/hydrorecipes’


```
# workboots

<details>

* Version: 0.2.0
* GitHub: https://github.com/markjrieke/workboots
* Source code: https://github.com/cran/workboots
* Date/Publication: 2022-05-16 06:40:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::revdep_details(, "workboots")` for more info

</details>

## In both

*   checking whether package ‘workboots’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/workboots/new/workboots.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘workboots’ ...
** package ‘workboots’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘gridExtra’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘workboots’
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/workboots/new/workboots.Rcheck/workboots’


```
### CRAN

```
* installing *source* package ‘workboots’ ...
** package ‘workboots’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘gridExtra’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘workboots’
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/workboots/old/workboots.Rcheck/workboots’


```
