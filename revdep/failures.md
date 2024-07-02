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
  dlopen(/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib, 0x000A): tried: '/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64')), '/System/Volumes/Preboot/Cryptexes/OS/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib' (no such file), '/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64'))
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
  dlopen(/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib, 0x000A): tried: '/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64')), '/System/Volumes/Preboot/Cryptexes/OS/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib' (no such file), '/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64'))
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
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.3.9.4)’
using C++11
using SDK: ‘MacOSX14.4.sdk’
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppParallel/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppParallel/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c lags.cpp -o lags.o
In file included from lags.cpp:7:
...
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘hydrorecipes’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/hydrorecipes/new/hydrorecipes.Rcheck/00LOCK-hydrorecipes/00new/hydrorecipes/libs/hydrorecipes.so':
  dlopen(/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/hydrorecipes/new/hydrorecipes.Rcheck/00LOCK-hydrorecipes/00new/hydrorecipes/libs/hydrorecipes.so, 0x0006): symbol not found in flat namespace '__ZN3tbb4task13note_affinityEt'
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/hydrorecipes/new/hydrorecipes.Rcheck/hydrorecipes’


```
### CRAN

```
* installing *source* package ‘hydrorecipes’ ...
** package ‘hydrorecipes’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.3.9.4)’
using C++11
using SDK: ‘MacOSX14.4.sdk’
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppParallel/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/Rcpp/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/recipes/revdep/library.noindex/hydrorecipes/RcppParallel/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c lags.cpp -o lags.o
In file included from lags.cpp:7:
...
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘hydrorecipes’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/hydrorecipes/old/hydrorecipes.Rcheck/00LOCK-hydrorecipes/00new/hydrorecipes/libs/hydrorecipes.so':
  dlopen(/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/hydrorecipes/old/hydrorecipes.Rcheck/00LOCK-hydrorecipes/00new/hydrorecipes/libs/hydrorecipes.so, 0x0006): symbol not found in flat namespace '__ZN3tbb4task13note_affinityEt'
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/emilhvitfeldt/Github/recipes/revdep/checks.noindex/hydrorecipes/old/hydrorecipes.Rcheck/hydrorecipes’


```
