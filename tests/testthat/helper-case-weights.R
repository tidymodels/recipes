set.seed(1)
freq_wts <- floor(runif(32, max = 9)) + 1
freq_wts <- as.integer(freq_wts)
frac_wts <- runif(32)
miss_wts <- freq_wts
miss_wts[1] <- NA

mtcars_expanded <- mtcars[rep(seq_along(freq_wts), freq_wts), ]

mtcar_mis <- mtcars
mtcar_mis[1, 2] <- NA
mtcar_mis[2, 3] <- NA
