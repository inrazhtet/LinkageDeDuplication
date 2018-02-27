# Generate random strings in the following way: ABCDE1234E, i.e each string contains 5 Characters, 4 Numerics, then 1 Char.

generateStrings <- function(n = 5000) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

generateStrings(10)
