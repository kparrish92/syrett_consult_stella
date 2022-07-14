x <- c(7, 8, 11, 13, 16, 13, 17, 5, 8, 10, 13,
       10, 14, 5, 7, 10, 7, 11, 8, 11, 8, 12,
       5, 6, 10, 9, 13, 8)


rownames(eng_mono)

x = dist(eng_mono)
names = eng_mono$speaker

M <- matrix(0, 45, 45)
M[lower.tri(M)] <- x
M <- t(M)
M[lower.tri(M)] <- x
dimnames(M) <- list(names, names)
tr <- nj(M)

plot(tr)

ex = 