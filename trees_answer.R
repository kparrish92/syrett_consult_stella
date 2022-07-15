x <- c(7, 8, 11, 13, 16, 13, 17, 5, 8, 10, 13,
       10, 14, 5, 7, 10, 7, 11, 8, 11, 8, 12,
       5, 6, 10, 9, 13, 8)


rownames(eng_mono)

make_tree(eng_mono)

make_tree = function(df)
{
x = dist(df)
names = df$speaker

M <- matrix(0, 45, 45)
M[lower.tri(M)] <- x
M <- t(M)
M[lower.tri(M)] <- x
dimnames(M) <- list(names, names)
tr <- nj(M)

plot = plot(tr)
return(plot)
}

eng_mono$speaker

eng_mono$speaker_2 = df_speakers$lang_2
eng_mono$speaker_1 = df_speakers$lang_1

geo <- factor(eng_mono$speaker_2)
(language_type <- c("#999999", "#E69F00", "#56B4E9","#009E73", "#F0E442"))[geo])

(mycol <- c("#999999", "#E69F00", "#56B4E9","#009E73", "#F0E442")[geo])

df = eng_mono

x = dist(df)
names = df$speaker

M <- matrix(0, 45, 45)
M[lower.tri(M)] <- x
M <- t(M)
M[lower.tri(M)] <- x
dimnames(M) <- list(names, names)
tr <- nj(M)

plot = plot(tr, tip.color = mycol)


