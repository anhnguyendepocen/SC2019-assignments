# input the matrix A
A <- matrix(c(0,1,0,1,1,
            0,0,1,1,1,
            1,0,0,1,0,
            0,0,0,0,1,
            0,0,1,0,0), nrow = 5)
# calculate eigenvalue & eigenvector of A
eiA <- eigen(A); eiA
# extracting feature vectors
vectors <- eiA$vectors; vectors
# the ranking vector is treated like a probability of relevance, so we just need the first column
rank1 <- vectors[, 1]; rank1
# sum of the probability should be one, so we make a scale factor k to achieve it
k <- 1 / sum(rank1)
newr1 <- k * rank1
# now equal to one
sum(newr1)
# get the ranking of sites
ranking <- matrix(c(order(newr1)), nrow = 1, dimnames = list(c("ranking"), c("site1", "site2", "site3", "site4", "site5"))); ranking
