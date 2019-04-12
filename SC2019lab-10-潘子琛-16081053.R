library(rARPACK);
library(jpeg);

factorize <- function(m, k)
{
  r <- svds(m[, , 1], k);
  g <- svds(m[, , 2], k);
  b <- svds(m[, , 3], k);
  return(list(r = r, g = g, b = b));
}

recoverimg <- function(lst, k)
{
  recover0 <- function(fac, k)
  {
    dmat <- diag(k);
    diag(dmat) <- fac$d[1:k];
    m <- fac$u[, 1:k] %*% dmat %*% t(fac$v[, 1:k]);
    m[m < 0] <- 0;
    m[m > 1] <- 1;
    return(m);
  }
  r <- recover0(lst$r, k);
  g <- recover0(lst$g, k);
  b <- recover0(lst$b, k);
  m <- array(0, c(nrow(r), ncol(r), 3));
  m[, , 1] <- r;
  m[, , 2] <- g;
  m[, , 3] <- b;
  return(m);
}

rawimg <- readJPEG("C:/Users/95898/Pictures/Saved Pictures/S6.jpg");
lst <- factorize(rawimg, 100);
neig <- c(1, 5, 20, 50, 100);
for(i in neig)
{
  m <- recoverimg(lst, i);
  writeJPEG(m, sprintf("svd_%d.jpg", i), 0.95);
}

