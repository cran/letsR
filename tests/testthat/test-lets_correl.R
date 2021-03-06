context("Test for lets.correl")

N <- 100
x <- rnorm(nrow(PAM[[1]]))[1:N]
y <- lets.distmat(PAM[[1]][1:N, 1:2])
y2 <- lets.distmat(PAM[[1]][1:N, 1:2], asdist = F)
z <- 10
equidistant <- FALSE
plot <- FALSE

test_that("lets.correl works fine", {
  correl <- lets.correl(x, y, z, equidistant, plot)
  
  expect_true(is.matrix(correl))
  expect_true(all(!is.na(correl)))
})

test_that("lets.correl works fine, equidistant = TRUE", {
  correl <- lets.correl(x, y, z, equidistant = TRUE, plot)
  
  expect_true(is.matrix(correl))
  expect_true(all(!is.na(correl)))
})

test_that("lets.correl works fine, plot = TRUE", {
  correl <- lets.correl(x, y, z, plot = TRUE)
  
  expect_true(is.matrix(correl))
  expect_true(all(!is.na(correl)))
})


test_that("lets.correl works fine, matrix", {
  correl <- lets.correl(x, y2, z, plot)
  
  expect_true(is.matrix(correl))
  expect_true(all(!is.na(correl)))
})

test_that("lets.correl works fine, multiple", {
  correl <- lets.correl(cbind(x, sample(x)), y, z, plot)
  
  expect_true(is.matrix(correl))
  expect_true(all(!is.na(correl)))
})


test_that("lets.correl gives error", {
  N <- 2
  x <- rnorm(nrow(PAM[[1]]))[1:N]
  y <- lets.distmat(PAM[[1]][1:N, 1:2])
  expect_error(lets.correl(x, y, z, plot))
})


test_that("lets.correl gives error", {
  y2[1, 1] <- NA
  expect_error(lets.correl(x, y2, z, plot))
})

