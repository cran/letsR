context("Test for lets.distmat")
data(PAM)
dimPAM <- summary(PAM)$Numberofcells

coords <- PAM[[1]][1:10, 1:2]


test_that("lets.distmat works fine, asdist = TRUE", {
  
  
  distPAM <- lets.distmat(PAM)   
  expect_true(class(distPAM)[1] == "dist")
  expect_true(all(dim(as.matrix(distPAM)) == dimPAM))
    
})


test_that("lets.distmat works fine, asdist = FALSE", {
  
  
  distPAM <- lets.distmat(PAM, asdist = FALSE)   
  expect_true(class(distPAM)[1] == "matrix")
  expect_true(all(dim(distPAM) == dimPAM))
  
})


