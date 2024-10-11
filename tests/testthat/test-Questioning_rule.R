test_that("eta bigger then 1", {
  states<-matrix(c(0,0,0,1,1,1 ),byrow = TRUE,ncol=3)
  likely<-c(.5,.5)
  eta<- c(.2,.1,1.4)
  beta<- c(.2,.1,.4)
  expect_error(halfsplit(likely,states,beta,eta))
})

test_that("eta bigger then 1", {
  states<-matrix(c(0,0,0,1,1,1 ),byrow = TRUE,ncol=3)
  likely<-c(.5,.5,.3)
  eta<- c(.2,.1,.4)
  beta<- c(.2,.1,.4)
  expect_error(halfsplit(likely,states,beta,eta))
})
