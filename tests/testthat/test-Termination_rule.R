test_that("wrong combination of termination and SC v1", {
  states<-matrix(c(0,0,0,1,1,1 ),byrow = TRUE,ncol=3)
  likely<-c(.5,.5)
  expect_warning(stopping_criterion(likely,states, termination="likelihood_maximization" ,SC=c(0.2,0.5)))
})
test_that("wrong combination of termination and SC v2", {
  states<-matrix(c(0,0,0,1,1,1 ),byrow = TRUE,ncol=3)
  likely<-c(.5,.5)
  expect_error(stopping_criterion(likely,states, termination="items_discrimination" ,SC=c(0.5)))
})


