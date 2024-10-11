test_that("Carless error equal to 1", {
  states<-matrix(c( 0,0,0,0,0,
                    0,0,0,0,1,
                    0,0,1,0,1,
                    0,0,0,1,1,
                    0,0,1,1,1,
                    1,0,1,0,1,
                    0,1,0,1,1,
                    1,0,1,1,1,
                    0,1,1,1,1,
                    1,1,0,1,1,
                    1,1,1,1,1), byrow=TRUE, ncol=5)
  beta  <-c(1,1,1,1,1)
  eta   <-c(5e-06, 5e-05, 4e-05,.007,.08)
  likelihood <-rep(1,nrow(states))/nrow(states)
  expect_error(performance_simulation(states,beta,eta,likelihood))
})


test_that("Likelihood and kstats have a different length", {
  states<-matrix(c( 0,0,0,0,0,
                    0,0,0,0,1,
                    0,0,1,0,1,
                    0,0,0,1,1,
                    0,0,1,1,1,
                    1,0,1,0,1,
                    0,1,0,1,1,
                    1,0,1,1,1,
                    0,1,1,1,1,
                    1,1,0,1,1,
                    1,1,1,1,1), byrow=TRUE, ncol=5)
  beta  <-c(5e-06, 5e-05, 4e-05,.007,.08)
  eta   <-c(5e-06, 5e-05, 4e-05,.007,.08)
  likelihood <- c(.5,.5)
  expect_error(performance_simulation(states,beta,eta,likelihood))
})


