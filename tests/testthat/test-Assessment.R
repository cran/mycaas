test_that("Convergence of the Assessment 1", {
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
  beta  <- integer(5)
  eta   <- integer(5)
  likelihood <-rep(1,nrow(states))/nrow(states)
  expect_equal(assessment(likelihood,states,beta,eta,ki=4,SC=c(.9))$kstate[[1]],4)
})

test_that("Convergence of the Assessment 2", {
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
  beta  <-c(.04,.03,.2,.1,.07)
  eta   <-c(.05, .04, .1 ,.07,.08)
  likelihood <-rep(1,nrow(states))/nrow(states)
  expect_gte(assessment(likelihood,states,beta,eta,ki=11,repetition=TRUE,SC=c(.9))$P_kstate,.9)
})


test_that("Convergence of the Assessment 3", {
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
  beta  <-c(.05,.05,.05,.05,.05)
  eta   <-c(.05,.05,.05,.05,.05)
  likelihood <-rep(1,nrow(states))/nrow(states)
  expect_gte(assessment(likelihood,states,beta,eta,ki=6,repetition=TRUE,SC=c(.7))$P_kstate,.7)
})

test_that("NO repetition 3", {
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
  beta  <-c(.004,.03,.02,.01,.007)
  eta   <-c(5e-06, 5e-05, 4e-05,.007,.08)
  likelihood <-rep(1,nrow(states))/nrow(states)
  expect_lte(assessment(likelihood,states,beta,eta,ki=6, repetition = FALSE, SC=1)$n,ncol(states))
})

