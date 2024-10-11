test_that("Absence of states entry in plot", {
  data("AA_knowledge_test")
  AA_knowledge_test$states<-NULL
  expect_error(plot(AA_knowledge_test))
})
