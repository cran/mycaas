test_that("Absence of states entry in summary", {
  data("AA_knowledge_test")
  AA_knowledge_test$states<-NULL
  expect_error(summary(AA_knowledge_test))
})
