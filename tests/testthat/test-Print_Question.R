test_that("Print question with two identical  answers", {
      # Simulate calling the function
      TextQuestion <- "Which option is correct?"
      TextResponse <- list(incorrect = c("wrong1", "wrong2", "correct"))
      expect_error(print_question(TextQuestion, TextResponse))
})

