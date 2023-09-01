test_that("credential works", {
  cred <- credentials("a", "b", "c")
  expect_true(is_valid_credentials(cred))
})
