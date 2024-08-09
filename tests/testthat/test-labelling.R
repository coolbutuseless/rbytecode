test_that("multiplication works", {

  add_inline <- function(z) { for (i in seq_along(z)) z[i] + 1 }
  f <- function(x) x + 1
  add_call <- function(z) { for (i in seq_along(z)) f(z[i]) }
  
  # Only has depth = 0
  expect_no_error(
    dis(add_inline)
  )
  
  # Has depth = 0 and 1
  # label location lookup matches two PCs - 
  #   one at depth=0 and one at depth=1
  #   row_idx filter changed to match the label depth
  expect_no_error(
    dis(add_call)
  )  
})
