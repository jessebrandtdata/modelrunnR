## Edge-case unit tests for mr_binds() / mr_envelopes() that the main
## test-mr-binds.R suite doesn't cover: cross-mode zero-length slots,
## .labels validation, length-1 recycling under cross, and the
## duplicate-.label warning path.

test_that("mr_binds(cross) rejects a zero-length slot", {
  expect_error(
    mr_binds(a = character(0), b = c(1, 2), mode = "cross"),
    "zero-length slot"
  )
})

test_that("mr_binds(cross) recycles a length-1 slot across the product", {
  b <- mr_binds(a = c(1, 2, 3), shared = "X", mode = "cross")
  expect_equal(length(b), 3L)
  expect_true(all(vapply(b, function(e) identical(e$shared, "X"), logical(1))))
})

test_that("mr_binds(.labels) rejects non-character or NA labels", {
  expect_error(mr_binds(a = c(1, 2), .labels = c(1, 2)),
               "non-NA character vector")
  expect_error(mr_binds(a = c(1, 2), .labels = c("x", NA)),
               "non-NA character vector")
})

test_that("mr_envelopes() warns on duplicate .label but still builds", {
  expect_warning(
    e <- mr_envelopes(list(.label = "dup", x = 1),
                      list(.label = "dup", y = 2)),
    "duplicate .label"
  )
  expect_s3_class(e, "mr_binds")
  expect_equal(length(e), 2L)
})

test_that("mr_envelopes() rejects a whitespace-only .label", {
  expect_error(
    mr_envelopes(list(.label = "   ", x = 1)),
    "non-empty string"
  )
})

test_that("mr_envelopes() does not warn when labels are distinct or absent", {
  expect_no_warning(
    mr_envelopes(list(.label = "a", x = 1), list(.label = "b", y = 2))
  )
  expect_no_warning(
    mr_envelopes(list(x = 1), list(y = 2))
  )
})
