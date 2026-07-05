## Unit tests for .mr_has_nondefault_rownames() — decides whether a
## data frame carries meaningful row names worth preserving before a
## DuckDB write drops them. Pure predicate.

test_that(".mr_has_nondefault_rownames is FALSE for default integer row names", {
  expect_false(.mr_has_nondefault_rownames(data.frame(x = 1:3)))
})

test_that(".mr_has_nondefault_rownames is FALSE for a zero-row frame", {
  # No rows -> nothing to preserve, regardless of the row.names attr.
  expect_false(.mr_has_nondefault_rownames(data.frame()))
  expect_false(.mr_has_nondefault_rownames(data.frame(x = integer(0))))
})

test_that(".mr_has_nondefault_rownames is TRUE for character row names", {
  d <- data.frame(x = 1:3)
  rownames(d) <- c("a", "b", "c")
  expect_true(.mr_has_nondefault_rownames(d))
})

test_that(".mr_has_nondefault_rownames is TRUE when integer row names are reordered", {
  d <- data.frame(x = 1:3)
  d2 <- d[c(3, 1, 2), , drop = FALSE]
  # The preserved labels are now 3,1,2 — not seq_len(nrow) — so this is
  # non-default and worth keeping.
  expect_true(.mr_has_nondefault_rownames(d2))
})

test_that(".mr_has_nondefault_rownames is FALSE for a non-data-frame", {
  expect_false(.mr_has_nondefault_rownames(1:3))
  expect_false(.mr_has_nondefault_rownames("not a frame"))
})
