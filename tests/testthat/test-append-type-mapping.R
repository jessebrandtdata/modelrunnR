## Unit tests for .mr_append_r_to_duckdb_type() — the R-column ->
## DuckDB-type mapping that fixes an append table's physical schema.
## Pure function; pins the type table so a change is deliberate.

test_that(".mr_append_r_to_duckdb_type maps the common atomic types", {
  expect_equal(.mr_append_r_to_duckdb_type(1L),      "INTEGER")
  expect_equal(.mr_append_r_to_duckdb_type(1.5),     "DOUBLE")
  expect_equal(.mr_append_r_to_duckdb_type(TRUE),    "BOOLEAN")
  expect_equal(.mr_append_r_to_duckdb_type("a"),     "TEXT")
})

test_that(".mr_append_r_to_duckdb_type maps temporal types", {
  expect_equal(.mr_append_r_to_duckdb_type(as.Date("2020-01-01")), "DATE")
  expect_equal(
    .mr_append_r_to_duckdb_type(as.POSIXct("2020-01-01 00:00:00", tz = "UTC")),
    "TIMESTAMP"
  )
})

test_that(".mr_append_r_to_duckdb_type maps factors to TEXT", {
  expect_equal(.mr_append_r_to_duckdb_type(factor("a")), "TEXT")
})

test_that(".mr_append_r_to_duckdb_type checks integer before numeric", {
  # is.numeric() is TRUE for integers too, so ordering matters: an
  # integer column must map to INTEGER, not DOUBLE.
  expect_equal(.mr_append_r_to_duckdb_type(1:3), "INTEGER")
})

test_that(".mr_append_r_to_duckdb_type maps integer64 to BIGINT", {
  skip_if_not_installed("bit64")
  expect_equal(.mr_append_r_to_duckdb_type(bit64::as.integer64(1)), "BIGINT")
})

test_that(".mr_append_r_to_duckdb_type falls back to TEXT for unknown types", {
  expect_equal(.mr_append_r_to_duckdb_type(1 + 2i),     "TEXT")  # complex
  expect_equal(.mr_append_r_to_duckdb_type(list(1, 2)), "TEXT")  # list column
})
