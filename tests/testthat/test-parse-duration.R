## Unit tests for .mr_parse_duration() — the "<n><unit>" spec parser
## behind prune(older_than = ...). Pure function; no DB needed.

test_that(".mr_parse_duration parses each unit into seconds", {
  as_secs <- function(x) as.numeric(.mr_parse_duration(x))
  expect_equal(as_secs("45s"), 45)
  expect_equal(as_secs("15m"), 15 * 60)
  expect_equal(as_secs("6h"),  6 * 3600)
  expect_equal(as_secs("30d"), 30 * 86400)
})

test_that(".mr_parse_duration returns a difftime in seconds", {
  d <- .mr_parse_duration("2h")
  expect_s3_class(d, "difftime")
  expect_equal(attr(d, "units"), "secs")
  expect_equal(as.numeric(d), 7200)
})

test_that(".mr_parse_duration accepts zero and multi-digit magnitudes", {
  expect_equal(as.numeric(.mr_parse_duration("0s")), 0)
  expect_equal(as.numeric(.mr_parse_duration("100d")), 100 * 86400)
})

test_that(".mr_parse_duration tolerates whitespace between number and unit", {
  # The regex is ^([0-9]+)\s*([smhd])$ — a space is permitted. Pin it so
  # a future tightening is a conscious choice, not a silent break.
  expect_equal(as.numeric(.mr_parse_duration("30 d")), 30 * 86400)
})

test_that(".mr_parse_duration rejects malformed specs with a helpful message", {
  expect_error(.mr_parse_duration("10x"), "could not parse duration")
  expect_error(.mr_parse_duration("d"),   "could not parse duration")
  expect_error(.mr_parse_duration("100"), "could not parse duration")
  expect_error(.mr_parse_duration(""),    "could not parse duration")
})

test_that(".mr_parse_duration is case-sensitive on the unit", {
  # Units are lower-case only; uppercase should not silently succeed.
  expect_error(.mr_parse_duration("30D"), "could not parse duration")
  expect_error(.mr_parse_duration("6H"),  "could not parse duration")
})

test_that(".mr_parse_duration surfaces the caller context in errors", {
  expect_error(.mr_parse_duration("nope", context = "prune_variants"),
               "prune_variants\\(\\): could not parse duration")
})
