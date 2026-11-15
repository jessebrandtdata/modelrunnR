## Unit tests for the two regex helpers behind the SQL @inputs ->
## physical-name substitution in launch_sql.R:
##   .mr_regex_escape             — escape a literal for use as a pattern
##   .mr_regex_escape_replacement — escape a literal for use as gsub repl
## Both are pure and security-relevant (a bad escape corrupts rendered
## SQL), so pin their contract directly.

test_that(".mr_regex_escape leaves word characters alone", {
  expect_equal(.mr_regex_escape("features"), "features")
  expect_equal(.mr_regex_escape("features_v2"), "features_v2")
})

test_that(".mr_regex_escape escapes regex metacharacters", {
  expect_equal(.mr_regex_escape("a.b"),  "a\\.b")
  expect_equal(.mr_regex_escape("a+b"),  "a\\+b")
  expect_equal(.mr_regex_escape("a*b"),  "a\\*b")
  expect_equal(.mr_regex_escape("x[y]"), "x\\[y\\]")
  expect_equal(.mr_regex_escape("a(b)"), "a\\(b\\)")
  expect_equal(.mr_regex_escape("d$"),   "d\\$")
  expect_equal(.mr_regex_escape("a\\b"), "a\\\\b")
})

test_that(".mr_regex_escape output matches only the literal string", {
  # The point of escaping: the escaped form, anchored, matches the
  # original literal and nothing structurally different.
  for (s in c("a.b", "a-b", "a+b*c", "x[y]", "a(b)", "d$", "a\\b")) {
    esc <- .mr_regex_escape(s)
    expect_true(grepl(paste0("^", esc, "$"), s, perl = TRUE),
                info = paste("literal self-match for", s))
  }
})

test_that(".mr_regex_escape prevents a metachar from matching wrongly", {
  # Unescaped, `.` in "a.b" would match "axb"; escaped it must not.
  esc <- .mr_regex_escape("a.b")
  expect_false(grepl(paste0("^", esc, "$"), "axb", perl = TRUE))
  expect_true(grepl(paste0("^", esc, "$"), "a.b", perl = TRUE))
})

test_that(".mr_regex_escape_replacement makes a string a literal gsub replacement", {
  # A replacement containing backslashes/backreferences must survive
  # gsub verbatim rather than being reinterpreted.
  round_trip <- function(s) gsub("X", .mr_regex_escape_replacement(s), "X", perl = TRUE)
  expect_equal(round_trip("name__abc"), "name__abc")
  expect_equal(round_trip("a\\1b"),      "a\\1b")
  expect_equal(round_trip("a\\\\b"),     "a\\\\b")
})

test_that(".mr_regex_escape_replacement composes with .mr_quote_ident", {
  # This is exactly how launch_sql.R uses them: quote the physical name,
  # then escape it for use as a replacement. The result must reproduce
  # the quoted identifier literally.
  quoted <- .mr_quote_ident('weird"name')
  repl   <- .mr_regex_escape_replacement(quoted)
  expect_equal(gsub("X", repl, "X", perl = TRUE), quoted)
})
