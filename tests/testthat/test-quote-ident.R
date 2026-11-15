## Unit tests for .mr_quote_ident() — DuckDB identifier quoting used
## everywhere a logical name reaches a SQL identifier slot. Pure fn.

test_that(".mr_quote_ident wraps a plain name in double quotes", {
  expect_equal(.mr_quote_ident("foo"), '"foo"')
  expect_equal(.mr_quote_ident("_mr_runs"), '"_mr_runs"')
})

test_that(".mr_quote_ident doubles embedded double-quotes", {
  # This is the injection-relevant behavior: a `"` inside the name must
  # be doubled so it cannot terminate the quoted identifier early.
  expect_equal(.mr_quote_ident('a"b'),   '"a""b"')
  expect_equal(.mr_quote_ident('a""b'),  '"a""""b"')
  expect_equal(.mr_quote_ident('"'),     '""""')
})

test_that(".mr_quote_ident leaves other characters untouched", {
  # Only `"` is special to identifier quoting; spaces and punctuation
  # pass through verbatim inside the quotes.
  expect_equal(.mr_quote_ident("weird name"), '"weird name"')
  expect_equal(.mr_quote_ident("a-b.c"),      '"a-b.c"')
})

test_that(".mr_quote_ident output is always a single quoted token", {
  for (n in c("x", 'x"y', "with space", "")) {
    q <- .mr_quote_ident(n)
    expect_true(startsWith(q, '"'))
    expect_true(endsWith(q, '"'))
  }
})
