## Unit tests for the external-inputs JSON codec used on the
## queue -> pickup round trip:
##   .mr_external_inputs_to_json        — serialize a resolved record
##   .mr_external_inputs_decl_from_json — recover just the declarations
## Hashes recorded at queue time are intentionally dropped by the
## decoder (pickup re-resolves), so the decl round trip keeps only
## paths and env names.

test_that("to_json serializes files and env with auto-unboxed scalars", {
  resolved <- list(
    files = list(list(path = "/a/b.csv", hash = "h1")),
    env   = list(list(name = "TOK", hash = "h2"))
  )
  j <- .mr_external_inputs_to_json(resolved)
  expect_true(grepl('"path":"/a/b.csv"', j, fixed = TRUE))
  expect_true(grepl('"name":"TOK"', j, fixed = TRUE))
  # auto_unbox = TRUE -> scalars are bare strings, not ["..."] arrays.
  expect_false(grepl('["', j, fixed = TRUE))
})

test_that("decl_from_json recovers paths and env names, dropping hashes", {
  resolved <- list(
    files = list(list(path = "/a/b.csv", hash = "h1"),
                 list(path = "/c/d.parquet", hash = "h2")),
    env   = list(list(name = "TOK", hash = "h3"))
  )
  decl <- .mr_external_inputs_decl_from_json(.mr_external_inputs_to_json(resolved))
  expect_equal(decl$files, c("/a/b.csv", "/c/d.parquet"))
  expect_equal(decl$env, "TOK")
  # Decl is a flat character vector of paths/names — no hash structure.
  expect_type(decl$files, "character")
  expect_named(decl, c("files", "env"))
})

test_that("decl_from_json returns NULL for NA / empty / malformed input", {
  expect_null(.mr_external_inputs_decl_from_json(NA_character_))
  expect_null(.mr_external_inputs_decl_from_json(""))
  expect_null(.mr_external_inputs_decl_from_json("{not valid json"))
})

test_that("decl_from_json returns NULL when there are no declarations", {
  empty_json <- .mr_external_inputs_to_json(list(files = list(), env = list()))
  expect_null(.mr_external_inputs_decl_from_json(empty_json))
})

test_that("decl_from_json handles a files-only record", {
  resolved <- list(files = list(list(path = "/only/file.csv", hash = "h")),
                   env = list())
  decl <- .mr_external_inputs_decl_from_json(.mr_external_inputs_to_json(resolved))
  expect_equal(decl$files, "/only/file.csv")
  expect_length(decl$env, 0L)
})
