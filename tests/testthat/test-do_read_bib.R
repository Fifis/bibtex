test_that("Do read bib", {
  file <- system.file("bib/xampl_single.bib", package = "bibtex")

  # Mock RefManageR behaviour
  encoding <- "UTF-8"

  srcfile <- switch(encoding,
    unknown = srcfile(file),
    srcfile(file, encoding = encoding)
  )

  f <- do_read_bib(file, encoding = "UTF-8", srcfile = srcfile)

  expect_snapshot_output(f[[1]])
})


test_that("Do read bib with non standard entries", {
  tmp <- tempfile(fileext = ".bib")
  entry <- "@Website{newspaper,
    author = {Van Damme, Jean-Claude},
    title  = {Article title},
    date   = {2016-12-21},
    journal = {Newspaper name},
  }"
  writeLines(entry, tmp)

  out <- do_read_bib(tmp, encoding = "UTF-8", srcfile = tmp)

  expect_snapshot_output(out[[1]])
})


test_that("do read with several entries", {
  bib <- system.file("bib/xampl_standard.bib", package = "bibtex")
  out <- do_read_bib(bib, encoding = "UTF-8", srcfile = bib)

  expect_snapshot_output(lapply(out, print))
})


test_that("make.bib.entry can handle year-month pairs (#56)", {
  j <- c(author = "Thor, Adrij Udaya", date = "2011-05",  # date: YYYY-MM
         journal = "Applied Baphometrics",
         title = "Robust VCOV estimators: A survey",
         number = "4", pages = "13--64", volume = "26")
  attributes(j) <- c(attributes(j), list(entry = "Article", key = "author2011"))
  out <- make.bib.entry(j)
  expect_equal(out$year, "2011")
})


test_that("make.bib.entry can handle 'journaltitle' without 'journal' (#57)", {
  j <- c(author =  "Thor, Adrij Udaya", year = "2011",
         journaltitle = "Applied Baphometrics",  # not 'journal'
         title = "Robust VCOV estimators: A survey",
         number = "4", pages = "13--64", volume = "26")
  attributes(j) <- c(attributes(j), list(entry = "Article", key = "author2011"))
  out <- make.bib.entry(j)
  expect_equal(out$year, "2011")
})
