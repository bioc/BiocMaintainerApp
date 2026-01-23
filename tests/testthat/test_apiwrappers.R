library(jsonlite)
library(utils)

ref_tbl <- get_maintainer_data()


test_that("bypackage_works", {

    pkg <- "BiocFileCache"
    tbl <- getInfoByPackage(pkg)
    expect_true(is(tbl, "data.frame"))
    expect_true(all(tbl$package == pkg))
    subtbl <- ref_tbl[ref_tbl$package==pkg,]
    expect_equal(nrow(subtbl), nrow(tbl))

    pkg <- "NotFoundPackage"
    tbl <- getInfoByPackage(pkg)
    expect_true(nrow(tbl) == 0)
    subtbl <- ref_tbl[ref_tbl$package==pkg,]
    expect_equal(nrow(subtbl), nrow(tbl))
    
})

test_that("byname_works", {

    name <- "Lori Shepherd"
    tbl <- getInfoByName(name)
    expect_true(all(tbl$name == name))
    subtbl <- ref_tbl[ref_tbl$name==name,]
    expect_equal(nrow(subtbl), nrow(tbl))

    name <- "Hervé Pagès"
    tbl <- getInfoByName(name)
    expect_true(all(tbl$name == name))
    subtbl <- ref_tbl[ref_tbl$name==name,]
    expect_equal(nrow(subtbl), nrow(tbl))
   
    name <- "Not Found"
    tbl <- getInfoByName(name)
    expect_true(nrow(tbl) == 0)
    subtbl <- ref_tbl[ref_tbl$name==name,]
    expect_equal(nrow(subtbl), nrow(tbl))
})

test_that("byemail_works", {

    email = "maintainer@bioconductor.org"
    tbl <- getInfoByEmail(email)
    expect_true(all(tbl$email == email))
    subtbl <- ref_tbl[ref_tbl$email==email,]
    expect_equal(nrow(subtbl), nrow(tbl))

    email = "notfound@domain.something"
    tbl <- getInfoByEmail(email)
    expect_true(nrow(tbl) == 0)
    subtbl <- ref_tbl[ref_tbl$email==email,]
    expect_equal(nrow(subtbl), nrow(tbl))    
})

test_that("isvalid_works", {

    email="maintainer@bioconductor.org"
    res <- isEmailValid(email)
    expect_true(res$valid)
    expect_true(is.null(res$data))
    expect_true(is(res, "list"))

    email = "notfound@email.someorg"
    res <- isEmailValid(email)
    expect_false(res$valid)
    expect_true(is.null(res$data))
    expect_true(is(res, "list"))

    email = ref_tbl[ref_tbl$is_email_valid==0,"email"][1]
    res <- isEmailValid(email)
    expect_false(res$valid)
    expect_true(!is.null(res$data))
    expect_true(is(res$data, "data.frame"))        
})

test_that("listinvalid_works", {

    tbl <- listInvalid()
    expect_true(is(tbl, "data.frame"))

    subtbl <- ref_tbl[ref_tbl$is_email_valid==0,]
    expect_equal(nrow(tbl), nrow(subtbl))
})

test_that("listneedsconsent_works", {

    tbl <- listNeedsConsent()
    expect_true(is(tbl, "character"))

    subtbl <- ref_tbl[ref_tbl$email %in% tbl,]
    expect_equal(length(unique(subtbl$email)), length(tbl))
    expect_true(all(subtbl$consent_date < Sys.Date() - 365))
})

test_that("listallbademails_works", {

    tbl <- listAllBadEmails()
    expect_true(is(tbl, "character"))

    subtbl <- ref_tbl[ref_tbl$email %in% tbl,]
    vec1 <- ref_tbl$is_email_valid == 0
    vec2 <- ref_tbl$consent_date < Sys.Date() - 365
    expect_equal(length(unique(ref_tbl[(vec1 | vec2),"email"])), length(tbl))
})

test_that("listsuppression_works", {

    tbl <- listEmailsOnSuppressionList()
    expect_true(is(tbl, "list"))
    expect_true("data" %in% names(tbl))
    data_tbl <- tbl$data
    expect_true(is(data_tbl, "data.frame"))
    
    subtbl <- ref_tbl[ref_tbl$email_status == "suppressed",c("email", "name")]
    subtbl2 <- unique(subtbl[, c("email", "name")])
    expect_equal(nrow(subtbl2), nrow(data_tbl))
})
