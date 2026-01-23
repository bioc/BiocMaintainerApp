library(testthat)
library(shiny)
library(DT)
library(jsonlite)


test_that("retrieves_data_with_correct_columns",{

    tbl <- get_maintainer_data()
    
    expect_true(all(c("package", "name", "email") %in% colnames(tbl)))
    expect_true(is(tbl, "data.frame"))
   
})
