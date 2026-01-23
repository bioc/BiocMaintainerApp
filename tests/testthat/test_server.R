library(testthat)
library(shiny)

test_that("Server reactive outputs work correctly", {

    
    testServer(BiocMaintainerShiny(), {

        expect_equal(input$show_cols, character(0))

        dt <- output$maintainers_table
        table_data <- dt()
        expect_true(all(c("package", "name", "email") %in% colnames(table_data)))

        optional_cols <- c(
            "consent_date",
            "needs_consent",
            "email_status",
            "is_email_valid",
            "last_verification_sent",
            "bounce_type",
            "bounce_subtype",
            "smtp_status",
            "diagnostic_code"
        )
        hidden_cols <- setdiff(optional_cols, input$show_cols)
        expect_true(all(!(hidden_cols %in% colnames(table_data)[1:length(hidden_cols)])))

        session$setInputs(show_cols = c("consent_date", "needs_consent",
                                        "email_status"))
        table_data2 <- dt()
        expect_true(all(c("consent_date", "needs_consent", "email_status") %in% colnames(table_data2)))
        not_selected <- setdiff(optional_cols, input$show_cols)
        expect_true(all(!(not_selected %in% colnames(table_data2))))
    })
})
