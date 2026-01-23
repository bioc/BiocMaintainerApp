library(testthat)
library(shiny)


test_that("Server reactive outputs work correctly", {

    
    testServer(BiocMaintainerShiny(), {

        expect_true(is.null(input$show_cols) || identical(input$show_cols, character(0)))

        table_data <- data()      
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
        visible_cols <- setdiff(colnames(table_data), hidden_cols)
        expect_true(all(c("package", "name", "email") %in% visible_cols))
        expect_true(all(!(hidden_cols %in% visible_cols)))
        
        session$setInputs(show_cols = c("consent_date", "needs_consent",
                                        "email_status"))
        selected_cols <- input$show_cols
        expect_equal(selected_cols, c("consent_date", "needs_consent", "email_status"))

        hidden_cols <- setdiff(optional_cols, selected_cols)
        visible_cols <- setdiff(colnames(table_data), hidden_cols)
        expect_true(all(selected_cols %in% visible_cols))
        expect_true(all(!(hidden_cols %in% visible_cols)))       
    })
})
