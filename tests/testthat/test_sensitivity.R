context("Sensitivity Implementation")

# Sensitivity {{{
test_that("Sensitivity", {
    skip_if_not(eplusr::is_avail_eplus(8.8))

    # read idf
    example <- copy_example()
    sen <- Sensitivity$new(example$idf, example$epw)

    expect_error(sen$param(GP01 = list(Thickness = c(0, 1))), class = "error_param_num_format")
    expect_error(sen$param(GP01 = list(Thickness = c(2, 2, 1))), class = "error_param_num_format")
    expect_error(sen$param(GP01 = list(Thickness = c(1, 2, 0))), class = "error_param_num_format")
    expect_error(sen$param(GP01 = list(Thickness = c(1, 2, 1.1))), class = "error_param_num_format")
    expect_error(
        sen$set_param(
            GP01 = list(Roughness = c("VeryRough", "Rough", "MediumRough", "Smooth"))
        )
    )

    expect_silent(
        sen$param(
            GP01 = list(Thickness = c(min = 0.01, max = 1, levels = 5)),
            `Supply Fan 1` = list(Fan_Total_Efficiency = c(0.1, 1.0, 5)),
            .r = 1, .grid_jump = 1
        )
    )

    # support Schedule:Compact
    expect_silent(
        sen$param(
            GP01 = list(Thickness = c(min = 0.01, max = 1, levels = 5)),
            ActSchd = list(Field_4 = c(30, 120, 5)),
            .r = 1, .grid_jump = 1
        )
    )
    # can stop if original field is not a schedule value
    expect_error(sen$param(ActSchd = list(Field_3 = c(30, 120, 5)), .r = 1, .grid_jump = 1))

})
# }}}
