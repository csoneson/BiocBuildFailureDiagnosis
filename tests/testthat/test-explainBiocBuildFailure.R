test_that("explainBiocBuildFailure works", {
    skip_if_offline()
    out <- explainBiocBuildFailure("Rhisat2")
    expect_type(out, "list")
})
