test_that("explainBiocBuildFailure works", {
    skip_if_offline()
    out <- explainBiocBuildFailure("Rhisat2")
    expect_s3_class(out, "visNetwork")
})
