# TODO: now too time intensive. But at a certain point, fix it, uncomment it:
# test_that("Can read data files", {
#   withr::local_options(
#     better.gbd_path = paste0(
#       "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared",
#       "/IRB Projects/Health Projects/Health Team/Public datasets on health"
#       ,"/Global Burden of Disease Study Data"
#     )
#   )
#   gbd <- gbd_data_read()
#   expect_true(is.data.frame(gbd))
#   expect_gt(nrow(gbd),1)
# })

# Set all better options to NULL
for (i in stringr::str_subset(names(options()),"better\\.")) {
  print(i)
  print(.Options[[i]])
  .Options[[i]] <- NULL
  print(.Options[[i]])
}

test_that("Can read codebook files", {
  withr::local_options(
    better.gbd_path = paste0(
      "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared",
      "/IRB Projects/Health Projects/Health Team/Public datasets on health"
      ,"/Global Burden of Disease Study Data"
    )
  )
  gbd_cb <- gbd_codebook_read()
  expect_equal(class(gbd_cb), c("list"))
  expect_true(is.data.frame(gbd_cb[[1]]))
  expect_gt(nrow(gbd_cb[[1]]),1)
})

# TODO: now too time intensive. But at a certain point, fix it, uncomment it:
# test_that("Can clean data", {
#   withr::local_options(
#     better.gbd_path = paste0(
#       "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared",
#       "/IRB Projects/Health Projects/Health Team/Public datasets on health"
#       ,"/Global Burden of Disease Study Data"
#     )
#   )
#   gbd <- gbd_data_read()
#   gbd <- gbd_data_clean(gbd)
#   expect_true(is.data.frame(gbd))
#   expect_gt(nrow(gbd),1)
# })

test_that("Can clean codebook", {
  withr::local_options(
    better.gbd_path = paste0(
      "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared",
      "/IRB Projects/Health Projects/Health Team/Public datasets on health"
      ,"/Global Burden of Disease Study Data"
    )
  )
  gbd_cb <- gbd_codebook_read()
  gbd_cb <- gbd_codebook_clean(gbd_cb)
  expect_equal(class(gbd_cb), c("list"))
  expect_true(is.data.frame(gbd_cb[[1]]))
  expect_gt(nrow(gbd_cb[[1]]),1)
})

if (FALSE) {

  # TODO: now too time intensive. But at a certain point, fix it, uncomment it:
  test_that("Saves data", {
    withr::local_options(
      better.gbd_path = paste0(
        "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared",
        "/IRB Projects/Health Projects/Health Team/Public datasets on health"
        ,"/Global Burden of Disease Study Data"
      )
    )
    gbd_save()
    expect_lt(
      difftime(Sys.time(),file.info(gbd_rds_path())$ctime,unit="secs"),
      1
    )
  })

  test_that("Arguments override parameters", {
    # withr::local_options(
    #   better.gbd_path = paste0(
    #     "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared",
    #     "/IRB Projects/Health Projects/Health Team/Public datasets on health"
    #     ,"/Global Burden of Disease Study Data"
    #   )
    # )
    pars <- list(
      sex = "female",
      year = 2019
    )
    print(gbd)
    expect_equal(unique(gbd_filter(sex = "female")$sex), "female")
    expect_equal(unique(gbd_filter(params = pars)$sex), "female")
    expect_equal(unique(gbd_filter(params = pars, sex = "male")$sex), "male")
  })
}

