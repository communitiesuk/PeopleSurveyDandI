test_that("summarize by group works", {
  #set up data
  #MHCLGflat<-data.frame()
  yr<-2024
  MHCLGflat<-readRDS(paste0("Q:/ADD Directorate/ARC/Staff Survey/People Survey ",yr,"/Data/MHCLG",yr,"_flat.rds"))
  #run summarize by group
  result<-summarize_by_group(MHCLGflat, J01A, "Gender")
  expect_equal(result[[2,"Char"]], "Female")
})


