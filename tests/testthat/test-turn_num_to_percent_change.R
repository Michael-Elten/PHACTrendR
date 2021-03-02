test_that("turn_num_to_percent_change turns a numeric vector/variable to formatted percent change", {

  test_vec<-c(-0.005,-0.34,0.054,0.001,0,NA,1/0)
  test_vec_correct<-c("-0.5%","-34.0%","+5.4%","+0.1%","0.0%","NA","+Inf")

  test_dataframe<-data.frame(obs_num=seq(1:7),num_var=test_vec)
  test_dataframe_correct<-data.frame(obs_num=seq(1:7), num_var=test_vec_correct)

  expect_identical(turn_num_to_percent_change(test_vec),test_vec_correct)
  expect_identical(turn_num_to_percent_change(test_dataframe, numeric_variable = "num_var"),test_dataframe_correct)
})
