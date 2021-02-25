test_that("turn_char_vec_to_comma_list turns a character vector to human readable list", {
  num_vec1<-1
  num_vec1_correct<-1

  char_vec1<-"one"
  char_vec1_correct<-"one"

  char_vec2<-c("one","two")
  char_vec2_correct<-"one and two"

  char_vec3<-c("one","two","three")
  char_vec3_correct<-"one, two, and three"

  expect_identical(turn_char_vec_to_comma_list(num_vec1),num_vec1_correct)
  expect_identical(turn_char_vec_to_comma_list(char_vec1),char_vec1_correct)
  expect_identical(turn_char_vec_to_comma_list(char_vec2),char_vec2_correct)
  expect_identical(turn_char_vec_to_comma_list(char_vec3),char_vec3_correct)
})
