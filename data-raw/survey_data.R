catch_nwfsc_combo <- pull_catch(common_name = "lingcod", years = 2003:2019)
bio_nwfsc_combo <- pull_bio(common_name = "lingcod", years = 2003:2019)

usethis::use_data(
  catch_nwfsc_combo,
  bio_nwfsc_combo,
  overwrite = TRUE
)
