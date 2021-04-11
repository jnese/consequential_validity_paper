library(tidyverse)
library(lubridate)

data_raw <- read_csv("C:/Users/jnese/Desktop/BRT/GRANT-CORE/Project/Publications/consequential_validity_study/consequential_validity_study/data/project_product_data/data_final.csv")

# core_students_y4
# id_core == 974 is the same as id_core == 1260
# id_core == 1033 is the same as id_core == 1259

case_974 <- data_raw %>% 
  filter(id_core == 974 | id_core == 1260,
         year == 4) %>% 
  pivot_longer(
    cols = -c(id_core),
    names_to = "vars",
    values_to = "value",
    values_transform = list(value = as.character)
  ) %>% 
  pivot_wider(
    names_from = id_core,
    values_from = value
  ) %>% 
  unnest() %>% 
  mutate(z = coalesce(`974`, `1260`),
         id_core = 974) %>% 
  select(id_core, vars, value = z) %>% 
  pivot_wider(
    names_from = vars,
    values_from = value
  ) %>%  
  mutate(across(names(select_if(data_raw, is.numeric)), as.numeric),
         across(names(select_if(data_raw, lubridate::is.POSIXct)), lubridate::ymd_hms),
         middle_state = as.logical(middle_state))

data_raw <- data_raw %>% 
  anti_join(
    data_raw %>% 
      filter(id_core == 974 | id_core == 1260,
             year == 4)
  ) %>% 
  bind_rows(case_974)


case_1033 <- data_raw %>% 
  filter(id_core == 1033 | id_core == 1259,
         year == 4) %>% 
  pivot_longer(
    cols = -c(id_core),
    names_to = "vars",
    values_to = "value",
    values_transform = list(value = as.character)
  ) %>% 
  pivot_wider(
    names_from = id_core,
    values_from = value
  ) %>% 
  unnest() %>% 
  mutate(z = coalesce(`1033`, `1259`),
         id_core = 1033) %>%
  select(id_core, vars, value = z) %>% 
  pivot_wider(
    names_from = vars,
    values_from = value
  ) %>% 
  mutate(across(names(select_if(data_raw, is.numeric)), as.numeric),
         across(names(select_if(data_raw, lubridate::is.POSIXct)), lubridate::ymd_hms),
         middle_state = as.logical(middle_state))

data_raw <- data_raw %>% 
  anti_join(
    data_raw %>% 
      filter(id_core == 1033 | id_core == 1259,
             year == 4)
  ) %>% 
  bind_rows(case_1033)

data_raw <- data_raw %>% 
  mutate(
    gender_state = recode(gender_state,
                        `F` = "Female",
                        `M` = "Male"),
    district_core = recode(district_core,
                       cpsd6 = "District 1",
                       shelton = "District 2",
                       slane = "District 3",
                       sps = "District 4"),
    school_core = recode(school_core,
                     bohemia = "School A",
                     bordeaux = "School B",
                     jewett = "School C",
                     maple = "School D",
                     mountainview = "School E",
                     mtvernon = "School F",
                     yolanda = "School G")
  ) 

write_csv(data_raw, here::here("nopublish", "data_final.csv"))

data_open <- data_raw %>% 
  select(
    id_core,
    wcpm_easycbmcore.wave1:wcpm_easycbmcore.wave4, wcpm_core.wave1:wcpm_core.wave4,
    wr_easycbmcore.wave1:wr_easycbmcore.wave4, 
    secs_easycbmcore.wave1:secs_easycbmcore.wave4, secs_core.wave1:secs_core.wave4,
    grade_core,
    readingcomp_easycbm.spring, sbac_score, sbac_prof, state, 
    gender_state, ethnicity_state, frl_state, sped_state, lep_state, district_core, school_core,
    starts_with("date.wave"), contains("npassages")
  )

write_csv(data_open, here::here("data", "data_open.csv"))

rm(case_974, case_1033)

school <- read_csv(here::here("nopublish", "school_data.csv"), skip = 6, skip_empty_rows = TRUE) %>% 
  clean_names() %>% 
  slice(1:7)

school1819 <- school %>% 
  mutate(
    district = case_when(
      str_detect(school_name, "BOHEMIA") ~ "District 3",
      str_detect(school_name, "BORDEAUX") ~ "District 2",
      str_detect(school_name, "JEWETT") ~ "District 1",
      str_detect(school_name, "MAPLE") ~ "4istrict 4",
      str_detect(school_name, "MOUNTAIN VIEW") ~ "District 2",
      str_detect(school_name, "MT VERNON") ~ "District 4",
      str_detect(school_name, "YOLANDA") ~ "District 4",
    ),
    school = case_when(
      str_detect(school_name, "BOHEMIA") ~ "School A",
      str_detect(school_name, "BORDEAUX") ~ "School B",
      str_detect(school_name, "JEWETT") ~ "School C",
      str_detect(school_name, "MAPLE") ~ "School D",
      str_detect(school_name, "MOUNTAIN VIEW") ~ "School E",
      str_detect(school_name, "MT VERNON") ~ "School F",
      str_detect(school_name, "YOLANDA") ~ "School G")
  ) %>% 
  select(school, district,
         total_students = total_students_all_grades_includes_ae_public_school_2018_19,
         gr2_students = grade_2_students_public_school_2018_19,
         gr3_students = grade_3_students_public_school_2018_19,
         gr4_students = grade_4_students_public_school_2018_19,
         locale = urban_centric_locale_public_school_2018_19,
         titlei = title_i_school_status_public_school_2018_19,
         male = male_students_public_school_2018_19,
         female = female_students_public_school_2018_19,
         studentteacherratio = pupil_teacher_ratio_public_school_2018_19,
         frl = free_and_reduced_lunch_students_public_school_2018_19,
         race_amind = american_indian_alaska_native_students_public_school_2018_19,
         race_asian = asian_or_asian_pacific_islander_students_public_school_2018_19,
         race_hispanic = hispanic_students_public_school_2018_19,
         race_black = black_students_public_school_2018_19,
         race_white = white_students_public_school_2018_19,
         race_pacisl = hawaiian_nat_pacific_isl_students_public_school_2018_19,
         race_multi = two_or_more_races_students_public_school_2018_19) %>% 
  mutate(race_black = parse_number(race_black),
         race_pacisl = parse_number(race_pacisl),
         gr234 = gr2_students + gr3_students + gr4_students,
         gr234_pct = round(gr234/total_students*100, 0),
         frl_pct = round(frl/total_students*100, 0),
         amin_pct = round(race_amind/total_students*100, 0),
         asian_pct = round(race_asian/total_students*100, 0),
         hispanic_pct = round(race_hispanic/total_students*100, 0),
         black_pct = round(race_black/total_students*100, 0),
         white_pct = round(race_white/total_students*100, 0),
         pacisl_pct = round(race_pacisl/total_students*100, 0),
         multi_pct = round(race_multi/total_students*100, 0))

rm(school)

write_csv(school1819, here::here("data", "school1819.csv"))
