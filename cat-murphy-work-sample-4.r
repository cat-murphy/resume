{r read_csv}
rail_recs <- read_csv("all_safety_recs_rail.csv")

data cleaning

{r clean_rail_recs}
clean_rail_recs <- rail_recs |>
  clean_names() |>
  mutate(
    abstract = str_to_upper(abstract),
    recommendation = str_to_upper(recommendation),
    addressee_details = str_to_upper(addressee_details),
    city = str_to_upper(city),
    state = str_to_upper(state),
    overall_status = str_to_upper(overall_status),
    sr_coding_mode = str_to_upper(sr_coding_mode),
    incident_date=as.Date(as.character(as.POSIXct(event_date))),
    rec_date=as.Date(as.character(as.POSIXct(date_issued))),
    date_closed=as.Date(as.character(as.POSIXct(overall_date_closed)))
  ) |>
  filter(
    recommendation_mode == "Railroad"
  )

clean_rail_recs <- clean_rail_recs |>
  mutate(
    rec_status = case_when(
        str_detect(overall_status, "CLOSED") ~ "CLOSED",
        str_detect(overall_status, "OPEN") ~ "OPEN"
    ),
    action_status = case_when(
        str_detect(overall_status, "UNACCEPTABLE") ~ "UNACCEPTABLE",
        str_detect(overall_status, "ACCEPTABLE ACTION") ~ "ACCEPTABLE",
        str_detect(overall_status, "ACCEPTABLE ALTERNATE") ~ "ACCEPTABLE ALTERNATE",
        str_detect(overall_status, "ACCEPTABLE RESPONSE") ~ "ACCEPTABLE",
        str_detect(overall_status, "RECONSIDERED") ~ "RECONSIDERED",
        str_detect(overall_status, "NO LONGER APPLICABLE") ~ "NO LONGER APPLICABLE",
        str_detect(overall_status, "AWAIT RESPONSE") ~ "AWAIT RESPONSE",
        str_detect(overall_status, "INITIAL RESPONSE RECEIVED") ~ "INITIAL RESPONSE RECEIVED",
        str_detect(overall_status, "SUPERSEDED") ~ "SUPERSEDED",
        str_detect(overall_status, "EXCEEDS RECOMMENDED ACTION") ~ "EXCEEDS RECOMMENDED ACTION"
    )
  ) |>
  select(-event_date, -date_issued, -overall_date_closed, -overall_status)



closed rail recommendations

{r closed_rail_recs }
closed_rail_recs <- clean_rail_recs |>
  filter(
    rec_status=="CLOSED"
  ) |>
  mutate(
    rec_open = date_closed - rec_date
  )

closed_rail_recs <- closed_rail_recs |>
  mutate(
    rec_open = as.double(rec_open)
  )

closed_rail_recs |>
  group_by(action_status) |>
  summarise(count=n()) |>
  mutate(
    pct = count/sum(count)*100
  ) |>
  arrange(desc(count))

mean(closed_rail_recs$rec_open)
# 1460.057 = 4.0 years

min(closed_rail_recs$rec_open)
# 53

max(closed_rail_recs$rec_open)
# 8532 = 23.38 years

{r closed_acceptable_recs}
closed_acceptable_recs <- closed_rail_recs |>
  filter(action_status=="ACCEPTABLE")

mean(closed_acceptable_recs$rec_open)
# 1295.759 = 3.55 years

min(closed_acceptable_recs$rec_open)
# 53

max(closed_acceptable_recs$rec_open)
# 8532 = 23.38 years

{r}
closed_class_i_recs_mean_open <- data.frame(
  entity = c("multiple_recipients", "bsnf", "canadian_national", "canadian_pacific", "csx", "kansas_city_southern", "norfolk_southern", "union_pacific"),
  recs_closed = c(
    nrow(closed_class_i_multiple_recipient_recs), 
    nrow(closed_bnsf_recs), 
    nrow(closed_canadian_national_recs), 
    nrow(closed_canadian_pacific_recs), 
    nrow(closed_csx_recs), 
    nrow(closed_kansas_city_southern_recs), 
    nrow(closed_norfolk_southern_recs),
    nrow(closed_union_pacific_recs)
    ),
  mean_open = c(
    mean(closed_class_i_multiple_recipient_recs$rec_open/365), 
    mean(closed_bnsf_recs$rec_open/365), 
    mean(closed_canadian_national_recs$rec_open/365), 
    mean(closed_canadian_pacific_recs$rec_open/365), 
    mean(closed_csx_recs$rec_open/365), 
    mean(closed_kansas_city_southern_recs$rec_open/365), 
    mean(closed_norfolk_southern_recs$rec_open/365),
    mean(closed_union_pacific_recs$rec_open/365)
  )
)

{r}
closed_class_i_action_status <-
  closed_class_i_multiple_recipient_recs_action_status |>
  full_join(closed_bnsf_recs_action_status, join_by(action_status)) |>
  full_join(closed_canadian_national_recs_action_status, join_by(action_status)) |>
  full_join(closed_canadian_pacific_recs_action_status, join_by(action_status)) |>
  full_join(closed_csx_recs_action_status, join_by(action_status)) |>
  full_join(closed_kansas_city_southern_recs_action_status, join_by(action_status)) |>
  full_join(closed_norfolk_southern_recs_action_status, join_by(action_status)) |>
  full_join(closed_union_pacific_recs_action_status, join_by(action_status))

closed_class_i_action_status <- closed_class_i_action_status |>
  rename(
    multiple_recipient_count=count.x, multiple_recipient_pct=pct.x,
    bnsf_count=count.y, bnsf_pct=pct.y,
    canadian_national_count=count.x.x, canadian_national_pct=pct.x.x,
    canadian_pacific_count=count.y.y, canadian_pacific_pct=pct.y.y,
    csx_count=count.x.x.x, csx_pct=pct.x.x.x,
    kansas_city_southern_count=count.y.y.y, kansas_city_southern_pct=pct.y.y.y,
    norfolk_southern_count=count.x.x.x.x, norfolk_southern_pct=pct.x.x.x.x,
    union_pacific_count=count.y.y.y.y, union_pacific_pct=pct.y.y.y.y
    )

