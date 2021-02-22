library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(stringr)

incarceration_trends <- read.csv("c:\\Users\\tommy\\project\\incarceration-trends\\incarceration_trends.csv")

# Step 1
white_and_poc_all_years <- incarceration_trends %>%
  group_by(year) %>%
  summarize(total_white_jail_pop = sum(white_jail_pop, na.rm=TRUE),
            total_aapi_jail_pop = sum(aapi_jail_pop, na.rm=TRUE),
            total_black_jail_pop = sum(black_jail_pop, na.rm=TRUE),
            total_latinx_jail_pop = sum(latinx_jail_pop, na.rm=TRUE),
            total_native_jail_pop = sum(native_jail_pop, na.rm=TRUE),
            total_other_race_jail_pop = sum(other_race_jail_pop, na.rm=TRUE)
  ) %>%
  select(year, white=total_white_jail_pop, aapi=total_aapi_jail_pop, black=total_black_jail_pop, 
       latinx=total_latinx_jail_pop, native=total_native_jail_pop, 
       other_race=total_other_race_jail_pop)  

white_and_poc_all_years_summary <- white_and_poc_all_years %>%
  gather(key=race, value=jail_pop, white, black, latinx, aapi, native, other_race) %>%
  select(year, race, jail_pop)

ggplot(data = white_and_poc_all_years_summary, aes(x=year, y=jail_pop, group=race)) +
  geom_line(aes(linetype="solid", color=race, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")

# Step 2 

white_and_poc <- incarceration_trends %>%
  filter(year >= 1985) %>%
  group_by(year) %>%
  summarize(total_white_jail_pop = sum(white_jail_pop, na.rm=TRUE),
            total_aapi_jail_pop = sum(aapi_jail_pop, na.rm=TRUE),
            total_black_jail_pop = sum(black_jail_pop, na.rm=TRUE),
            total_latinx_jail_pop = sum(latinx_jail_pop, na.rm=TRUE),
            total_native_jail_pop = sum(native_jail_pop, na.rm=TRUE),
            total_other_race_jail_pop = sum(other_race_jail_pop, na.rm=TRUE)
  ) %>%
  mutate(total_non_white_jail_pop = total_aapi_jail_pop + total_black_jail_pop +
           total_latinx_jail_pop + total_native_jail_pop + total_other_race_jail_pop) %>%
  select(year, white=total_white_jail_pop, non_white=total_non_white_jail_pop, aapi=total_aapi_jail_pop, black=total_black_jail_pop, 
         latinx=total_latinx_jail_pop, native=total_native_jail_pop, 
         other_race=total_other_race_jail_pop)
#  gather(key = race, value = jail_pop, white, non_white, aapi, black, latinx, native, other_race) %>%
#  select(year, race, jail_pop)
View(white_and_poc)

# Step 3 
white_vs_non_white_summary <- white_and_poc %>%
  gather(key=race, value=jail_pop, white, non_white) %>%
  select(year, race, jail_pop)

ggplot(data = white_vs_non_white_summary, aes(x=year, y=jail_pop, group=race)) +
  geom_line(aes(linetype="solid", color=race, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")

# Step 4
white_vs_each_race_summary <- white_and_poc %>%
  gather(key=race, value=jail_pop, white, black, latinx, aapi, native, other_race) %>%
  select(year, race, jail_pop)
ggplot(data = white_vs_each_race_summary, aes(x=year, y=jail_pop, group=race)) +
  geom_line(aes(linetype="solid", color=race, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")

# Step 5

# This shows the top 5 states that has the highest latinx incarceration rate since 1985, 
# it consistently shows FL, NY, AZ, NJ, MA
state_latinx <- incarceration_trends %>%
  filter(year >= 1985) %>%
  group_by(year, state) %>%
  summarize(total_latinx_jail_pop = sum(latinx_jail_pop, na.rm=TRUE)) %>%
  select(year, state, total_latinx_jail_pop) %>%
  arrange(-total_latinx_jail_pop) %>%
  slice(1:5)
ggplot(data = state_latinx, aes(x=year, y=total_latinx_jail_pop, group=state)) +
  geom_line(aes(linetype="solid", color=state, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")

# Step 6
# This shows the top 5 states that has the highest black incarceration rate since 1985, 
state_black <- incarceration_trends %>%
  filter(year >= 1985) %>%
  group_by(year, state) %>%
  summarize(total_black_jail_pop = sum(black_jail_pop, na.rm=TRUE)) %>%
  select(year, state, total_black_jail_pop) %>%
  arrange(-total_black_jail_pop) %>%
  slice(1:5)
View(state_black)
ggplot(data = state_black, aes(x=year, y=total_black_jail_pop, group=state)) +
  geom_line(aes(linetype="solid", color=state, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")


# Step 7

# full join to find common top 5 states and which are not
state_latinx_black <- full_join(state_latinx, state_black, by=c("year", "state")) %>%
  select(year, state, latinx = total_latinx_jail_pop, black = total_black_jail_pop) %>%
  gather(key=race, value=total_jail, latinx, black) %>%
  mutate(race_in_state = paste(race, state, sep="::")) %>%
  filter(!is.na(total_jail)) %>%
  arrange(year, race_in_state) %>%
  select(year, race_in_state, total_jail)
View(state_latinx_black)
ggplot(data = state_latinx_black, aes(x=year, y=total_jail, group=race_in_state)) +
  geom_line(aes(linetype="solid", color=race_in_state, size=1)) +
  geom_point() +
  theme(legend.position = "right") +
  guides(color=guide_legend(override.aes = list(size=3))) +
  scale_color_brewer(palette = "Paired")

# TODO : following is work in progress


# Step 8 : obtain state geo long/lat definition and state abbreviation, 
#          and define centralized coordinate for each state
us_states <- map_data("state") %>%
  mutate(state = state.abb[match(str_to_title(us_states$region), state.name)])
View(us_states)

centroids <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)
centroids$state<-state.abb[match(centroids$region,tolower(state.name))]
View(centroids)


# step 9 : summarize black total jail by state since 1985
black_by_states_summary <- incarceration_trends %>%
  filter(year >= 1985) %>%
  group_by(state) %>%
  summarize(total_black_jail_pop = sum(black_jail_pop, na.rm=TRUE)
  ) %>%
  select(state, total_black_jail_pop)
black_by_states_summary$black_jail_pop_range <- cut(black_by_states_summary$total_black_jail_pop, 12)
View(black_by_states_summary)



# Step 10 : merge black_by_states_summary, and us_states by state
black_by_states_summary_merged <- merge(us_states, black_by_states_summary, by="state")

ggplot(black_by_states_summary_merged, 
       aes(x=long, y=lat, group=state, fill=black_jail_pop_range, color=black_jail_pop_range)) +
  geom_polygon(color="grey", size=0.05) + coord_equal() +
  with(centroids,
       annotate(geom="text", x=long, y=lat, label=state, size=4, color="white")
       ) +
  scale_fill_brewer(palette = "Spectral")

# step 11 : summarize latinx total jail by state since 1985
latinx_by_states_summary <- incarceration_trends %>%
  filter(year >= 1985) %>%
  group_by(state) %>%
  summarize(total_latinx_jail_pop = sum(latinx_jail_pop, na.rm=TRUE)
  ) %>%
  select(state, total_latinx_jail_pop)
latinx_by_states_summary$latinx_jail_pop_range <- cut(latinx_by_states_summary$total_latinx_jail_pop, 12)
View(latinx_by_states_summary)

# Step 12 : merge latinx_by_states_summary, and us_states by state
latinx_by_states_summary_merged <- merge(us_states, latinx_by_states_summary, by="state")

ggplot(latinx_by_states_summary_merged, 
       aes(x=long, y=lat, group=state, fill=latinx_jail_pop_range, color=latinx_jail_pop_range)) +
  geom_polygon(color="grey", size=0.05) + coord_equal() +
  with(centroids,
       annotate(geom="text", x=long, y=lat, label=state, size=4, color="white")
  ) +
  scale_fill_brewer(palette = "Spectral")



