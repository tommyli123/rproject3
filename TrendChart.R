library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)

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

us_states <- map_data("state")
View(us_states)


p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group, fill=region))
p + geom_polygon(color = "gray90", size=0.1)



