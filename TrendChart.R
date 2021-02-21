library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)

incarceration_trends <- read.csv("c:\\Users\\tommy\\project\\incarceration-trends\\incarceration_trends.csv")



#  gather(key = race, value = jail_pop, white, non_white, aapi, black, latinx, native, other_race) %>%
#  select(year, race, jail_pop)
View(white_and_poc)

white_vs_non_white_summary <- white_and_poc %>%
  gather(key=race, value=jail_pop, white, non_white) %>%
  select(year, race, jail_pop)

ggplot(data = white_vs_non_white_summary, aes(x=year, y=jail_pop, group=race)) +
  geom_line(aes(linetype="solid", color=race, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")

white_vs_each_race_summary <- white_and_poc %>%
  gather(key=race, value=jail_pop, white, black, latinx, aapi, native, other_race) %>%
  select(year, race, jail_pop)
ggplot(data = white_vs_each_race_summary, aes(x=year, y=jail_pop, group=race)) +
  geom_line(aes(linetype="solid", color=race, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")



# This shows the top 5 states that has the highest latinx incarceration rate since 2000, 
# it consistently shows FL, NY, AZ, NJ, MA
state_latinx <- incarceration_trends %>%
  filter(year >= 2000) %>%
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

# This shows the top 5 states that has the highest black incarceration rate since 2000, 
state_black <- incarceration_trends %>%
  filter(year >= 2000) %>%
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



