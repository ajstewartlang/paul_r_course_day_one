# First let's load the `tidyverse` set of packages
library(tidyverse) 

# Now let's read in our data and map it onto the variable `ufo_sightings` 
ufo_sightings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

# This is our first plot that also involves a little bit of initial data wranglin
plot1 <- ufo_sightings %>%
  filter(!is.na(state)) %>%
  mutate(state = str_to_upper(state)) %>%
  group_by(state) %>%
  tally() %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(state, n), y = n, fill = state)) +
  geom_col() + 
  coord_flip() +
  guides(fill = FALSE) + 
  labs(title = "Top 10 States for UFO Sightings",
       x = NULL, 
       y = NULL) +
  ylim(0, 11000) +
  theme_minimal() +
  theme(text = element_text(size = 15))

plot1

# Let's work out the top states for UFO sightings and map that top 10 onto a 
# new variable we're calling `top_states`
top_states <- ufo_sightings %>%
  filter(!is.na(state)) %>%
  group_by(state) %>%
  tally() %>%
  top_n(10) %>% 
  pull(state)

top_states

# In this next code chunk we're removing Alaska and Hawaii from our data set
tidied_ufo <- ufo_sightings %>%
  filter(country == "us") %>%
  filter(state != "ak" & state != "hi")

# Using the latitiude and longitude co-ordinates we now plot each UFO sighting
plot2 <- tidied_ufo %>%
  ggplot(aes(x = longitude, y = latitude)) + 
  geom_point(size = .5, alpha = .25) +
  theme_void() +
  coord_cartesian() +
  labs(title = "Sites of UFO Sightings in the US") +
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  theme(text = element_text(size = 15))

plot2

# Let's now focus on California and plot the top 10 UFO shapes cited there
plot3 <- tidied_ufo %>%
  filter(state == "ca") %>%
  filter(ufo_shape != "other") %>%
  filter(ufo_shape != "unknown") %>%
  group_by(ufo_shape) %>%
  tally() %>%
  top_n(10) %>%
  mutate(ufo_shape = str_to_title(ufo_shape)) %>%
  ggplot(aes(x = reorder(ufo_shape, n), y = n, fill = ufo_shape)) +
  geom_col() + 
  coord_flip() +
  guides(fill = FALSE) + 
  labs(title = "California Top 10 UFO Shapes",
       x = NULL, 
       y = NULL) +
  theme_minimal() +
  theme(text = element_text(size = 15))

plot3

# We can save our `plot3` visualisation with the following
ggsave("ufo_plot3.jpg", plot = plot1, width = 12, height = 10, bg = "white")
