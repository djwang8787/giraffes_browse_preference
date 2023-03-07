pacman::p_load(dplyr, ggplot2, lubridate, reshape2, scales, Rmisc, sjstats, gridExtra)

#### Data read ####
data = read.csv('giraffe-data-complete.csv', na.strings = c("", "NA"))

data$DateTime2 = as.POSIXct(data$DateTime, format = "%d/%m/%Y %H:%M")

data.complete = data %>%
  dplyr::select(SessionID, Observer, DateTime2, Frame.Number, Focal.Name, Interval.Channel.1.Value,
         Space.Use.Coordinate.X, Space.Use.Coordinate.Y,Question..Crowd.size_31,Question..Weather_32,
         Question..Temperature_33, Question..Humidity_34, Space.Use.Coordinate.XY,
         LocationA1, LocationB1,  LocationC1, LocationD) %>%
  dplyr::rename(Temperature = Question..Temperature_33,
                Humidity = Question..Humidity_34,
                Weather = Question..Weather_32,
                Crowd = Question..Crowd.size_31,
                Coord.X = Space.Use.Coordinate.X,
                Coord.Y = Space.Use.Coordinate.Y,
                Coord.XY = Space.Use.Coordinate.XY,
                Time = DateTime2,
                Animal.ID = Focal.Name,
                Behaviour = Interval.Channel.1.Value) %>%
  filter(Time >= "2022-06-07") %>% 
  filter(!is.na(Coord.X)) %>%
  dplyr::mutate(Week = week(Time),
                Day = wday(Time, label = TRUE, abbr = FALSE),
                Day.no = yday(Time))

# Clean behaviour responses; +ve interactions into Feeding.
data.complete$Behaviour[which(data.complete$Behaviour == "Positive interactions")] <- "Feeding"

# Clean Loc. A responses
data.complete$LocationA1[which(data.complete$LocationA1 == "Acacia ")] <- "Acacia"
data.complete$LocationA1[which(data.complete$LocationA1 == "Acaica")] <- "Acacia"
data.complete$LocationA1[which(data.complete$LocationA1 == "Fishtail ")] <- "Fishtail"

# Clean Loc. B responses
data.complete$LocationB1[which(data.complete$LocationB1 == "Acacia ")] <- "Acacia"

# Clean Loc. C responses
data.complete$LocationC1[which(data.complete$LocationC1 == "Starfruit ")] <- "Starfruit"

data.complete$Weather = as.factor(data.complete$Weather)
data.complete$Behaviour = as.factor(data.complete$Behaviour)
data.complete$Animal.ID = as.factor(data.complete$Animal.ID)



#### Distance / Spatial work ####
feed_loc = read.csv("Feeding_locations.csv")
feed_loc = feed_loc %>%
  select(Space.Use.Coordinate.X, Space.Use.Coordinate.Y, Space.Use.Coordinate.XY) %>%
  dplyr::rename(Coord.X = Space.Use.Coordinate.X,
         Coord.Y = Space.Use.Coordinate.Y,
         Coord.XY = Space.Use.Coordinate.XY) 
feed.areas = data.frame(
  Area = c("Area.D", "Area.C", "Area.B", "Area.A", "Feed.P", "Water", "BOH", "View.G"))

# Defining feeding areas
feed_loc = cbind(feed_loc, feed.areas) 
Area.A = feed_loc[4,1:2]
Area.B = feed_loc[3,1:2]
Area.C = feed_loc[2,1:2]
Area.D = feed_loc[1,1:2]
Feed.P = feed_loc[5,1:2]
Water = feed_loc[6,1:2]
BOH = feed_loc[7,1:2]
View.G = feed_loc[8,1:2]

# Formula for distance calculation

euclidean <- function(a, b) sqrt(sum((a - b)^2))

# Loop to find distance to Areas A / B / C / D

for (i in 1:nrow(data.complete)) {
  data.complete$Dist.A[i] = euclidean(data.complete[i,7:8], Area.A)
  data.complete$Dist.B[i] = euclidean(data.complete[i,7:8], Area.B)
  data.complete$Dist.C[i] = euclidean(data.complete[i,7:8], Area.C)
  data.complete$Dist.D[i] = euclidean(data.complete[i,7:8], Area.D)
}



  
#### Space use #### 
ggplot(data.complete, aes(x = Coord.X, y = Coord.Y, colour = Animal.ID)) + 
  geom_point() +
  facet_grid(.~Animal.ID)

#### Activity budgets ####
data.AB = data.complete %>% 
  group_by(Animal.ID) %>%
  dplyr::summarise(total = n(),
                   Feed = sum(Behaviour %in% c("Feeding", "Positive interactions")),
                   Locomotion = sum(Behaviour %in% "Locomotion"),
                   Interactions = sum(Behaviour %in% "Negative interactions"),
                   Others = sum(Behaviour %in% "Others"),
                   OOS = sum(Behaviour %in% "OOS"),
                   Rest = sum(Behaviour %in% "Resting"),
                   Ruminate = sum(Behaviour %in% "Ruminating"),
                   Stereotypy = sum(Behaviour %in% "Stereotypy"),
                   Vigilance = sum(Behaviour %in% "Vigilance"))
  
data.AB.prop = data.AB %>%
  group_by(Animal.ID) %>%
  dplyr::summarise(Feed.prop = Feed/total,
                   Locomotion.prop = Locomotion/total,
                   Ineractions.prop = Interactions/total,
                   Others.prop = Others/total,
                   OOS.prop = OOS/total,
                   Rest.prop = Rest/total,
                   Ruminate.prop = Ruminate/total,
                   Stereotypy.prop = Stereotypy/total,
                   Vigilance.prop = Vigilance/total) %>%
  melt(id = "Animal.ID")

data.AB.prop %>%
ggplot(aes(x = variable, y = value, fill = as.factor(Animal.ID))) +
  geom_bar(stat = "identity", position = position_dodge2()) 
  
# This is not particularly interesting because they are supposed to be feeding.
# So what does this then constitute ?

#### Feed preference; BT models####
preference.data = data.complete %>% 
  filter(Behaviour == "Feeding") %>%
  rowwise() %>% 
  mutate(min = pmin(Dist.A, Dist.B, Dist.C, Dist.D),
         prefer.browse = ifelse(min == Dist.A, LocationA1, 
                             ifelse(min  ==  Dist.B, LocationB1,
                                    ifelse(min  ==  Dist.C, LocationC1, LocationD)))) %>%
  filter(!prefer.browse %in% c("Ficus", "Saltblock")) %>%
  rowwise() %>%
  mutate(available.1 = ifelse(prefer.browse == LocationA1, LocationB1, LocationA1),
         available.2 = ifelse(prefer.browse == LocationC1, LocationB1, LocationC1),
         avoid.browse = ifelse(available.1 == "Ficus", available.2, available.1))%>%
  group_by(SessionID, Animal.ID) %>%
  dplyr::mutate(total = n())

# Separated by Animal ID
adhil.bt = preference.data %>%
  filter(Animal.ID == "Adhil") %>%
  group_by(SessionID, prefer.browse) %>%
  dplyr::summarise(prefer = n(),
            avoid.browse = first(avoid.browse),
            total = first(total)) %>%
  rowwise() %>%
  dplyr::mutate(avoid = total-prefer) 
  
adhil.bt.model = BradleyTerry2::BTm(cbind(prefer, avoid), 
                     as.factor(prefer.browse), 
                     as.factor(avoid.browse),
                     data = adhil.bt,
                     id = "browse")

balaji.bt = preference.data %>%
  filter(Animal.ID == "Balaji") %>%
  group_by(SessionID, prefer.browse) %>%
  dplyr::summarise(prefer = n(),
            avoid.browse = first(avoid.browse),
            total = first(total)) %>%
  rowwise() %>%
  dplyr::mutate(avoid = total-prefer)

balaji.bt.model = BradleyTerry2::BTm(cbind(prefer, avoid), 
                                    as.factor(prefer.browse), 
                                    as.factor(avoid.browse),
                                    data = balaji.bt,
                                    id = "browse")

jubilee.bt = preference.data %>%
  filter(Animal.ID == "Jubilee") %>%
  group_by(SessionID, prefer.browse) %>%
  dplyr::summarise(prefer = n(),
            avoid.browse = first(avoid.browse),
            total = first(total)) %>%
  rowwise() %>%
  dplyr::mutate(avoid = total-prefer) 

jubilee.bt.model = BradleyTerry2::BTm(cbind(prefer, avoid), 
                                     as.factor(prefer.browse), 
                                     as.factor(avoid.browse),
                                     data = jubilee.bt,
                                     id = "browse")

marco.bt = preference.data %>%
  filter(Animal.ID == "Marco") %>%
  group_by(SessionID, prefer.browse) %>%
  dplyr::summarise(prefer = n(),
            avoid.browse = first(avoid.browse),
            total = first(total)) %>%
  rowwise() %>%
  dplyr::mutate(avoid = total-prefer)

marco.bt.model = BradleyTerry2::BTm(cbind(prefer, avoid), 
                                      as.factor(prefer.browse), 
                                      as.factor(avoid.browse),
                                      data = marco.bt,
                                      id = "browse")

bt.pref.data = preference.data %>%
  group_by(Animal.ID, SessionID, prefer.browse) %>%
  dplyr::summarise(prefer = n(),
            avoid.browse = first(avoid.browse),
            total = first(total)) %>%
  rowwise() %>%
  dplyr::mutate(avoid = total-prefer) 


overall.bt.model <- BradleyTerry2::BTm(cbind(prefer,avoid), 
                               as.factor(prefer.browse), 
                               as.factor(avoid.browse), 
                               data= bt.pref.data,
                               id = "browse")
overall.bt.model

summary(overall.bt.model)

update(overall.bt.model, refcat = "Acacia")

pref.plot = data.complete %>% 
  filter(Behaviour == "Feeding") %>%
  rowwise() %>% 
  mutate(min = min(Dist.A, Dist.B, Dist.C, Dist.D),
         prefer.browse = ifelse(min %in% Dist.A, LocationA1, 
                                ifelse(min %in% Dist.B, LocationB1,
                                       ifelse(min %in% Dist.C, LocationC1, LocationD)))) %>%
  group_by(Animal.ID, prefer.browse) %>%
  summarise(counts = n())

fig1 = pref.plot %>%
  filter(!prefer.browse %in% c("Ficus", "Saltblock")) %>%
ggplot(.) +
  geom_bar(aes(x = Animal.ID, y = counts, fill = as.factor(prefer.browse)), 
           stat = "identity",
           color = "black",
           position = position_dodge()) +
  theme_minimal() +
  ylab("Absolute consumption") +
  xlab("Browse species") +
  theme_minimal() +
  theme(legend.position = 'bottom')

make_wide_fig(fig1)


##### Preference ratios ####
ratio.data = preference.data %>%
  group_by(Week, Animal.ID, prefer.browse) %>%
  summarise(counts = n()) %>%
  group_by(Week, Animal.ID) %>%
  mutate(total = sum(counts)) %>%
  rowwise() %>%
  mutate(ratio = counts/total)
  
ratio.data$index = as.numeric(rep(c(1:2), each = 1, time = 40))
rd1 = filter(ratio.data, index == '1')
rd2 = filter(ratio.data, index == '2')

t = t.test(rd2[1:4,6], rd1[1:4,6])
t$p.value
Rmisc::CI(unlist(rd2[1:4,6]))
parameters::standard_error((unlist(rd2[1:4,6])))

for (i in 1:nrow(rd1)) {
  feed.a[i] = rd2[i,3]
  feed.b[i] = rd1[i,3]
  dmi.a[i] = mean(unlist(rd2[i:(i+3), 4]))
  dmi.a.se[i] = parameter::standard_error(unlist(rd2[i:(i+3), 4]))
  dmi.b[i] = mean(unlist(rd1[i:(i+3), 4]))
  dmi.b.se[i] = parameter::standard_error(unlist(rd1[i:(i+3), 4]))
  t.test = t.test(c(0.5, 0.5, 0.5, 0.5), unlist(rd2[i:(i+3), 6]))
  p.val = t.test$p.value
}

feed.a = c()
feed.b = c()
dmi.a = c()
dmi.a.se = c()
dmi.b = c()
dmi.b.se = c()
p.val = c()
pref.ratio = c()

for (i in 1:(nrow(rd2)/4)) {
  feed.a[i] = rd2[i*4-3,3]
  feed.b[i] = rd1[i*4-3,3]
  dmi.a[i] = mean(unlist(rd2[(i*4-3):(i*4), 4]))
  dmi.a.se[i] = parameters::standard_error(unlist(rd2[(i*4-3):(i*4), 4]))
  dmi.b[i] = mean(unlist(rd1[(i*4-3):(i*4), 4]))
  dmi.b.se[i] = parameters::standard_error(unlist(rd1[(i*4-3):(i*4), 4]))
  t.test = t.test(c(0.5, 0.5, 0.5, 0.5), unlist(rd2[(i*4-3):(i*4), 6]))
  p.val[i] = round(t.test$p.value, 2)
  pref.ratio[i] = round(rd2[i*4-3,6], 2)
}

true.dmi.a = paste(dmi.a, round(dmi.a.se, 1))
true.dmi.b = paste(dmi.b, round(dmi.b.se, 1))
data.frame = cbind(feed.a, feed.b, true.dmi.a, true.dmi.b, pref.ratio, p.val)

stargazer::stargazer(data.frame, summary = F, type = "html", out = "table1.doc",
          title = "Table X. Pair wise comparison")

# Use frame number to see the occupation of ficus between giraffes?


