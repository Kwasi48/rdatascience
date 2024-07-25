sin(pi / 2)

primes <- c(2,3,5,7,11,13)
print(primes)
primes
fligths %>% filter(dest == "IAH") %>%  group_by(year, month, day) %>% summarize( arr_delay = mean(arr_delay, na.rm = TRUE))
flights %>% filter(dest == "IAH") %>%  group_by(year, month, day) %>% summarize( arr_delay = mean(arr_delay, na.rm = TRUE))
flights %>% filter(month == 1 & day == 1)
flights %>% filter(month %in% c(1, 2))
view(flights %>% filter(month %in% c(1, 2)))

flights %>%  arrange(desc(dep_delay))
flights %>%  arrange(desc(month))

flights %>%  distinct()

flights %>% distinct(origin, dest)

flights %>%  distinct(origin, dest, .keep_all = TRUE)

flights %>%  count(origin, dest, sort = TRUE)

#Excercise
esex <- flights %>% filter(dep_delay >= 120) %>%  filter(dest  == "IAH" | dest == "HOU") %>%  filter( carrier == 'UA' | carrier == 'AA' | carrier == "DL")  %>% filter(month == 7 | month == 8 | month == 9)
esex1 <- esex %>% filter(arr_delay > 2 ) %>%  filter(sched_dep_time >= dep_time) 
esex1
#esex2 <- esex1 %>%  filter(dep_delay == -1)
#esex2

flights %>% arrange(desc(distance))
flights %>% arrange(distance)
dayss <- flights %>% distinct(day)
view(dayss)

