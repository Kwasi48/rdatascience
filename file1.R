sin(pi / 2)

primes <- c(2,3,5,7,11,13)
print(primes)
primes
fligths %>% filter(dest == "IAH") %>%  group_by(year, month, day) %>% summarize( arr_delay = mean(arr_delay, na.rm = TRUE))
flights %>% filter(dest == "IAH") %>%  group_by(year, month, day) %>% summarize( arr_delay = mean(arr_delay, na.rm = TRUE))
flights %>% filter(month == 1 & day == 1)
flights %>% filter(month %in% c(1, 2))
view(flights %>% filter(month %in% c(1, 2)))