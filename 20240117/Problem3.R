magazza = read.table("StorageCentres.txt", header = TRUE)
plot(magazza)
attach(magazza)
# we see that size variable is correlated to both cost0 and cost. Cost0 and cost are also correlated. 
id.factor = factor(id_storage_centre)
time.factor = factor(time)
growth.factor = factor(growth)
m0 = lm(costs ~ time.factor + costs0 + time.factor:growth.factor + size + rad_less_15_city, data = magazza)
summary(m0)


