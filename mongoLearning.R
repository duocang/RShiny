library(mongolite)
library(ggplot2)
library(jsonlite)

# crete collection with example data
dmd <- mongo("diamonds")
dmd$insert(ggplot2::diamonds)

# verify all our data was inserted
dmd$count() == nrow(ggplot2::diamonds)

# the emty query {} means: select all data
# get all records
dmd$count('{}')

# read all the data back into R
alldata <- dmd$find('{}')
print(alldata)

# to query all rows where cut == "Premium" and price < 1000 
premium_diamonds <- dmd$find('{"cut" : "Premium", "price" : { "$lt" : 1000 } }')
print(premium_diamonds)

# we can confirm that we get the same subset in R
nrow(premium_diamonds)

nrow(subset(ggplot2::diamonds, cut == "Premium" & price < 1000))


test <- dmd$find(
    query = '{"cut" : "Premium", "price" : { "$gt" : 1000 } }', 
    fields = '{"cut" : true, "clarity" : true, "price" : true}',
    limit = 5
)
print(test)

# By default mongo always includes the id field. 
# To prevent this we need to explicitly disable it:
test <- dmd$find(
    query = '{"cut" : "Premium", "price" : { "$lt" : 1000 } }', 
    fields = '{"cut" : true, "clarity" : true, "_id": false}',
    limit = 5
)
print(test)

# sort
dmd$find('{"cut" : "Premium"}', sort = '{"price": -1}', limit = 7)

# By default a collection only has an index for _id, 
# which means that selecting or sorting by any other
# field is relatively slow.
system.time(dmd$find(sort = '{"price" : 1}', limit = 1))

# By adding an index, the field gets presorted and selecting
# or sorting it is almost immediate:
dmd$index(add = '{"price" : 1}')

dmd$index(add = '{"depth" : 1, "price" : 1}')

# to remove indices from the collection
dmd$index(remove = 'depth_1_price_1')

dmd$index(remove = 'depth_1')

# perform query and return the iterator
it <- dmd$iterate('{"cut" : "Premium"}', sort = '{"price": -1}', limit = 7)

# read records from  the iterator
while(!is.null(x <- it$one())){
    cat(sprintf("Found %.2f carat diamond for $%d\n", x$carat, x$price))
}


## Manipulate data

## insert 

test <- mongo()
test$drop()
test$insert(iris)
# Mon Oct 30 16:44:06 2017 ------------------------------
test$find(limit = 5)


subjects <- mongo("subjects")
str <- c('{"name" : "jerry"}' , '{"name": "anna", "age" : 23}', '{"name": "joe"}')
subjects$insert(str)

# remove
test$count()

test$remove('{"Species" : "setosa"}')
test$count()

# use the just_one option to delete a single record
test$remove('{"Sepal_Length" : {"$lt" : 5}}', just_one = TRUE)
test$count()

test$remove('{}')
test$count()

# drop() will delete an entire collection, including all data, as well as metadata
# such as collection indices

test$drop()

## updata/ upsert
subjects$find()
subjects$update('{"name" : "jerry"}', '{"$set":{"age" : 31}}')
subjects$find()

subjects$update('{}', '{"$set":{"has_age": false}}', multiple = TRUE)
subjects$find()

# export to a file
subjects$export(stdout())
dmd$export(file("dump.json"))

# delete database and import from the file
dmd$drop()
dmd$count()

dmd$import(file("dump.json"))
dmd$count()

mydata <- jsonlite::stream_in(file("dump.json"), verbose = FALSE)
print(mydata)

jsonlite::stream_out(mtcars, file("mtcars.json"), verbose = FALSE)
mt <- mongo("mtcars")
mt$import(file("mtcars.json"))
mt$find()

# import via HTTP stream
flt <- mongo("flights")
flt$import(gzcon(curl::curl("https://jeroenooms.github.io/data/nycflights13.json.gz")))
flt$count()

flights <- jsonlite::stream_in(
    gzcon(curl::curl("https://jeroenooms.github.io/data/nycflights13.json.gz")), verbose = FALSE)
nrow(flights)