1
1+1

a <- 2
b <- 1
subtract <- function(a,b) a-b
subtract(a,b)

"<>" <- function(a,b) paste0("some_func(",a,",",b,")")
'<>'(a,b)
'<>'

add <- function(a,b) a+b
'1' <- function(a,b) a-b
reasinge_plus()
2 + 1
add(2,1)
'1'(2,1)
rm('+')

method_name <- function(x) UseMethod("method_name")
method_name.class_name <- function(x) paste0("method calls ", x)
obj <- 1
class(obj) <- "class_name"
method_name(obj)
'+.class_name' <- function(a,b) 2*a-b
class(abs)
attr(abs, "hhoho") <- "hey"
attr(abs, "hhoho")
class(function() "alo")
1 + obj

1+1
mean.my_object <- function(x, ...) sum(x, ...)
a <- 1:10
class(a) <- "my_object"
class(a)
mean(a)

class(as.name("+"))
some <- quote(abc(1,2))
some
some[[1]] <- as.name("+")
some
el <- quote(1-2)
some[[2]] <- el
some
eval(some)
Sys.getenv()
{
    tryCatch({
        print("some")
        stop("some_error")
        warning("some_warning")
        print("else")
    }, error = function(e) print("error"), warning = function(w) print(w))
    print("cover")
}

tryCatch(
  { # some code throwing an error
    stop("Error occurred")
  },
  error = function(e) print(message(e)),
  warning = function(w) print(w)
)

file.exists("some_file")

{try({
    print("HI")
stop("warn")
print("nasty")
})
print("there")}

debug(mean.my_object)
library(ggplot2)
?ggplot2
a <- ggplot() + geom_point()
class(ggplot())
class(a)
class(plot)


library(jsonlite)
fromJSON("some_folder/bc.json")
setwd("C:\\Users\\seraf\\SynologyDrive\\GTD\\Referenz Material\\UZH\\Building an R Package\\Intorduction_to_R\\")

! TRUE

readJson <- function(path) {
    if (!file.exists(path)) 
        stop(paste(
            "No file",path,"exists."
        ))
    fromJSON(path)}
readJson("some_file.json")

# Assertions
#-----------------------------------------
assert <- function(condition, message){
  if(exists("assert_off")&&assert_off)
    return("assertions off")
  if (!condition) {
    stop(message, call. = FALSE)
  }
}
rm(assert_off)
assert(FALSE, "something failed")

assert_off <- FALSE
assert(FALSE, "something failed")
# Usage
#----------------------------------------
fromJSON("some_file.json")

readJSON <- function(path) {
  assert(
    file.exists(path),
    paste0("can't find '", path, "'"))
  fromJSON(path)
}

readJSON("some_file.json")


# Object orientated
#----------------------------------------
worker <- 4
class(worker)
class(worker) <- "proletariat"
class(worker)
mean(worker)
mean.proletariat <- function(x)
  "doesn't work"
mean(worker)
"+.proletariat" <- function(a,b){
  if (sum(a,b) >= 10) "revolte" else
    sum(a,b)*0.75
}
worker + worker
worker + worker + worker

future <- function(x) UseMethod("future")
future.proletariat <- function(x) 
  "irrelevant"

future(worker)
future(4)
#---------------------------------------
str(knitr::opts_knit$get())
paste0(getwd(), "/.latex")
# Rust in R
devtools::install_github("S3r4f1n/rpackageUsingRust")
library(rpackageUsingRust)
remove.packages("rpackageUsingRust")
unloadNamespace("rpackageUsingRust")
greeting_n_times()
rPackage::hello_from_r()
rPackage::hello_world_from_not_rust()
rPackage::hello_world_from_rust()
# install.package(rextendr)

rextendr::document()
library(rextendr)
rust_sitrep()

rust_function("fn add_from_rust(a:f64, b:f64) -> f64 { a + b }")

# call it from R
add_from_rust(2.5, 4.7)

# Rust function that computes a sum of integer or double vectors, preserving the type

rust_function(
  "fn get_sum(x : Either<Integers, Doubles>) -> Either<Rint, Rfloat> {
      match x {
          Either::Left(x) => Either::Left(x.iter().sum()),
          Either::Right(x) => Either::Right(x.iter().sum()),
      }
  }",
  use_dev_extendr = TRUE,                        # Use development version of extendr from GitHub
  features = "either",                           # Enable support for Either crate
)

x <- 1:5
y <- c(1, 2, 3, 4, 5)
con <- list(x, y)
concon <- list(con, con)
sum(concon)
sum(con)
get_sum(x)
library(dplyr)

sum(x)
purrr::flatten_dbl(purrr::map(list(x, y), sum))

tibble::tibble(
  Name = c("x", "y"),
  Data = list(x, y),
  Types = purrr::map_chr(Data, typeof),
  Sum = purrr::map(Data, sum),
  SumRaw = purrr::flatten_dbl(Sum),
  ResultType = purrr::map_chr(Sum, typeof)
)

# dataframes--------------------------------
characters <- tibble(
  Name = c("Momo", "Beppo", "Gigi"),
  Age = c(12, 52, 27),
  Skill = c("Listening", "Steady Pace", "Storytelling"),
  Child = c(TRUE,FALSE,FALSE)
)

characters %>%
  group_by(Child) %>%
  summarise(mean(Age))

characters %>%
  mutate(name_length = nchar(Name)) %>%
  group_by(name_length) %>%
  summarise(paste(Skill, collapse = ", "))

characters %>%
  group_by(age_cat = cut(Age, c(0,50,100))) %>%
  summarise(Skills = paste(Skill, collapse = ", "))


#-------------------------------------------------
characters <- tibble(
  Name = c("Momo", "Beppo", "Gigi"),
  Age = c(12, 52, 27),
  Skill = c("Listening", "Steady Pace", "Storytelling"),
  Height = c(120, 180, 150),
)
ch <- characters # used for base R examples
# filter select ----------------------------------
# dplyr
characters %>%
  filter(Age < 50) %>%
  select(Name, Skill) %>%
  arrange(Name)

# base
tmp <- ch[ch$Age < 50, c("Name", "Skill")]
tmp[order(tmp$Name),]

# grouping and summarizing ----------------------------
# dplyr
characters %>%
  group_by(age_cat = cut(Age, c(0,50,100))) %>%
  summarise(avg_height = mean(Height))

# base
tapply(
  ch$Height,
  cut(ch$Age, c(0, 50, 100)),
  mean
)

# mutate -------------------------------------------
# dplyr
characters %>%
  mutate(name_length =
      if_else(Age < 50, nchar(Name), NA))

# base !?
tmp <- characters
tmp$name_length <- NA
tmp$name_length[tmp$Age < 50] <- nchar(tmp$Name)
tmp
