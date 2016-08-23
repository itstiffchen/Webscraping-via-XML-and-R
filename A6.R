# Assignment 6 
# tiffanychen

# PART 1
# GET TAGS so i can use in part 3!
# axis::nametest[predicate]/axis::nametest[predicate]/...
# make the ones that xpath doesn't work as NA

# read in the data from the webpage
library(RCurl); library(XML)
url = "http://stackoverflow.com/questions/tagged/r?sort=newest&pagesize=50"
u = getURLContent(url)
doc = htmlParse(u, asText = TRUE)

# who posted it
path = "//div[@class = 'user-details']/a/text()"
nodes = getNodeSet(doc, path)
users = sapply(nodes, xmlValue)

# this is one line
users = xpathSApply(doc, path, xmlValue)

# to deal with missing values
path = "//div[@class = 'user-details']"
nodes = getNodeSet(doc, path)
# from piazza to add NA
users = xmlSApply(nodes, function(x) xmlValue(xmlChildren(x)$a)) 

# the title of the post
path = "//div[@class = 'summary']/h3/a/text()"
titles = getNodeSet(doc, path)
alltitles = sapply(titles, xmlValue)
# this is one line
alltitles = xpathSApply(doc, path, xmlValue)

##  when it was posted,
path = "//div[@class = 'user-action-time']/span/@title"
# when = getNodeSet(doc, path)
# sapply(when, xmlValue)
dates = xpathSApply(doc, path, `[[`, 1)

## the reputation level of the poster,
path = "//div[@class = '-flair']"
# replevel = xpathSApply(doc, path, xmlValue)
nodes = getNodeSet(doc, path)
rep = xmlSApply(nodes, function(x) xmlValue(xmlChildren(x)$span))

# the current number of views for the post,
path = "//div[@class = 'views ']"
nodes = getNodeSet(doc, path)
xmlSApply(nodes, function(x) xmlValue(x))
views = xpathSApply(doc, path, xmlValue)
# to remove the word "views" 
views1 = gsub("[a-z\n\r ]", "", views)
views1
length(views1)

# the current number of answers for the post,
path = "//div[@class = 'status answered-accepted' or @class = 'status unanswered' or @class = 'status answered']/strong"
node = getNodeSet(doc, path)
ans = sapply(node, xmlValue)
# one line
ans = xpathSApply(doc, path, xmlValue)

# the vote "score" for the post,
path = "//span[@class = 'vote-count-post ']" # space!!
node = getNodeSet(doc, path)
score = sapply(node, xmlValue)
# one line
votes = xpathSApply(doc, path, xmlValue)

# the URL for the page with the post, answers and comments,
path = "//div[@class = 'summary']/h3/a/@href"
urls = xpathSApply(doc, path, `[[`, 1)
url = "http://stackoverflow.com" # cus getRelativeURL() didnt work
# so i paste it manually
urls = paste(url, urls, sep = "")
names(urls) = NULL


# the id (a number) uniquely identifying the post.
path = "//div[@class = 'question-summary']/@id"
node = getNodeSet(doc, path)
ids = xpathSApply(doc, path, `[[`, 1)
# regular ex and gsub to get just the numerical id
ids1 = gsub("[[:punct:][:alpha:]]", "", ids)
ids1

# tags
path = "//div[starts-with(@class, 'tags') ]"
node = getNodeSet(doc, path)
tags = xpathSApply(doc, path, xmlValue, trim = TRUE)
all_tags = gsub(" ","; ", tags)

# part 1
# ~3-9 small functions for each item
# 1 function for scraping a page
# 1 function for scraping all pages
# ~300 lines of code
# half an hr to scrape all pages. build url 

### SMALL functions ####
users = function(doc){
  path = "//div[@class = 'user-details']"
  nodes = getNodeSet(doc, path)
  users = xmlSApply(nodes, function(x) xmlValue(xmlChildren(x)$a))
  data.frame(user = users)
}

dates = function(doc){
  dates = xpathSApply(doc, "//div[@class = 'user-action-time']/span/@title", `[[`, 1)
  data.frame(date = dates)
}

titles = function(doc){
  alltitles = xpathSApply(doc, "//div[@class = 'summary']/h3/a/text()", xmlValue)
  data.frame(title = alltitles)
}

rep = function(doc){
  path = "//div[@class = '-flair']"
  nodes = getNodeSet(doc, path)
  replevel = xmlSApply(nodes, function(x) xmlValue(xmlChildren(x)$span))
  data.frame(reputation = replevel)
}


views = function(doc){
  views = gsub("[a-z\n\r ]", "", xpathSApply(doc, "//div[@class = 'views ']/text()", xmlValue))
  data.frame(views = views)
}


ans = function(doc){
  ans = xpathSApply(doc, path = "//div[@class = 'status answered-accepted' or @class = 'status unanswered' or @class = 'status answered']/strong", xmlValue)
  data.frame(answers = ans)
}


votes = function(doc){
  votes = xpathSApply(doc, "//span[@class = 'vote-count-post ']", xmlValue)
  data.frame(votes = votes)
}

url = function(doc){
  path = "//div[@class = 'summary']/h3/a/@href"
  urls = xpathSApply(doc, path, `[[`, 1)
  urls = paste("http://stackoverflow.com", urls, sep = "")
  data.frame(url = urls)
}

id = function(doc){
  ids1 = gsub("[[:punct:][:alpha:]]", "", xpathSApply(doc, "//div[@class = 'question-summary']/@id", `[[`, 1))
  data.frame(id = ids1)
}

tags = function(doc){
  path = "//div[starts-with(@class, 'tags') ]"
  node = getNodeSet(doc, path)
  tags = xpathSApply(doc, path, xmlValue, trim = TRUE)
  all_tags = gsub(" ","; ", tags)
  data.frame(tags = all_tags)
}

# function that does one page
# calls the SMALL functions!
one_page = function(i){
  cbind(id(i), dates(i), tags(i), 
        titles(i), url(i), views(i), 
        votes(i), ans(i), users(i), rep(i))
}

getNextURL =
  function(doc)
  {
    nxt = unique(unlist(getNodeSet(doc, "//a[@rel = 'next']/@href")))
    # getRelativeURL(nxt, url)
    paste("http://stackoverflow.com", nxt, sep = "")
  }

## function
# takes in tag in quotes and
# number of pages you want to process
stackoverflow = function(tag, n){
  
  # build the url depending on the tag
  base_url = "http://stackoverflow.com/questions/tagged/"
  url = paste(base_url, tag, sep = "")
  url = paste(url, "?sort=newest&pagesize=50", sep = "")
  u = getURLContent(url)
  doc = htmlParse(u, asText = TRUE)
  
  ans = NULL
  # from page 1 to n (specified by user)
  for(i in 1:n){
    # calls function one_page()
    onepage = one_page(doc)
    
    # since it goes onto next page
    # by calling getNextURL()
    # redo the reading in url process
    u = getNextURL(doc)
    u = getURLContent(u)
    doc = htmlParse(u, asText = TRUE)
    
    # rbind each page's results to eachother
    ans = rbind(ans, onepage)
  }
  return(ans)
}

y = stackoverflow("r", 100)

## all the pages
stackoverflow = function(tag){
  base_url = "http://stackoverflow.com/questions/tagged/"
  url = paste(base_url, tag, sep = "")
  url = paste(url, "?sort=newest&pagesize=50", sep = "")
  u = getURLContent(url)
  doc = htmlParse(u, asText = TRUE)
  
  ans = NULL
  while(TRUE){
    onepage = one_page(doc)
    # browser()
    u = getNextURL(doc)
    if(length(u) == 0)
      break
    u = getURLContent(u)
    doc = htmlParse(u, asText = TRUE)
    
    ans = rbind(ans, onepage)
  }
  return(ans)
}

# ====================================================
## small things we need to take care of!!!! ## 
# deleted users, community wiki posts, using contains() for "status" "views"

# if user is deleted
# just set reputation as NA
library(curl)
library(xml2)

xpath = "//div[@class = 'user-details'"
url = "http://stackoverflow.com/questions/tagged/"
html = read_html(url)
user_details = xml_find_all(html, xpath)

# xml_length() 
n_child = sapply(user_details, xml_length)
n_child == 6

## use starts with views  "views hot" 
# to deal with views that are high like 13k 
# and status/number of answers changing class
# theres 2 usernames per person

# ====================================================
# PART 2
lapply(y$url, stackoverflow("r", 10))

url = "http://stackoverflow.com/questions/34162997/how-to-add-tool-tip-and-animations-in-bubbles-chart-in-r-shiny"
u = getURLContent(url)
doc = htmlParse(u, asText = TRUE)

# user
users = function(doc){
  path = "//div[@class = 'user-details']"
  nodes = getNodeSet(doc, path)
  users = xmlSApply(nodes, function(x) xmlValue(xmlChildren(x)$a))
  data.frame(user = users)
}

# userid
usersid = function(doc){
  path = "//div[@class = 'user-details']/a"
  nodes = getNodeSet(doc, path)
  usersid = xmlSApply(nodes, function(x) xmlValue(xmlChildren(x)$href))
  data.frame(userid = usersid)
}

# date
date = function(doc){
  path = "//p[@class = 'label-key']/@title"
  dates = xpathSApply(doc, path, `[[`, 1)
  data.frame(date = dates)
}

# users rep
rep = function(doc){
  path = "//div[@class = '-flair']"
  nodes = getNodeSet(doc, path)
  rep = xmlSApply(nodes, function(x) xmlValue(xmlChildren(x)$span))
  data.frame(rep = rep)
}


# the score/votes for this entry
votes = function(doc){
  votes = xpathSApply(doc, "//span[@itemprop = 'upvoteCount']", xmlValue)
  data.frame(votes = votes)
}

# the HTML content as a string for this entry
doc = htmlParse(u, asText = TRUE)

# the identifier of the "parent" entry, 
# i.e., which entry this is a response to - a comment to an answer, or an answer to a post,


# the unique identifier for the overall post.
postid = function(doc){
  path = "//h1[@itemprop = 'name']/a/@href"
  id = xpathSApply(doc, path, `[[`, 1)
  library(stringr)
  actualid = str_extract_all(id, "[0-9]*")
  
}

# the type of entry (post, answer, comment)

### ================================================
#### PART 3
# rQAs data
load("/Users/tiffanychen/Downloads/rQAs.rda")
rqas = rQAs
rownames(rqas)
# user: username
# userid: unique user id
# date: date
# reputation: user reputation
# score: number of votes
# text: message they posted
# type: question, comment, or answer
# parent: unique id of parent post
# id: unique id of this post
# qid: unique id of related question

# Q1
# What is the distribution of the number of questions each person answered?
# from piazza..
# try to have the x axis contain amount of answers 
# y axis the frequency of users who posted that many times
# 5 people (y) posted 2 answers (x)
answers = rqas[rqas$type == "answer", ]
count = table(answers$user)
doublecount = table(count)
plot(doublecount, main = "Distribution of Number of Q's Each Person Answered", xlab = "Amount of Answers", ylab = "Count of users who Posted x times")

# lets make it look a little better
plot(doublecount, main = "Distribution of Number of Q's Each Person Answered", xlab = "Amount of Answers", ylab = "Count of users who Posted x times",  xlim = c(1, 150), type = "l", ylim = c(0, 400))

## Q2
# What are the most common tags?
# using scraped 100 pages, with 50 posts per page
y = stackoverflow("r", 100) # use my data from part 1
tags = y$tags

# convert into character 
# since strsplit only works with character
ctags = lapply(tags, as.character)
z = unlist(ctags)

# want the sorted table
# of individual tags
# in decreasing order
tagstable = head(sort(table(unlist(strsplit(z, "; "))), decreasing = TRUE))

tagstableall = table(unlist(strsplit(z, "; ")))

## Q3
# How many questions are about ggplot?
tagstableall[grep("ggplot", rownames(tagstableall)) ]

# look at the questions
q = rqas[rqas$type == "question", ]
# text with word ggplot
text = q$text
length(unique(grep("ggplot", text))) #  956

## Q4
# How many questions involve XML, HTML or Web Scraping?
tagstableall[grep("xml|html|web-scraping|scraping", rownames(tagstableall), ignore.case = TRUE) ]

length(unique(grep("xml|html|web scraping|scraping", text)))
# 1159

## Q5
# What are the names of the R functions 
# referenced in the titles of the posts?
titles = y$title
library(stringr)
text = "func.tion() this is a reg_ex()" # testing

regex = "[A-Za-z_.]+[(]"
result = str_extract_all(titles, regex)
result
result = sapply(result, function(t) { 
  if (length(t) >0)t[1]
  else NA
})

funcs = result[!is.na(result)] # names of the R functions
substr(funcs, 1, nchar(funcs)-1)

titlecount = length(result) - sum(is.na(result))
titlecount

## Q6
# What are the names of the R functions referenced 
# in the accepted answers and comments of the posts? 
# use rQAs data

answers = rqas[rqas$type == "answer", ]
comments = rqas[rqas$type == "comment", ]

regex = "[A-Za-z_.]+[(]"
result = str_extract_all(answers$text, regex)
result
result = sapply(result, function(t) { 
  if (length(t) >0)t[1]
  else NA
})

a_funcs = result[!is.na(result)]
substr(a_funcs, 1, nchar(a_funcs)-1)


regex = "[A-Za-z_.]+[(]"
result = str_extract_all(comments$text, regex)
result
result = sapply(result, function(t) { 
  if (length(t) >0)t[1]
  else NA
})

comment_func = result[!is.na(result)]
substr(comment_func, 1, nchar(comment_func)-1)



