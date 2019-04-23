require(jsonlite)
library(tidyverse)

## SEASON 1

url = 'https://storage.googleapis.com/kaggle-datasets/13668/18443/season1.json?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1556306533&Signature=NC4hUMCuyLv4J9dk9JR7lTZDCgTL9UraL0BA8OPoUbKJWwjU14cfFCB4h3BdUJkSbngROdTYGWDfItNyKrBTrOpPt7hfVCMPp3PiRtFje%2BHfe%2F0ZGaycpzH4ASPn6tdbeJULF933OoVdbi%2B5dJzHyeYl%2FOPun5ZgfcTVs5VvD0JmepjhZOm0noBz8i%2BYPHjoUA2RggCg%2FTq3HNVgRjk2J3COZHrSW%2Bxus1F7ieXd0tgDoFRM0fzHQ3hThjz70PM7sjNGCwcA3UJn%2B6s9CdylBQUMBipQXw3h8Ong9lpl8kBDvaQU9kFkbvmmUWOCkd26%2FXArhpMJ6qKyXw2VvRxXbg%3D%3D'

s1 = fromJSON(url)

list_of_data = list()

for(i in 1:length(s1)){
  holder = tibble(episode = names(s1[i]), number = names(s1[[i]]), text = NA)
  for(j in 1:nrow(holder)){
    holder$text[j]=s1[[i]][[j]]
  }
  list_of_data[[i]]=holder
}

require(data.table)
df1 = rbindlist(list_of_data) %>% as_tibble()


## SEASON 2
url = 'https://storage.googleapis.com/kaggle-datasets/13668/18443/season2.json?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1556308489&Signature=ose2M0i%2FJbjDuriKzkmp657fS0V%2FfuKAp0%2FR%2FiTZ%2BcE%2BI%2BF4dsDN2wweoCz%2Fd4xm0t%2BKmNatuNsmqwl0QP7KspYuUXjMofZcPcHEjNJ8tZHErVLKhFK3z3Eh0O1mAqkKAo7W2lPGmaueUsI47AdAkOAla%2BYR1qZ2zZgjv2CmBNRzMuYcys1d94hCXINZqDzFUw8q9BjGnpyZqfvyh0Cuq69lcN%2Bo9et8XDvf0KLn%2BeQMjt8emnvsz1uM3DnkgoDGdlSF53WNBAMU7KfVtLUuvGRwSyS98pOjuA9f373hZLgr8I7ikK3HtpA4w%2BFfK9AY5YhtjmcEElMgD1N%2F908ISg%3D%3D'

s2 = fromJSON(url)

list_of_data = list()

for(i in 1:length(s2)){
  holder = tibble(episode = names(s2[i]), number = names(s2[[i]]), text = NA)
  for(j in 1:nrow(holder)){
    holder$text[j]=s2[[i]][[j]]
  }
  list_of_data[[i]]=holder
}

require(data.table)
df2 = rbindlist(list_of_data) %>% as_tibble()


## SEASON 3

url = 'https://storage.googleapis.com/kaggle-datasets/13668/18443/season3.json?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1556309226&Signature=I6QR0TNX%2FkFUnfaN4n3GHrMzq65wI%2BL77wDIj%2FM99%2FIdflVeLRqkcw1wP9ZMVIo89w8I%2BzKCKIIgXURmEKuNSKSwx4fVllI06ZCxzpaAEYuUnmq0ciBAE6pxUBxBtt4eXoAZLj%2BDhJDsD21%2BAjiJhRC1X24BonpnCfFcTBXw4F%2F5JFuQ%2B%2B9dFIW4GE%2BdzJjsTDS8thIMc5vAokTSiVGdrFdL3Dzlbdyk%2Bz5OfNpbcnxCGJxpz21SD3emtioGrID4ZCLgD91eWcvbFd%2BxUKjZYGdAeqXggR1mGa29FbouZQ3Fg5qFizL8mgJpdjuCm%2Bg4Qjtg0%2BrsyDByGLucc366Uw%3D%3D'

s3 = fromJSON(url)

list_of_data = list()

for(i in 1:length(s3)){
  holder = tibble(episode = names(s3[i]), number = names(s3[[i]]), text = NA)
  for(j in 1:nrow(holder)){
    holder$text[j]=s3[[i]][[j]]
  }
  list_of_data[[i]]=holder
}

require(data.table)
df3 = rbindlist(list_of_data)%>% as_tibble()

## SEASON 4

url = 'https://storage.googleapis.com/kaggle-datasets/13668/18443/season4.json?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1556309294&Signature=gaU4DRIj50odIXKHVfZTYgfn97SXwZKT0TPgwe3I4t40zuoRXHD%2FDZ8uAtxyjOUYKLH4DzRDN6pIF%2B8xOaEVUI75jPO850%2BzuZLxNg1UHT7A%2BbztSSmbreQG02mkwBCfv7JTGEdMnWfBY632T81KP9KPf%2B56Oq9FAvTgTV%2FJjSnZgzVER5iVLJodYGyLYCRb7QV%2BBizXQfJf2S1GZIladrpuSyB1XcXJPF3AjPQB94c6w6e%2FcKgq2gEzuQicE8o0Fl68tT4ijLvrZ1hPB6DOZzksmru2qKy78hIetYI6f1PASUrtyxzXJccnuuv%2FmLLatFfE3G2tWq9%2F277YnXYd6Q%3D%3D'

s4 = fromJSON(url)

list_of_data = list()

for(i in 1:length(s4)){
  holder = tibble(episode = names(s4[i]), number = names(s4[[i]]), text = NA)
  for(j in 1:nrow(holder)){
    holder$text[j]=s4[[i]][[j]]
  }
  list_of_data[[i]]=holder
}
require(data.table)
df4 = rbindlist(list_of_data)%>% as_tibble()

## SEASON 5

url = 'https://storage.googleapis.com/kaggle-datasets/13668/18443/season5.json?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1556309373&Signature=FhMwlAvVi95HVu%2B2loLb3ZUKkwQV8%2FPtQfGYNvGX6hEOh22tSSHySdd1S5MWpc%2B6%2Fk4PDD5fgZU8AdupEsl6LtJ8a7dwrARe3dkHstKVrzwg%2Bzub931IdghDXqJn5VVHqnjCqxbcVHP9POm9R4SiLdFFsqk9OQAgzcY1RiBnP%2BbQuB37%2FddOtSoLHnKWW2MGUz2haci8qZvYKMvMt1RmND2PnTC4kQeiZlXQ3ONuFWt0lTz5jVjy1jqmjoQJI63fK6HiRL6AFuH2z3L6MsWch72UM40MuKYsAxaN9Cku9kNeA6tikKFLLJAiYiTQ7xY7Kltj7KbCu6Z0Zo3gIXlXnA%3D%3D'

s5 = fromJSON(url)

list_of_data = list()

for(i in 1:length(s5)){
  holder = tibble(episode = names(s5[i]), number = names(s5[[i]]), text = NA)
  for(j in 1:nrow(holder)){
    holder$text[j]=s5[[i]][[j]]
  }
  list_of_data[[i]]=holder
}
require(data.table)
df5 = rbindlist(list_of_data)%>% as_tibble()

## SEASON 6

url = 'https://storage.googleapis.com/kaggle-datasets/13668/18443/season6.json?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1556309426&Signature=AHgAeZm6akrvfQQw3WmPSNeYrJR6F9SEE1eVYCfUoTDG8h3kus43q%2FnW9p%2B1CevisIa09wnoUQWSLNbS5iyj90WLa5ZXQ0Po5zE69YH3fuyvPMf1lyjAy51y2VyFOqLji%2FBD4pvqewszWbpRgSl1TlgKzmwONIC5HHufbcgair9HYSxDqsNm8R%2Bac678o3EhtyGLRMh01ONZFVbrkJ0CSwUBLR%2BFudyR0yM8bC%2BiC%2FYqro9dLfBxPn59mYRy2LmIk6pVpCzbeuUoj%2BXbWUftwllYmKkAKe8o5bqVhewsjdHVGtsrDcQb8qpXQDo4HAy7m0rHAaX6dwL0x3Y7XWT1VQ%3D%3D'

s6 = fromJSON(url)

list_of_data = list()

for(i in 1:length(s6)){
  holder = tibble(episode = names(s6[i]), number = names(s6[[i]]), text = NA)
  for(j in 1:nrow(holder)){
    holder$text[j]=s6[[i]][[j]]
  }
  list_of_data[[i]]=holder
}
require(data.table)
df6 = rbindlist(list_of_data)%>% as_tibble()

## SEASON 7

url = 'https://storage.googleapis.com/kaggle-datasets/13668/18443/season7.json?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1556309485&Signature=JxOdSJYkVE9xtOrjktxPdjKcbfGlTXsdjBPZt%2BEjkrnM17GWsuY%2BQvJqxOk%2BNOaYon6VGF9vFZ15BhPbsRo0R4HxqxtcRzVjhjfmbjzN3v4qLerpxvsyeSxn91HHcDC9%2FBZz7ZFKu1%2F5rVfW6ld2zDJSyh9YxCiZR9B3tSj3ISzbDFSSiNMoVWH2FfMzImZkwiuNWLBgPx%2FZUMLLRU%2Bq5Kt0yFQ3Bel6TCSbTnCMEQyygl4Ore0YZN00S8wq3e4qZ0kziHzpa5Wp2khUileFQU83ZA4tyP4HC20mdPY%2Bs31fIhF%2BXHKrO9IpiuN9OA7Bc0R9dmXcOpcsXEDqUbEHXw%3D%3D'

s7 = fromJSON(url)

list_of_data = list()

for(i in 1:length(s7)){
  holder = tibble(episode = names(s7[i]), number = names(s7[[i]]), text = NA)
  for(j in 1:nrow(holder)){
    holder$text[j]=s7[[i]][[j]]
  }
  list_of_data[[i]]=holder
}
require(data.table)
df7 = rbindlist(list_of_data)%>% as_tibble()

data = bind_rows(df1, df2, df3, df4, df5, df6, df7)
