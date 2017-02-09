library(dplyr)

setwd("C:/Users/miller/Desktop/maggie-work/chinese_philanthropy")
raw_data = read.csv('data.csv', stringsAsFactors = F)
x = raw_data %>% dplyr::select(Donation.Origin..Province., 
                               Donation.Destination..Province.)
colnames(x) = c('from', 'to')
x$each.amount = suppressWarnings(as.numeric(raw_data[, 23]))
x[x == ''] = 'Unknown'
foreign = c('Unknown', 'California', 'Nationwide', 'France', 
            'New York City', 'Rome', 'Inner Mongolia', 'India', 
            'Multiple', 'New York', 'Naitonwide', 'Mutiple')
x = x %>% filter(!from %in% foreign, !to %in% foreign)
x = x %>% group_by(from, to) %>% summarize(amount = sum(each.amount, na.rm = T))

# this part is for generating the dataset for Javascript 
y = x
y$flow2 = 0
y$year  = 2016
y = y[, c(5, 1, 2, 3, 4)]
colnames(y) = c('year', 'importer1', 'importer2', 'flow1', 'flow2')
for(i in 1:nrow(y)){
    arr = rev(y[i, 2:3])
    for(j in 1:nrow(y)){
        if(all(arr == y[j, 2:3])){
            y[i, 5] = y[j, 4]
        }
    }
}
y = data.frame(y)
y$parties = ''
for(i in 1:nrow(y)){
    s = sort(c(y[i, 2], y[i, 3]))
    y[i, 6] = paste(s, collapse = '')
}
y = y[!duplicated(y[, 6]), ]
write.csv(y[, 1:5], file = 'donations.csv', row.names = F)
