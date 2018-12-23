#Homework 6
library(ggplot2)
library(raster)
###Problem 1
#The following code is a function that reads a digits file and returns a dataframe with columns that have appropriate data types.
read_digits = function(directory){
  file = read.table(directory)
}
#returns a data frame of three rows that each have 257 elements in them
test = read_digits("test.txt")
train = read_digits("train.txt")


###Problem 2
#The following code displays graphically how the pixels look like.
#It takes a data frame's row where the first element is the class label for the digits and the 256 columns after that make a 16x16 matrix
make_all_images <- function(data_frame, label){
  rows = nrow(data_frame)
  for(idx in 1:rows){
    digit = data_frame[idx,]
    class = digit[1]
    if(digit != label){next}
    image = digit[2:257]
    x = matrix(as.numeric(image), 16,16)
    image(rotate(raster(t(x))), col = paste("gray", 1:99, sep = ""))
  }#go through the rows
}

#Function for calculating the average digit using colMeans
make_average_image <- function(data_frame, label){
  rows = nrow(data_frame)
  means = colMeans(subset(data_frame,data_frame[,1]==label))
  image = means[2:257]
  x = matrix(as.numeric(image), 16,16)
  image(rotate(raster(t(x))), col = paste("gray", 1:100, sep = ""))
}

explore_variance<- function(data){
  x=as.numeric(apply(data,2,sd))
  y = sort(x, decreasing=TRUE, index.return=TRUE)
  message(y$ix[1])
  message(y$x[1])
  message(y$ix[2])
  message(y$x[2])
  message(y$ix[3])
  message(y$x[3])
  x=matrix(x[2:257],16,16)
  image(rotate(raster(t(x))), 
        col = paste("gray", 1:99, sep = ""))
}
explore_variance(train)


x = rbind(train, test)
d_m = as.matrix(dist(x, method = metric))
train_len = nrow(train) +1 #7291
test_len = nrow(test) #2007
total = nrow(train) + nrow(test)
dist_mat = d_m[7292:9298, 1:7291]

#The following code takes a train and test data and produces a distance matrix based on the metric provided
#Returns the distance matrix of the values that direclty map training and test distances (gets rid of unnecesary data)
produce_matrix <- function(train, test, metric){
  x = rbind(train, test)
  d_m = as.matrix(dist(x, method = metric))
  train_len = nrow(train) +1 #7292
  test_len = nrow(test) #2007
  total = nrow(train) + nrow(test)
  dist_mat = d_m[train_len:total, 1:nrow(train)] #Each row will be a prediction
  #dist_mat = d_m[7292:9298, 1:7291]
  return(dist_mat)
}

###Problem 3
#Input:m, train, test, model, k, distance_matrix
#Output: error estimate

#Does K_NN for every single pixel to every other single pixel and compare that to all the data sets.
predict_knn <- function(train, test, k, idx, metric,dist_mat){

  di= sort(dist_mat[idx,], index.return=TRUE)
  min.di = di$x[1:k]
  min.dix = di$ix[1:k] #column in the training set!
  labs_array = c()
  l_index = 1
  for(idx in 1:length(min.dix)){
    index = min.dix[idx]
    label = train[index, 1] #because the first column is our label
    labs_array[l_index] = as.numeric(label)
    l_index = l_index + 1
  }

  lab = sort_labels(labs_array)
  return(label)
}

#Helper Function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Takes a vector of labels and returns the kth one
sort_labels <- function(labs){
  if(is.na(labs)){
    return (NA)
  }
  if(length(labs) == 1){
    return (labs)
  }
  if(var(labs) == 0){
    return(labs[1])
  }
  lab = getmode(labs)
  
  return(lab)
}




#GARBAGE:
split_len = total/10
shuffled_indexes = sample(indexes, total) #sample all of them

g1 = shuffled_indexes[1:split_len]
start = split_len+1
end = split_len*2 
print(start)
print(end)

g2 = shuffled_indexes[start:end]
start = end+1
end = split_len*3 
print(start)
print(end)

g3=shuffled_indexes[start:end]
start = end+1
end = split_len*4 
print(start)
print(end)

g4=shuffled_indexes[start:end]
start = end+1
end = split_len*5
print(start)
print(end)

g5=shuffled_indexes[start:end]
start = end+1
end = split_len*6
print(start)
print(end)

g6=shuffled_indexes[start:end]
start = end+1
end = split_len*7
print(start)
print(end)

g7=shuffled_indexes[start:end]
start = end+1
end = split_len*8
print(start)
print(end)

g8=shuffled_indexes[start:end]
start = end+1
end = split_len*9
print(start)
print(end)

g9=shuffled_indexes[start:end]
start = end+1
end = split_len*10
print(start)
print(end)

g10=shuffled_indexes[start:end]
print(start)
print(end)






Error_array = c(0.007782101,.00389,.0155642,.019455, .0233,.01945,.0155642,.031184,.0116735,.01945525)
average_error = sum(Error_array)/10



cv_error_knn_k <- function(train, kstart, kend, metric){
  total = nrow(train)
  indexes = 1:total
  shuffled_indexes = sample(indexes, total)
  t_data<-train[sample(nrow(train)),]
  
  folds <- cut(seq(1,nrow(t_data)),breaks=10,labels=FALSE)  #Create 10 equally size folds
  k_average_array <- c()
  kidx = 1
  for(j in kstart:kend){
    
    error_array = c()
    err_idx = 1
    for(i in 1:10){
      #Loops for each fold
      testIndexes <- which(folds==i,arr.ind=TRUE)
      testData <- na.omit(t_data[testIndexes, ])
      trainData <- na.omit(t_data[-testIndexes, ])
      dist_matrix = produce_matrix(trainData, testData, metric)
      correct = 0
      wrong = 0
      for(index in 1: length(testData)){
        true_val = testData[index, 1]
        predict_val = predict_knn(trainData, testData, j, index, metric,dist_matrix)
        if(true_val == predict_val){
          correct = correct +1
        }
        else{
          wrong = wrong + 1
        }
      }
      message(error)
      error = wrong/(correct+wrong)
      error_array[err_idx] = error
      err_idx = err_idx+1
    }
    message("AVERAGE ERROR")
    
    err_average = (sum(error_array))/10
    message(err_average)
    k_average_array[kidx] = err_average
    kidx = kidx + 1
  }
  return(k_average_array)
}




#Problem ###5
#k1_arr 0.003891051 0.019455253 0.019455253 0.007782101 0.007782101 0.031128405 0.019455253 0.011673152 0.011673152 0.007782101
#k2_arr 0.00778210116731518 0.0194552529182879 0.0233463035019455 0.0272373540856031 0.0311284046692607 0.0233463035019455 0.0116731517509728 0.0272373540856031 0.0311284046692607 0.0116731517509728
#k3_arr 0.02723735 0.03112840 0.04280156 0.03891051 0.01945525 0.02723735 0.01945525 0.03891051 0.03112840 0.03891051
k1_arr = cv_error_knn(train, 1, model)
k1_average = sum(k1_arr)/10
k2_arr = cv_error_knn(train, 2, model)
k2_average = sum(k2_arr)/10
k3_arr = cv_error_knn(train, 3, model)
k3_average = sum(k3_arr)/10
k4_arr = cv_error_knn(train, 4, model)
k4_average = sum(k4_arr)/10
k5_arr = cv_error_knn(train, 5, model)
k5_average = sum(k5_arr)/10
k6_arr = cv_error_knn(train, 6, model)
k6_average = sum(k6_arr)/10
k7_arr = cv_error_knn(train, 7, model)
k7_average = sum(k7_arr)/10
k8_arr = cv_error_knn(train, 8, model)
k8_average = sum(k8_arr)/10
k9_arr = cv_error_knn(train, 9, model)
k9_average = sum(k9_arr)/10

k4 = 0.033463035019455
k5 = 0.041634241245136
k6 = 0.041634241245136
k7 = 0.056809338521401
k8 = 0.052140077821012
k9 = 0.057587548638132
k10 = 0.060311284046693
k11 = 0.062645914396887
k12 = 0.059533073929961
k13 = 0.068871595330739
k14 = 0.068093385214008
k15 = 0.071206225680934

x =  0.003891051+ 0.019455253+ 0.019455253+ 0.007782101+ 0.007782101+ 0.031128405+ 0.019455253+ 0.011673152+ 0.011673152 +0.007782101
x = x/10
y =  0.00778210116731518 + 0.0194552529182879 + 0.0233463035019455 +0.0272373540856031+ 0.0311284046692607 + 0.0233463035019455 +0.0116731517509728 +0.0272373540856031 + 0.0311284046692607 + 0.0116731517509728
y = y/10
z =0.02723735+ 0.03112840+ 0.04280156+ 0.03891051 +0.01945525 +0.02723735+ 0.01945525+ 0.03891051 +0.03112840+ 0.03891051
z = z/10
euclidean_arr = c(x,y,z,k_4_15)

k_4_15 = cv_error_knn_k(train, 4, 15, "euclidean")
k_1_15_manhattan = cv_error_knn_k(train, 1, 15, "manhattan")

m = matrix(c(euclidean_arr,k_1_15_manhattan), ncol = 15, nrow = 2, byrow = TRUE)
colnames(m) = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
barplot(m, main = "Error Estimates for K: 1-15 for 10 fold C.V.", col = c("green", "red"), beside = TRUE, xlab = "K Value", ylab = "Error Rate")
legend("topleft",c("Euclidean","Manhattan"),fill = c("green", "red"))



#0.03501946 0.04241245 0.04357977 0.05486381 0.05291829 0.05875486 0.06070039 0.06108949
#0.06070039 0.07003891 0.06692607 0.07315175


#k_1_15_manhattan= 0.02529183 0.03774319 0.03968872 0.04863813 0.06653696 0.06070039 0.06653696 0.08015564 0.06809339 0.08715953 0.09143969 0.09338521 0.09649805 0.09571984 0.10505837

###Problem 6

x = rbind(train, test)
d_m = as.matrix(dist(x, method = metric))
train_len = nrow(train) +1 #7291
test_len = nrow(test) #2007
total = nrow(train) + nrow(test)
calc_dist_mat = d_m[7292:9298, 1:7291] 

#(train, test, k, idx, metric,dist_mat
calculate_error<-function(train, test, metric){

  k_error_arr <- c()
  k_num = 1
  ###HERE add a loop for each k 1:15
  for(k in 1:15){

  correct = 0
  wrong = 0
  for(index in 1: length(test)){
    true_val = test[index, 1]
    predict_val = predict_knn(train, test,k, index, metric, calc_dist_mat) #k = 1
    if(true_val == predict_val){
      correct = correct +1
    }
    else{
      wrong = wrong + 1
    }
  } #for one k test
  error = wrong/(correct+wrong)
  message(k)
  message(error)
  k_error_arr[k_num] = error
  k_num = k_num +1
  }
  return (k_error_arr)
}


k_euclidean_error_test = calculate_error(train, test, "euclidean")
k_minkowski_error_test = calculate_error(train, test, "minkowski")

k_manhattan_error_test = calculate_error(train, test, "manhattan")

mer = matrix(c(k_euclidean_error_test,k_minkowski_error_test), ncol = 15, nrow = 2, byrow = TRUE)
ma = matrix(c(k_euclidean_error_test,k_manhattan_error_test), ncol = 15, nrow = 2, byrow = TRUE)
colnames(ma) = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
colnames(mer) = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
barplot(ma, main = "Euclidean and Manhattan Error Estimates for K: 1-15", col = c("green", "red"), beside = TRUE, xlab = "K Value", ylab = "Error Rate")
legend("topleft",c("Euclidean","Manhattan"),fill = c("green", "red"))


barplot(mer, main = "Euclidean and Minkowski Error Estimates for K: 1-15", col = c("green", "red"), beside = TRUE, xlab = "K Value", ylab = "Error Rate")
legend("topleft",c("Euclidean","Minkowski"),fill = c("green", "red"))


###Problem 6: wrong digits
#predict_knn <- function(train, test, k, idx, metric,dist_mat){

calculate_error_digit<-function(train, test, k,metric, digit){
    correct = 0
    wrong = 0
    for(index in 1: length(test)){
      true_val = test[index, 1]
      if(true_val != digit){next}
      predict_val = predict_knn(train, test,k, index, metric, calc_dist_mat) #k = 1
      if(true_val == predict_val){
        correct = correct +1
      }
      else{
        wrong = wrong + 1
      }
    } #for one k test
    error = wrong/(correct+wrong)
  return (error)
}

zero = calculate_error_digit(train, test, 3,"euclidean", 0)
one = calculate_error_digit(train, test, 3,"euclidean", 1)
two = calculate_error_digit(train, test, 3,"euclidean", 2)
three = calculate_error_digit(train, test, 3,"euclidean", 3)
four = calculate_error_digit(train, test, 3,"euclidean", 4)
five = calculate_error_digit(train, test, 3,"euclidean", 5)
six = calculate_error_digit(train, test, 3,"euclidean", 6)
seven = calculate_error_digit(train, test, 3,"euclidean", 7)
eight = calculate_error_digit(train, test, 3,"euclidean", 8)
nine = calculate_error_digit(train, test, 3,"euclidean", 9)


plot(k_1_15_manhattan, type = "l",col = "blue")
lines(k_minkowski_error_test, type = "l",col = "red")


###References https://www.tutorialspoint.com/r/r_mean_median_mode.htm
###https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
