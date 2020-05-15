library(grid); library(broman); library(ggplot2); library(reshape2) # Load libraries
setwd("C:/Users/qizhe/Desktop/STA 141A/final") # Set working directory

### 1
list_to_matrix = function(bin_list) {
  bin_list = as.character(bin_list)
  bin_matrix = matrix(bin_list, nrow = 10000, byrow = TRUE)
  return(bin_matrix)
} # Converts a list to a matrix

load_training_images = function(input_dir, output_file) {
  bin_data = list.files(path = input_dir, # Extract binary files
                        pattern = "data_batch_[0-9].bin", full.names = TRUE)
  bin_list = lapply(bin_data, function(x) readBin(con = x, # Read the binary data
                                                  what = "raw", n = 3073*10000))
  bin_matrices = lapply(bin_list, list_to_matrix) # Change vectors to matrices in list 
  train = do.call("rbind", bin_matrices) # Combine the list into a single data
  save(train, file = output_file) # Save to file
} # Loads the binary data and converts them to workable data (training)

load_testing_images = function(input_dir, output_file) {
  bin_data = list.files(path = input_dir, # Extract binary file
                        pattern = "test_batch.bin", full.names = TRUE)
  bin_vec = readBin(con = bin_data, what = "raw", n = 3073*10000) # Read the binary data
  bin_vec = as.character(bin_vec) # Set to character
  test = matrix(bin_vec, nrow = 10000, byrow = TRUE) # Change to matrix
  save(test, file = output_file) # Save to file
} # Loads the binary data and converts them to workable data (test)

training_images = load_training_images(input_dir = "C:/Users/qizhe/Desktop/STA 141A/final", 
                                       output_file = "C:/Users/qizhe/Desktop/STA 141A/final/training_set.rds")

testing_images = load_testing_images(input_dir = "C:/Users/qizhe/Desktop/STA 141A/final", 
                                     output_file = "C:/Users/qizhe/Desktop/STA 141A/final/test_set.rds")

load("C:/Users/qizhe/Desktop/STA 141A/final/training_set.rds") # Load the saved data
load("C:/Users/qizhe/Desktop/STA 141A/final/test_set.rds")

data_rescale <- function(labels, k = 500) {
  sort(as.vector(sapply(unique(labels), 
                        function(i) which(labels == i))[1:k, ]))
} # TA code for rescaling

train2 <- train[data_rescale(train[,1], k = 500),] # Rescale the data
test2 <- test[data_rescale(test[,1], k = 100),]

### 2
view_images = function(image_data, observation, image_labels) {
  photo_label = as.numeric(image_data[observation,1]) # Get the integer of the observation label
  photo_label = image_labels[photo_label + 1,] # Get the corresponding name of the label
  
  red = sapply(image_data[observation, 2:1025], hex2dec) # Change colors to decimal
  green = sapply(image_data[observation, 1026:2049], hex2dec)
  blue = sapply(image_data[observation, 2050:3073], hex2dec)
  
  rgb_mat = matrix(rgb(red, green, blue, # Create matrix of colors
                       maxColorValue = 255), nrow = 32, ncol = 32, byrow = T)
  grid.raster(rgb_mat) # Plot image
  return(photo_label)
} # Display the image of a particular data point

image_labels = read.delim("batches.meta.txt", header = FALSE) # Receive classes of images

### 3
# Find an example of each class within the data
view_images(train2, 105, image_labels) # 1. Frog
view_images(train2, 31, image_labels) # 2. Airplane
view_images(train2, 9, image_labels) # 3. Ship
view_images(train2, 44, image_labels) # 4. Horse
view_images(train2, 10, image_labels) # 5. Cat
view_images(train2, 499, image_labels) # 6. Automobile
view_images(train2, 109, image_labels) # 7. Bird
view_images(train2, 300, image_labels) # 8. Deer
view_images(train2, 358, image_labels) # 9. Truck
view_images(train2, 785, image_labels) # 10. Dog

color_range = function(color) {
  if (color == "red") {
    color = 2:1025
  } else if (color == "green") {
    color = 1026:2049
  } else if (color == "blue") {
    color = 2050:3073
  }
  return(color)
} # Determine the range for a specific color

class_index = function(image_labels, class_name) {
  label_index = (which(image_labels == class_name) - 1)
  return(paste0("0",label_index))
} # Return the index of the class

pixel_variance = function(color, image_data) {
  color_subset = color_range(color = color) # Determine the range for given color
  colored_data = image_data[1:nrow(image_data), color_subset] # Subset the color from the data
  colored_data_mat = matrix(hex2dec(colored_data), # Transform data into matrix
                            nrow = nrow(colored_data), ncol = ncol(colored_data), byrow = TRUE)
  colored_data_var = apply(colored_data_mat, 2, var)
  special_pixel = which(colored_data_var == max(colored_data_var))
  unspecial_pixel = which(colored_data_var == min(colored_data_var))
  pixels = c(special_pixel, unspecial_pixel)
  print("Special, then unspecial pixels:")
  return(pixels)
} # Find the pixel with greatest variance from a specific color

# Find variance for RGB within test and train data
pixel_variance(color = "red", image_data = test2)
pixel_variance(color = "green", image_data = test2)
pixel_variance(color = "blue", image_data = test2)

pixel_variance(color = "red", image_data = train2)
pixel_variance(color = "green", image_data = train2)
pixel_variance(color = "blue", image_data = train2)

### 4
top_k = function(dist_mat, k, test_data, training_data) {
  top_k_mat = label_indices = matrix(NA, nrow = nrow(dist_mat), 
                                     ncol = k) # Create dummy matrix for ordered labels
  
  for (i in 1:nrow(dist_mat)) {
    top_5 = names(head(sort(dist_mat[i,], # Retrieve the top k training observations that match the test
                            decreasing = FALSE), k))
    top_5_index = as.integer(top_5) - nrow(test_data) # Convert to format of training indices
    
    top_k_mat[i,] = top_5_index # Fill the rows of the dummy matrix
  } # Fill a dummy matrix with the top k training observations
  
  for (i in 1:nrow(label_indices)) {
    label_indices[i,] = training_data[top_k_mat[i,], 1]
  } # Determine the label of each of the top k training observations
  
  label_indices2 = as.integer(label_indices) + 1 # Convert to the image_labels indices
  label_indices2 = matrix(label_indices2, # Convert back to matrix
                          nrow = nrow(label_indices), ncol = ncol(label_indices))
  
  return(label_indices2)
} # Find the top k labels per observation from the distance matrix

vote_k = function(test_data, training_data, k_mat) {
  vote_label = rep(NA, nrow(k_mat)) # Dummy vector for vote labels
  
  for (i in 1:nrow(k_mat)) {
    vote_label[i] = sample(names(which(table(k_mat[i,]) == # Vote for the test label
                                         sort(table(k_mat[i,]), decreasing = TRUE)[1])), 1)
  } 
  vote_label = as.integer(vote_label) # Convert back to integer
  
  return(vote_label)
} # Vote for the test label, and determine the accuracy

predict_knn = function(test_data, training_data, distance_metric, k) {
  test_data2 = test_data[,-1] # Remove the label column from both test and training data
  training_data2 = training_data[,-1]
  
  test_train_mat = rbind(test_data2, training_data2) # Combine both into a single matrix
  test_train_mat2 = matrix(hex2dec(test_train_mat), # Convert to integer pixels, and keep as matrix
                           nrow = (nrow(test_data2) + nrow(training_data2)), ncol = ncol(test_data2))
  
  dist_mat = as.matrix(dist(test_train_mat2, method = distance_metric)) # Take the distance matrix of the combined matrix
  dist_mat2 = dist_mat[1:nrow(test_data2), # Subset the informative parts of the distance matrix
                       (nrow(test_data2) + 1):(nrow(test_data2) + nrow(training_data2))]
  
  k_mat = top_k(dist_mat = dist_mat2, k = k, # Discern the k nearest labels
                test_data = test_data, training_data = training_data) 
  
  k_votes = vote_k(test_data = test_data, # Vote for the test label
                   training_data = training_data, k_mat = k_mat)
  
  return(k_votes)
}

# predict_knn(test_data = test3, training_data = train3, distance_metric = "euclidean", k = 3)

### 5
### Create the entire distance matrix once for Euclidean and Manhattan distances
test2c = test2[,-1]; test2d = apply(test2c, 2, function(x) strtoi(x, 16L)) # Remove labels, set to integer
train2c = train2[,-1]; train2d = apply(train2c, 2, function(x) strtoi(x, 16L)) # Same for train data
dist_euclidean = dist(rbind(test2d, train2d)); dist_euc = as.matrix(dist_euclidean) # Create distance matrix
save(dist_euc, file = "dist_euclidean") # Save file
dist_manhattan = dist(rbind(test2d, train2d), method = "manhattan"); dist_man = as.matrix(dist_manhattan)
save(dist_man, file = "dist_manhattan") # Same for Manhattan distances

# read in dist_euc
load("dist_euclidean.rda"); load("dist_manhattan.rda"); load("test_set.rds"); load("training_set.rds")

cv_error_knn = function(train2, test2, k = 3, numOfFolds = 10, all_distance){
  n = nrow(train2)
  m = nrow(test2)
  real_labels = train2[,1] # Retrieve labels
  all_distance = as.matrix(all_distance) # Change to distance matrix
  all_distance = all_distance[-c(1:m), -c(1:m)] # Subset the correct data
  colnames(all_distance) = 1:nrow(all_distance)
  row.names(all_distance) = 1:nrow(all_distance)
  classes = sample(rep(1:10,500)) # Generate list of classes
  indexes = split(1:n, classes) # indexes[[1]] show the index of the images from training set goes to first fold
  fold_distance = lapply(1:numOfFolds, function(x) all_distance[do.call("c", indexes[-x]), indexes[[x]]]) # calculate distance of one fold vs the other folds (9 folds)
  top_ks = lapply(fold_distance, function(x) apply(x, 2, function(y) real_labels[as.numeric(names(sort(y))[1:k])])) # select the top k
  if (k == 1) {
    top_classes = lapply(top_ks, function(x) names(x)[1])
  } else {
    top_classes = lapply(top_ks, function(x) apply(x, 2, function(y) names(sort(table(y), decreasing=TRUE))[1]))
  }
  return(list(true = as.numeric(real_labels[do.call("c", indexes)]), predict = as.numeric(do.call("c", top_classes))))
}

### 6
err.euc = err.man = rep(0, 15) # Create empty vector for errors
err.euc.true = err.man.true = err.euc.predict = err.man.predict = vector("list", 15) # Empty list for pred/true

set.seed(456) # Set seed
for (i in c(1:15)) {
  out_euc = cv_error_knn(train2, test2, k = i, numOfFolds = 10, all_distance = dist_euc)
  err.euc[i] = mean(out_euc$true != out_euc$predict)
  err.euc.true[[i]] = out_euc$true
  err.euc.predict[[i]] = out_euc$predict
  
  out_man = cv_error_knn(train2, test2, k = i, numOfFolds = 10, all_distance = dist_man)
  err.man[i] = mean(out_man$true != out_man$predict)
  err.man.true[[i]] = out_man$true
  err.man.predict[[i]] = out_man$predict
} # Produce output for k = 1,...,15 and 10 folds for Euclidean and Manhattan distances

plot(1:15, err.euc, main = "Error Rates for k-NN Using CV", # Plot the error rates
     xlab= "k", ylab= "error rate", ylim=c(0.5, 1), col="red", type = "l")
points(1:15, err.euc, col = "red"); lines(1:15, err.man, col="blue")
points(1:15, err.man, col = "blue"); abline(v=c(1:15), col="grey")
legend("topright", c("Euclidean", "Manhattan"), col = c('red', 'blue'), lty=c(1,1))

### 7
# Determine the top 3 for Euclidean and Manhattan distances
order(err.euc)[1:3] # 9, 6, 7
order(err.man)[1:3] # 10, 15, 7

# Euclidean Confusion Matrix
table(data.frame(true = err.euc.true[[9]], predict = err.euc.predict[[9]]))
table(data.frame(true = err.euc.true[[6]], predict = err.euc.predict[[6]]))
table(data.frame(true = err.euc.true[[7]], predict = err.euc.predict[[7]]))
# Manhattan Confusion Matrix
table(data.frame(true = err.man.true[[10]], predict = err.man.predict[[10]]))
table(data.frame(true = err.man.true[[15]], predict = err.man.predict[[15]]))
table(data.frame(true = err.man.true[[7]], predict = err.man.predict[[7]]))

### 8
table(data.frame(true = err.man.true[[10]], predict = err.man.predict[[10]]))

# Heatmap
bestcombo.tab = table(data.frame(true = err.man.true[[10]], predict = err.man.predict[[10]]))
bestcombo.df = matrix(bestcombo.tab, ncol = 10)

image(bestcombo.df[, ncol(bestcombo.df):1], 
      xlab = "Predicted Classes", ylab = "Actual Classes", 
      main = "Heatmap of Confusion Matrix (Distance Metric = Manhattan; k = 10",
      axes = F, col = heat.colors(5))

### 9
test_error_knn = function(train2, test2, k = 10, all_distance){
  n = nrow(train2) # Get rows for test and train data
  m = nrow(test2)
  real_labels = train2[,1] # Subset true labels from train
  all_distance = as.matrix(all_distance) # Convert to distance matrix
  all_distance = all_distance[-c(1:m), c(1:m)] # Subset the test x train data
  colnames(all_distance) = 1:m # Correct column and row names
  row.names(all_distance) = 1:nrow(all_distance)
  top_k = apply(all_distance, 2, function(y) real_labels[as.numeric(names(sort(y))[1:k])]) # Find top k
  if (k == 1) {
    predict_labels = as.numeric(top_k)
  } else {
    predict_labels = apply(top_k, 2, function(x) as.numeric(names(sort(table(x), decreasing=TRUE))[1]))
  }
  return(list(true = as.numeric(test2[,1]), predict = predict_labels))
} # Find the knn error rate for the test data

euc_test_err = rep(0, 15); man_test_err = rep(0, 15) # Create empty vectors
set.seed(141)
for (i in c(1:15)) {
  predict1 = test_error_knn(train2, test2, k = i, dist_euc)
  euc_test_err[i] = mean(predict1$true != predict1$predict)
  
  predict1 = test_error_knn(train2, test2, k = i, dist_man)
  man_test_err[i] = mean(predict1$true != predict1$predict)
} # Retrieve the error rates for the test data

plot(1:15, err.euc, main = "Error Rates for k-NN Using CV", # Plot the error rates
     xlab= "k", ylab= "error rate", ylim=c(0.5, 1), col="red", type = "l", lty = 2)
points(1:15, err.euc, col = "red"); lines(1:15, err.man, col="blue", lty = 2)
points(1:15, err.man, col = "blue"); abline(v=c(1:15), col="grey")
lines(1:15, euc_test_err, col = "red"); lines(1:15, man_test_err, col = "blue")
legend("topright", c("Euclidean (train)", "Manhattan (train)", "Euclidean (test)", "Manhattan (test)"), 
       col = c('red', 'blue', 'red', 'blue'), lty=c(2,2,1,1))

order(euc_test_err)[1:3] # 10, 1, 8
order(man_test_err)[1:3] # 8, 5, 6
