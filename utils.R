library(hdf5r)

load_datasets <- function(dataset_file) {
  # Create a new HDF5 file handle in read-only mode
  h5file <- H5File$new(dataset_file, mode = "r")
  # List all top-level groups (e.g., "train", "valid", "test")
  group_names <- names(h5file)
  # Initialize an empty list to store datasets
  datasets <- list()
  # Loop over groups
  for (ds in group_names) {
    # Access the group object
    group <- h5file[[ds]]
    # Get all data set names inside this group (e.g., "x", "t", "e")
    array_names <- names(group)
    # Initialize a sub-list for this group
    group_list <- list()
    # Loop through each data set in the group
    for (array_name in array_names) {
      # Read the entire data set into memory
      group_list[[array_name]] <- group[[array_name]][] 
      # Note: [[]] accesses the HDF5 dataset; [] reads all its contents
    }
    # Store the sublist in the outer list
    datasets[[ds]] <- group_list
  }
  # Close file connections
  h5file$close_all()
  return(datasets)
}

format_dataset_to_df<- function(dataset, duration_col, event_col, trt_idx = NULL){
  xdf <- dataset$x
  if (!is.null(trt_idx)){
    colnames(xdf)[trt_idx] <- 'treat'
  }
  dt <- dataset$t[,duration_col]
  censor <- dataset$e[,event_col]
  cdf <- cbind(xdf, dt, censor)
  return(cdf)
}

standardize_x <- function(x, offset = NULL, scale = NULL) {
  # Compute means and SDs if not provided
  if (is.null(offset)) offset <- colMeans(x)
  if (is.null(scale)) scale <- apply(x, 2, sd)
  # Avoid dividing by zero
  scale[scale == 0] <- 1
  # Standardize
  x_std <- sweep(x, 2, offset, "-") #subtracts mean from values
  x_std <- sweep(x_std, 2, scale, "/") #divides by sd 
  return(list(x = x_std, offset = offset, scale = scale))
}

