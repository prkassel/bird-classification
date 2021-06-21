library(seewave)
library(tuneR)
library(caret)
library(tidyverse)
library(abind)
library(parallel)


dir <- paste(getwd(), 'data/xeno-canto-bird-recordings-extended-a-m/A-M/houwre', sep="/")
files <- list.files(path=dir)


gen_file_specs <- function(filename, size=10) {
  file <- readMP3(paste(dir, filename, sep="/"))
  file <- downsample(file, 22050)
  duration <- length(file@left)/file@samp.rate
  remainder <-  duration %% (size / 2)
  
  if (duration < size) {
    stop <- duration - remainder
  }
  else {
    stop <- duration - size - remainder
  }
  windows  <- seq(0, stop, size / 2)
  print(windows)
  process_sample <- function(start_point) {
    if (duration < size) {
      to <- duration
    }
    else {
      to <- start_point + size
    }
    clip <- cutw(file, f=file@samp.rate, from=start_point, to=to, output = "Wave")
    m <- melfcc(clip, sr=clip@samp.rate, usecmp=TRUE, minfreq = 1400,
           spec_out = TRUE, nbands=128, hoptime=size/128)$aspectrum
    m
  }


  samples <- mclapply(windows, function(window) {
    process_sample(window)
  }, mc.cores = 4)

  samples
}


process_files <- function() {
  dat <- data.frame(filenames=c(), spectrograms=c())
  
  for (file in files) {
    print(file)
    spectrograms <- gen_file_specs(file)
    filenames <- c()
    for (spec in spectrograms) {
      filenames <- c(filenames, file)
    }
    labeled_spectrograms <- cbind(filenames, spectrograms)
    dat <- rbind(dat,labeled_spectrograms)
  }
  dat
}

df_spectrograms <- process_files()
