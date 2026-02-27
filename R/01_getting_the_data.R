library(tidyverse)
library(readr)
library(pbapply)

base_url <- "https://storage.googleapis.com/ad-manager-political-ads-dump/political"
years    <- 2018:2025
data_dir <- "data"

dir.create(data_dir, showWarnings = FALSE)

for (year in years) {
  cat(sprintf("\n[%s] Downloading...\n", year))
  
  zip_url  <- sprintf("%s/%s/PoliticalAds.zip", base_url, year)
  zip_path <- file.path(data_dir, sprintf("%s_PoliticalAds.zip", year))
  out_dir  <- file.path(data_dir, as.character(year))
  
  # Download
  tryCatch({
    download.file(zip_url, destfile = zip_path, mode = "wb", quiet = FALSE)
    cat(sprintf("[%s] Unzipping into %s/\n", year, out_dir))
    dir.create(out_dir, showWarnings = FALSE)
    unzip(zip_path, exdir = out_dir)
    file.remove(zip_path)   # clean up the zip after extraction
    cat(sprintf("[%s] Done.\n", year))
  }, error = function(e) {
    cat(sprintf("[%s] ERROR: %s\n", year, e$message))
  })
}

csv_files <- file.path("data", years, "PoliticalAds.csv")

data <- lapply(csv_files, function(f) {
  yr <- as.integer(basename(dirname(f)))
  cat(sprintf("Reading %s...\n", f))
  read_csv(f, col_types = cols(.default = col_character())) |>
    mutate(year = yr, .before = 1)
}) |>
  bind_rows()

data <- data %>%
  filter((CountryCode=="united states") & !is.na(CreativeUrl) & grepl("mediaType=png",CreativeUrl))
data$id <- 1:nrow(data)
image_dir <- "images"
dir.create(image_dir, showWarnings = FALSE)


base_url <- "https://storage.googleapis.com/ad-manager-political-ads-dump-shadow/"

download_image <- function(link, id) {
  link <- gsub("https://www.snap.com/political-ads/asset/", "", link)
  link <- gsub("\\?mediaType=png", "", link)
  link <- paste0(base_url, link, ".png")
  path <- paste0(image_dir, "/", id, ".png")
  Sys.sleep(0.1)
  download.file(link, destfile = path, mode = "wb", quiet = FALSE)
}

for (i in 1:nrow(data)) {
  tryCatch({
    download_image(data$CreativeUrl[i], i)
  }, error = function(e) {
    message("Error on row ", i, ": ", e$message)
  })
}


library(googleCloudVisionR)
library(googleAuthR)

gar_auth_service("cloud_vision_key.json")

data$text <- NA
library(progress)

pb <- progress_bar$new(
  format = "[:bar] :current/:total eta: :eta",
  total = nrow(data)
)

for (i in 1:nrow(data)) {
  
  tryCatch({
    path <- paste0(image_dir, "/", i, ".png")
    result <- gcv_get_image_annotations(
      path, 
      feature = "DOCUMENT_TEXT_DETECTION"
    )
    data$text[i] <- paste(result$description, collapse = " ")
  }, error = function(e) {
    message("Error on row ", i, ": ", e$message)
    data$text[i] <<- NA
  })
}

write.csv(data,"data.csv",row.names = F)


data <- read.csv("data.csv")
set.seed(42)
data_sample <- data[!is.na(data$text),]
data_sample <- data_sample[sample(nrow(data_sample), 200), ]
write.csv(data_sample,"data_sample.csv",row.names=F)


# Create output folder
dir.create("training_images", showWarnings = FALSE)
ids <- data_sample$id
copied <- 0
for (id in ids) {
  src  <- paste0("images/", id, ".png")
  dst  <- paste0("training_images/", id, ".png")
  if (file.exists(src)) {
    file.copy(src, dst)
    copied <- copied + 1
  }

}
