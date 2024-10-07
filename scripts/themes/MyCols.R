## Color for missing data ----
na_color <- "#E2A2BE"

## 2-color palettes ----
gold_gray <- c("#E69F00", "#999999") # original from Mirco (Three Major Axes paper)
orange_teal <- c("#DC421E", "#20CAC1") # colorblind-friendly with some contrast in grayscale
orange_teal_light <- c("#F15620", "#2CDDD7") # lighter variant
orange_teal_dark <- c("#D1400D", "#1EB8B3") # darker/desaturated variant
red_cyan <- c("#A00000", "#00A0A0") # more contrast
gray_scale <- c("#000000", "#B3B3B3") # grayscale max contrast

## 3-color palettes ----
Three_colorblind_dark2   <- c("#4a132e", "#483101", "#0c4d4b") # -90p 2x brightness in GIMP
Three_colorblind_dark1   <- c("#731d47", "#704c01", "#137774") # -90p brightness in GIMP
Three_colorblind         <- c("#B22D6E", "#AD7602", "#1EB8B3") # original
Three_colorblind_light1  <- c("#cd77a1", "#caa75c", "#6ed1ce") # +90p brightness in GIMP
Three_colorblind_light2  <- c("#dfa7c2", "#ddc696", "#a1e1df") # +90p x2 brightness in GIMP
Three_colorblind_light3  <- c("#eac6d8", "#e9dabb", "#c2ecea") # +90p x3 brightness in GIMP
Three_colorblind_light4  <- c("#f1dae6", "#f1e7d3", "#d8f3f1") # +90p x4 brightness in GIMP

Three_colorblind2_dark2  <- c("#4a1313", "#0c2e4d", "#3b4801") # -90p 2x brightness in GIMP
Three_colorblind2_dark1  <- c("#731e1d", "#134877", "#5c7001") # -90p brightness in GIMP
Three_colorblind2        <- c("#B22E2D", "#1E70B8", "#8FAD02") # original
Three_colorblind2_light1 <- c("#cd7877", "#6ea3d1", "#b7ca5c") # +90p brightness in GIMP
Three_colorblind2_light2 <- c("#dfa8a7", "#a1c4e1", "#d1dd96") # +90p x2 brightness in GIMP

Three_colors3_dark2      <- c("#305019", "#65240e", "#19173a") # -90p 2x brightness in GIMP
Three_colors3_dark1      <- c("#4a7c27", "#9c3815", "#27235a") # -90p brightness in GIMP
Three_colors3            <- c("#73C03C", "#F15620", "#3C368C") # original
Three_colors3_light1     <- c("#a5d681", "#f6926f", "#817db5") # +90p brightness in GIMP
Three_colors3_light2     <- c("#c5e5ae", "#f9b9a2", "#aeabcf") # +90p x2 brightness in GIMP

## 4-color palettes ----
Four_colorblind_dark2  <- c("#4d0c0f", "#0c4d4b", "#2e4d0c", "#2b0c4d") # -90p 2x brightness in GIMP
Four_colorblind_dark1  <- c("#771317", "#137774", "#487713", "#421377") # -90p brightness in GIMP
Four_colorblind        <- c("#b81e23", "#1eb8b3", "#70b81e", "#661eb8") # original
Four_colorblind_light1 <- c("#d16e71", "#6ed1ce", "#a3d16e", "#9c6ed1") # +90p brightness in GIMP
Four_colorblind_light2 <- c("#e1a1a3", "#a1e1df", "#c4e1a1", "#bfa1e1") # +90p x2 brightness in GIMP

## 5-color palettes ----
Five_colorblind <- c("#b77508", "#e5ab16", "#1eb8b3", "#a354d1", "#6a2a99")
Five_colorblind2 <- c("#085578", "#538085", "#e3baaa", "#e47e8c", "#ffaa6a")

## 6-color palettes ----
Six_colorblind <- c("#085578", "#538085", "#faf1e2", "#e3baaa", "#e47e8c", "#ffaa6a")

## 7-color palettes ----
Seven_sequential <- c("#f94144", "#f3722c", "#f8961e", "#f9c74f", "#90be6d", "#43aa8b", "#577590")
Seven_sequential2a <- c("#264653", "#287271", "#2a9d8f", "#8ab17d", "#e9c46a", "#f4a261", "#e76f51")
Seven_sequential2b <- c("#d9b9ac", "#d78d8e", "#d56270", "#754e82", "#163b95", "#0b5d9e", "#1890ae") # inverted of 2a
Seven_sequential2c <- c("#533326", "#722829", "#9D2A38", "#A47DB1", "#6A8FE9", "#61B3F4", "#51C9E7") # complementary of 2a
Seven_sequential2d <- c("#53717e", "#569493", "#60ada3", "#a9b9a3", "#d8c8a2", "#ddba9e", "#d29f92") # +45 lightness, -45 saturation of 2a

## 8-color palettes ----
Eight_categorical1 <- c("#b60e10", "#233341", "#d1e2f0", "#6098b6", "#987143", "#d6828c", "#c9ad99", "#3e9896")
Eight_categorical2 <- c("#900c3f", "#182b55", "#5f4e94", "#a291c7", "#82cbec", "#d94f21", "#febd2b", "#9aab4b")
Eight_categorical3 <- c("#6f1926", "#de324c", "#f4895f", "#f8e16f", "#95cf92", "#369acc", "#9656a2", "#cbabd1")

## 9-color palettes ----


## 10-color categorical palettes ----
CatCol <- c(
  "#586158", "#C46B39", "#4DD8C0", "#3885AB", "#9C4DC4",
  "#C4AA4D", "#443396", "#CC99CC", "#88C44D", "#AB3232"
)
CatCol_igbp <- c(
  CVM = "#586158", DBF = "#C46B39", EBF = "#4DD8C0", ENF = "#3885AB", GRA = "#9C4DC4", # since CSH not available, otherwise: CSH = "#586158"
  MF = "#C4AA4D", OSH = "#443396", SAV = "#CC99CC", WET = "#88C44D", WSA = "#AB3232"
)
Ten_sequential <- c("#F94144", "#F3722C", "#F8961E", "#F9844A", "#F9C74F",
                    "#90BE6D", "#43AA8B", "#4D908E", "#577590", "#277DA1")


## 12-color categorical ----
Twelve_safe <- c("#88CCEEFF", "#CC6677FF", "#DDCC77FF", "#117733FF", "#332288FF", "#AA4499FF",
  "#44AA99FF", "#999933FF", "#882255FF", "#661100FF", "#6699CCFF", "#888888FF")

## 13-color categorical ----
Thirteen_safe <- c("#88CCEEFF", "#CC6677FF", "#DDCC77FF", "#117733FF", "#332288FF", "#AA4499FF",
                 "#44AA99FF", "#999933FF", "#882255FF", "#661100FF", "#6699CCFF", "#888888FF",
                 "#000000")

## More-colors ----
palette14_compl <- c(
  "#264653", "#287271", "#2a9d8f", "#8ab17d", "#e9c46a", "#f4a261", "#e76f51",
  "#533326", "#722829", "#9D2A38", "#A47DB1", "#6A8FE9", "#61B3F4", "#51C9E7"
)
palette14_lumin <- c(
  "#264653", "#287271", "#2a9d8f", "#8ab17d", "#e9c46a", "#f4a261", "#e76f51",
  "#53717e", "#569493", "#60ada3", "#a9b9a3", "#d8c8a2", "#ddba9e", "#d29f92"
)