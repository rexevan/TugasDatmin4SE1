# We only need tidyverse btw
# install.packages(tidyverse)

library(tidyverse)
# Input Data 
hepatitis <- read_csv("hepatitis/hepatitis.data", col_names = FALSE, na = "?")

# Ubah nama kolom
hepatitis_header <- c("Class", "AGE", "SEX", "STEROID", "ANTIVIRALS", 
                      "FATIGUE", "MALAISE", "ANOREXIA", "LIVER_BIG", 
                      "LIVER_FIRM", "SPLEEN_PALPABLE", "SPIDERS", "ASCITIES", 
                      "VARICIES", "BILIRUBIN", "ALK_PHOSPHATE", "SGOT", 
                      "ALBUMIN", "PROTIME", "HISTOLOGY")

colnames(hepatitis) <- hepatitis_header

# atur jenis masing-masing kolom 
hepatitis <- hepatitis %>% 
  mutate(
    Class = as.factor(Class),
    SEX = as.factor(SEX),
    STEROID = as.factor(STEROID),
    ANTIVIRALS = as.factor(ANTIVIRALS),
    FATIGUE = as.factor(FATIGUE),
    MALAISE = as.factor(MALAISE),
    ANOREXIA = as.factor(ANOREXIA),
    LIVER_BIG = as.factor(LIVER_BIG),
    LIVER_FIRM = as.factor(LIVER_FIRM),
    SPLEEN_PALPABLE = as.factor(SPLEEN_PALPABLE),
    SPIDERS = as.factor(SPIDERS),
    ASCITIES = as.factor(ASCITIES),
    VARICIES = as.factor(VARICIES),
    HISTOLOGY = as.factor(HISTOLOGY)
  )

# Fungsi untuk mengisi missing value 

# Untuk vektor kategorik
# Modus, anggaplah x adalah vektor.
# Item yang bukan NA dari x dihitung dan dibuatkan tablenya 
# Kemudian faktor yang memiliki hitungan terbanyak akan mengisi 
# Item yang NA. 

modus <- function(x) {
  x[is.na(x)] <- x[!is.na(x)] %>% 
    table() %>% 
    which.max()
  x = x %>% as.factor()
}

# Untuk vektor integer 
# Diisi nilai mediannya

summary(hepatitis)

# FATIGUE, MALAISE, DAN ANOREXIA diisi langsung 

hepatitis <- hepatitis %>% 
  mutate(FATIGUE = modus(FATIGUE),
         MALAISE = modus(MALAISE),
         ANOREXIA = modus(ANOREXIA)
         )

# Sisanya lakukan per class 

hepatitis <- hepatitis %>% 
  group_by(Class) %>% 
  mutate(
    # Kategorik
    STEROID = modus(STEROID),
    LIVER_BIG = modus(LIVER_BIG),
    LIVER_FIRM = modus(LIVER_FIRM),
    SPLEEN_PALPABLE = modus(SPLEEN_PALPABLE),
    SPIDERS = modus(SPIDERS),
    ASCITIES = modus(ASCITIES),
    VARICIES = modus(VARICIES),
    
    # Integer
   BILIRUBIN = median(BILIRUBIN),
   ALK_PHOSPHATE = median(ALK_PHOSPHATE),
   SGOT = median(SGOT),
   ALBUMIN = median(ALBUMIN),
   PROTIME = median(PROTIME)
  )

# Terakhir, simpan datanya 
write_csv(hepatitis, "hepatitis/hepatitis_after_PreProcessing.csv")
