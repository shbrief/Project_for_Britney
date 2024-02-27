## Create TCGA_validationDatasets.rda

dat_dir <- "~/Documents/GitHub/GSS/data"

# Raw read counts from GSE62944 from ExperimentHub
tcga <- GSEABenchmarkeR::loadEData("tcga",
                                   cache = FALSE,
                                   paired = FALSE,
                                   map2entrez = FALSE)

## log2 transformation
assay(tcga$BLCA) <- log2(assay(tcga$BLCA) + 1)
assay(tcga$HNSC) <- log2(assay(tcga$HNSC) + 1)
assay(tcga$KICH) <- log2(assay(tcga$KICH) + 1)
assay(tcga$KIRC) <- log2(assay(tcga$KIRC) + 1)
assay(tcga$KIRP) <- log2(assay(tcga$KIRP) + 1)
assay(tcga$LIHC) <- log2(assay(tcga$LIHC) + 1)
assay(tcga$LUSC) <- log2(assay(tcga$LUSC) + 1)

TCGA_validationDatasets <- vector(mode = "list", length = 7)
names(TCGA_validationDatasets) <- c("BLCA", "HNSC", "KICH", "KIRC", "KIRP", "LIHC", "LUSC")
TCGA_validationDatasets[[1]] <- tcga$BLCA
TCGA_validationDatasets[[2]] <- tcga$HNSC
TCGA_validationDatasets[[3]] <- tcga$KICH
TCGA_validationDatasets[[4]] <- tcga$KIRC
TCGA_validationDatasets[[5]] <- tcga$KIRP
TCGA_validationDatasets[[6]] <- tcga$LIHC
TCGA_validationDatasets[[7]] <- tcga$LUSC

save(TCGA_validationDatasets, file = file.path(dat_dir, "TCGA_validationDatasets.rda"))












