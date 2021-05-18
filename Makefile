# R executable path
# --slave is even quieter than --quiet
R_EXEC := singularity exec --bind /snfs1:/snfs1 --bind /tmp:/tmp /ihme/singularity-images/lbd/releases/lbd_full_20200128.simg R --no-save --slave

lib_path := /share/code/geospatial/link_table

export R_LIBS_USER := "$(lib_path):${R_LIBS_USER}"

test:
	$(R_EXEC) -e "devtools::test()"

document:
	$(R_EXEC) -e "devtools::document()"

build:
	$(R_EXEC) -e "devtools::build(path='$(lib_path)')"

install:
	$(R_EXEC) -e "devtools::install(upgrade = 'never', build_vignettes = TRUE)"

build-vignettes:
	$(R_EXEC) -e "devtools::build_vignettes()"

all: document install
