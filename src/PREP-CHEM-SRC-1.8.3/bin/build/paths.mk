#Makefile include paths.mk

#Prepsource related

LIB_RAMS_PATH =../../../../src/utils/lib
LIB_RAMS_MOD_PATH = ../../../../src/utils/lib/modules
LIB_RAMS_EFF_PATH = ../../../../src/utils/eff
LIB_RAMS_DUM_PATH = ../../../../src/utils/dump
INC_RAMS_MOD_PATH = ../../../../src/utils/include
RAMS_PATH     =../../aux_src/brams
CHEM_PATH = ../../../../src/brams/ccatt
PREPSOURCE_SRC=../../src
LIB_WPS_PATH  =../../aux_src/wps

AER_DIR=$(AER)
ifeq ($(CHEM), RADM_WRF_FIM)
AER_DIR=SIMPLE
endif
