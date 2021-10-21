################### WPS LIB ##############################

constants_module.o : $(LIB_WPS_PATH)/constants_module.F
	 cp -f $< constants_module.F
	 $(F_COMMAND) constants_module.F
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #rm -f constants_module.F

misc_definitions_module.o : $(LIB_WPS_PATH)/misc_definitions_module.F
	 cp -f $< misc_definitions_module.F  
	 $(F_COMMAND) misc_definitions_module.F
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #rm -f misc_definitions_module.F

module_debug.o : $(LIB_WPS_PATH)/module_debug.F parallel_module.o
	 cp -f $< module_debug.F
	 $(F_COMMAND) module_debug.F
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #rm -f module_debug.F

module_map_utils.o : $(LIB_WPS_PATH)/module_map_utils.F module_debug.o \
                 misc_definitions_module.o constants_module.o
	 cp -f $< module_map_utils.F
	 $(F_COMMAND) module_map_utils.F
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #rm -f module_map_utils.F

cio.o : $(LIB_WPS_PATH)/cio.c 
	cp -f $< cio.c
	$(C_COMMAND) cio.c
	@mv -f $(<F:.f90=.f90) ../../doc/src
	#rm -f cio.c

llxy_module.o : $(LIB_WPS_PATH)/llxy_module.F gridinfo_module.o \
                 list_module.o module_map_utils.o module_debug.o \
                 misc_definitions_module.o
	 cp -f $< llxy_module.F
	 $(F_COMMAND) llxy_module.F
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #rm -f llxy_module.F

gridinfo_module.o : $(LIB_WPS_PATH)/gridinfo_module.F \
                    constants_module.o misc_definitions_module.o \
                    module_debug.o
	 cp -f $< gridinfo_module.F
	 $(F_COMMAND) gridinfo_module.F
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #rm -f gridinfo_module.F

list_module.o : $(LIB_WPS_PATH)/list_module.F \
                module_debug.o
	 cp -f $< list_module.F
	 $(F_COMMAND) list_module.F
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #rm -f list_module.F

parallel_module.o : $(LIB_WPS_PATH)/parallel_module.F 
	 cp -f $< parallel_module.F
	 $(F_COMMAND) parallel_module.F
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #rm -f parallel_module.F

################### BRAMS LIB ############################
an_header.o  : $(LIB_RAMS_MOD_PATH)/an_header.f90 #utils/lib/modules * 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src

charutils.o  : $(LIB_RAMS_PATH)/charutils.f90 #utils/lib *
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src

dateutils.o  : $(LIB_RAMS_PATH)/dateutils.f90 #utils/lib *
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
error_mess.o : $(LIB_RAMS_PATH)/error_mess.f90 #utils/lib *
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
getvar.o     : $(LIB_RAMS_PATH)/getvar.f90  an_header.o dump.o #utils/lib *
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
dump.o	: $(LIB_RAMS_DUM_PATH)/dump.f90 #utils/lib *
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
interp_lib.o : $(LIB_RAMS_PATH)/interp_lib.f90 #utils/lib *
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
htint-opt.o  : $(LIB_RAMS_PATH)/htint-opt.f90 #utils/lib *
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
map_proj.o   : $(LIB_RAMS_PATH)/map_proj.f90 #utils/lib *
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
numutils.o   : $(LIB_RAMS_PATH)/numutils.f90 #utils/lib *
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
polarst.o    : $(LIB_RAMS_PATH)/polarst.f90 #utils/lib *
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
therm_lib.o  : $(LIB_RAMS_PATH)/therm_lib.f90 #utils/lib *
	 cp -f $< $(<F:.f90=.f90)
#	 $(F_COMMAND) -pi $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
utils_f.o    : $(LIB_RAMS_PATH)/utils_f.f90 ModDateUtils.o #utils/lib *
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
ModDateUtils.o  : $(LIB_RAMS_MOD_PATH)/ModDateUtils.f90 #utils/lib/modules * 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
vformat.o    : $(LIB_RAMS_PATH)/vformat.f90 #utils/lib *
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
filelist.o   : $(LIB_RAMS_PATH)/filelist.F90 #utils/lib *
	  cp -f $< $(<F:.F90=.F90)
	  $(F_COMMAND) -D$(CMACH) $(<F:.F90=.F90)
	  @mv -f $(<F:.f90=.f90) ../../doc/src
	  #@mv -f $(<F:.f90=.f90) ../../doc/src
	  
rsys.o       : $(LIB_RAMS_PATH)/rsys.F90 #utils/lib *
	  cp -f $< $(<F:.F90=.F90)
	  $(F_COMMAND) -D$(CMACH) $(<F:.F90=.F90)
	  @mv -f $(<F:.f90=.f90) ../../doc/src
	  #@mv -f $(<F:.f90=.f90) ../../doc/src

#parlibf.o : $(LIB_RAMS_PATH)/parlibf.F90 #utils/lib
#	cp -f $< $(<F:.F90=.F90)
#	$(F_COMMAND) $(<F:.F90=.F90)
#	@mv -f $(<F:.f90=.f90) ../../doc/src
#	#@mv -f $(<F:.f90=.f90) ../../doc/src

dted.o       : $(LIB_RAMS_PATH)/dted.c #utils/lib *
	  $(C_COMMAND) $<
	  
#parlib.o     : $(LIB_RAMS_PATH)/parlib.c 
#	  $(C_COMMAND) $<
	  
tmpname.o    : $(LIB_RAMS_PATH)/tmpname.c  #utils/lib *
	  $(C_COMMAND) $<
	  
utils_c.o    : $(LIB_RAMS_PATH)/utils_c.c #utils/lib *
	  $(C_COMMAND) $<
	  
eenviron.o   : $(LIB_RAMS_EFF_PATH)/eenviron.c #utils/eff *
	  $(C_COMMAND) $<
	  
#spAllocate.o : $(LIB_RAMS_PATH)/spAllocate.c #utils/lib
#	  $(C_COMMAND) $<

#spBuild.o : $(LIB_RAMS_PATH)/spBuild.c #utils/lib
#	  $(C_COMMAND) $<

#spFactor.o : $(LIB_RAMS_PATH)/spFactor.c #utils/lib
#	  $(C_COMMAND) $<

#spOutput.o : $(LIB_RAMS_PATH)/spOutput.c #utils/lib
#	  $(C_COMMAND) $<

#spSolve.o : $(LIB_RAMS_PATH)/spSolve.c #utils/lib
#	  $(C_COMMAND) $<

#spUtils.o : $(LIB_RAMS_PATH)/spUtils.c #utils/lib
#	  $(C_COMMAND) $<

#spFortran.o : $(LIB_RAMS_PATH)/spFortran.c #utils/lib
#	  $(C_COMMAND) $<

##########################################################
################### RAMS LIB #############################


grid_dims.o : $(RAMS_PATH)/grid_dims.f90 #dif
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
io_params.o : $(RAMS_PATH)/io_params.f90 #dif
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
node_mod.o : $(RAMS_PATH)/node_mod.f90 #Dif          
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src

rconstants.o : $(RAMS_PATH)/rconstants.f90 #Dif - Mas poderia unificar       
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src

rams_grid.o : $(RAMS_PATH)/rams_grid.f90 rconstants.o #Dif
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src

lllc_utils.o : $(RAMS_PATH)/lllc_utils.f90 # Uni
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src

adap_init_prepchem.o : $(RAMS_PATH)/adap_init_prepchem.f90
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src

gridset_prepchem.o : $(RAMS_PATH)/gridset_prepchem.f90
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src

mem_grid.o : $(RAMS_PATH)/mem_grid.f90 grid_dims.o #dif
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src
	 
rcio.o : $(RAMS_PATH)/rcio.f90 mem_grid.o #dif
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 #@mv -f $(<F:.f90=.f90) ../../doc/src

##########################################################
################### WRF ##########################
	
wrf_fim_utils.o : $(PREPSOURCE_SRC)/wrf_fim_utils.f90 chem1_list.o grid_dims_output.o gocart_background.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

wps_util.o : $(PREPSOURCE_SRC)/wps_util.f90 mem_grid.o module_map_utils.o \
        llxy_module.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

write_gocart_bg.o : $(PREPSOURCE_SRC)/write_gocart_bg.f90 gocart_background.o grid_dims_output.o prep_chem_sources_utils.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

##########################################################
################### PREPSOURCES ##########################
aer1_list.o : $(CHEM_PATH)/aer1_list_SIMPLE.f90 #src/brams/ccatt/aer1_list_SIMPLE.f90
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src 
	
chem1_list.o : $(CHEM_PATH)/$(CHEM)/chem1_list.f90 #/src/brams/ccatt/RELACS_TUV/
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90) 
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src 
	
prep_chem_sources_utils.o : $(PREPSOURCE_SRC)/prep_chem_sources_utils.f90 volc_emissions.o volc_degassing_emissions.o chem1_list.o grid_dims.o mem_grid.o grid_dims_output.o 3bem_plumerise.o emission_fields.o aer1_list.o gfedv2_8days_emissions.o 3bem_emissions.o wrf_fim_utils.o an_header.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_retro_to_CB07.o : $(PREPSOURCE_SRC)/convert_retro_to_CB07.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src	

convert_AeM_to_CB07.o : $(PREPSOURCE_SRC)/convert_AeM_to_CB07.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o 
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_bioge_to_CB07.o : $(PREPSOURCE_SRC)/convert_bioge_to_CB07.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_edgar_to_RACM.o : $(PREPSOURCE_SRC)/convert_edgar_to_RACM.f90 edgar_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_retro_to_RACM.o : $(PREPSOURCE_SRC)/convert_retro_to_RACM.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src	

convert_AeM_to_RACM.o : $(PREPSOURCE_SRC)/convert_AeM_to_RACM.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o  
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_bioge_to_RACM.o : $(PREPSOURCE_SRC)/convert_bioge_to_RACM.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src		

convert_edgar_to_RACM_REAC.o : $(PREPSOURCE_SRC)/convert_edgar_to_RACM_REAC.f90 edgar_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_retro_to_RACM_REAC.o : $(PREPSOURCE_SRC)/convert_retro_to_RACM_REAC.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_AeM_to_RACM_REAC.o : $(PREPSOURCE_SRC)/convert_AeM_to_RACM_REAC.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_bioge_to_RACM_REAC.o : $(PREPSOURCE_SRC)/convert_bioge_to_RACM_REAC.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_edgar_to_RELACS.o : $(PREPSOURCE_SRC)/convert_edgar_to_RELACS.f90 edgar_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_retro_to_RELACS.o : $(PREPSOURCE_SRC)/convert_retro_to_RELACS.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_AeM_to_RELACS.o : $(PREPSOURCE_SRC)/convert_AeM_to_RELACS.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_bioge_to_RELACS.o : $(PREPSOURCE_SRC)/convert_bioge_to_RELACS.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_edgar_to_RELACS_REAC.o : $(PREPSOURCE_SRC)/convert_edgar_to_RELACS_REAC.f90 edgar_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_retro_to_RELACS_REAC.o : $(PREPSOURCE_SRC)/convert_retro_to_RELACS_REAC.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_AeM_to_RELACS_REAC.o : $(PREPSOURCE_SRC)/convert_AeM_to_RELACS_REAC.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_bioge_to_RELACS_REAC.o : $(PREPSOURCE_SRC)/convert_bioge_to_RELACS_REAC.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src
	
convert_megan_to_RELACS_REAC.o : $(PREPSOURCE_SRC)/convert_megan_to_RELACS_REAC.f90 megan_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_megan_to_RACM_REAC.o : $(PREPSOURCE_SRC)/convert_megan_to_RACM_REAC.f90 megan_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_megan_to_CB07.o : $(PREPSOURCE_SRC)/convert_megan_to_CB07.f90 megan_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	#@mv -f $(<F:.f90=.f90) ../../doc/src

convert_megan_to_RACM.o : $(PREPSOURCE_SRC)/convert_megan_to_RACM.f90 megan_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
		
convert_megan_to_RELACS.o : $(PREPSOURCE_SRC)/convert_megan_to_RELACS.f90 megan_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
	
convert_edgar_to_aer.o : $(PREPSOURCE_SRC)/convert_edgar_to_aer.f90 edgar_emissions.o emission_fields.o aer1_list.o \
	megan_emissions.o biogenic_emissions.o grid_dims_output.o 
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

convert_AeM_to_aer.o : $(PREPSOURCE_SRC)/convert_AeM_to_aer.f90 emission_fields.o aer1_list.o \
	grid_dims_output.o 3bem_plumerise.o AeM_emission_factors.o fwb_awb_emissions.o gfedv2_8days_emissions.o 
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

convert_edgar_to_CB07.o : $(PREPSOURCE_SRC)/convert_edgar_to_CB07.f90 edgar_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src		


convert_edgar_to_RADM_WRF_FIM.o : $(PREPSOURCE_SRC)/convert_edgar_to_RADM_WRF_FIM.f90 edgar_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

convert_retro_to_RADM_WRF_FIM.o : $(PREPSOURCE_SRC)/convert_retro_to_RADM_WRF_FIM.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src	

convert_AeM_to_RADM_WRF_FIM.o : $(PREPSOURCE_SRC)/convert_AeM_to_RADM_WRF_FIM.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o   
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

convert_bioge_to_RADM_WRF_FIM.o : $(PREPSOURCE_SRC)/convert_bioge_to_RADM_WRF_FIM.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src		

convert_edgar_to_RADM_WRF_FIM_REAC.o : $(PREPSOURCE_SRC)/convert_edgar_to_RADM_WRF_FIM_REAC.f90 edgar_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

convert_retro_to_RADM_WRF_FIM_REAC.o : $(PREPSOURCE_SRC)/convert_retro_to_RADM_WRF_FIM_REAC.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

convert_AeM_to_RADM_WRF_FIM_REAC.o : $(PREPSOURCE_SRC)/convert_AeM_to_RADM_WRF_FIM_REAC.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

convert_bioge_to_RADM_WRF_FIM_REAC.o : $(PREPSOURCE_SRC)/convert_bioge_to_RADM_WRF_FIM_REAC.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

convert_megan_to_RADM_WRF_FIM_REAC.o : $(PREPSOURCE_SRC)/convert_megan_to_RADM_WRF_FIM_REAC.f90 megan_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

convert_megan_to_RADM_WRF_FIM.o : $(PREPSOURCE_SRC)/convert_megan_to_RADM_WRF_FIM.f90 megan_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
	
#>>> DEFINED 

retro_emissions.o : $(PREPSOURCE_SRC)/retro_emissions.f90 cetesb_update.o util_geometry.o mem_grid.o grid_dims_output.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

diurnal_cycle.o : $(PREPSOURCE_SRC)/diurnal_cycle.f90 retro_emissions.o grid_dims_output.o util_geometry.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

diurnal_cycle_fwb.o : $(PREPSOURCE_SRC)/diurnal_cycle_fwb.f90 fwb_awb_emissions.o grid_dims_output.o util_geometry.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

diurnal_cycle_edgar.o : $(PREPSOURCE_SRC)/diurnal_cycle_edgar.f90 edgar_emissions.o grid_dims_output.o util_geometry.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

fwb_awb_emissions.o : $(PREPSOURCE_SRC)/fwb_awb_emissions.f90 aer1_list.o AeM_emission_factors.o grid_dims_output.o grid_dims.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

wb_emissions.o : $(PREPSOURCE_SRC)/wb_emissions.f90 grid_dims_output.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

fire_properties.o : $(PREPSOURCE_SRC)/fire_properties.f90 
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
	
3bem_emissions.o : $(PREPSOURCE_SRC)/3bem_emissions.f90 fire_properties.o AeM_emission_factors.o grid_dims_output.o 3bem_plumerise.o mem_grid.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

volc_emissions.o : $(PREPSOURCE_SRC)/volc_emissions.f90 grid_dims_output.o mem_grid.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

volc_degassing_emissions.o : $(PREPSOURCE_SRC)/volc_degassing_emissions.f90 grid_dims_output.o mem_grid.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
	
gfedv2_8days_emissions.o : $(PREPSOURCE_SRC)/gfedv2_8days_emissions.f90 grid_dims_output.o 3bem_plumerise.o AeM_emission_factors.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
	
biogenic_emissions.o : $(PREPSOURCE_SRC)/biogenic_emissions.f90 grid_dims_output.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
	
megan_emissions.o : $(PREPSOURCE_SRC)/megan_emissions.f90 grid_dims_output.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

gocart_background.o : $(PREPSOURCE_SRC)/gocart_background.f90 mem_grid.o grid_dims_output.o grid_dims.o
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src
	 
gocart_emissions.o : $(PREPSOURCE_SRC)/gocart_emissions.f90 grid_dims_output.o cetesb_update.o mem_grid.o
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src

streets_emissions.o : $(PREPSOURCE_SRC)/streets_emissions.f90 grid_dims_output.o cetesb_update.o mem_grid.o
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src

seac4rs_emissions.o : $(PREPSOURCE_SRC)/seac4rs_emissions.f90 grid_dims_output.o cetesb_update.o mem_grid.o
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src

cetesb_update.o : $(PREPSOURCE_SRC)/cetesb_update.f90
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

util_geometry.o : $(PREPSOURCE_SRC)/util_geometry.f90
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

edgar_emissions.o : $(PREPSOURCE_SRC)/edgar_emissions.f90 cetesb_update.o util_geometry.o grid_dims_output.o mem_grid.o 
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

AeM_emission_factors.o : $(PREPSOURCE_SRC)/AeM_emission_factors.f90
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

3bem_plumerise.o : $(PREPSOURCE_SRC)/3bem_plumerise.f90 AeM_emission_factors.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
	
emission_fields.o : $(PREPSOURCE_SRC)/emission_fields.f90 3bem_plumerise.o grid_dims_output.o 
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	@mv -f $(<F:.f90=.f90) ../../doc/src

grid_dims_output.o : $(PREPSOURCE_SRC)/grid_dims_output.f90 grid_dims.o
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 @mv -f $(<F:.f90=.f90) ../../doc/src

prep_chem_sources.o : $(PREPSOURCE_SRC)/prep_chem_sources.f90  wrf_fim_utils.o \
grid_dims_output.o mem_grid.o an_header.o diurnal_cycle.o diurnal_cycle_fwb.o \
diurnal_cycle_edgar.o chem1_list.o aer1_list.o emission_fields.o edgar_emissions.o \
gocart_emissions.o streets_emissions.o seac4rs_emissions.o retro_emissions.o \
biogenic_emissions.o megan_emissions.o gfedv2_8days_emissions.o \
3bem_emissions.o 3bem_plumerise.o fwb_awb_emissions.o volc_emissions.o \
volc_degassing_emissions.o wb_emissions.o wps_util.o write_gocart_bg.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.F90)
	@mv -f $(<F:.f90=.f90) ../../doc/src
	
##########################################################
