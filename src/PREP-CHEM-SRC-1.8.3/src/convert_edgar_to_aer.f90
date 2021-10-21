!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_edgar_to_aer(isp,iespc,ident)
use chem1_list
use emiss_vars_emissions
use edgar_emissions, only:   edgar_nspecies=>nspecies&
                            ,edgar_spc_name=>spc_name&
			    ,edgar_g                     &
                            ,CO      &   
                            ,NOX     &
			    ,CO2     &
			    ,CH4     &
			    ,SO2     &
			    ,N2O     &
			    ,SF6     &
			    ,NMVOC   &
			    ,SO4     


!implicit none  ! < necessario para outros esquemas 
integer, intent(in) :: isp
integer, intent(inout) :: iespc,ident

IF(isp .GT. nspecies) RETURN

!-- edgar    |    aer 


!print*,'dentro conv=',isp,spc_name(isp)


! SO4      =>     
if(spc_name(isp) == 'SO41') then
   ident = SO41
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name(isp)
   emiss_g(iespc)%src_antro(:,:,1)   = 0.5*edgar_g(SO4)%src(:,:,1)!*
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name(isp)
   return
endif

! SO4      =>    ORA2 (acetic acid and higher acids) 
if(spc_name(isp) == 'SO42') then
   ident = SO42
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name(isp)
   emiss_g(iespc)%src_antro(:,:,1)   = 0.5*edgar_g(SO4)%src(:,:,1)!*
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name(isp)
   return
endif
!* ref: Stier et al., The aerosol-climate model ECHAM5-HAM. Atmos.
!  Chem. Phys., 5,1125-1156,2005.

end subroutine convert_edgar_to_aer
