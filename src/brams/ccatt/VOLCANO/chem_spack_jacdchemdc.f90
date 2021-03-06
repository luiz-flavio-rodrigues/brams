                                                                                                                                                      
 MODULE mod_chem_spack_jacdchemdc                                                                                                                     
                                                                                                                                                      
!   USE mod_chem_spack_dratedc, ONLY: dratedc  ! subroutine                                                                                            
   IMPLICIT NONE                                                                                                                                      
   PRIVATE                                                                                                                                            
   PUBLIC :: jacdchemdc ! subroutine                                                                                                                  
 CONTAINS                                                                                                                                             
                                                                                                                                                      
   SUBROUTINE jacdchemdc(y,rk,JacC,ngas,ijkbeg,ijkend,maxblock_size,nr)                                                                               
                                                                                                                                                      
!------------------------------------------------------------------------                                                                             
!                                                                                                                                                     
!     -- DESCRIPTION                                                                                                                                  
!                                                                                                                                                     
!     This routine computes the Jacobian matrix for the gas-phase.                                                                                    
!     This routine is automatically generated by SPACK.                                                                                               
!     Mechanism: ../Mechanism/RELACS                                                                                                                  
!     Species: ../Mechanism/ciRLCS                                                                                                                    
!                                                                                                                                                     
!------------------------------------------------------------------------                                                                             
!                                                                                                                                                     
!     -- INPUT VARIABLES                                                                                                                              
!                                                                                                                                                     
!     Y: chemical concentrations.                                                                                                                     
!     RK: kinetic rates.                                                                                                                              
!                                                                                                                                                     
!     -- INPUT/OUTPUT VARIABLES                                                                                                                       
!                                                                                                                                                     
!     -- OUTPUT VARIABLES                                                                                                                             
!                                                                                                                                                     
!     JACC: Jacobian matrix.                                                                                                                          
!                                                                                                                                                     
!------------------------------------------------------------------------                                                                             
!                                                                                                                                                     
!     -- REMARKS                                                                                                                                      
!                                                                                                                                                     
!     The matrix JACC could be stored in a low-dimensional vector.                                                                                    
!                                                                                                                                                     
!------------------------------------------------------------------------                                                                             
!                                                                                                                                                     
!     -- MODIFICATIONS                                                                                                                                
!                                                                                                                                                     
!------------------------------------------------------------------------                                                                             
!                                                                                                                                                     
!     -- AUTHOR(S)                                                                                                                                    
!                                                                                                                                                     
!     SPACK.                                                                                                                                          
!                                                                                                                                                     
!------------------------------------------------------------------------                                                                             
                                                                                                                                                      
      IMPLICIT NONE                                                                                                                                   
                                                                                                                                                      
                                                                                                                                                      
       INTEGER 	 , INTENT(IN)  :: ngas                                                                                                                
       INTEGER 	 , INTENT(IN)  :: ijkbeg			                                                                                                           
       INTEGER 	 , INTENT(IN)  :: ijkend			                                                                                                           
       INTEGER 	 , INTENT(IN)  :: maxblock_size 		                                                                                                    
       INTEGER 	 , INTENT(IN)  :: nr 				                                                                                                             
       DOUBLE PRECISION , INTENT(IN)  :: rk(maxblock_size,nr)		                                                                                       
       DOUBLE PRECISION , INTENT(IN)  :: y(maxblock_size,NGAS) 	                                                                                      
       DOUBLE PRECISION , INTENT(OUT) :: JacC(maxblock_size,NGAS,NGAS)                                                                                
       								                                                                                                                                       
       DOUBLE PRECISION :: dw(maxblock_size,nr,NGAS)			                                                                                               
       INTEGER :: ijk							                                                                                                                          
                                                                                                                                                      
     END SUBROUTINE jacdchemdc                                                                                                                          
                                                                                                                                                      
  END MODULE mod_chem_spack_jacdchemdc                                                                                                                
                                                                                                                                                      
