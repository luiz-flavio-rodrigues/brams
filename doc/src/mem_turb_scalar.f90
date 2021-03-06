!############################# Change Log ##################################
! 5.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000, 2003 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!###########################################################################

! For CATT

module mem_turb_scalar

  use grid_dims

  implicit none

  type turb_s_vars

     real, pointer, dimension(:,:,:) :: &
          hksc

  end type turb_s_vars

  type (turb_s_vars), allocatable :: turb_s(:),turbm_s(:)


contains

  subroutine alloc_turb_s(turb_s_local, n1, n2, n3, ng)

    implicit none

    type (turb_s_vars)  :: turb_s_local
    integer, intent(in) :: n1,n2,n3,ng

    !print*, 'enter alloc_turb_s',n1,n2,n3

    allocate (turb_s_local%hksc(n1,n2,n3));turb_s_local%hksc=0.0

    return
  end subroutine alloc_turb_s

  !---------------------------------------------------------------

  subroutine nullify_turb_s(turb_s_local)

    implicit none

    type (turb_s_vars) :: turb_s_local

    integer :: nsc

    ! Deallocate all scratch arrays

    if (associated(turb_s_local%hksc ))  nullify (turb_s_local%hksc )

    return
  end subroutine nullify_turb_s
  !---------------------------------------------------------------

  subroutine dealloc_turb_s(turb_s_local)

    implicit none

    type (turb_s_vars) :: turb_s_local

    integer :: nsc

    ! Deallocate all scratch arrays

    if (associated(turb_s_local%hksc ))  deallocate (turb_s_local%hksc )

    return
  end subroutine dealloc_turb_s

  !---------------------------------------------------------------

  subroutine filltab_turb_s(turb_s,turbm_s,imean,n1,n2,n3,ng)
    use var_tables, only: InsertVTab
    implicit none
    include "i8.h"
    type (turb_s_vars) :: turb_s,turbm_s
    integer, intent(in) :: n1,n2,n3,ng,imean
    integer(kind=i8) :: npts

    ! Fill pointers to arrays into variable tables

    npts=n1*n2*n3

    if (associated(turb_s%hksc))  &
         call InsertVTab (turb_s%hksc,turbm_s%hksc  &
         ,ng, npts, imean,  &
         'HKSC :3:hist:anal:mpti:mpt3:mpt1')

    return
  end subroutine filltab_turb_s

end module mem_turb_scalar
