!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                  
!   ICM Wetland Morphology Raster Differencing
!                                                  
!                                                  
!   Fortran code to compare two binary rasters saved by ICM-Morph 
!   
!   Command line arguments passed into this executable:
!
!   1 = ras0_xyz_pth  : full path to XYZ ASCI raster file for raster that ras0 will be compared to ras1
!   2 = ras1_xyz_pth  : full path to XYZ ASCI raster file for raster that ras1 will be compared to ras0
!   3 = nras_str      : number of raster pixels of dataset, must match size of binary arrays

!                                                  
!   Questions: eric.white@la.gov                   
!   last update: 7/26/2021                          
!                                                     
!   project site: https://github.com/CPRA-MP      
!   documentation: http://coastal.la.gov/our-plan  
!                                                  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    
program main
    implicit none
    
    character*100 :: ras0_xyz_pth
    character*100 :: ras1_xyz_pth
    character*20 :: nras_str
    integer,parameter :: sp=selected_real_kind(p=6) 
    integer :: nras,i
    real(sp),dimension(:),allocatable :: x0,y0,z0
    real(sp),dimension(:),allocatable :: x1,y1,z1

    call GET_COMMAND_ARGUMENT(1,ras0_xyz_pth)
    call GET_COMMAND_ARGUMENT(2,ras1_xyz_pth)
    call GET_COMMAND_ARGUMENT(3,nras_str)
    
    read(nras_str,*) nras
    
    allocate(x0(nras))
    allocate(y0(nras))
    allocate(z0(nras))
    allocate(x1(nras))
    allocate(y1(nras))
    allocate(z1(nras))

    write(*,*) 'Reading in:', trim(adjustL(ras0_xyz_pth))
    open(unit=100, file = trim(adjustL(ras0_xyz_pth)))
    do i = 1,nras
        read(100,*) x0(i),y0(i),z0(i)
    end do
    close(100)    
    
    write(*,*) 'Reading in:', trim(adjustL(ras1_xyz_pth))
    open(unit=101, file = trim(adjustL(ras1_xyz_pth)))
    do i = 1,nras
        read(101,*) x1(i),y1(i),z1(i)
    end do
    close(101)
    
    write(*,*) 'checking XY pairs for alignment...'
    do i = 1,nras
        if (x1(i) /= x0(i)) then
            write(*,'(A,I,A,I,A,I,A,I,A,I,A)') 'mismatch in x @ i:', i, 'xy0:(', x0(i), ',' ,y0(i), ') xy1:(', x1(i), ',' ,y1(i), ')'
        elseif (y1(i) /= y0(i)) then
             write(*,'(A,I,A,I,A,I,A,I,A,I,A)') 'mismatch in y @ i:', i, 'xy0:(', x0(i), ',' ,y0(i), ') xy1:(', x1(i), ',' ,y1(i), ')'
        end if
    end do
end program
