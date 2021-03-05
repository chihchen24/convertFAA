program readAlt
!------------------------------------------------------------------------
! This program reads in a series of FAA ascii files and writes out the 
! data to individual netCDF files
!
! The number of times is driven by the number of input files
!------------------------------------------------------------------------

  use netcdf

  implicit none

integer,parameter :: SHR_KIND_R8 = selected_real_kind(12) ! 8 byte real
integer,parameter :: R8 = SHR_KIND_R8


  integer,parameter :: NLAT_FAA=180, NLON_FAA=360, NALT_FAA=95
  integer,parameter :: NFIELDS_FAA=14
  integer,parameter :: NFIELDS_FAA2=12  ! does not include track_dist and fuelburn
  integer,parameter :: MAXDIDS=4
  integer,parameter :: MAXDIMS=4
  integer,parameter :: STARTDAYSOUT=1
  integer,parameter :: ENDDAYSOUT=365

! For Photon
! character*256,dimension(1) :: climFile(1)='/data/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_01_climo.nc'
! character(len=36) :: fileprefix=('/data6/cacraig/FAAData_netCDF-test3/')

! For Bluefire
!  Copied from /project/utls/satftp1/ccsm3/microp2/test1yr/ camdev55_cam3_6_72_u175_1xco2_icealb2 on CGD machines
 character*256 :: climFile(12)=(/'/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_01_climo.nc',&
                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_02_climo.nc',&
                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_03_climo.nc',&
                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_04_climo.nc',&
                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_05_climo.nc',&
                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_06_climo.nc',&
                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_07_climo.nc',&
                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_08_climo.nc',&
                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_09_climo.nc',&
                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_10_climo.nc',&
                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_11_climo.nc',&
                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_12_climo.nc'/)

  character(len=44) :: fileprefix=('/fis/cgd/home/cacraig/ACCRI_2006_MixRat/')

!! For Calgary
!  character*256 :: climFile(12)=(/'/project/utls/satftp1/ccsm3/microp2/test1yr/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_01_climo.nc',&
!                                  '/project/utls/satftp1/ccsm3/microp2/test1yr/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_02_climo.nc',&
!                                  '/project/utls/satftp1/ccsm3/microp2/test1yr/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_03_climo.nc',&
!                                  '/project/utls/satftp1/ccsm3/microp2/test1yr/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_04_climo.nc',&
!                                  '/project/utls/satftp1/ccsm3/microp2/test1yr/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_05_climo.nc',&
!                                  '/project/utls/satftp1/ccsm3/microp2/test1yr/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_06_climo.nc',&
!                                  '/project/utls/satftp1/ccsm3/microp2/test1yr/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_07_climo.nc',&
!                                  '/project/utls/satftp1/ccsm3/microp2/test1yr/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_08_climo.nc',&
!                                  '/project/utls/satftp1/ccsm3/microp2/test1yr/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_09_climo.nc',&
!                                  '/project/utls/satftp1/ccsm3/microp2/test1yr/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_10_climo.nc',&
!                                  '/project/utls/satftp1/ccsm3/microp2/test1yr/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_11_climo.nc',&
!                                  '/project/utls/satftp1/ccsm3/microp2/test1yr/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_12_climo.nc'/)
!
! character(len=43) :: fileprefix=('/project/utls/cacraig/ACCRI_2006_MixRat/')
!  character(len=47) :: fileprefix=('/project/utls/cacraig/ACCRI_2006_GRIDCELLS-new/')
  
  character(len=300)  :: string
  integer             :: i,ierr,j,k,l
  integer             :: ntime

  integer,dimension(500000)               :: month,day,ilat,ilon,ialt
  integer                                 :: didLat,didLon,didlev,didilev,didTime,levid,varid,latid,lonid
  integer,dimension(NFIELDS_FAA)              :: vidmix,ncid,vidlev
  integer,dimension(NFIELDS_FAA)              :: vidlat,vidlon,vidPS,vidalt_int,viddate,viddatesec
  integer,dimension(NFIELDS_FAA)              :: vidhyam,vidhybm,vidp0
!******************** add hyai & hybi ****************************************
  integer,dimension(NFIELDS_FAA)              :: vidhyai,vidhybi
!*****************************************************************************
  real(r8),dimension(NLON_FAA,NLAT_FAA,NALT_FAA,NFIELDS_FAA):: AllData
  real,allocatable,dimension(:,:,:,:)     :: mixing_ratio  ! (NLON_FAA,NLAT_FAA,NLEV_CLIMO,NFIELDS_FAA2)
  integer,dimension(MAXDIMS)              :: start,count,stride,chunksizes
  integer,dimension(500000)               :: hour
  integer,dimension(1)                    :: date,datesec

  integer :: status,ifield,itime,ifirst1,ilast1,ifirst2,ilast2,iday
  integer :: dids(MAXDIDS),didone(1),didthree(3),didfour(4)
  integer :: nlev_CLIMO,nlat_CLIMO,nlon_CLIMO,indexRev
  integer :: maxalt

  real(r8):: latGrid(NLAT_FAA),lonGrid(NLON_FAA),altgrid_int(NALT_FAA+1),altgrid_int3d(NLON_FAA,NLAT_FAA,NALT_FAA+1)

  real(r8)                                 :: P0_CLIMO
  real(r8), allocatable,dimension(:)       :: hyam_CLIMO,hybm_CLIMO
  real(r8), allocatable,dimension(:)       :: hyai_CLIMO,hybi_CLIMO
  real(r8), allocatable,dimension(:,:,:)   :: pin_CLIMO
  real(r8), allocatable,dimension(:)       :: lev



  character(len=30) :: datestr
  character(len=10) :: unitsStr

  character(len=500),dimension(:),allocatable :: inputfile
  character(len=500)                          :: tempChar

  character(len=10),dimension(NFIELDS_FAA) :: Fields=(/'SLANT_DIST','TRACK_DIST',&
  'FUELBURN  ','CO        ','HC        ','NOX       ','PMNV      ','PMSO      ',&
  'PMFO      ','CO2       ','H2O       ','SOX       '/)
  character(len=19) :: filesuffix=('_grid.nc')
  character(len=80) :: filelist

!-------------------------------------------------------------------
! Set up the lat/lon/alt arrays
!-------------------------------------------------------------------

  do i=1,NLAT_FAA
    latGrid(i)=-89.5+i-1
  end do

  do i=1,NLON_FAA
    lonGrid(i)=.5+i-1
  end do

! Reverse the order of heights for the model and store as meters
  do i=1,NALT_FAA+1
    indexRev=NALT_FAA+1-i+1
    altGrid_int(indexRev)=0+(i-1)*152
    altgrid_int3d(:,:,indexRev)=altgrid_int(indexRev)
  end do

  write(0,*) ' altgrid_int=',altgrid_int
  maxalt=0
!-------------------------------------------------------------------
! Loop over all files
!-------------------------------------------------------------------
do iday=STARTDAYSOUT,ENDDAYSOUT

!-------------------------------------------------------------------
! Determine how many times there are and allocate arrays appropriately
!-------------------------------------------------------------------
  if (iday < 10) then
    write(filelist,995) 'InputFAAFileList-',iday,'.txt'
995 format(a,i1,a)
  else if (iday < 100) then
    write(filelist,994) 'InputFAAFileList-',iday,'.txt'
994 format(a,i2,a)
  else
    write(filelist,993) 'InputFAAFileList-',iday,'.txt'
993 format(a,i3,a)
  end if

  write(0,*)  filelist
  open(file=filelist,unit=10)
  
  ntime=0
  do
    ntime=ntime+1
    read(10,997,end=10) tempChar
997 format(a500)
  end do

10 continue

  ntime=ntime-1
  rewind 10

  allocate(inputfile(ntime))

  do i=1,ntime
    read(10,997) inputfile(i)
  end do
  
!----------------------------------------------------------
! Determine the first and last days and put in filenames
! NOTE - This assumes that all of the input data contains "2006"
!----------------------------------------------------------

  ifirst1= index(inputfile(1),'/',.true.)+1
  ifirst2= index(inputfile(1),'_2006')+4
  ilast1= index(inputfile(ntime),'/',.true.)+1
  ilast2= index(inputfile(ntime),'_2006')+4
  if (inputfile(1)(ifirst1:ifirst2) ==  inputfile(ntime)(ilast1:ilast2)) then
    datestr='_'//inputfile(1)(ifirst1:ifirst2)
  else 
    datestr='_'//inputfile(1)(ifirst1:ifirst2)//'_to_'//inputfile(ntime)(ilast1:ilast2)
  end if
  
!----------------------------------------------------------
! Read the climatology file to retrieve the number of levels and number of levels of intervals
!  (lev and ilev) 
!    ASSUMES THIS IS THE SAME FOR EVERY FILE AND JUST USES THE FIRST ONE
!----------------------------------------------------------

!----------------------------------------------------------
! Loop over every time step (input file)
!----------------------------------------------------------
  do itime=1,ntime

     do l=1,NFIELDS_FAA
       do k=1,NALT_FAA
         do j=1,NLAT_FAA
          do i=1,NLON_FAA
              AllData(i,j,k,l)=0.
          end do
        end do
       end do
     end do

!----------------------------------------------------------
! Read in the data
!----------------------------------------------------------

     open(file=inputfile(itime),unit=20,iostat=ierr)
     if (ierr /= 0) then
        write(0,*) ' problem opening ',trim(inputfile(itime)),' ... Stopping'
        stop
     endif
     read (20,*) tempChar  ! Read the comment line and discard
     i=0

     do 
       i=i+1
       read (20,*,end=20) month(i),day(i),hour(i),ilat(i),ilon(i),ialt(i),&
            AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),1),&
            AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),2),&
            AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),3),&
            AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),4),&
            AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),5),&
            AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),6),&
            AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),7),&
            AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),8),&
            AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),9),&
            AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),10),&
            AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),11),&
            AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),12)
     if (ialt(i) > maxalt) then
        write(0,*) ' file=',trim(inputfile(itime)),' ialt(i)=',ialt(i)
        maxalt=ialt(i)
     end if
!    write(0,*) ' inside read, i=',i,month(i),day(i),hour(i),ilat(i),ilon(i),ialt(i),&
!                  AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),1:NFIELDS_FAA)

! 999 format(i,i,i,3i4,13G25.21)

     enddo
20   continue

   enddo
 !   write(0,*) ' after read' , month(1),day(1),hour(1),ilat(1),ilon(1),ialt(1),&
 !                 AllData(ilon(1)+1,ilat(1)+1,NALT_FAA-ialt(1),1:NFIELDS_FAA)
!----------------------------------------------------------
! Convert from grams/hour mixing ratio (just the fields from index 3 onwards in field direction)
! output units are kg/kg/sec
!     Pick the climFile based on the month(1) in the FAA file
!----------------------------------------------------------

  deallocate(inputfile)
!  deallocate(lev)
!  deallocate(pin_CLIMO)

end do  ! Number of days

end program readAlt
