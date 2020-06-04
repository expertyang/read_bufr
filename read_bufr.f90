program read_bufr
use ieee_arithmetic
implicit none
   integer, parameter :: lnbufr=11, lntabl=12, lunmt1=90, lunmt2=91, lr_unit=31
   integer, parameter :: i_kind=4, r_kind=8
   integer, parameter :: maxlvl=65536, maxvar=10

   real(r_kind),dimension(:,:), allocatable :: bfrdat
   character(80)                            :: cmnstr, cmnstr_h,cmnstr_p,cmnstr_t, cmnstr_td, cmnstr_w
   integer(i_kind)                          :: nvar, nlvl

   integer :: i, k, k1, n, icount, tcount, narg
   character(len=800) :: filename
   character(len=800) :: cmtdir="fix"

   integer(i_kind) :: ireadmg, ireadsb, iupvs01, igetprm, ibfms, isetprm

   integer                 :: year, month, day, hour, minute, second, icat,isubcat
   real                    :: lat, lon, alt, pmsl
   real, dimension(maxlvl) :: p, h, t, td, dir, spd
   integer                 :: kx, ip, ih, it, iw, itd
   character(len=40)       :: platform, id, name

   real, parameter   :: lr_missing=-888888.

   integer(i_kind) iret,idate
   character(8) subset,subfgn

   integer :: iargc

   narg=iargc()
   if(narg<1)then
      stop "read_bufr bufr_file ..."
   endif

   iret=isetprm("MAXSS",300000)
   open(lr_unit, file="bufr.little_r", status="unknown")
   do n=1, narg
      call getarg(n, filename)
      open(lnbufr,  file=filename,        status="old",form="unformatted")

      write(*,*) "Openbf: ", trim(filename)
      call openbf(lnbufr,"SEC3",lnbufr)
      call mtinfo(cmtdir, lunmt1, lunmt2)
      !write(*,*) "set datelen..."
      call datelen(10)

      tcount=0
      icount=0
      MG: do while (ireadmg(lnbufr,subset,idate)==0 ) 
         !write(*,*) "subset,idate: ", subset,idate
         icat=iupvs01(lnbufr,"MTYP")
         isubcat=iupvs01(lnbufr,"MSBTI")
         if(isubcat<0) isubcat=iupvs01(lnbufr,"MSBT")
         !read(subset(6:8),*) icat
         select case(icat)
         case (0)
            platform="FM-12 SYNOP"
         case (1)
            platform="FM-13 SHIP"
         case (2)
            platform="FM-35 TEMP"
         ! 3 -GPSRO
         case (4)
            platform="FM-42 AMDAR"
         case (5) ! Cloud Trace Wind
            platform="FM-88 SATOB"
         case (6)
            platform="FM-32 PILOT"
         case default
            write(*,*) "Unknown data category:", icat
            stop
         end select
         
         write(*,*) "Data category:", icat, " subcategory:", isubcat, " "//trim(platform)
         SB:do while (ireadsb(lnbufr)==0)
            tcount=tcount+1
            nlvl=1
            ! ID 
            id=""
            if(platform=="FM-12 SYNOP".or.platform=="FM-32 PILOT".or.platform=="FM-35 TEMP")then
               ! WMO Id
               nvar=2
               call allocate_bfrdat(nvar,nlvl)
               cmnstr="WMOB WMOS"
               call ufbint(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
               if(ibfms(bfrdat(1,1))==1.or.ibfms(bfrdat(2,1))==1.or.iret==0)then
                  id="-7777"
               else
                  write(id,"(I5.5)") int(bfrdat(1,1)*1000+bfrdat(2,1))
               endif
            endif

            if(platform=="FM-13 SHIP")then
               id="-7777"
            endif

            ! Name
            name=""
            nvar=1
            call allocate_bfrdat(nvar,nlvl)
            if(platform=="FM-12 SYNOP".or.platform=="FM-35 TEMP")then
               if(len_trim(name)==0)then
                  cmnstr="STSN"
                  call ufbint(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
                  if(ibfms(bfrdat(1,1))==0.and.(.not.ieee_is_nan(bfrdat(1,1))).and.iret/=0)then
                  !if(ibfms(bfrdat(1,1))==0.and.(.not.isnan(bfrdat(1,1))).and.iret/=0)then
                     call readlc(lnbufr,name,cmnstr)
                  endif
                  !write(*,*) trim(cmnstr)//":"//trim(name)
               endif
            endif
            if(platform=="FM-13 SHIP".or.platform=="FM-35 TEMP")then
               if(len_trim(name)==0)then
                  cmnstr="SMID"
                  call ufbint(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
                  if(ibfms(bfrdat(1,1))==0.and.(.not.ieee_is_nan(bfrdat(1,1))).and.iret/=0)then
                  !if(ibfms(bfrdat(1,1))==0.and.(.not.isnan(bfrdat(1,1))).and.iret/=0)then
                     call readlc(lnbufr,name,cmnstr)
                  endif
                  !write(*,*) trim(cmnstr)//":"//trim(name)
               endif
            endif
            if(platform=="FM-42 AMDAR")then
               cmnstr="ACRN"
               call ufbint(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
               if(ibfms(bfrdat(1,1))==0.and.(.not.ieee_is_nan(bfrdat(1,1))).and.iret/=0)then
               !if(ibfms(bfrdat(1,1))==0.and.(.not.isnan(bfrdat(1,1))).and.iret/=0)then
                  call readlc(lnbufr,name,cmnstr)
                  !write(*,*) trim(cmnstr)//":"//trim(name)
               endif
            endif
            if(len_trim(name)==0)   name=id
            if(len_trim(id  )==0)   id=name
            !write(*,*) "ID, Name :", trim(id),",", trim(name)

            ! Time
            nvar=6
            cmnstr="YEAR MNTH DAYS HOUR MINU SECO"
            call allocate_bfrdat(nvar,nlvl)
            call ufbint(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
            do i=1, nvar-1
               if(ibfms(bfrdat(i,1))==1.or.iret==0)then ! No time
                 ! Next SB
                 cycle SB 
               endif
            enddo
            year  =bfrdat(1,1)
            month =bfrdat(2,1)
            day   =bfrdat(3,1)
            hour  =bfrdat(4,1)
            minute=bfrdat(5,1)
            if(ibfms(bfrdat(6,1))==1)then ! second missing
               second=0      
            else
               second=bfrdat(6,1)      
            endif

            ! site location: lat lon
            nvar=2
            cmnstr="CLATH CLONH"
            call allocate_bfrdat(nvar,nlvl)
            call ufbint(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
            if(ibfms(bfrdat(1,1))==1.or.iret==0)then
               cmnstr="CLAT CLON"
               call ufbint(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
            endif
            do i=1, nvar
               if(ibfms(bfrdat(i,1))==1)then
                  bfrdat(i,1)=lr_missing      
               endif
            enddo
             lat=bfrdat(1,1)
             lon=bfrdat(2,1)
             
            ! site height 
            nvar=1
            cmnstr="HSMSL"
            call allocate_bfrdat(nvar,nlvl)
            call ufbint(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
            if(ibfms(bfrdat(1,1))==1.or.iret==0)then
               cmnstr="SELV"
               call ufbint(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
            endif
            do i=1, nvar
               if(ibfms(bfrdat(i,1))==1)then
                  bfrdat(i,1)=lr_missing      
               endif
            enddo
            alt=bfrdat(1,1)

            ! PMSL
            nvar=1
            cmnstr="PMSL"
            call allocate_bfrdat(nvar,nlvl)
            call ufbint(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
            do i=1, nvar
               if(ibfms(bfrdat(i,1))==1)then
                  bfrdat(i,1)=lr_missing      
               endif
            enddo
            pmsl=bfrdat(1,1)

            !write(*,*) "Location:", lat, lon, alt, pmsl
            
              p=lr_missing
              h=lr_missing
              t=lr_missing
             td=lr_missing
            dir=lr_missing
            spd=lr_missing

            kx=0

            nlvl=maxlvl

            ! Temperature
            nvar=1
            call allocate_bfrdat(nvar,nlvl)
            cmnstr="TMDB"
            call ufbrep(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
            if(iret==0)then
               cmnstr="TMDBST"
               call ufbrep(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
            endif
            cmnstr_t=cmnstr
            do i=1, nvar
               do k=1, iret
                  if(ibfms(bfrdat(i,k))==1)then
                     bfrdat(i,k)=lr_missing      
                  endif
               enddo
            enddo
            kx=max(kx,iret)
            do k=1, iret
               t (k)=bfrdat(1,k)
            enddo
            !write(*,*) "Temperature:", iret
            it=iret
          
            ! Dew-point
            nvar=1
            call allocate_bfrdat(nvar,nlvl)
            cmnstr="TMDP"
            call ufbrep(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
            cmnstr_td=cmnstr
            do i=1, nvar
               do k=1, iret
                  if(ibfms(bfrdat(i,k))==1)then
                     bfrdat(i,k)=lr_missing      
                  endif
               enddo
            enddo
            kx=max(kx,iret)
            do k=1, iret
               td(k)=bfrdat(1,k)
            enddo
            !write(*,*) "Dewpoint:", iret
            itd=iret

            ! Wind
            nvar=3
            call allocate_bfrdat(nvar,nlvl)
            cmnstr="WDIR WSPD WCMP"
            call ufbrep(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
            cmnstr_w=cmnstr
            do i=1, nvar
               do k=1, iret
                  if(ibfms(bfrdat(i,k))==1)then
                     bfrdat(i,k)=lr_missing      
                  endif
               enddo
            enddo
            kx=max(kx,iret)
            do k=1, iret
               dir(k)=bfrdat(1,k)
               spd(k)=bfrdat(2,k)
            enddo
            !write(*,*) "Wind:       ", iret
            iw=iret

! SYNOP: HMSL HEIT
! SHIP : 
! AMDAR: HMSL FLVLST
! PILOT: HEIT
! TEMP : HEIT GPH10 GPHTST HAST

            ! Height
            iret=0
            nvar=1
            call allocate_bfrdat(nvar,nlvl)
            if(platform=="FM-12 SYNOP".or.platform=="FM-42 AMDAR")then
               if(iret==0.or.(iret<kx))then
                  cmnstr="HMSL"
                  call ufbrep(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
                  !write(*,*) trim(cmnstr), iret
               endif
            endif

            if(platform=="FM-12 SYNOP".or.platform=="FM-32 PILOT".or.platform=="FM-35 TEMP")then
               if(iret==0.or.(iret<kx))then
                  cmnstr="HEIT"
                  call ufbrep(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
                  if(iret==1.and.(ibfms(bfrdat(1,1))==1.or.(abs(bfrdat(1,1)-alt)<10.and.alt/=lr_missing)))then ! missing or <10m
                     iret=0
                  endif
                  !write(*,*) trim(cmnstr), iret
               endif
            endif

            if(platform=="FM-42 AMDAR")then
               if(iret==0.or.(iret<kx))then
                  cmnstr="FLVLST"
                  call ufbrep(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
                  !write(*,*) trim(cmnstr), iret
               endif
            endif
! 
            if(platform=="FM-32 PILOT".or.platform=="FM-35 TEMP")then
               if(iret==0.or.(iret<kx))then
                  cmnstr="GPHTST"
                  call ufbrep(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
                  !write(*,*) trim(cmnstr), iret
               endif

               if(iret==0.or.(iret<kx))then
                  cmnstr="HAST"
                  call ufbrep(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
                  !write(*,*) trim(cmnstr), iret
               endif

               if(iret==0.or.(iret<kx))then
                  cmnstr="GPH10"
                  call ufbrep(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
                  !write(*,*) trim(cmnstr), iret
               endif
            endif
            cmnstr_h=cmnstr

            do i=1, nvar
               do k=1, iret
                  if(ibfms(bfrdat(i,k))==1)then
                     bfrdat(i,k)=lr_missing      
                  endif
               enddo
            enddo
            if(trim(cmnstr_h)=="HAST")then ! ABOVE STATION TO MSL
               if(alt/=lr_missing)then
                  do i=1, nvar
                     do k=1, iret
                        if(bfrdat(i,k)/=lr_missing)then
                           bfrdat(i,k)=bfrdat(i,k)+alt
                        endif
                     enddo
                  enddo
               else
                  bfrdat(1:nvar,1:iret)=lr_missing
                  iret=0
               endif
            endif
            kx=max(kx,iret)
            do k=1, iret
               h(k)=bfrdat(1,k)
            enddo
            ih=iret
            if(iret==0.and.platform=="FM-12 SYNOP")then
               h(1)=alt
               ih=1
            endif
            !write(*,*) "Height:     ", iret

            ! Pressure
            nvar=1
            cmnstr="PRES"
            call allocate_bfrdat(nvar,nlvl)
            call ufbrep(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)

            if(iret==0)then
               cmnstr="PRLC"
               call ufbrep(lnbufr,bfrdat,nvar,nlvl,iret,cmnstr)
            endif
            cmnstr_p=cmnstr
            do i=1, nvar
               do k=1, iret
                  if(ibfms(bfrdat(i,k))==1)then
                     bfrdat(i,k)=lr_missing      
                  endif
               enddo
            enddo
            kx=max(kx,iret)
            do k=1, iret
               p(k)=bfrdat(1,k)
               if(p(k)==0) p(k)=lr_missing ! 0Pa
            enddo
            !write(*,*) "Pressure:   ", iret
            ip=iret

            if(platform=="FM-13 SHIP")then
               ip=1
            endif
            write(*,"(A,5(',',2X,A,I8))") "ID:"//trim(id),trim(cmnstr_p )//":",ip , &
                                                          trim(cmnstr_h )//":",ih , &
                                                          trim(cmnstr_t )//":",it , &
                                                          trim(cmnstr_td)//":",itd, &
                                                          trim(cmnstr_w )//":",iw
            if(kx<1.or.(ip+ih)==0.or.(it+iw)==0)then
               write(*,*) "Site:"//trim(ID)//" "//trim(name)//" Error: no obs!!!"
               cycle
            endif
            icount=icount+1
            call write_little_r_obs(lr_unit, platform, year, month, day, hour, minute,second, id, name, lat, lon, alt, pmsl, kx, p, h, t, td, dir, spd)
         enddo SB
      enddo MG
      write(*,*) "write ", icount,"/", tcount, " obs."
      call closbf(lnbufr)
      close(lnbufr)
   enddo
   close(lr_unit)
contains
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine allocate_bfrdat(nvar, nlvl)
   implicit none
   integer, parameter :: r_kind=8
   integer, intent(in) :: nvar, nlvl
   
   if(allocated(bfrdat))then
      deallocate(bfrdat)
   endif
   allocate(bfrdat(nvar,nlvl))
   end subroutine
end program

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine write_little_r_obs (ounit, platform, year, month, day, hour, minute, second, id, name, lat, lon, alt, pmsl, kx, p, h, t, td, dir, spd)
   implicit none

   integer,             intent(in) :: ounit, year, month, day, hour, minute, second, kx
   real,                intent(in) :: lat, lon, alt, pmsl
   real, dimension(kx), intent(in) :: p, h, t, td, dir, spd
   character(len=*),    intent(in) :: id, name, platform
 
   integer :: i, k
   real    :: ps

   real, parameter   :: lr_missing = -888888. , lr_end_data=-777777.
   character(len=84) :: rpt_format
   character(len=22) :: meas_format
   character(len=14) :: end_format
   character(len=40) :: source

   character(len=20) :: date_char
   logical :: is_sound, is_bogus

   rpt_format =  ' ( 2f20.5 , 2a40 , '                   &
                // ' 2a40 , 1f20.5 , 5i10 , 3L10 , '     &
                // ' 2i10 , a20 ,  13( f13.5 , i7 ) ) '
   meas_format =  ' ( 10( f13.5 , i7 ) ) '
   end_format = ' ( 3 ( i7 ) ) '

   source="WIS-JMA BUFR"
   if(platform=="FM-12 SYNOP".or.platform=="FM-13 SHIP")then
      is_sound=.false.
      ps=p(1)
   else
      is_sound=.true.
   endif
   is_bogus=.false. 

   ps=lr_missing
   if(platform=="FM-12 SYNOP")then
      ps=p(1)
   endif
   write(date_char,"(6X,I4.4,5I2.2)" ) year, month, day, hour, minute, second
   ! header:
   WRITE ( UNIT = ounit    , FMT = rpt_format )                             &
           lat             , lon        , id           , name             , &
           platform        , source     , alt          , (kx*6)           , &
           0               , 0          , 1            , 0                , &
           is_sound        , is_bogus   , .false.      , int(lr_missing)  , &
           int(lr_missing) , date_char  , pmsl         , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , ps           , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0

   ! report:
   do k=1, kx
   WRITE ( UNIT = ounit  , FMT = meas_format )                &
           p (k) , 0 , h  (k) , 0 , t  (k) , 0 , &
           td(k) , 0 , spd(k) , 0 , dir(k) , 0 , &
           lr_missing , 0 , lr_missing , 0 , lr_missing , 0 , lr_missing , 0
   enddo
   

   ! end of report line:

   WRITE ( UNIT = ounit, FMT = meas_format )                    &
           lr_end_data , 0 , lr_end_data , 0 , real(kx*6) , 0 , &
           lr_missing  , 0 , lr_missing  , 0 , lr_missing , 0 , &
           lr_missing  , 0 , lr_missing  , 0 , lr_missing , 0 , &
           lr_missing  , 0

   ! end of message line:
   WRITE ( UNIT = ounit  , FMT = end_format )  kx*6, 0, 0

   end subroutine

