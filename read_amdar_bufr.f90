program read_amdar_bufr
implicit none
   integer, parameter :: lnbufr=11, lntabl=12, use_unit=21, csv_unit=98
   integer, parameter :: i_kind=4, r_kind=8
   integer, parameter :: maxinfo=12, maxchanl=100

   integer :: i, tcount

   integer(i_kind) :: ireadmg,ireadsb
   integer(i_kind),parameter:: n0bhdr=8 ! ID
   integer(i_kind),parameter:: n1bhdr=6 ! Time
   integer(i_kind),parameter:: n2bhdr=2 ! lat, lon
   integer(i_kind),parameter:: n3bhdr=2 ! dir, spd
   integer(i_kind),parameter:: n4bhdr=2 ! height, temp
   character(len=8) :: acrn
   equivalence(bfr0bhdr, acrn)
   real(r_kind),dimension(n0bhdr):: bfr0bhdr
   real(r_kind),dimension(n1bhdr):: bfr1bhdr
   real(r_kind),dimension(n2bhdr):: bfr2bhdr
   real(r_kind),dimension(n3bhdr):: bfr3bhdr
   real(r_kind),dimension(n4bhdr):: bfr4bhdr
   character(80) hdr0b
   character(80) hdr1b
   character(80) hdr2b
   character(80) hdr3b
   character(80) hdr4b1, hdr4b2

   real, parameter   :: bufr_missing=1.E11, lr_missing=-888888.

   integer(i_kind) iret,idate,nchan
   character(8) subset,subfgn

   character(len=80) :: title
   integer :: iargc

   data hdr0b  /'ACRN'/
   data hdr1b  /'YEAR MNTH DAYS HOUR MINU SECO'/
   data hdr2b  /'CLATH CLONH'/
   data hdr3b  /'WDIR WSPD'/
   data hdr4b1 /'HMSL TMDBST'/
   data hdr4b2 /'FLVLST TMDB'/

   subfgn="FN005000"

   if(iargc()>=1)then
      call getarg(1, title)
   else
      title=""
   endif
   open(use_unit ,file="amdar.little_r",status="unknown")
   open(csv_unit ,file="amdar.csv",status="unknown")
   write(csv_unit,"(A)") trim(title)
   write(csv_unit,"(6(A6,','),7(A10,','),A10)") "year","month","day","hour","minute","second","lat","lon","hgt","dir","spd","tmp","acrn","subset"

   open(lnbufr,  file="amdar.bufr",        status="old",form="unformatted")
   open(lntabl,  file="AMDAR.DXT" ,        status="old")

   write(*,*) "openbf..."
   call openbf(lnbufr,"IN",lntabl)
   write(*,*) "set datelen..."
   call datelen(10)

   tcount=0
   obs: do while (ireadmg(lnbufr,subset,idate)==0 ) 
      write(*,*) "subset,idate: ", subset,idate
      do while (ireadsb(lnbufr)==0)
         ! 1.0     Read header record and data record
         call ufbint(lnbufr,bfr0bhdr,n0bhdr,1,iret,hdr0b)
         call ufbint(lnbufr,bfr1bhdr,n1bhdr,1,iret,hdr1b)
         call ufbint(lnbufr,bfr2bhdr,n2bhdr,1,iret,hdr2b)
         call ufbint(lnbufr,bfr3bhdr,n3bhdr,1,iret,hdr3b)
         if(acrn(1:2)=="CN")then
         call ufbint(lnbufr,bfr4bhdr,n4bhdr,1,iret,hdr4b1)
         else
         call ufbint(lnbufr,bfr4bhdr,n4bhdr,1,iret,hdr4b2)
         endif
         if(abs(bfr1bhdr(6)-bufr_missing)<10.)then ! second
            bfr1bhdr(6)=0      
         endif
         do i=1, n2bhdr
            if(abs(bfr2bhdr(i)-bufr_missing)<10.)then
               bfr2bhdr(i)=lr_missing      
            endif
         enddo
         do i=1, n3bhdr
            if(abs(bfr3bhdr(i)-bufr_missing)<10.)then
               bfr3bhdr(i)=lr_missing      
            endif
         enddo
         do i=1, n4bhdr
            if(abs(bfr4bhdr(i)-bufr_missing)<10.)then
               bfr4bhdr(i)=lr_missing      
            endif
         enddo
         write(csv_unit,"(6(I6,','),6(F10.2,','),A10,',',A10)") &
               int(bfr1bhdr(1)), int(bfr1bhdr(2)), int(bfr1bhdr(3)), int(bfr1bhdr(4)), int(bfr1bhdr(5)), int(bfr1bhdr(6)), &
               bfr2bhdr(1), bfr2bhdr(2), bfr4bhdr(1), bfr3bhdr(1), bfr3bhdr(2), bfr4bhdr(2), acrn, subset
         tcount=tcount+1
         call write_little_r_obs(use_unit, bfr1bhdr(1), bfr1bhdr(2), bfr1bhdr(3), bfr1bhdr(4), bfr1bhdr(5), bfr1bhdr(6), &
                                 bfr2bhdr(1), bfr2bhdr(2), bfr4bhdr(1), bfr3bhdr(1), bfr3bhdr(2), bfr4bhdr(2), acrn, title)
      enddo
   enddo obs
   write(*,*) "write ", tcount, " obs."
   call closbf(lnbufr)
   close(lnbufr)
   close(lntabl)
   close(use_unit)
end program

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine write_little_r_obs (ounit, ryear, rmonth, rday, rhour, rminute, rsecond, lat, lon, hgt, dir, spd, tmp, acrn, title)
   implicit none

   integer, intent(in) :: ounit
   real(kind=8),    intent(in) :: ryear, rmonth, rday, rhour, rminute, rsecond, lat, lon, hgt, dir, spd, tmp
   character(len=*), intent(in) :: acrn, title
   
   integer :: year, month, day, hour, minute, second
   integer :: i

   real, parameter   :: lr_missing = -888888. , lr_end_data=-777777.
   character(len=84) ::  rpt_format
   character(len=22) ::  meas_format
   character(len=14) ::  end_format
   character(len=40) :: id, platform, name, source

   character(len=20) :: date_char

   rpt_format =  ' ( 2f20.5 , 2a40 , '                   &
                // ' 2a40 , 1f20.5 , 5i10 , 3L10 , '     &
                // ' 2i10 , a20 ,  13( f13.5 , i7 ) ) '
   meas_format =  ' ( 10( f13.5 , i7 ) ) '
   end_format = ' ( 3 ( i7 ) ) '

   year  =ryear  
   month =rmonth 
   day   =rday   
   hour  =rhour  
   minute=rminute
   second=rsecond

   id=acrn
   name=acrn
   platform="FM-42 AMDAR"
   source="AMDAR BUFR"
   if(len_trim(title)>0) source=title
   write(date_char,"(6X,I4.4,5I2.2)" ) year, month, day, hour, minute, second
   ! header:
   WRITE ( UNIT = ounit    , FMT = rpt_format )                             &
           lat             , lon        , id           , name             , &
           platform        , source     , lr_missing   , 6                , &
           0               , 0          , 1            , 0                , &
           .true.          , .false.    , .false.      , int(lr_missing)  , &
           int(lr_missing) , date_char  , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0

   ! report:
   WRITE ( UNIT = ounit  , FMT = meas_format )                &
           lr_missing , 0 , hgt        , 0 , tmp        , 0 , &
           lr_missing , 0 , spd        , 0 , dir        , 0 , &
           lr_missing , 0 , lr_missing , 0 , lr_missing , 0 , lr_missing , 0
   

   ! end of report line:
   WRITE ( UNIT = ounit, FMT = meas_format )                    &
           lr_end_data , 0 , lr_end_data , 0 , 6.         , 0 , &
           lr_missing  , 0 , lr_missing  , 0 , lr_missing , 0 , &
           lr_missing  , 0 , lr_missing  , 0 , lr_missing , 0 , &
           lr_missing  , 0

   ! end of message line:
   WRITE ( UNIT = ounit  , FMT = end_format )  6, 0, 0

   end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

