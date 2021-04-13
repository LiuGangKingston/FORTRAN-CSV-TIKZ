!   This is the template source file for
!       https://github.com/LiuGangKingston/FORTRAN-CSV-TIKZ.git
!            Version 2.0
!   free for non-commercial use.
!   Please send us emails for any problems/suggestions/comments.
!   Please be advised that none of us accept any responsibility
!   for any consequences arising out of the usage of this
!   software, especially for damage.
!   For usage, please refer to the README file.
!   This code was written by
!        Gang Liu (gl.cell@outlook)
!                 (http://orcid.org/0000-0003-1575-9290)
!          and
!        Shiwei Huang (huang937@gmail.com)
!   Copyright (c) 2021
!
!   To use this, you can add your code at the
!   "Specific calculation to generate CSV files" area in the
!   "subroutine mycomputing()" at the end of this file.
!
!
module somebasicdataandroutines
    implicit none
    real*8,  parameter :: pi=3.1415926d0
    real*8,  parameter :: rad2deg = 180/pi
    real*8,  parameter :: deg2rad = pi/180
    integer, parameter :: minimumstartingfileunit = 30
    integer, parameter :: maximumfileunit = 100
    integer, parameter :: numberofcolors = 68
    integer, parameter :: lengthofcolors = 16
    character (len=lengthofcolors), parameter :: colors(numberofcolors) =  (/&
              &'Apricot         ','Aquamarine      ','Bittersweet     ','Black           ', &
              &'Blue            ','BlueGreen       ','BlueViolet      ','BrickRed        ', &
              &'Brown           ','BurntOrange     ','CadetBlue       ','CarnationPink   ', &
              &'Cerulean        ','CornflowerBlue  ','Cyan            ','Dandelion       ', &
              &'DarkOrchid      ','Emerald         ','ForestGreen     ','Fuchsia         ', &
              &'Goldenrod       ','Gray            ','Green           ','GreenYellow     ', &
              &'JungleGreen     ','Lavender        ','LimeGreen       ','Magenta         ', &
              &'Mahogany        ','Maroon          ','Melon           ','MidnightBlue    ', &
              &'Mulberry        ','NavyBlue        ','OliveGreen      ','Orange          ', &
              &'OrangeRed       ','Orchid          ','Peach           ','Periwinkle      ', &
              &'PineGreen       ','Plum            ','ProcessBlue     ','Purple          ', &
              &'RawSienna       ','Red             ','RedOrange       ','RedViolet       ', &
              &'Rhodamine       ','RoyalBlue       ','RoyalPurple     ','RubineRed       ', &
              &'Salmon          ','SeaGreen        ','Sepia           ','SkyBlue         ', &
              &'SpringGreen     ','Tan             ','TealBlue        ','Thistle         ', &
              &'Turquoise       ','Violet          ','VioletRed       ','White           ', &
              &'WildStrawberry  ','Yellow          ','YellowGreen     ','YellowOrange    ' /)
    integer, parameter :: numberoftikzcolors = 19
    integer, parameter :: lengthoftikzcolors = 16
    character (len=lengthoftikzcolors), parameter :: tikzcolors(numberoftikzcolors) =  (/&
              &'red             ','purple          ','magenta         ','pink            ', &
              &'violet          ','white           ','orange          ','yellow          ', &
              &'green           ','lime            ','brown           ','olive           ', &
              &'blue            ','cyan            ','teal            ','lightgray       ', &
              &'gray            ','darkgray        ','black           ' /)
    integer, parameter :: numberoftypicalcolors = 5
    integer, parameter :: lengthoftypicalcolors = 16
    character (len=lengthoftypicalcolors), parameter :: typicalcolors(numberoftypicalcolors) = (/&
              &'red             ','orange          ','yellow          ','green           ', &
              &'blue            ' /)


contains


    function pickcolor(i)
       implicit none
       integer  :: i, k
       character (len=lengthofcolors) :: pickcolor
       k = abs(mod(i-1, numberofcolors)) + 1
       pickcolor = colors(k)
       return
    end function pickcolor


    function picktikzcolor(i)
       implicit none
       integer  :: i, k
       character (len=lengthoftikzcolors) :: picktikzcolor
       k = abs(mod(i-1, numberoftikzcolors)) + 1
       picktikzcolor = tikzcolors(k)
       return
    end function picktikzcolor


    function picktypicalcolor(i)
       implicit none
       integer  :: i, k
       character (len=lengthoftypicalcolors) :: picktypicalcolor
       k = abs(mod(i-1, numberoftypicalcolors)) + 1
       picktypicalcolor = typicalcolors(k)
       return
    end function picktypicalcolor


    function integer_to_character(i,length)
       implicit none
       character*20 :: integer_to_character
       integer :: i,length,j,k,l

       length=0
       integer_to_character=' '
       if(i.lt.0) then
          length=1
          integer_to_character(1:1)='-'
       end if

       j=abs(i)
       k=0
       loop: do
          k=k+1
          j=j/10
          if(j.eq.0) exit loop
       end do loop
       length=length+k

       j=abs(i)
       do l=1,k
          integer_to_character(length+1-l:length+1-l)=char(mod(j,10)+48)
          j=j/10
       end do

       return
    end function integer_to_character


    function totalsplitfileneeded(totalelements, filesize)
       implicit none
       integer  :: totalsplitfileneeded, totalelements, filesize
       totalsplitfileneeded = 1
       if(totalelements .le. 0) then
          print*, 'This is in the "function totalsplitfileneeded(totalelements, filesize)"'
          print*, '        the value of "totalelements" is ', totalelements, ', not positive.'
          print*, '        Not reasonable. Then stopped.'
          stop
       end if
       if(filesize .le. 0) then
          print*, 'This is in the "function totalsplitfileneeded(totalelements, filesize)"'
          print*, '        the value of "filesize" is ', filesize, ', not positive.'
          print*, '        Not reasonable. Then stopped.'
          stop
       end if
       if(totalelements .gt. 0) then
          totalsplitfileneeded = (totalelements - 1)/filesize + 1
       end if
       return
    end function totalsplitfileneeded


    subroutine groupfileopenwithunits(filenameprefix,startingunit,totalfiles)
       implicit none
       character (len=*), intent(in) :: filenameprefix
       integer, intent(in) ::  startingunit,totalfiles
       integer :: i,j,k,l
       logical :: ex
       if(startingunit .lt. minimumstartingfileunit) then
          print*, 'This is in the "groupfileopenwithunits(filenameprefix,startingunit,totalfiles)"'
          print*, '        the value of "startingunit" is ', startingunit, ', being less than ', &
                          & minimumstartingfileunit, ' .'
          print*, '        This code does not support such. Then stopped.'
          stop
       end if
       if(totalfiles .le. 0) then
          print*, 'This is in the "groupfileopenwithunits(filenameprefix,startingunit,totalfiles)"'
          print*, '        the value of "filesize" is ', totalfiles, ', not positive.'
          print*, '        Not reasonable. Then stopped.'
          stop
       end if
       if(startingunit + totalfiles .gt. maximumfileunit) then
          print*, 'This is in the "groupfileopenwithunits(filenameprefix,startingunit,totalfiles)"'
          print*, '        the value of "startingunit + totalfiles" is ', startingunit + totalfiles, &
                 &',       greater than ', maximumfileunit, ' .'
          print*, '        This code does not support such. Then stopped.'
          stop
       end if
       do i = 1, totalfiles
          j = startingunit + i - 1
          inquire(j,opened=ex)
          if(ex) then
             print*, 'This is in the "groupfileopenwithunits(filenameprefix,startingunit,totalfiles)"'
             print*, '        the unit number ', j, ' is being used now, which can not be used to open file:'
             print*, '        '//filenameprefix//trim(integer_to_character(i,l))//'.csv'
             print*, '        Then stopped.'
             stop
          else
              open(j, file = filenameprefix//trim(integer_to_character(i,l))//'.csv')
          end if
       end do
       return
    end subroutine groupfileopenwithunits


    function pickunit(startingunit, filesize, totalfiles, startingelement, element)
       implicit none
       integer  ::    startingunit, filesize, totalfiles, startingelement, element
       integer  ::    pickunit, i, j, k
       if(startingunit .lt. minimumstartingfileunit) then
          print*, 'This is in the "pickunit(startingunit, filesize, totalfiles, startingelement, element)"'
          print*, '        the value of "startingunit" is ', startingunit, ', being less than ', &
                          & minimumstartingfileunit, ' .'
          print*, '        This code does not support such. Then stopped.'
          stop
       end if
       if(filesize .le. 0) then
          print*, 'This is in the "pickunit(startingunit, filesize, totalfiles, startingelement, element)"'
          print*, '        the value of "filesize" is ', filesize, ', not positive.'
          print*, '        Not reasonable. Then stopped.'
          stop
       end if
       if(totalfiles .le. 0) then
          print*, 'This is in the "pickunit(startingunit, filesize, totalfiles, startingelement, element)"'
          print*, '        the value of "filesize" is ', totalfiles, ', not positive.'
          print*, '        Not reasonable. Then stopped.'
          stop
       end if
       if(startingunit + totalfiles .gt. maximumfileunit) then
          print*, 'This is in the "pickunit(startingunit, filesize, totalfiles, startingelement, element)"'
          print*, '        the value of "startingunit + totalfiles" is ', startingunit + totalfiles, &
                 &',       greater than ', maximumfileunit, ' .'
          print*, '        This code does not support such. Then stopped.'
          stop
       end if
       k = abs(element - startingelement) + 1
       j = (k - 1) / filesize + 1
       if(j .gt. totalfiles) then
          print*, 'This is in the "pickunit(startingunit, filesize, totalfiles, startingelement, element)"'
          print*, '        the new caculated file number is ', j, ' greater than the "totalfiles": ', totalfiles, ' .'
          print*, '        This should not happen. Then stopped.'
          stop
       end if
       pickunit = startingunit + j - 1
       return
    end function pickunit


    subroutine firstlinetogroupfiles(startingunit,totalfiles,firstlinewords)
       implicit none
       character (len=*), intent(in) :: firstlinewords
       integer, intent(in) ::  startingunit,totalfiles
       integer :: i,j,k,l
       if(startingunit .lt. minimumstartingfileunit) then
          print*, 'This is in the "firstlinetogroupfiles(startingunit,totalfiles,firstlinewords)"'
          print*, '        the value of "startingunit" is ', startingunit, ', being less than ', &
                          & minimumstartingfileunit, ' .'
          print*, '        This code does not support such. Then stopped.'
          stop
       end if
       if(totalfiles .le. 0) then
          print*, 'This is in the "firstlinetogroupfiles(startingunit,totalfiles,firstlinewords)"'
          print*, '        the value of "filesize" is ', totalfiles, ', not positive.'
          print*, '        Not reasonable. Then stopped.'
          stop
       end if
       if(startingunit + totalfiles .gt. maximumfileunit) then
          print*, 'This is in the "firstlinetogroupfiles(startingunit,totalfiles,firstlinewords)"'
          print*, '        the value of "startingunit + totalfiles" is ', startingunit + totalfiles, &
                 &',       greater than ', maximumfileunit, ' .'
          print*, '        This code does not support such. Then stopped.'
          stop
       end if
       do i = 1, totalfiles
           write(startingunit+i-1, '(a)') trim(firstlinewords)
       end do
       return
    end subroutine firstlinetogroupfiles


    subroutine groupfileclosewithunits(startingunit,totalfiles)
       implicit none
       integer, intent(in) ::  startingunit,totalfiles
       integer :: i,j,k,l
       if(startingunit .lt. minimumstartingfileunit) then
          print*, 'This is in the "groupfileclosewithunits(startingunit,totalfiles)"'
          print*, '        the value of "startingunit" is ', startingunit, ', being less than ', &
                          & minimumstartingfileunit, ' .'
          print*, '        This code does not support such. Then stopped.'
          stop
       end if
       if(totalfiles .le. 0) then
          print*, 'This is in the "groupfileclosewithunits(startingunit,totalfiles)"'
          print*, '        the value of "filesize" is ', totalfiles, ', not positive.'
          print*, '        Not reasonable. Then stopped.'
          stop
       end if
       if(startingunit + totalfiles .gt. maximumfileunit) then
          print*, 'This is in the "groupfileclosewithunits(startingunit,totalfiles)"'
          print*, '        the value of "startingunit + totalfiles" is ', startingunit + totalfiles, &
                 &',       greater than ', maximumfileunit, ' .'
          print*, '        This code does not support such. Then stopped.'
          stop
       end if
       do i = 1, totalfiles
           close(startingunit+i-1)
       end do
       return
    end subroutine groupfileclosewithunits


end module somebasicdataandroutines




program secondstep
    implicit none
    call mycomputing()
    stop
end program secondstep




subroutine mycomputing()
    use somebasicdataandroutines
    implicit none

! Specific calculation to generate CSV files
! Specific calculation to generate CSV files

    !integer :: i, j, k, u, totallines, startingunitforsplitfiles, datalinesineachfile, totalfiles
    !double precision :: refractiveindex,bigradius,a,b,z,anglez,c,anglea,incidentangle, &
    !                   &refractiveangle,anglede,dx,ee,et,ex,ey,anglece,angleced,outangle

    !totallines = 500
    !datalinesineachfile = 50
    !startingunitforsplitfiles = 30
    !totalfiles = totalsplitfileneeded(totallines, datalinesineachfile)

    !open(21, file='setup.scalars.csv')
    !write(21,"(a)")'totallines,refractiveindex,bigradius,a,b,z,anglez,c,anglea'
    !write(21,"(1x,i10,',',7(f20.8, ','),e20.8)") totallines,refractiveindex,bigradius,a,b,z,anglez,c,anglea
    !close(21)

    !call groupfileopenwithunits('iterated.alldata.',startingunitforsplitfiles,totalfiles)
    !call firstlinetogroupfiles(startingunitforsplitfiles,totalfiles,'totallines,i,refractiveindex,bigradius,'//&
    !                                          &'a,b,z,anglez,c,anglea,incidentangle,refractiveangle,anglede,'//&
    !                                          &'dx,ee,et,ex,ey,anglece,angleced,outangle,mycolor')
    !do i = 1, totallines







       !u = pickunit(startingunitforsplitfiles, datalinesineachfile, totalfiles, 1, i)
       !write(u,"(1x,2(i10,','),19(f20.8, ','),a)") &
       !        &totallines,i,refractiveindex,bigradius,a,b,z,anglez,c,anglea,incidentangle, &
       !        &refractiveangle,anglede,dx,ee,et,ex,ey,anglece,angleced,outangle,picktikzcolor(i)

    !end do

    !call groupfileclosewithunits(startingunitforsplitfiles,totalfiles)

    return
end subroutine mycomputing



