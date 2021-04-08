!   This is the template source file for
!       https://github.com/LiuGangKingston/FORTRAN-CSV-TIKZ.git
!            Version 1.0
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
module someimportantdata
    implicit none
    real*8,  parameter :: pi=3.1415926d0
    real*8,  parameter :: rad2deg = 180/pi
    real*8,  parameter :: deg2rad = pi/180
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
       k = mod(i-1, numberofcolors) + 1
       if(k .le. 0) k = 1
       pickcolor = colors(k)
       return
    end function pickcolor


    function picktikzcolor(i)
       implicit none
       integer  :: i, k
       character (len=lengthoftikzcolors) :: picktikzcolor
       k = mod(i-1, numberoftikzcolors) + 1
       if(k .le. 0) k = 1
       picktikzcolor = tikzcolors(k)
       return
    end function picktikzcolor


    function picktypicalcolor(i)
       implicit none
       integer  :: i, k
       character (len=lengthoftypicalcolors) :: picktypicalcolor
       k = mod(i-1, numberoftypicalcolors) + 1
       if(k .le. 0) k = 1
       picktypicalcolor = typicalcolors(k)
       return
    end function picktypicalcolor

end module someimportantdata


program secondstep
    implicit none
    call mycomputing()
    stop
end program secondstep




subroutine mycomputing()
    use someimportantdata
    implicit double precision (a-z)

! Specific calculation to generate CSV files
! Specific calculation to generate CSV files

    integer :: i, totallines

    open(31, file='setup.scalars.csv')
    open(32, file='iterated.alldata.csv')


    !write(31,"(a)")'totallines,refractiveindex,bigradius,a,b,z,anglez,c,anglea'
    !write(31,"(1x,i10,',',7(f20.8, ','),e20.8)") totallines,refractiveindex,bigradius,a,b,z,anglez,c,anglea

    !write(32,"(a)")'totallines,i,refractiveindex,bigradius,a,b,z,anglez,c,anglea,incidentangle,'//&
    !              &'refractiveangle,anglede,dx,ee,et,ex,ey,anglece,angleced,outangle,mycolor'
    do i = 1, totallines
       !write(32,"(1x,2(i10,','),19(f20.8, ','),a)") &
       !        &totallines,i,refractiveindex,bigradius,a,b,z,anglez,c,anglea,incidentangle, &
       !        &refractiveangle,anglede,dx,ee,et,ex,ey,anglece,angleced,outangle,picktypicalcolor(i)

    end do


    close(31)
    close(32)


    return
end subroutine mycomputing



