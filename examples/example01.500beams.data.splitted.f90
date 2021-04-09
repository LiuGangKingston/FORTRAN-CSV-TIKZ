!   This is the example01.100beams source file for
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
!   We only coded the area after
!      "Specific calculation to generate CSV files"
!   from the "template.f90".
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

    integer :: i, totallines, j, k, u

    open(31, file='setup.scalars.csv')
    open(40, file='iterated.alldata.0.csv')
    open(41, file='iterated.alldata.1.csv')
    open(42, file='iterated.alldata.2.csv')
    open(43, file='iterated.alldata.3.csv')
    open(44, file='iterated.alldata.4.csv')
    open(45, file='iterated.alldata.5.csv')
    open(46, file='iterated.alldata.6.csv')
    open(47, file='iterated.alldata.7.csv')
    open(48, file='iterated.alldata.8.csv')
    open(49, file='iterated.alldata.9.csv')

    refractiveindex=1.5d0
    bigradius=8.0d0
    a=3.0d0
    b=3.0d0
    z=sqrt(bigradius**2-b**2)
    anglez=asin(b/bigradius)*rad2deg
    c=-sqrt(bigradius**2-(a+b)**2)
    anglea=acos(c/bigradius)*rad2deg
    totallines = 500
    j = totallines/10

    write(31,"(a)")'totallines,refractiveindex,bigradius,a,b,z,anglez,c,anglea'
    write(31,"(1x,i10,',',7(f20.8, ','),e20.8)") totallines,refractiveindex,bigradius,a,b,z,anglez,c,anglea

    do i = 1, totallines
       k = (i-1) / j
       u = 40 + k
       if ((i - k*j) .eq. 1) &
       write(u,"(a)")'totallines,i,refractiveindex,bigradius,a,b,z,anglez,c,anglea,incidentangle,'//&
                    &'refractiveangle,anglede,dx,ee,et,ex,ey,anglece,angleced,outangle,mycolor'
       incidentangle=3.0d0 + 0.1d0*i
       refractiveangle=asin(sin(incidentangle*deg2rad)/refractiveindex)*rad2deg
       anglede=180-refractiveangle-anglea
       ! x component of D position
       dx=a/tan(anglede*deg2rad) + c

       ! To find E position by solving equations, with t and et as DE length:
       !  (t sin(anglede) + \b)^2 + (t cos(anglede) + \dx )^2 = 64
       !  t^2 + 2 (\b sin + \dx cos ) + (\b sin + \dx cos )^2
       !  = 64 - \b^2 - \dx^2 + (\b sin +  \dx cos  )^2
       !  t = sqrt ((\b sin + \dx cos )^2 +  64 - \b^2 - \dx^2 )  - (\b sin + \dx cos )

       ee=b*sin(anglede*deg2rad) + dx*cos(anglede*deg2rad)
       et=sqrt(ee**2 + bigradius**2 -b**2 - dx**2) - ee

       ! x and y components of E position
       ex=et*cos(anglede*deg2rad) + dx
       ey=et*sin(anglede*deg2rad) + b
       anglece=acos(ex/sqrt(ex**2+ey**2))*rad2deg
       angleced=anglece-anglede
       outangle=asin(sin(angleced*deg2rad) * refractiveindex)*rad2deg

       write(u,"(1x,2(i10,','),19(f20.8, ','),a)") &
               &totallines,i,refractiveindex,bigradius,a,b,z,anglez,c,anglea,incidentangle, &
               &refractiveangle,anglede,dx,ee,et,ex,ey,anglece,angleced,outangle,picktikzcolor(i)

    end do


    close(31)
    close(40)
    close(41)
    close(42)
    close(43)
    close(44)
    close(45)
    close(46)
    close(47)
    close(48)
    close(49)


    return
end subroutine mycomputing

