!   This is the template source file for
!       https://github.com/LiuGangKingston/FORTRAN-CSV-TIKZ.git
!            Version 2.2
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
module fortrancsvtikzbasics
    implicit none
    real*8,  parameter :: pi = 3.14159265358979323846d0
    real*8,  parameter :: rad2deg = 180/pi
    real*8,  parameter :: deg2rad = pi/180
    real*8,  parameter :: napierconstant = 2.71828182845904523536d0
    real*8,  parameter :: eulernumber = napierconstant

    real*8,  parameter :: accelerationduetoearthgravity = 9.80D0               !"m/s$^2$"
    real*8,  parameter :: atomicmassconstant            = 1.66053906660D-27    !kg
    real*8,  parameter :: avogadronumber                = 6.02214076D23        !"mol$^{-1}$"
    real*8,  parameter :: bohrmagneton                  = 9.2740100783D-24     !"J/T"
    real*8,  parameter :: bohrradius                    = 5.29177210903D-11    !m
    real*8,  parameter :: boltzmannconstant             = 1.380649D-23         !"J/K"
    real*8,  parameter :: classicalelectronradius       = 2.8179403262D-15     !m
    real*8,  parameter :: coulombconstant               = 8.9875517923D9       !"N$\cdot $m$^2$/C$^2$"
    real*8,  parameter :: elementarycharge              = 1.602176634D-19      !"C"
    real*8,  parameter :: faradayconstant               = 9.648533212D4        !C/mol
    real*8,  parameter :: finestructureconstant         = 7.2973525693D-3      !
    real*8,  parameter :: firstradiationconstant        = 3.741771852D-16      !W$\dot m^2$
    real*8,  parameter :: massofelectron                = 9.1093837015D-31     !"kg"
    real*8,  parameter :: massofneutron                 = 1.67492749804D-27    !"kg"
    real*8,  parameter :: massofproton                  = 1.67262192369D-27    !"kg"
    real*8,  parameter :: nuclearmagneton               =  5.0507837461D-27    !"J/T"
    real*8,  parameter :: planckconstant                = 6.62607015D-34       !"J$\cdot $s"
    real*8,  parameter :: rydbergconstant               = 1.0973731568160D7    !1/m
    real*8,  parameter :: secondradiationconstant       = 1.438776877D-2       !m$\dot K$
    real*8,  parameter :: speedoflightinvacuum          = 2.99792458D+8        !"m/s"
    real*8,  parameter :: thomsoncrosssection           = 6.6524587321D-29     ! $m^2$
    real*8,  parameter :: universalgasconstant          = 8.314462618D0        !"J/(mol$\cdot $K)"
    real*8,  parameter :: universalgravitationalconst   = 6.67430D-11          !"N$\cdot $m$^2$/kg$^2$"
    real*8,  parameter :: vacuumelectricpermittivity    = 8.8541878128D-12     !"F/m"
    real*8,  parameter :: vacuummagneticpermeability    = 1.25663706212D-6     !"N/$A^2$"

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

end module fortrancsvtikzbasics


module fortrancsvtikzgroupfiles
    use fortrancsvtikzbasics
    implicit none
    character(len=*), parameter          :: fortrancsvtikzfileextension='.csv'
    integer, parameter                   :: fortrancsvtikzminimumfileunit = 30
    integer, parameter                   :: fortrancsvtikzmaximumfileunit = 100
    integer, parameter                   :: fortrancsvtikzgroupinforwidth = 10
    integer, private                     :: fortrancsvtikzprefixsize
    integer, private                     :: fortrancsvtikzprefixused
    integer, private                     :: fortrancsvtikzgroupsize
    integer, private                     :: fortrancsvtikztotalgroups
    integer, private, allocatable        :: fortrancsvtikzfilegroupinfor(:,:)
    character(len=1),private,allocatable :: fortrancsvtikzfilenameprefixes(:)

!   The array fortrancsvtikzfilenameprefixes is used for all file prefix names.
!   The array fortrancsvtikzfilegroupinfor is used for informations about every file groups.
!   If fortrancsvtikzfilegroupinfor(groupnumber,1) is 1, the group is being used.
!      fortrancsvtikzfilegroupinfor(groupnumber,2) is the start position of the prefix name in the above character array.
!      fortrancsvtikzfilegroupinfor(groupnumber,3) is the final position of the prefix name in the above character array.
!      fortrancsvtikzfilegroupinfor(groupnumber,4) is the starting unit number.
!      fortrancsvtikzfilegroupinfor(groupnumber,5) is the total lines to be output in each new file.
!      fortrancsvtikzfilegroupinfor(groupnumber,6) is the total number of files to be opened.
!      fortrancsvtikzfilegroupinfor(groupnumber,7) is the starting line (startingline).
!      fortrancsvtikzfilegroupinfor(groupnumber,8) is the ending line (endingline).
!      fortrancsvtikzfilegroupinfor(groupnumber,9) is 1, or -1 if (endingline .lt. startingline).
!      fortrancsvtikzfilegroupinfor(groupnumber,10) is abs(endingline - startingline).


contains

    subroutine fortrancsvtikzgroupinitialize()
        implicit none
        fortrancsvtikzprefixsize = 20
        fortrancsvtikzprefixused = 0
        fortrancsvtikzgroupsize  = 20
        fortrancsvtikztotalgroups = 0
        allocate(fortrancsvtikzfilenameprefixes(fortrancsvtikzprefixsize))
        allocate(fortrancsvtikzfilegroupinfor(fortrancsvtikzgroupsize,fortrancsvtikzgroupinforwidth))
        return
    end subroutine fortrancsvtikzgroupinitialize


    subroutine fortrancsvtikzgroupfinalize()
        implicit none
        integer :: i
        do i = 1, fortrancsvtikztotalgroups
            call filegroupclose(i)
        end do
        if (allocated(fortrancsvtikzfilegroupinfor))   deallocate(fortrancsvtikzfilegroupinfor)
        if (allocated(fortrancsvtikzfilenameprefixes)) deallocate(fortrancsvtikzfilenameprefixes)
        fortrancsvtikzprefixsize = 0
        fortrancsvtikzprefixused = 0
        fortrancsvtikzgroupsize  = 0
        fortrancsvtikztotalgroups = 0
        return
    end subroutine fortrancsvtikzgroupfinalize


    subroutine filegroupsetupandopen(groupnumber,filenameprefix,startingunit,startingline,endingline,linesineachfile)
       implicit none
       character (len=*), intent(in) :: filenameprefix
       integer,           intent(in) :: groupnumber,startingunit,startingline,endingline,linesineachfile
       integer,          allocatable :: infortemp(:,:)
       character(len=1), allocatable :: pretemp(:)
       character (len=len(filenameprefix)):: at
       integer :: i,j,k,l,n,totalfiles,inforextent,preextent
       logical :: ex, samestring

       if((groupnumber .le. 0) .or. (groupnumber .gt. (fortrancsvtikztotalgroups+1))) then
          print*, 'In the "filegroupsetupandopen(groupnumber,filenameprefix,startingunit,...,linesineachfile)"'
          print*, '        the value of "groupnumber" can only be a positive integer: 1, 2, 3, ... in sequence.'
          print*, '        Here it can only be the (next) one, which is ', fortrancsvtikztotalgroups+1
          if(fortrancsvtikztotalgroups .gt. 0) &
          print*, '        or one used earlier, which means less than ', fortrancsvtikztotalgroups+1
          print*, '        Since you are using ', groupnumber, ' , this code run stopped.'
          call finalize()
          stop
       else if(groupnumber .le. fortrancsvtikztotalgroups) then
          if(fortrancsvtikzfilegroupinfor(groupnumber,1) .eq. 1) then
          print*, 'In the "filegroupsetupandopen(groupnumber,filenameprefix,startingunit,...,linesineachfile)"'
          print*, '        Since the "groupnumber" ', groupnumber, ' is being used now, you can not use it. '
          print*, '        The next one is ', fortrancsvtikztotalgroups+1
          print*, '        For the above reason, this code run stopped.'
          call finalize()
          stop
          end if
       end if

       at = ' '
       at = adjustl(filenameprefix)
       l = len(at)
       if(l.le.0) then
          print*, 'In the "filegroupsetupandopen(groupnumber,filenameprefix,startingunit,...,linesineachfile)"'
          print*, '        with the "groupnumber" ', groupnumber
          print*, '        Since the filenameprefix is empty, this code run stopped.'
          call finalize()
          stop
       end if

       do i = 1, fortrancsvtikztotalgroups
       if(i .ne. groupnumber) then
           j = fortrancsvtikzfilegroupinfor(i,2)
           k = fortrancsvtikzfilegroupinfor(i,3)
           if(k-j+1.eq.l) then
              samestring = .true.
              do n = 1, l
                 if(at(n:n) .ne. fortrancsvtikzfilenameprefixes(j+n-1)) samestring = .false.
              end do
              if(samestring) then
                 print*, 'In the "filegroupsetupandopen(groupnumber,filenameprefix,startingunit,...,linesineachfile)"'
                 print*, '        with the "groupnumber" ', groupnumber
                 print*, '        the filenameprefix: "'//at(1:l)//'" was used in previous group number: ', i
                 print*, '        Although just a WARNING, maybe you are trying to overwrite exsisting file(s).'
                 print*, '        Although just a WARNING, maybe you are trying to overwrite exsisting file(s).'
                 print*, '        Although just a WARNING, maybe you are trying to overwrite exsisting file(s).'
              end if
           end if
       end if
       end do

       if(startingunit .lt. fortrancsvtikzminimumfileunit) then
          print*, 'In the "filegroupsetupandopen(groupnumber,filenameprefix,startingunit,...,linesineachfile)"'
          print*, '        with the "groupnumber" ', groupnumber
          print*, '        the value of "startingunit" is ', startingunit, ', being less than ', &
                          & fortrancsvtikzminimumfileunit, ' .'
          print*, '        This code does not support such. Then stopped.'
          call finalize()
          stop
       end if

       if(linesineachfile .le. 0) then
          print*, 'In the "filegroupsetupandopen(groupnumber,filenameprefix,startingunit,...,linesineachfile)"'
          print*, '        with the "groupnumber" ', groupnumber
          print*, '        the value of "linesineachfile" is ', linesineachfile, ', not positive.'
          print*, '        Not reasonable. Then stopped.'
          call finalize()
          stop
       end if

       totalfiles = abs(startingline - endingline) / linesineachfile + 1

       if(startingunit + totalfiles .gt. fortrancsvtikzmaximumfileunit) then
          print*, 'In the "filegroupsetupandopen(groupnumber,filenameprefix,startingunit,...,linesineachfile)"'
          print*, '        with the "groupnumber" ', groupnumber
          print*, '        the value of "startingunit + totalfiles" is ', startingunit + totalfiles, &
                 &',       greater than ', fortrancsvtikzmaximumfileunit
          print*, '        which means too many files resulting in too big unit number.'
          print*, '        This code does not support such. Then stopped.'
          call finalize()
          stop
       end if

       preextent = 100
       if((fortrancsvtikzprefixused+l) .gt. fortrancsvtikzprefixsize) then
          allocate(pretemp(fortrancsvtikzprefixsize))
          pretemp =  fortrancsvtikzfilenameprefixes
          deallocate(fortrancsvtikzfilenameprefixes)
          allocate(  fortrancsvtikzfilenameprefixes(fortrancsvtikzprefixsize+l+preextent))
          fortrancsvtikzfilenameprefixes(1:fortrancsvtikzprefixsize) = pretemp(1:fortrancsvtikzprefixsize)
          fortrancsvtikzprefixsize = fortrancsvtikzprefixsize+l+preextent
          deallocate(pretemp)
       end if

       if(groupnumber .gt. fortrancsvtikztotalgroups) fortrancsvtikztotalgroups = groupnumber

       inforextent = 100
       if(groupnumber .gt. fortrancsvtikzgroupsize) then
          allocate(infortemp(fortrancsvtikzgroupsize,fortrancsvtikzgroupinforwidth))
          infortemp = fortrancsvtikzfilegroupinfor
          deallocate( fortrancsvtikzfilegroupinfor)
          allocate(   fortrancsvtikzfilegroupinfor(fortrancsvtikzgroupsize+inforextent,fortrancsvtikzgroupinforwidth))
                      fortrancsvtikzfilegroupinfor(1:fortrancsvtikzgroupsize,1:fortrancsvtikzgroupinforwidth) = &
                                        &infortemp(1:fortrancsvtikzgroupsize,1:fortrancsvtikzgroupinforwidth)
          fortrancsvtikzgroupsize = fortrancsvtikzgroupsize + inforextent
          deallocate(infortemp)
       end if

       fortrancsvtikzfilegroupinfor(groupnumber,1) = 1
       j = fortrancsvtikzprefixused + 1
       k = fortrancsvtikzprefixused + l
       fortrancsvtikzprefixused = k
       do n = 1, l
          fortrancsvtikzfilenameprefixes(j+n-1) = at(n:n)
       end do
       fortrancsvtikzfilegroupinfor(groupnumber,2) = j
       fortrancsvtikzfilegroupinfor(groupnumber,3) = k
       fortrancsvtikzfilegroupinfor(groupnumber,4) = startingunit
       fortrancsvtikzfilegroupinfor(groupnumber,5) = linesineachfile
       fortrancsvtikzfilegroupinfor(groupnumber,6) = totalfiles
       fortrancsvtikzfilegroupinfor(groupnumber,7) = startingline
       fortrancsvtikzfilegroupinfor(groupnumber,8) = endingline
       fortrancsvtikzfilegroupinfor(groupnumber,9) = 1
       if (endingline .lt. startingline)           &
      &fortrancsvtikzfilegroupinfor(groupnumber,9) = -1
       fortrancsvtikzfilegroupinfor(groupnumber,10)= abs(endingline - startingline)

       do i = 1, totalfiles
          j = startingunit + i - 1
          inquire(j,opened=ex)
          if(ex) then
             print*, 'In the "filegroupsetupandopen(groupnumber,filenameprefix,startingunit,...,linesineachfile)"'
             print*, '        with the "groupnumber" ', groupnumber
             print*, '        the unit number ', j, ' is being used now, which can not be used to open file:'
             print*, '        '//at(1:l)//trim(integer_to_character(i,n))//fortrancsvtikzfileextension
             print*, '        Then stopped.'
             call finalize()
             stop
          else
              open(j, file = at(1:l)//trim(integer_to_character(i,n))//fortrancsvtikzfileextension)
          end if
       end do

       return
    end subroutine filegroupsetupandopen


    function pickunitinafilegroup(groupnumber, linenumber)
       implicit none
       integer, intent(in) :: groupnumber, linenumber
       integer             :: pickunitinafilegroup, i, j, k
       if((groupnumber .le. 0) .or. (groupnumber .gt. fortrancsvtikztotalgroups)) then
          print*, 'In the "function pickunitinafilegroup(groupnumber, linenumber)"'
          print*, '   the value of "groupnumber": ', groupnumber, ' is not available.'
          print*, 'This code run stopped.'
          call finalize()
          stop
       else if(fortrancsvtikzfilegroupinfor(groupnumber,1) .ne. 1) then
          print*, 'In the "function pickunitinafilegroup(groupnumber, linenumber)"'
          print*, '   the "groupnumber" ', groupnumber, ' is not active now. This code run stopped.'
          call finalize()
          stop
       end if

       k = abs(linenumber - fortrancsvtikzfilegroupinfor(groupnumber,7))
       if( ((linenumber-fortrancsvtikzfilegroupinfor(groupnumber,7)) * &
           & fortrancsvtikzfilegroupinfor(groupnumber,9).lt.0) .or. &
           &     (k .gt. fortrancsvtikzfilegroupinfor(groupnumber,10)) ) then
          print*, 'In the "function pickunitinafilegroup(groupnumber, linenumber)" '
          print*, 'with "groupnumber": ', groupnumber
          print*, '   the "linenumber": ', linenumber, ' is not in the range from ', &
                 &fortrancsvtikzfilegroupinfor(groupnumber,7),&
                 &' to ', fortrancsvtikzfilegroupinfor(groupnumber,8)
          print*, 'Then stopped.'
          call finalize()
          stop

       end if

       j = k / fortrancsvtikzfilegroupinfor(groupnumber,5) + 1
       pickunitinafilegroup = fortrancsvtikzfilegroupinfor(groupnumber,4) + j - 1

       return
    end function pickunitinafilegroup


    subroutine firstlinetoafilegroup(groupnumber,firstlinewords)
       implicit none
       integer, intent(in)           :: groupnumber
       character (len=*), intent(in) :: firstlinewords
       integer :: i
       if((groupnumber .le. 0) .or. (groupnumber .gt. fortrancsvtikztotalgroups)) then
          print*, 'In the "firstlinetoafilegroup(groupnumber,firstlinewords)"'
          print*, '   the value of "groupnumber": ', groupnumber, ' is not available. This code run stopped.'
          call finalize()
          stop
       else if(fortrancsvtikzfilegroupinfor(groupnumber,1) .ne. 1) then
          print*, 'In the "firstlinetoafilegroup(groupnumber,firstlinewords)"'
          print*, '   the "groupnumber" ', groupnumber, ' is not active now. This code run stopped.'
          call finalize()
          stop
       end if

       do i = 1, fortrancsvtikzfilegroupinfor(groupnumber,6)
           write(fortrancsvtikzfilegroupinfor(groupnumber,4)+i-1, '(a)') trim(firstlinewords)
       end do

       return
    end subroutine firstlinetoafilegroup


    subroutine filegroupclose(groupnumber)
       implicit none
       integer, intent(in):: groupnumber
       integer :: i, k
       logical :: ex
       if((groupnumber .le. 0) .or. (groupnumber .gt. fortrancsvtikztotalgroups)) then
          print*, 'In the "filegroupclose(groupnumber)"'
          print*, '   the value of "groupnumber": ', groupnumber, ' is not available. This code run stopped.'
          if (allocated(fortrancsvtikzfilegroupinfor))   deallocate(fortrancsvtikzfilegroupinfor)
          if (allocated(fortrancsvtikzfilenameprefixes)) deallocate(fortrancsvtikzfilenameprefixes)
          stop
       end if

       do i = 1, fortrancsvtikzfilegroupinfor(groupnumber,6)
          k = fortrancsvtikzfilegroupinfor(groupnumber,4)+i-1
          inquire(k,opened=ex)
          if(ex) close(k)
       end do
       !fortrancsvtikzfilegroupinfor(groupnumber,1:fortrancsvtikzgroupinforwidth) = 0
       fortrancsvtikzfilegroupinfor(groupnumber,1:1) = 0

       return
    end subroutine filegroupclose

end module fortrancsvtikzgroupfiles


module fortrancsvtikzallmodules
    use fortrancsvtikzbasics
    use fortrancsvtikzgroupfiles
end module fortrancsvtikzallmodules




subroutine initialize()
    use fortrancsvtikzallmodules
    implicit none
    call fortrancsvtikzgroupinitialize()
    return
end subroutine initialize




subroutine finalize()
    use fortrancsvtikzallmodules
    implicit none
    call fortrancsvtikzgroupfinalize()
    return
end subroutine finalize




program fortrancsvtikz
    use fortrancsvtikzallmodules
    implicit none
    call initialize()
    call mycomputing()
    call finalize()
    stop
end program fortrancsvtikz




subroutine mycomputing()
    use fortrancsvtikzallmodules
    implicit none

! Specific calculation to generate CSV files
! Specific calculation to generate CSV files


    !integer :: i, j, k, u, groupnumber, startingunitforsplitfiles, datalinesineachfile, startingline, totallines
    !double precision :: manydoubleprecisionvariables
    
    !groupnumber = 1
    !totallines = 500
    !startingline = 1
    !datalinesineachfile = 50
    !startingunitforsplitfiles = 30

    !...
    !open(21, file='setup.scalars.csv')
    !write(21,"(a)")'correspondingvariablenamesseparatedbycommaswithoutanyotherletters'
    !write(21,"(1x,i10,',',7(f20.8, ','),e20.8)") thevariables
    !close(21)

    !...
    !call filegroupsetupandopen(groupnumber,'iterated.alldata.',startingunitforsplitfiles,startingline,totallines, &
    !                          &datalinesineachfile)
    !call firstlinetoafilegroup(groupnumber,'correspondingvariablenamesseparatedbycommaswithoutanyingelse')
    !do i = startingline, totallines
        !...
        !u = pickunitinafilegroup(groupnumber, i)
        !write(u,"(1x,2(i10,','),19(f20.8, ','),a)") thevariables, picktikzcolor(i)
    !end do
    !call filegroupclose(groupnumber)

    return
end subroutine mycomputing




