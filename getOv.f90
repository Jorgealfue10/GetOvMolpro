program Ovget
    implicit none
    integer,parameter :: nf=95,nso=14
    integer :: i,j,k,l,m,nl,io,ns
    integer :: counti,deci,centi,uni
    integer :: nsdap,nsdapp,nsqapp
    real(kind=8) :: val
    real(kind=8),allocatable,dimension(:) :: rph
    real(kind=8),allocatable,dimension(:,:,:) :: Sdap,Sdapp,Sqapp
    character(len=1) :: s11,s12,s13
    character(len=7) :: st,string11,string12,string13
    character(len=3) :: st2,string21,string22,string23
    character(len=6) :: reffile,dap,dapp,qapp
    character(len=9) :: st4,string4
    character(len=15) :: aux
    character(len=17) :: output

    string11="SETTING"
    string12="Transit"
    string21="RPH"
    string22="mom"
    string13 = "SETTING"
    string23 = "OVM"

    dap="7001.2"
    dapp="6002.2"
    qapp="6004.2"


    nl=233221

    nsdap=6
    nsdapp=5
    nsqapp=2

    allocate(rph(nf))
    allocate(Sdap(nf,nsdap,nsdap),Sdapp(nf,nsdapp,nsdapp),Sqapp(nf,nsqapp,nsqapp))

    open(15,file="ci-PHM.out",action="read")


    l=1
    rph(1)=2.01515152
    do i=1,nl
        read(15,*,iostat=io) st,st2
        !print*,st,st2
        if ((st.eq.string11).and.(st2.eq.string21)) then
            l=l+1
            backspace(15)
            read(15,*) st,st2,aux,rph(l)
            print*,rph(l),l
        endif

        if ((st.eq.string12).and.(st2.eq.string22)) then
            read(15,*,iostat=io)
            read(15,*,iostat=io)
            read(15,*,iostat=io)
            read(15,*,iostat=io)
            read(15,*) aux,aux,aux,aux,aux,reffile
            read(15,*,iostat=io)
            read(15,*,iostat=io)
            read(15,*,iostat=io)
            read(15,*,iostat=io)
            read(15,*,iostat=io)
            
            if (reffile.eq.dap) then
                ns=6
                j=1
                k=1
            else if (reffile.eq.dapp) then
                ns=5
                j=1
                k=1
            else if (reffile.eq.qapp) then
                ns=2
                j=1
                k=1
            endif

            do m=1,9
                backspace(15)
            enddo    

        endif


        if ((st.eq.string13).and.((st2.eq.string23).or.(st2.eq."OMV"))) then
            
            if ((j.eq.1).and.(k.eq.1)) then
                backspace(15)
                backspace(15)
            else
                backspace(15)
            endif
            
            
            read(15,*) aux,aux,aux,val
            print*,aux,val,st2
            
            if (reffile.eq.dap) then
                if ((abs(val).gt.0.5).and.(abs(val).lt.(1.2))) then
                    Sdap(l,j,k) = 1.d0
                else !if ((abs(val).lt.0.2)) then
                    Sdap(l,j,k) = 0.d0
                !else
                !    Sdap(l,j,k) = val
                endif
            else if (reffile.eq.dapp) then
                if ((abs(val).gt.0.5).and.(abs(val).lt.(1.2))) then
                    Sdapp(l,j,k) = 1.d0
                else! if ((abs(val).lt.0.2)) then
                    Sdapp(l,j,k) = 0.d0
                !else
                !    Sdapp(l,j,k) = val
                endif
            else if (reffile.eq.qapp) then
                if ((abs(val).gt.0.5).and.(abs(val).lt.(1.2))) then
                    Sqapp(l,j,k) = 1.d0
                else! if ((abs(val).lt.0.2)) then
                    Sqapp(l,j,k) = 0.d0
                !else
                !    Sqapp(l,j,k) = val
                endif
            endif
            

            !print*,st,aux,Sdap(k,j,k),j,k

            if ((k.lt.ns).and.(j.le.ns)) then
                k=k+1
            else if ((k.eq.ns).and.(j.lt.ns)) then
                k=1
                j=j+1
            !else if ((k.eq.ns).and.(j.eq.ns)) then
            endif
        endif   
    enddo

    !if (1.eq.0) then
    do i=1,nf
        do j=1,nsdap-1
            do k=j+1,nsdap
                Sdap(i,k,j) = Sdap(i,j,k)
            enddo
        enddo

        do j=1,nsdapp-1
            do k=j+1,nsdapp
                Sdapp(i,k,j) = Sdapp(i,j,k)
            enddo
        enddo

        do j=1,nsqapp-1
            do k=j+1,nsqapp
                Sqapp(i,k,j) = Sqapp(i,j,k)
            enddo
        enddo
    enddo
    !endif



    counti=5
    deci=0
    centi=0
    uni=16
    do i=1,nf
        counti=counti+1
        if (deci.eq.10) then
            centi=centi+1
            s11=char(48+centi)
            s12=char(48)
            s13=char(48)
            deci=0
            counti=0
        else if (counti.eq.10) then
            deci=deci+1
            if (deci.eq.10) cycle
            s11=char(48+centi)
            s12=char(48+deci)
            s13=char(48)
            counti=0
        else
            s11=char(48+centi)
            s12=char(48+deci)
            s13=char(48+counti)
        endif

        output="over-"//s11//s12//s13//"-2Ap-.dat"
        open(uni,file=output,action="write")

        !write(uni,'(F16.12,1x)',advance='no') rph(i)
        do j=1,nsdap
            write(uni,*) Sdap(i,j,:)
        enddo

        close(uni)

        output="over-"//s11//s12//s13//"-2App.dat"
        open(uni,file=output,action="write")

        !write(uni,'(F16.12,1x)',advance='no') rph(i)
        do j=1,nsdapp
            write(uni,*) Sdapp(i,j,:)
        enddo

        output="over-"//s11//s12//s13//"-4App.dat"
        open(uni,file=output,action="write")

        !write(uni,'(F16.12,1x)',advance='no') rph(i)
        do j=1,nsqapp
            write(uni,*) Sqapp(i,j,:)
        enddo

    enddo

end program Ovget
