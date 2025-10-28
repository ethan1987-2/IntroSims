program guia4

	implicit none
	use ziggurat
	
	integer(kind=4):: N, i, j, seed, M, PreTs,PreTmax,k,ii,repsMC
	real, allocatable :: r(:,:),v(:,:),f(:,:)
	real :: E, L, sig, eps, modDrji, Drji(3), E_pot, sigrji, escf
	logical :: es, esint, recup,preterm,Tesint,pasarT
	
	
	integer(kind=1)::  sij
	integer(kind=2):: foutnbr,foutnbr2,dE,L,jj,tt
	integer(kind=8):: flips, MCs,MCmax,sampstep,Nsamp !esto debo hacerlo asi porque si no el simple producto que define a MCmax falla y hasta me han dado negativos solo por ser kind distintos!
	integer(kind=1), allocatable:: s(:,:)
	integer(kind=4),allocatable :: vE(:),vM(:)
	character(len=20)::  ident
	character(len=50):: fmat,foutname,foutname2
	
	sig=1.0	; eps=1.0 ; E_pot=0.0
	
	open(newunit=foutnbr,file='input.txt',status='old',action='read')
	
		read(foutnbr, *)
		read(foutnbr,*) N
		read(foutnbr, *)
		read(foutnbr,*) L
		read(foutnbr, *)
		read(foutnbr,*) ssads
		read(foutnbr, *)
		read(foutnbr,*) adfgfg
		
	close(foutnbr)
	
	allocate(r(3,N),v(3,N),f(3,N))
	
	!SORTEO POSICIONES
	
	do j=1,N
		do i=1,3
			r(i,j)=uni()*L
		end do
	end do
	
	!EVALÚO POTENCIAL Y FUERZA
	!el siguiente do esta mal porque no es suma de un j con toda otra particula
	do j=1,N-1
		f(:,j)=0
		do i=1,j-1
			Drji= r(:,j)-r(:,i)
			modDrji=sqrt(sum(Drji**2))
			sigrji=(sig/modDrji)**6
			escf= 24*eps*(-sigrji+2*sigrji**2)/modDrji
			f(:,j)=f(:,j)+escf*Drji/modDrji
		end do
		do i=j+1,N
			Drji= r(:,j)-r(:,i)
			modDrji=sqrt(sum(Drji**2))
			sigrji=(sig/modDrji)**6
			E_pot=E_pot+4*eps*(-sigrji+sigrji**2)
			escf= 24*eps*(-sigrji+2*sigrji**2)/modDrji
			f(:,j)=f(:,j)+escf*Drji/modDrji
		end do
		
	contains		
			! >>> Guardar y recuperar semilla en/de archivo "seed.dat"

		subroutine guardarSemilla()
		   integer(kind=1):: iou
		   open(newunit=iou, file='seed.dat', status='unknown')
		   write(iou,*) shr3()  ! seed
		   close(iou)
		end subroutine guardarSemilla

		subroutine recuperarSemilla()
		   logical:: es
		   integer:: seed
		   integer(kind=1):: iou
		   inquire(file='seed.dat',exist=es)
		   if(es) then
			  open(newunit=iou, file='seed.dat', status='old')
			  read(iou,*) seed
			  close(iou)
			  print *, "...leyendo semilla desde archivo seed.dat"
		   else
			  print *, "No se encuentra archivo seed.dat"
			  seed = 24583490
		   end if
		   call zigset(seed)
		end subroutine recuperarSemilla
end program guia4