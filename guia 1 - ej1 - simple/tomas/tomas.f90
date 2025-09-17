program tomas

    use ziggurat
    implicit none
    logical :: es
    integer :: seed,i ,j,k
    real (kind=8) :: x(10),a(10,10),b(10,10)
    real (kind=8), allocatable  :: y(:),c(:,:)
	real (kind=8) :: v(2)

![NO TOCAR] Inicializa generador de número random

    inquire(file='seed.dat',exist=es)
    if(es) then
        open(unit=10,file='seed.dat',status='old')
        read(10,*) seed
        close(10)
        print *,"  * Leyendo semilla de archivo seed.dat"
    else
        seed = 24583490
    end if

    call zigset(seed)
![FIN NO TOCAR]    
!parte practica de tomas 	
	
	


    do i=1,100
		v(1) = uni()
		v(2) = uni()
        
		if(v(2) > v(1))  then 
			print *,"(",v(1),",",v(2),")"
		end if
            
        
    end do
!FIN parte practica de tomas 
    
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios 

        open(unit=10,file='seed.dat',status='unknown')
        seed = shr3() 
         write(10,*) seed
        close(10)
![FIN no Tocar]        


end program tomas