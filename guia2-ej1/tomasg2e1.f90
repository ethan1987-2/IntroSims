program tomas_g2ej1

    use ziggurat
    implicit none
    logical :: es
    integer :: seed,N,i
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
	
	
	open(unit=11,file='input.dat',status='old')
	open(unit=12,file='output.dat',status='new')
	read(11,*) N
	close(11)
	
    do i=1,N
		v(1) = uni()*2
		v(2) = uni()
        
		if(v(2) >= funcdens(v(1)))  then 
		! if(v(2) >= (call funcdens(v(1))))  then 
			write(12,*) "(X=",v(1),",Y=",v(2),")"
		end if        
    end do
	close(12)
!FIN parte practica de tomas 
    
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios 

        open(unit=10,file='seed.dat',status='unknown')
        seed = shr3() 
         write(10,*) seed
        close(10)
![FIN no Tocar]        

CONTAINS

	real function funcdens(x)
		implicit none
		real (kind = 8):: x 
		funcdens = (x-1)**2
	end function funcdens

	! FUNCTION func_dens(x) RESULT( proba )
	   ! REAL(kind=8)  :: x,proba

	   ! proba = (x-1)**2
	   ! RETURN
	! END FUNCTION func_dens
	
	! subroutine funcdens(x,proba)
		! implicit none
		! real (kind = 8), intent(out) :: proba
		! real (kind = 8), intent(in) :: x 
		! proba = (x-1)**2
	! end subroutine funcdens

end program tomas_g2ej1