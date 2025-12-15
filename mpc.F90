PROGRAM MPICUDAAWARE

!USE CUDAFOR
!USE OPENACC
USE HIPFORT
USE ISO_C_BINDING
USE MPI
 
IMPLICIT NONE

!INCLUDE 'mpif.h'

REAL, ALLOCATABLE         :: C_SEND (:), C_RECV (:)



!REAL, ALLOCATABLE, DEVICE :: D_SEND (:), D_RECV (:)
REAL, ALLOCATABLE :: D_SEND (:), D_RECV (:)



REAL, ALLOCATABLE         :: H_SEND (:), H_RECV (:)
REAL, ALLOCATABLE         :: dummy1(:), dummy2(:)

INTEGER :: IREQ_RECV, IREQ_SEND
INTEGER :: ISTATUS (MPI_STATUS_SIZE)
INTEGER :: ISIZE, IERROR
INTEGER :: IRANK, IRANKP, IRANKN
INTEGER, PARAMETER :: taille=16
INTEGER     :: compteur
integer(c_int)::num_carte,flags

CALL MPI_INIT (ierror)

CALL MPI_COMM_RANK (MPI_COMM_WORLD, IRANK, IERROR)
CALL MPI_COMM_SIZE (MPI_COMM_WORLD, ISIZE, IERROR)

write (0,*) "using hipSetDevice, result : ", hipSetDevice(IRANK)
write (0,*) "using hipInit, result : ", hipInit(flags)
write (0,*) "using hipGetDevice, result : ", hipGetDevice(num_carte)
print *, "Rank ",IRANK ," running on GPU number ",num_carte

ALLOCATE (C_SEND (taille))
ALLOCATE (C_RECV (taille))
ALLOCATE (D_RECV (taille))
ALLOCATE (D_SEND (taille))
ALLOCATE (H_RECV (taille))
ALLOCATE (H_SEND (taille))
allocate (dummy1(taille),dummy2(taille))

CALL RANDOM_NUMBER (C_SEND)
CALL RANDOM_NUMBER (H_SEND)
call random_number (dummy1)
call random_number (dummy2)
!$omp target data map(tofrom:dummy1,dummy2)

print *, "c_send", C_SEND(1:10)
print *, "h_send", H_SEND(1:10)

do compteur=1,taille
  C_SEND(compteur) = C_SEND(compteur)+1.0
  C_RECV(compteur) = -1.0
enddo
D_SEND=C_SEND
D_RECV=C_RECV

IRANKP = MODULO (IRANK-1, ISIZE)
IRANKN = MODULO (IRANK+1, ISIZE)

!$omp target 
do compteur=1,taille
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target

PRINT *, " DEVICE DATA "

CALL MPI_IRECV (D_RECV, taille,mpi_real, IRANKP, 1001, MPI_COMM_WORLD, IREQ_RECV, &
             & ierror)

CALL MPI_BARRIER (MPI_COMM_WORLD,ierror)

CALL MPI_ISEND (D_SEND, taille,mpi_real,IRANKN, 1001, MPI_COMM_WORLD, IREQ_SEND, &
             & ierror)

CALL MPI_WAIT (IREQ_RECV,istatus,ierror)
CALL MPI_WAIT (IREQ_SEND,istatus,ierror)

!$omp target
do compteur=1,taille
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target


C_SEND=D_SEND
C_RECV=D_RECV
PRINT *, IRANK, " reçu ==> ", C_RECV (1:10)
PRINT *, IRANK, " envoyé ==> ", C_SEND(1:10)

!$omp target
do compteur=1,taille
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target


do compteur=1,taille
  H_SEND(compteur) = H_SEND(compteur)+irank
  H_RECV(compteur) = -1.0
enddo

!$omp target
do compteur=1,taille
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target


PRINT *, " HOST DATA "

CALL MPI_IRECV (H_RECV,taille, mpi_real, IRANKP, 1001, MPI_COMM_WORLD, IREQ_RECV, &
             & ierror)

CALL MPI_BARRIER (MPI_COMM_WORLD,ierror)

CALL MPI_ISEND (H_SEND, taille, mpi_real, IRANKN, 1001, MPI_COMM_WORLD, IREQ_SEND, &
             & ierror)

CALL MPI_WAIT (IREQ_RECV,istatus,ierror)
CALL MPI_WAIT (IREQ_SEND,istatus,ierror)

PRINT *, IRANK, " reçu  ==> ", H_RECV (1:10)
PRINT *, IRANK, " envoyé ==> ", H_SEND(1:10)

!$omp target
do compteur=1,taille
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target


do compteur=1,taille
  h_send(compteur) = h_send(compteur)+irank
  H_RECV(compteur) = -1.0
enddo

!$omp target data map (tofrom:H_RECV, H_SEND)

!$omp target
do compteur=1,taille
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target


PRINT *, "HOST DATA UPDATE"

CALL MPI_IRECV (H_RECV,taille, mpi_real, IRANKP, 1001, MPI_COMM_WORLD, IREQ_RECV, &
             & ierror)

CALL MPI_BARRIER (MPI_COMM_WORLD,ierror)

!$omp target update from(h_send)   !!!updates data to host before using MPI
CALL MPI_ISEND (H_SEND,taille, mpi_real,IRANKN, 1001, MPI_COMM_WORLD,IREQ_SEND, &
             & ierror)

CALL MPI_WAIT (IREQ_RECV,istatus,ierror)
CALL MPI_WAIT (IREQ_SEND,istatus,ierror)
CALL MPI_BARRIER(MPI_COMM_WORLD,ierror)
!$omp target update to(h_recv)    !!!updates data to gpu after using MPI

print *, irank, " H_RECV ==> ", H_RECV (1:10)
PRINT *, irank, "H_SEND ==> ", H_SEND (1:10)

!$omp target
do compteur=1,taille
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target


!$omp target
PRINT *, IRANK, " H_RECV ==> ", H_RECV (1)
PRINT *,irank, " H_SEND ==> ", H_SEND (1)
!$omp end target

!$omp target
do compteur=1,taille
  h_send(compteur) = h_send(compteur)+irank
  H_RECV(compteur) = -1
enddo
!$omp end target

!$omp target
do compteur=1,taille
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target


PRINT *, " HOST DATA (USE_DEVICE) "

!$OMP TARGET DATA USE_DEVICE_ADDR (H_RECV)
CALL MPI_IRECV (H_RECV, taille,mpi_real,IRANKP, 1001, MPI_COMM_WORLD, IREQ_RECV, &
             & ierror)
!$OMP END TARGET DATA

CALL MPI_BARRIER (MPI_COMM_WORLD,ierror)

!$OMP TARGET DATA USE_DEVICE_ADDR (H_SEND)
CALL MPI_ISEND (H_SEND, taille, mpi_real,IRANKN, 1001, MPI_COMM_WORLD, IREQ_SEND, &
             & ierror)
!$OMP END TARGET DATA

CALL MPI_WAIT (IREQ_RECV,istatus,ierror)
CALL MPI_WAIT (IREQ_SEND,istatus,ierror)

!$omp target
do compteur=1,taille
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target


!$OMP END TARGET DATA
!$omp end target data

PRINT *, IRANK, " received  ==> ", H_RECV (1:10)
PRINT *, IRANK, " sent ==> ", H_SEND(1:10)

DEALLOCATE (C_SEND)
DEALLOCATE (C_RECV)
DEALLOCATE (D_SEND)
DEALLOCATE (D_RECV)
DEALLOCATE (H_SEND)
DEALLOCATE (H_RECV)

PRINT *, "FINALIZE"

CALL MPI_FINALIZE(ierror)
 
END PROGRAM MPICUDAAWARE
