PROGRAM MPICUDAAWARE

USE MPI
USE OMP_LIB
USE ISO_C_BINDING, ONLY : C_PTR, C_SIZE_T, C_INT, C_F_POINTER, C_SIZEOF
 
IMPLICIT NONE


REAL, ALLOCATABLE         :: C_SEND (:), C_RECV (:)



!REAL, ALLOCATABLE, DEVICE :: D_SEND (:), D_RECV (:)
REAL, POINTER :: D_SEND (:), D_RECV (:)
TYPE(C_PTR) :: D_SEND_PTR, D_RECV_PTR
INTEGER(C_SIZE_T) :: ICSIZE
INTEGER(C_INT)    :: DEV

REAL, ALLOCATABLE         :: H_SEND (:), H_RECV (:)
REAL, ALLOCATABLE         :: dummy1(:), dummy2(:)

INTEGER :: IREQ_RECV, IREQ_SEND
INTEGER :: ISTATUS (MPI_STATUS_SIZE)
INTEGER :: ISIZE, IERROR_R,IERROR_S,IERROR_B,IERROR
INTEGER :: IRANK, IRANKP, IRANKN
INTEGER, PARAMETER :: ISIZEDATA=16
INTEGER     :: COMPTEUR,NUM_GPUS,NUM_CARTE

CALL MPI_INIT (IERROR)

CALL MPI_COMM_RANK (MPI_COMM_WORLD, IRANK, IERROR)
CALL MPI_COMM_SIZE (MPI_COMM_WORLD, ISIZE, IERROR)
NUM_GPUS=OMP_GET_NUM_DEVICES()
NUM_CARTE=MODULO(IRANK,NUM_GPUS)
CALL OMP_SET_DEFAULT_DEVICE(NUM_CARTE)

DEV=NUM_CARTE


PRINT *, "Rank ",IRANK ," running on GPU number ",OMP_GET_DEFAULT_DEVICE()

ALLOCATE (C_SEND (ISIZEDATA))
ALLOCATE (C_RECV (ISIZEDATA))
!ALLOCATE (D_RECV (ISIZEDATA))
!ALLOCATE (D_SEND (ISIZEDATA))
ALLOCATE (H_RECV (ISIZEDATA))
ALLOCATE (H_SEND (ISIZEDATA))
allocate (dummy1(ISIZEDATA),dummy2(ISIZEDATA))

ICSIZE=C_SIZEOF(H_RECV(1))*ISIZEDATA
D_SEND_PTR=OMP_TARGET_ALLOC(ICSIZE,DEV)
D_RECV_PTR=OMP_TARGET_ALLOC(ICSIZE,DEV)
CALL C_F_POINTER(D_SEND_PTR,D_SEND,[ISIZEDATA])
CALL C_F_POINTER(D_RECV_PTR,D_RECV,[ISIZEDATA])
write (0,*) "shape(d_send)",shape(d_send)
write (0,*) "d_send(1)",d_send(1)
CALL RANDOM_NUMBER (C_SEND)
CALL RANDOM_NUMBER (H_SEND)
call random_number (dummy1)
call random_number (dummy2)
!$omp target data map(tofrom:dummy1,dummy2)


do compteur=1,ISIZEDATA
  C_SEND(compteur) = C_SEND(compteur)+1.0
  C_RECV(compteur) = -1.0
enddo

print *, "c_send", C_SEND(1:10)
print *, "h_send", H_SEND(1:10)


!$omp target data map(tofrom: c_send,c_recv) 
!$omp target 
D_SEND(:)=C_SEND(:)
D_RECV(:)=C_RECV(:)
!$omp end target
!$omp end target data

IRANKP = MODULO (IRANK-1, ISIZE)
IRANKN = MODULO (IRANK+1, ISIZE)

!$omp target 
do compteur=1,ISIZEDATA
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target

PRINT *, " DEVICE DATA "

CALL MPI_IRECV (D_RECV, ISIZEDATA,mpi_real, IRANKP, 1001, MPI_COMM_WORLD, IREQ_RECV, &
             & ierror_r)


CALL MPI_ISEND (D_SEND, ISIZEDATA,mpi_real,IRANKN, 1001, MPI_COMM_WORLD, IREQ_SEND, &
             & ierror_s)

CALL MPI_WAIT (IREQ_RECV,istatus,ierror)
CALL MPI_WAIT (IREQ_SEND,istatus,ierror)

!$omp target
do compteur=1,ISIZEDATA
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target

write (0,*) "reçu => "
!$omp target 
do compteur=1,10
  write (0,*) d_recv(compteur)
enddo
!$omp end target

write (0,*) "envoyé => "
!$omp target 
do compteur=1,10
  write (0,*) d_send(compteur)
enddo
!$omp end target

!C_SEND=D_SEND
!C_RECV=D_RECV
!PRINT *, IRANK, " reçu ==> ", C_RECV (1:10)
!PRINT *, IRANK, " envoyé ==> ", C_SEND(1:10)

!$omp target
do compteur=1,ISIZEDATA
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target


do compteur=1,ISIZEDATA
  H_SEND(compteur) = H_SEND(compteur)+irank
  H_RECV(compteur) = -1.0
enddo

!$omp target
do compteur=1,ISIZEDATA
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target


CALL MPI_BARRIER (MPI_COMM_WORLD,ierror_b)


PRINT *, " HOST DATA "

CALL MPI_IRECV (H_RECV,ISIZEDATA, mpi_real, IRANKP, 1001, MPI_COMM_WORLD, IREQ_RECV, &
             & ierror_r)


CALL MPI_ISEND (H_SEND, ISIZEDATA, mpi_real, IRANKN, 1001, MPI_COMM_WORLD, IREQ_SEND, &
             & ierror_s)

CALL MPI_WAIT (IREQ_RECV,istatus,ierror)
CALL MPI_WAIT (IREQ_SEND,istatus,ierror)

PRINT *, IRANK, " reçu  ==> ", H_RECV (1:10)
PRINT *, IRANK, " envoyé ==> ", H_SEND(1:10)

!$omp target
do compteur=1,ISIZEDATA
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target


do compteur=1,ISIZEDATA
  h_send(compteur) = h_send(compteur)+irank
  H_RECV(compteur) = -1.0
enddo

!$omp target data map (tofrom:H_RECV, H_SEND)

!$omp target
do compteur=1,ISIZEDATA
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target

CALL MPI_BARRIER (MPI_COMM_WORLD,ierror_b)

PRINT *, "HOST DATA UPDATE"

CALL MPI_IRECV (H_RECV,ISIZEDATA, mpi_real, IRANKP, 1001, MPI_COMM_WORLD, IREQ_RECV, &
             & ierror_r)


!$omp target update from(h_send)   !!!updates data to host before using MPI
CALL MPI_ISEND (H_SEND,ISIZEDATA, mpi_real,IRANKN, 1001, MPI_COMM_WORLD,IREQ_SEND, &
             & ierror_s)

CALL MPI_WAIT (IREQ_RECV,istatus,ierror)
CALL MPI_WAIT (IREQ_SEND,istatus,ierror)
CALL MPI_BARRIER(MPI_COMM_WORLD,ierror)
!$omp target update to(h_recv)    !!!updates data to gpu after using MPI

print *, irank, " H_RECV ==> ", H_RECV (1:10)
PRINT *, irank, "H_SEND ==> ", H_SEND (1:10)

!$omp target
do compteur=1,ISIZEDATA
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target


!$omp target
PRINT *, IRANK, " H_RECV ==> ", H_RECV (1)
PRINT *,irank, " H_SEND ==> ", H_SEND (1)
!$omp end target

!$omp target
do compteur=1,ISIZEDATA
  h_send(compteur) = h_send(compteur)+irank
  H_RECV(compteur) = -1
enddo
!$omp end target

!$omp target
do compteur=1,ISIZEDATA
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target

CALL MPI_BARRIER (MPI_COMM_WORLD,ierror_b)

PRINT *, " HOST DATA (USE_DEVICE) "

!$OMP TARGET DATA USE_DEVICE_ADDR (H_RECV)
CALL MPI_IRECV (H_RECV, ISIZEDATA,mpi_real,IRANKP, 1001, MPI_COMM_WORLD, IREQ_RECV, &
             & ierror_r)
!$OMP END TARGET DATA


!$OMP TARGET DATA USE_DEVICE_ADDR (H_SEND)
CALL MPI_ISEND (H_SEND, ISIZEDATA, mpi_real,IRANKN, 1001, MPI_COMM_WORLD, IREQ_SEND, &
             & ierror_s)
!$OMP END TARGET DATA

CALL MPI_WAIT (IREQ_RECV,istatus,ierror)
CALL MPI_WAIT (IREQ_SEND,istatus,ierror)

!$omp target
do compteur=1,ISIZEDATA
  dummy2(compteur)=dummy2(compteur)+1.01*dummy1(compteur)
enddo
!$omp end target


!$OMP END TARGET DATA
!$omp end target data

PRINT *, IRANK, " received  ==> ", H_RECV (1:10)
PRINT *, IRANK, " sent ==> ", H_SEND(1:10)

CALL MPI_BARRIER (MPI_COMM_WORLD,ierror_b)


DEALLOCATE (C_SEND)
DEALLOCATE (C_RECV)
!DEALLOCATE (D_SEND)
!DEALLOCATE (D_RECV)
DEALLOCATE (H_SEND)
DEALLOCATE (H_RECV)

PRINT *, "FINALIZE"

CALL MPI_FINALIZE(IERROR)
 
END PROGRAM MPICUDAAWARE
