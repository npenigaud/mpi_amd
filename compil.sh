module purge
source /home/afar/modules/use.sh
module load rocm
module load afar/22.3.0
module load openmpi/afar-22.3.0/5.0.9
module unload -f openmpi-rt/ucx
module load openmpi-rt/sm

module list

#mpif90 -march=native -fPIC -ffree-form -cpp -fbackslash -fconvert=big-endian -O2 -ffp-contract=off -DARCH="'afar221ompd'" -fopenmp -fPIC -fopenmp --offload-arch=gfx942 -I/home/afar/software/compilers/afar/rocm-afar-10004-drop-22.3.0/include/hipfort/amdgcn -L/home/afar/software/compilers/afar/rocm-afar-10004-drop-22.3.0/lib/ -Qunused-arguments /home/afar/software/compilers/afar/rocm-afar-10004-drop-22.3.0/lib/libhipfort-amdgcn.a -L/home/afar/software/compilers/afar/rocm-afar-10004-drop-22.3.0/lib/ -lamdhip64   mpc.F90 -o mpc.x \

mpif90 -march=native -fPIC -ffree-form -cpp -fbackslash -fconvert=big-endian -O2 -ffp-contract=off -DARCH="'afar221ompd'" -fopenmp -fPIC -fopenmp --offload-arch=gfx942 -Qunused-arguments  mpc.F90 -o mpc.x 
