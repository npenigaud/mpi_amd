source /home/afar/modules/use.sh
module load rocm
module load afar/22.2.0
module load openmpi-rt/ucx
module load openmpi/afar-22.2.0/5.0.8


mpif90 -march=native -fPIC -ffree-form -cpp -fbackslash -fconvert=big-endian -O2 -ffp-contract=off -DARCH="'afar221ompd'" -fopenmp -fPIC -fopenmp --offload-arch=gfx942 -lflang_rt.hostdevice -I/home/afar/software/compilers/afar/rocm-afar-8873-drop-22.2.0/include/hipfort/amdgcn/  -L/home/afar/software/compilers/afar/rocm-afar-8873-drop-22.2.0/lib/ -Qunused-arguments /home/afar/software/compilers/afar/rocm-afar-8873-drop-22.2.0/lib/libhipfort-amdgcn.a -L/home/afar/software/compilers/afar/rocm-afar-8873-drop-22.2.0/lib/ -lamdhip64   mpc.F90 -o mpc.x \


