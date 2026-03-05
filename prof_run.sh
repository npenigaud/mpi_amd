#!/bin/bash
##SBATCH -N2
#SBATCH -N1
#SBATCH --time 00:01:00
#SBATCH --export="NONE"
#SBATCH --partition MI300X
#SBATCH --gres=gpu:2
#SBATCH -n 2
#SBATCH -c 2

source /home/afar/modules/use.sh
module load rocm/7.1.1
module load afar/22.3.0
module load openmpi-rt/ucx
module load openmpi/afar-22.3.0/5.0.9
module unload -f openmpi-rt/ucx
module load openmpi-rt/sm

export OMPI_MCA_btl=vader,self
export OMPI_MCA_coll_hcoll_enable=0
export OMPI_MCA_hwloc_base_binding_policy=none
export OMPI_MCA_mca_base_component_show_load_errors=0
export OMPI_MCA_osc='^ucx'
export OMPI_MCA_pml='^ucx'
export OMPI_MCA_smsc='^knem'
export OMPI_MCA_spml='^ucx'
export OMP_NUM_THREADS=4
export OMP_PLACES=cores
export OMP_PROC_BIND=true
#export OMP_STACKSIZE=64M
export OMP_STACKSIZE=16M
ulimit -s unlimited

#ldd -v $(which mpirun)

/home/afar/software/mpi/openmpi/afar-22.3.0/5.0.9_ucx1.20.0_rocm7.1.1/bin/mpirun -n 2 /home/afar/software/compilers/afar/rocm-afar-10004-drop-22.3.0/bin/rocprofv3 --runtime-trace --output-format=pftrace -- ./mpc.x
#/home/afar/software/compilers/afar/rocm-afar-10004-drop-22.3.0/bin/rocprofv3 --runtime-trace --output-format=pftrace -- /home/afar/software/mpi/openmpi/afar-22.3.0/5.0.9_ucx1.20.0_rocm7.1.1/bin/mpirun --report-bindings -n 2 ./mpc.x

