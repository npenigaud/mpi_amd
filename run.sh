##!/bin/bash
###SBATCH -N2
##SBATCH -N1
##SBATCH --time 00:10:00
##SBATCH --export="NONE"
##SBATCH --partition gpu
###SBATCH --gres=gpu:1
##SBATCH --gres=gpu:2
##SBATCH --mem=30G 
###SBATCH --mem=12G
##SBATCH -c 2

source /home/afar/modules/use.sh
module load rocm
module load afar/22.2.0
module load openmpi-rt/ucx
module load openmpi/afar-22.2.0/5.0.8


#source /usr/local/apps/hpc_sdk/22.3/Linux_x86_64/2022/comm_libs/hpcx/latest/hpcx-mt-init.sh hpcx_load#!/bin/bash

set -x

ulimit -s unlimited
export OMP_STACK_SIZE=16G

#cd $SLURM_SUBMIT_DIR

#\rm *.grb

#export SLURM_EXPORT_ENV='ALL'
#export MPIAUTOCONFIG=mpiauto.PGI.conf

#mpirun -np 4 ./mpicudaaware.x / ajouter -nn 2 si -N 2
#/opt/softs/mpiauto/mpiauto --prefix-command ./nsys.sh --verbose -openmp 1 --nouse-slurm-mpi --wrap --wrap-stdeo -nn 2 -np 2 -- ./mpc.x
#mpirun -np 2 --oversubscribe nsys profile -o mpc.x #./mpc6.x 

#mpirun -np 4 --oversubscribe ./mpc.x #N1
#mpirun -N 2 -np 4 --oversubscribe ./mpc.x #N2

mpirun -n 2 -mca pml ucx --oversubscribe --mca pml_base_verbose 10 --mca pml_base_verbose 10 ./mpc.x
