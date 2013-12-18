module class_Neuron
    use weights
    use vector
    implicit none
    private     
    public :: Neuron, feedForward, setOutput, getOutput, calcOutputGradients, calcHiddenGradients, adaptWeights, getWeights

    type Neuron
       integer :: output_num
       integer :: id
       real :: eta
       integer :: alpha
       integer :: no
       real(kind = 8), dimension(no) :: outputWeights 
    end type Neuron
contains
    subroutine      
