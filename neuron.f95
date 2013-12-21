module class_Neuron
    use weights
    !use vector
implicit none
!    private :: init_Neuron     
    public :: Neuron!, feedForward, setOutput, getOutput, calcOutputGradients, calcHiddenGradients, adaptWeights, getWeights
     
    type Neuron
       integer :: id
       integer :: eta
       integer :: alpha
       integer :: no
       
       type(tweights), dimension(:), allocatable :: outputWeights
    end type Neuron
contains
    subroutine init_Neuron(this,id,eta,alpha,no)
        type(Neuron) :: this
        integer :: i, id,eta,alpha,no
        this%id = id
        this%eta = eta
        this%alpha = alpha
        this%no = no
 
        allocate(this%outputWeights(0:this%no)) 
        init_loop: do, i=0, this%no
           this%outputWeights%weight = 1.0 !rand here 
        end do init_loop
         
        write(*,*) this%outputWeights%weight 

    end subroutine init_Neuron
end module class_Neuron      
