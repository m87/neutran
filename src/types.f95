module nt_TypesModule
    implicit none    
    
    type nt_Neuron
        real, dimension(:), allocatable :: weights
        real :: delta
        integer :: weightsSize
    end type nt_Neuron

    type nt_Layer
        integer :: id
        type(nt_Neuron), dimension(:), allocatable :: neurons
    end type nt_Layer

    type nt_Net
        integer :: numberOfLayers
        integer, dimension(:), allocatable :: topology
        type(nt_Layer), dimension(:), allocatable :: layers
    end type nt_Net

end module nt_TypesModule
