module nt_TypesModule
    implicit none    
    
    type nt_Neuron
        real, dimension(:), allocatable :: weights
        real :: delta
        integer :: nextLayerSize
    end type nt_Neuron

    type nt_Layer
        integer :: id
        integer :: layerSize
        type(nt_Neuron), dimension(:), allocatable :: neurons
    end type nt_Layer

    type nt_Net
        integer :: numberOfLayers
        integer, dimension(:), allocatable :: topology
        type(nt_Layer), dimension(:), allocatable :: layers
    end type nt_Net

end module nt_TypesModule
