module neutran_Types
    implicit none    
    
    type Neuron
        real, dimension(:), allocatable :: weights
        real :: delta
        integer :: weightsSize
    end type Neuron

    type Layer
        integer :: id
        type(Neuron), dimension(:), allocatable :: neurons
    end type Layer

    type NNet
        integer :: numberOfLayers
        type(Layer), dimension(:), allocatable :: layers
    end type NNet

end module neutran_Types
