module Types
    implicit none    
    
    type Neuron
        real :: weight
        real :: delta
    end type Neuron

    type Layer
        integer :: id
        type(Neuron), dimension(:), allocatable :: neurons
    end type Layer

    type NNet
        integer :: numberOfLayers
        type(Layer), dimension(:), allocatable :: layers
    end type NNet

end module Types
