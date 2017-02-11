module nt_NeuronModule
    use nt_TypesModule
    
    public :: neu_neuronInit

    contains

        subroutine nt_neuronInit(this, nextLayerSize, bias)
            type(nt_Neuron) :: this
            integer :: nextLayerSize
            logical :: bias
            integer :: layerSize

            if (bias) then
                layerSize = nextLayerSize + 1
            else
                layerSize = nextLayerSize
            end if

            allocate(this%weights(0:nextLayerSize))
            init_loop: do, i=0, nextLayerSize
               call random_number(this%weights(i))
            end do init_loop
            
            this%delta  = 0.0
            this%nextLayerSize = layerSize

        end subroutine nt_neuronInit
            

end module nt_NeuronModule
