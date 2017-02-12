module nt_NeuronModule
    use nt_TypesModule
    
    public :: nt_hiddenNeuronInit, nt_outputNeuronInit, nt_inputNeuronInit

    contains

        subroutine nt_hiddenNeuronInit(this, nextLayerSize, bias)
            type(nt_Neuron) :: this
            integer :: nextLayerSize
            logical :: bias
            integer :: layerSize

            if (bias) then
                layerSize = nextLayerSize + 1
            else
                layerSize = nextLayerSize
            end if

            allocate(this%synapses(0:nextLayerSize))
            init_loop: do, i=0, nextLayerSize
               call random_number(this%synapses(i)%weight)
               this%synapses(i)%delta = 0.0 
            end do init_loop
            
            this%nextLayerSize = layerSize

        end subroutine nt_hiddenNeuronInit
        
        subroutine nt_inputNeuronInit(this, nextLayerSize, bias)
            type(nt_Neuron) :: this
            integer :: nextLayerSize
            logical :: bias
            integer :: layerSize
            
            call nt_hiddenNeuronInit(this, nextLayerSize, bias)

        end subroutine nt_inputNeuronInit
        
        subroutine nt_outputNeuronInit(this)
            type(nt_Neuron) :: this
            
            this%nextLayerSize = 0

        end subroutine nt_outputNeuronInit
             

end module nt_NeuronModule
