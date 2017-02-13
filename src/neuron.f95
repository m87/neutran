module nt_NeuronModule
    use nt_TypesModule
    
    public :: nt_hiddenNeuronInit, nt_outputNeuronInit, nt_inputNeuronInit

    contains

        subroutine nt_hiddenNeuronInit(this, nextLayerSize, weightInitMethod, weightInitMethodArgs)
            type(nt_Neuron) :: this
            integer :: nextLayerSize
            real, intent(in) :: weightInitMethodArgs(0:)
            interface 
                subroutine weightInitMethod(weight, args)
                    real, intent(out) :: weight
                    real, intent(in) :: args(0:)
                end subroutine weightInitMethod
            end interface

            allocate(this%synapses(0:nextLayerSize))
            init_loop: do, i=0, nextLayerSize
               call weightInitMethod(this%synapses(i)%weight, weightInitMethodArgs)
               this%synapses(i)%delta = 0.0 
            end do init_loop
            
            this%nextLayerSize = nextLayerSize

        end subroutine nt_hiddenNeuronInit
        
        subroutine nt_inputNeuronInit(this, nextLayerSize, weightInitMethod, weightInitMethodArgs)
            type(nt_Neuron) :: this
            integer :: nextLayerSize
            real, intent(in) :: weightInitMethodArgs(0:)
            interface 
                subroutine weightInitMethod(weight, args)
                    real, intent(out) :: weight
                    real, intent(in) :: args(0:)
                end subroutine weightInitMethod
            end interface

            call nt_hiddenNeuronInit(this, nextLayerSize, weightInitMethod, weightInitMethodArgs)

        end subroutine nt_inputNeuronInit
        
        subroutine nt_outputNeuronInit(this)
            type(nt_Neuron) :: this
            
            this%nextLayerSize = 0

        end subroutine nt_outputNeuronInit
             

end module nt_NeuronModule
