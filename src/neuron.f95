module nt_NeuronModule
    use nt_TypesModule
    
    public :: nt_hiddenNeuronInit, nt_outputNeuronInit, nt_inputNeuronInit

    interface nt_neuronFeed
        module procedure nt_neuronFeed_default, nt_neuronFeed_custom
    end interface

    contains

        subroutine nt_hiddenNeuronInit(this, nextLayerSize, weightInitMethod, weightInitMethodArgs, bias)
            type(nt_Neuron) :: this
            integer :: nextLayerSize, layerSize
            real, intent(in) :: weightInitMethodArgs(0:)
            logical :: bias

            interface 
                subroutine weightInitMethod(weight, args)
                    real, intent(out) :: weight
                    real, intent(in) :: args(0:)
                end subroutine weightInitMethod
            end interface

            allocate(this%synapses(0:nextLayerSize - 1))
            init_loop: do, i=0, nextLayerSize
               call weightInitMethod(this%synapses(i)%weight, weightInitMethodArgs)
               this%synapses(i)%delta = 0.0 
            end do init_loop

            if (bias) then
                this%synapses(nextLayerSize - 1)%weight = 1.0
            end if

            this%nextLayerSize = nextLayerSize

        end subroutine nt_hiddenNeuronInit
        
        subroutine nt_inputNeuronInit(this, nextLayerSize, weightInitMethod, weightInitMethodArgs, bias)
            type(nt_Neuron) :: this
            integer :: nextLayerSize
            real, intent(in) :: weightInitMethodArgs(0:)
            logical :: bias

            interface 
                subroutine weightInitMethod(weight, args)
                    real, intent(out) :: weight
                    real, intent(in) :: args(0:)
                end subroutine weightInitMethod
            end interface

            call nt_hiddenNeuronInit(this, nextLayerSize, weightInitMethod, weightInitMethodArgs, bias)

        end subroutine nt_inputNeuronInit
        
        subroutine nt_outputNeuronInit(this)
            type(nt_Neuron) :: this
            
            this%nextLayerSize = 0

        end subroutine nt_outputNeuronInit

        subroutine nt_neuronFeed_default(this, previousLayer)
            type(nt_Neuron) :: this
            type(nt_Layer) :: previousLayer

            call nt_neuronFeed_default(this, previousLayer, logistc, (/ 1.0 /))

        end subroutine nt_neuronFeed_default     

        subroutine nt_neuronFeed_custom(this, previousLayer, activationFunction, args)
            type(nt_Neuron) :: this
            type(nt_Layer) :: previousLayer
            real, intent(in) :: args(0:)

            interface
                function activationFunction(x, args) result(fx)
                    real, intent(in) :: x
                    real, intent(in) :: args(0:)
                    real :: fx
                end function activationFunction
            end interface

        end subroutine nt_neuronFeed_custom

end module nt_NeuronModule
