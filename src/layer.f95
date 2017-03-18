module nt_LayerModule
    use nt_TypesModule
    use nt_NeuronModule
    use nt_InitMethodsModule
    use nt_FunctionsModule
    implicit none

    public :: nt_layerInit

    interface nt_layerInit
        module procedure nt_layerInit_random, nt_layerInit_custom
    end interface

    interface nt_layerFeed
        module procedure nt_layerFeed_default, nt_layerFeed_custom
    end interface nt_layerFeed

    contains
        subroutine nt_layerInit_custom(this, layerType, layerSize, nextLayerSize, id, weightInitMethod, weightInitMethodArgs, bias)
            type(nt_Layer) :: this
            integer :: layerSize
            integer :: nextLayerSize
            integer :: id
            logical :: bias
            integer :: i
            integer :: layerType
            real :: weightInitMethodArgs(0:)
            interface
                subroutine weightInitMethod(weight, args)
                    real, intent(out) :: weight
                    real, intent(in) :: args(0:)
                end subroutine weightInitMethod
            end interface

            if (bias) then
                layerSize = layerSize + 1
            end if

            this%layerSize = layerSize
            this%id = id
            
            allocate(this%neurons(0:layerSize-1))
            
            select case (layerType)
                case (0)
                    do, i=0, layerSize - 1
                       call nt_inputNeuronInit(this%neurons(i), nextLayerSize, weightInitMethod, weightInitMethodArgs, bias, i)
                    end do    
                case (2)
                    output_loop: do, i=0, layerSize - 1
                        call nt_outputNeuronInit(this%neurons(i))
                    end do output_loop
                case default
                    hidden_loop: do, i=0, layerSize - 1
                        call nt_hiddenNeuronInit(this%neurons(i), nextLayerSize, weightInitMethod, weightInitMethodArgs, bias, i)
                    end do hidden_loop
            end select

        end subroutine nt_layerInit_custom

        subroutine nt_layerInit_random(this, layerType, layerSize, nextLayerSize, id, bias)
            type(nt_Layer) :: this
            integer :: layerSize
            integer :: nextLayerSize
            integer :: id
            logical :: bias
            integer :: i
            integer :: layerType

            call nt_layerInit_custom(this, layerType, layerSize, nextLayerSize, id, randomInit, (/ 0.0 /), bias)

        end subroutine nt_layerInit_random

        subroutine nt_layerFeed_default(previous, next)
            type(nt_Layer) :: previous
            type(nt_Layer) :: next
            
            call nt_layerFeed_custom(previous, next, nt_logistic, (/ 1.0 /))           
        end subroutine nt_layerFeed_default

        subroutine nt_layerFeed_custom(previous, next, activationFunction, args)
            type(nt_Layer) :: previous
            type(nt_Layer) :: next
            real, intent(in) :: args(0:)
            integer :: i

            interface
                function activationFunction(x, args) result(fx)
                    real, intent(in) :: x
                    real, intent(in) :: args(0:)
                    real :: fx
                end function activationFunction
            end interface

            do, i = 0, next%layerSize -1
                call nt_neuronFeed(next%neurons(i), previous, activationFunction, args)
            end do

        end subroutine nt_layerFeed_custom

        subroutine nt_layerFeedInput(this, input)
            type(nt_Layer) :: this
            real :: input(0:)
            integer :: i

            do, i = 0, this%layerSize - 1
                this%neurons(i)%output = input(i)
            end do

        end subroutine nt_layerFeedInput

        subroutine nt_gradient(this, layer)
            type(nt_Layer) :: this
            type(nt_Layer) :: layer
        end subroutine

end module nt_LayerModule
