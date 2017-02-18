module nt_LayerModule
    use nt_TypesModule
    use nt_NeuronModule
    use nt_InitMethodsModule
    implicit none

    public :: nt_layerInit

    interface nt_layerInit
        module procedure nt_layerInit_random, nt_layerInit_custom
    end interface

    contains
        subroutine nt_layerInit_custom(this, layerType, layerSize, nextLayerSize, id, weightInitMethod, weightInitMethodArgs)
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

            this%layerSize = layerSize
            this%id = id
            
            allocate(this%neurons(0:layerSize - 1))
            
            if (layerType .eq. 0) then
                do, i=0, layerSize
                   call nt_inputNeuronInit(this%neurons(i), nextLayerSize, weightInitMethod, weightInitMethodArgs)
                end do
            !else if (layerType .eq. 2) then
            !    output_loop: do, i=0, layerSize
            !        call nt_outputNeuronInit(this%neurons(i))
            !    end do output_loop
            !else
            !    hidden_loop: do, i=0, layerSize
            !        call nt_hiddenNeuronInit(this%neurons(i), nextLayerSize, weightInitMethod, weightInitMethodArgs)
            !    end do hidden_loop
            end if


        end subroutine nt_layerInit_custom

        subroutine nt_layerInit_random(this, layerType, layerSize, nextLayerSize, id)
            type(nt_Layer) :: this
            integer :: layerSize
            integer :: nextLayerSize
            integer :: id
            logical :: bias
            integer :: i
            integer :: layerType

            call nt_layerInit_custom(this, layerType, layerSize, nextLayerSize, id, randomInit, (/ 0.0 /))

        end subroutine nt_layerInit_random



end module nt_LayerModule
