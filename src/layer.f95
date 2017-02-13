module nt_LayerModule
    use nt_TypesModule
    use nt_NeuronModule
    implicit none

    public :: nt_hiddenLayerInit, nt_firstLayerInit, nt_lastLayerInit

    contains

        subroutine nt_hiddenLayerInit(this, layerSize, nextLayerSize, id, bias)
            type(nt_Layer) :: this
            integer :: layerSize
            integer :: nextLayerSize
            integer :: id
            logical :: bias
            integer :: i

            this%layerSize = layerSize
            this%id = id

            allocate(this%neurons(0:layerSize))
            init_loop: do, i=0, layerSize
               call nt_neuronInit(this%neurons(i), nextLayerSize, bias)
            end do init_loop

        end subroutine nt_hiddenLayerInit

        subroutine nt_firstLayerInit(this, layerSize, nextLayerSize, bias)
            type(nt_Layer) :: this
            integer :: layerSize
            integer :: nextLayerSize
            integer :: i
            logical :: bias

            this%layerSize = layerSize
            this%id = 0

            allocate(this%neurons(0:layerSize))
            init_loop: do, i=0, layerSize
               call nt_neuronInit(this%neurons(i), nextLayerSize, bias)
            end do init_loop

        end subroutine nt_firstLayerInit

        subroutine nt_lastLayerInit(this, layerSize, bias, lastId)
            type(nt_Layer) :: this
            integer :: layerSize
            integer :: lastId
            integer :: i
            logical :: bias

            this%layerSize = layerSize
            this%id = lastId

            allocate(this%neurons(0:layerSize))
            init_loop: do, i=0, layerSize
               call nt_neuronInit(this%neurons(i), 0, bias)
            end do init_loop

        end subroutine nt_lastLayerInit

end module nt_LayerModule
