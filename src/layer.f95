module nt_LayerModule
    use nt_TypesModule
    use nt_NeuronModule
    implicit none

    public :: nt_layerInit, nt_layerInitFirst, nt_layerInitLast

    interface nt_layerInit
        module procedure nt_layerInit_hidden, nt_layerInit_last, nt_layerInit_first
    end interface nt_layerInit

    contains

        subroutine nt_layerInit_hidden(this, layerSize, nextLayerSize, id, bias)
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

        end subroutine nt_layerInit_hidden

        subroutine nt_layerInit_first(this, layerSize, nextLayerSize, bias)
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

        end subroutine nt_layerInit_first

        subroutine nt_layerInit_last(this, layerSize, bias, lastId)
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



        end subroutine nt_layerInit_last

        subroutine nt_layerInitFirst(this, layerSize, nextLayerSize, bias)
            type(nt_Layer) :: this
            integer :: layerSize
            integer :: nextLayerSize
            logical :: bias
            call nt_layerInit_first(this, layerSize, nextLayerSize, bias)
        end subroutine nt_layerInitFirst

        subroutine nt_layerInitLast(this, layerSize, bias, lastId)
            type(nt_Layer) :: this
            integer :: layerSize
            integer :: lastId
            logical :: bias
            call nt_layerInit_last(this, layerSize, bias, lastId)
        end subroutine nt_layerInitLast

end module nt_LayerModule
