module neutran_Neuron
    use neutran_Types
    
    public :: neuron_init

    contains

        subroutine neuron_init(this, nextLayerSize, bias)
            type(Neuron) :: this
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
               this%weightsSize = layerSize
            end do init_loop
            this%delta  = 0.0

        end subroutine neuron_init
            

end module neutran_Neuron
