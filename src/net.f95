module nt_NetModule
    use nt_TypesModule
    use nt_LayerModule

    public :: nt_netInit, nt_train, nt_test, nt_classify, nt_trainAll

    contains

        subroutine nt_netInit(this, topology, bias)
            type(nt_Net) :: this
            integer, dimension(0:) :: topology
            logical :: bias
               
            this%topology = topology
            allocate(this%layers(0:size(topology)))
            call nt_firstLayerInit(this%layers(0), topology(0), topology(1), bias)

            init_loop: do, i=1, size(topology) - 1
                call nt_hiddenLayerInit(this%layers(i), topology(i), topology(i+1), i, bias)
            end do init_loop
            
            call nt_lastLayerInit(this%layers(size(topology)-1), topology(size(topology)-1), bias, size(topology)-1)

        end subroutine

        subroutine nt_train(this)
            type(nt_Net) :: this
        end subroutine nt_train    

        subroutine nt_trainAll(this)
            type(nt_Net) :: this
        end subroutine nt_trainAll

        subroutine nt_test(this)
            type(nt_Net) :: this
        end subroutine nt_test

        subroutine nt_classify(this)
            type(nt_Net) :: this
        end subroutine nt_classify    




end module nt_NetModule
