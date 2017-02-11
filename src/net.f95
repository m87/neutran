module nt_NetModule
    use nt_TypesModule
    use nt_LayerModule

    public :: nt_netInit

    contains

        subroutine nt_netInit(this, topology, bias)
            type(nt_Net) :: this
            integer, dimension(0:) :: topology
            logical :: bias
               
            this%topology = topology
            allocate(this%layers(0:size(topology)))
            call nt_layerInitFirst(this%layers(0), topology(0), topology(1), bias)

            init_loop: do, i=1, size(topology) - 1
                call nt_layerInit(this%layers(i), topology(i), topology(i+1), i, bias)
            end do init_loop
            
            call nt_layerInitLast(this%layers(size(topology)-1), topology(size(topology)-1), bias, size(topology)-1)


        end subroutine


end module nt_NetModule
