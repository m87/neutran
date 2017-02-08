module nt_NetModule
    use nt_TypesModule

    public :: nt_netInit

    contains

        subroutine nt_netInit(this, topology)
            type(nt_Net) :: this
            integer, dimension(:) :: topology
               
            this%topology = topology

        end subroutine


end module nt_NetModule
