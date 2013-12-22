subroutine init_net(this, topology, n, eta, alpha)
    use types
    type(Net) :: this
    integer,dimension(0:n-1) :: topology
    integer :: n
    real :: eta, alpha
    this%lnum = n-1
        allocate(this%layers(0:this%lnum)) 
        call init_layer(this%layers(this%lnum),topology(this%lnum),0,eta,alpha) 
            
        init_loop: do, i=0, this%lnum-1
            call init_layer(this%layers(i),topology(i),topology(i+1),eta,alpha)
        end do init_loop
 
end subroutine init_net
