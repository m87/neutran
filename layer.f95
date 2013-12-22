   
    subroutine init_layer(this,ni,no,eta, alpha)
        use types
        type(layer) :: this
        integer :: ni,i,no
        real :: eta, alpha
        this%n = ni-1          
        allocate(this%neurons(0:this%n)) 
        init_loop: do, i=0, this%n
            call init_Neuron(this%neurons(i),no,i,eta,alpha) 
        end do init_loop
 
    end subroutine init_layer 


